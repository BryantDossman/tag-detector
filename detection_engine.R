# detection_engine.R
# Core detection engine for VHF tag identification

# Requires: pattern_matching.R

#' Main tag detection function
#' @param pulse_data Data frame with pulse information
#' @param tag_database Data frame with tag definitions  
#' @param pulse_slop_ms Timing tolerance for pulse gaps (ms)
#' @param freq_slop_khz Frequency consistency tolerance (kHz)
#' @param sig_slop_db Signal strength consistency tolerance (dB)
#' @param min_match_score Minimum score threshold for valid detections
#' @param strict_mode Use stricter validation criteria
#' @param verbose Print progress messages
#' @return List of detection objects
detect_known_tags <- function(pulse_data,
                              tag_database,
                              pulse_slop_ms = 1.5,
                              freq_slop_khz = 2,
                              sig_slop_db = 10,
                              min_match_score = 0.7,
                              strict_mode = FALSE,
                              max_pulse_rate = 0,
                              verbose = TRUE,
                              ...) {
  
  if (nrow(pulse_data) < 4) {
    if (verbose) {
      cat("Insufficient pulse data for detection (need at least 4 pulses)\n")
    }
    return(list())
  }
  
  # Sort pulses chronologically
  pulse_data <- pulse_data[order(pulse_data$timestamp), ]
  
  # Apply rate limiting if specified
  if (max_pulse_rate > 0) {
    pulse_data <- apply_rate_limiting(pulse_data, max_pulse_rate = max_pulse_rate, verbose = verbose)
  }
  
  if (verbose) {
    cat("Searching", nrow(pulse_data), "pulses for", nrow(tag_database), "known tags...\n")
  }
  
  detected_tags <- list()
  n_pulses <- nrow(pulse_data)
  used_indices <- c()  # Greedy approach: once used, pulses can't be reused
  
  # Sliding window search for 4-pulse patterns
  for (i in 1:(n_pulses - 3)) {
    candidate_indices <- i:(i + 3)
    
    if (any(candidate_indices %in% used_indices)) {
      next
    }
    
    candidate_burst <- pulse_data[candidate_indices, ]
    
    # Find best matching tag pattern
    best_match <- find_best_tag_match(
      candidate_burst, 
      tag_database, 
      min_score = min_match_score,
      pulse_slop_ms = pulse_slop_ms,
      freq_slop_khz = freq_slop_khz,
      sig_slop_db = sig_slop_db
    )
    
    # If we found a good match, create detection object
    if (!is.null(best_match)) {
      detection <- create_detection_object(candidate_burst, best_match, candidate_indices)
      
      # Validate detection quality
      if (validate_detection(detection, strict_mode = strict_mode)) {
        detected_tags <- append(detected_tags, list(detection))
        used_indices <- c(used_indices, candidate_indices)
        
        if (verbose) {
          cat(sprintf("Found Tag %d at time %.3f (score: %.3f)\n", 
                      best_match$tag_id, candidate_burst$timestamp[1], best_match$score))
        }
      }
    }
  }
  
  if (verbose) {
    cat("Detection complete:", length(detected_tags), "tags found\n")
  }
  
  return(detected_tags)
}

#' Apply rate limiting to pulse data (prevents excessive processing during noise bursts)
#' @param pulse_data Data frame with pulse data
#' @param max_pulse_rate Maximum pulses per second
#' @param pulse_rate_window Time window for rate calculation (seconds)
#' @param verbose Print filtering messages
#' @return Filtered pulse data
apply_rate_limiting <- function(pulse_data, max_pulse_rate = 100, 
                                pulse_rate_window = 60, verbose = TRUE) {
  
  if (max_pulse_rate <= 0 || nrow(pulse_data) == 0) {
    return(pulse_data)
  }
  
  n_pulses <- nrow(pulse_data)
  keep_indices <- rep(TRUE, n_pulses)
  discarded_windows <- 0
  
  for (i in 1:n_pulses) {
    window_start <- pulse_data$timestamp[i]
    window_end <- window_start + pulse_rate_window
    
    # Count pulses in window
    window_mask <- pulse_data$timestamp >= window_start & 
      pulse_data$timestamp <= window_end
    window_pulse_count <- sum(window_mask)
    
    # If rate too high, mark pulses in this window for removal
    if (window_pulse_count / pulse_rate_window > max_pulse_rate) {
      keep_indices[window_mask] <- FALSE
      discarded_windows <- discarded_windows + 1
    }
  }
  
  filtered_data <- pulse_data[keep_indices, ]
  
  if (verbose && nrow(filtered_data) < nrow(pulse_data)) {
    cat("Rate limiting: discarded", nrow(pulse_data) - nrow(filtered_data), 
        "pulses from", discarded_windows, "high-rate periods\n")
  }
  
  return(filtered_data)
}

#' Detect burst sequences from the same tag
#' @param detections List of individual detection objects
#' @param tag_database Data frame with tag definitions
#' @param burst_slop_ms Tolerance for burst interval timing (ms)
#' @param max_skipped_bursts Maximum consecutive missed bursts
#' @param min_bursts_to_confirm Minimum bursts needed to confirm sequence
#' @param verbose Print progress messages
#' @return List of burst sequence objects
detect_burst_sequences <- function(detections, tag_database, 
                                   burst_slop_ms = 20,
                                   max_skipped_bursts = 3,
                                   min_bursts_to_confirm = 2,
                                   verbose = TRUE) {
  
  if (length(detections) == 0) {
    return(list())
  }
  
  # Group detections by tag ID
  tag_groups <- split(detections, sapply(detections, function(x) x$tag_id))
  
  sequences <- list()
  
  for (tag_id in names(tag_groups)) {
    tag_detections <- tag_groups[[tag_id]]
    
    # Get expected burst interval for this tag
    tag_info <- tag_database[tag_database$id == as.numeric(tag_id), ]
    if (nrow(tag_info) == 0) next
    
    expected_bi <- tag_info$bi[1]  # Expected burst interval in seconds
    
    # Sort detections by time
    detection_times <- sapply(tag_detections, function(x) x$start_time)
    sorted_indices <- order(detection_times)
    sorted_detections <- tag_detections[sorted_indices]
    
    # Find sequences of bursts
    tag_sequences <- find_burst_sequences(sorted_detections, expected_bi, 
                                          burst_slop_ms, max_skipped_bursts,
                                          min_bursts_to_confirm)
    
    sequences <- c(sequences, tag_sequences)
  }
  
  if (verbose) {
    cat("Found", length(sequences), "burst sequences\n")
  }
  
  return(sequences)
}

#' Find sequences of bursts from a single tag
#' @param detections Sorted list of detections from one tag
#' @param expected_bi Expected burst interval (seconds)
#' @param burst_slop_ms Burst interval tolerance (ms)
#' @param max_skipped Maximum skipped bursts allowed
#' @param min_bursts Minimum bursts for valid sequence
#' @return List of sequence objects
find_burst_sequences <- function(detections, expected_bi, burst_slop_ms, 
                                 max_skipped, min_bursts) {
  
  if (length(detections) < min_bursts) {
    return(list())
  }
  
  sequences <- list()
  used_detections <- logical(length(detections))
  
  for (i in 1:(length(detections) - min_bursts + 1)) {
    if (used_detections[i]) next
    
    # Start a new sequence
    current_sequence <- list(detections[[i]])
    used_detections[i] <- TRUE
    last_time <- detections[[i]]$start_time
    skipped_count <- 0
    
    # Look for subsequent bursts
    for (j in (i + 1):length(detections)) {
      if (used_detections[j]) next
      
      current_time <- detections[[j]]$start_time
      time_diff <- current_time - last_time
      expected_diff <- expected_bi * (skipped_count + 1)
      
      # Check if this burst fits the expected pattern
      if (abs(time_diff - expected_diff) <= (burst_slop_ms / 1000)) {
        # This burst fits - add it to sequence
        current_sequence <- append(current_sequence, list(detections[[j]]))
        used_detections[j] <- TRUE
        last_time <- current_time
        skipped_count <- 0
      } else if (time_diff > expected_diff && skipped_count < max_skipped) {
        # Possible skipped burst - continue looking
        skipped_count <- skipped_count + 1
      } else {
        # Too far off or too many skipped - end this sequence
        break
      }
    }
    
    # Save sequence if it meets minimum length
    if (length(current_sequence) >= min_bursts) {
      seq_obj <- create_sequence_object(current_sequence, expected_bi)
      sequences <- append(sequences, list(seq_obj))
    }
  }
  
  return(sequences)
}

#' Create sequence object from list of detections
#' @param detections List of detection objects in sequence
#' @param expected_bi Expected burst interval
#' @return Sequence object
create_sequence_object <- function(detections, expected_bi) {
  start_times <- sapply(detections, function(x) x$start_time)
  
  # Calculate actual burst intervals
  if (length(start_times) > 1) {
    actual_intervals <- diff(start_times)
    interval_errors <- abs(actual_intervals - expected_bi)
  } else {
    actual_intervals <- numeric(0)
    interval_errors <- numeric(0)
  }
  
  sequence <- list(
    tag_id = detections[[1]]$tag_id,
    tag_freq_mhz = detections[[1]]$tag_freq_mhz,
    codeset = detections[[1]]$codeset,
    burst_count = length(detections),
    start_time = min(start_times),
    end_time = max(start_times),
    duration_s = max(start_times) - min(start_times),
    expected_bi_s = expected_bi,
    actual_intervals_s = actual_intervals,
    mean_interval_s = ifelse(length(actual_intervals) > 0, mean(actual_intervals), NA),
    max_interval_error_s = ifelse(length(interval_errors) > 0, max(interval_errors), 0),
    mean_match_score = mean(sapply(detections, function(x) x$match_score)),
    detections = detections
  )
  
  return(sequence)
}
