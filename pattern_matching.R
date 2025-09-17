# pattern_matching.R
# Core pattern matching functions for VHF tag detection

#' Check if pulse sequence matches a specific tag pattern
#' @param pulse_subset Data frame with 4 consecutive pulses
#' @param tag_row Single row from tag database
#' @param pulse_slop_ms Timing tolerance in milliseconds
#' @param freq_slop_khz Frequency consistency tolerance in kHz
#' @param sig_slop_db Signal strength consistency tolerance in dB
#' @param snr_slop_db SNR consistency tolerance in dB (optional)
#' @param min_snr_db Minimum SNR threshold in dB (optional)
#' @return List with match status and score
matches_tag_pattern <- function(pulse_subset, tag_row, 
                                pulse_slop_ms = 1.5, 
                                freq_slop_khz = 2, 
                                sig_slop_db = 10,
                                snr_slop_db = 5,
                                min_snr_db = 3) {
  
  if (nrow(pulse_subset) != 4) {
    return(list(match = FALSE, score = 0, reason = "wrong_pulse_count"))
  }
  
  timestamps <- pulse_subset$timestamp
  intervals_s <- diff(timestamps)
  
  # Expected gaps for this tag (in seconds)
  expected_gaps <- c(tag_row$g1_s, tag_row$g2_s, tag_row$g3_s)
  
  # Check timing match
  timing_errors <- abs(intervals_s - expected_gaps)
  max_timing_error_ms <- max(timing_errors) * 1000
  
  if (max_timing_error_ms > pulse_slop_ms) {
    return(list(match = FALSE, score = 0, reason = "timing_mismatch", 
                max_error_ms = max_timing_error_ms))
  }
  
  # Check frequency consistency within burst
  freq_range <- max(pulse_subset$dfreq_khz) - min(pulse_subset$dfreq_khz)
  if (freq_range > freq_slop_khz) {
    return(list(match = FALSE, score = 0, reason = "frequency_inconsistent",
                freq_range = freq_range))
  }
  
  # Check signal strength consistency within burst
  sig_range <- max(pulse_subset$sig_db) - min(pulse_subset$sig_db)
  if (sig_range > sig_slop_db) {
    return(list(match = FALSE, score = 0, reason = "signal_inconsistent",
                sig_range = sig_range))
  }
  
  # Check SNR consistency and minimum threshold (if SNR data available)
  snr_score <- 1  # Default if no SNR data
  snr_range <- NA
  mean_snr <- NA
  
  if ("snr_db" %in% names(pulse_subset) && !all(is.na(pulse_subset$snr_db))) {
    mean_snr <- mean(pulse_subset$snr_db, na.rm = TRUE)
    snr_range <- max(pulse_subset$snr_db, na.rm = TRUE) - min(pulse_subset$snr_db, na.rm = TRUE)
    
    # Check minimum SNR threshold
    if (mean_snr < min_snr_db) {
      return(list(match = FALSE, score = 0, reason = "snr_too_low",
                  mean_snr = mean_snr))
    }
    
    # Check SNR consistency
    if (snr_range > snr_slop_db) {
      return(list(match = FALSE, score = 0, reason = "snr_inconsistent",
                  snr_range = snr_range))
    }
    
    # Calculate SNR component score
    snr_score <- max(0, 1 - (snr_range / snr_slop_db)) * 
      min(1, mean_snr / min_snr_db)  # Bonus for higher SNR
  }
  
  # Check frequency offset match with tag's expected offset
  mean_dfreq <- mean(pulse_subset$dfreq_khz)
  freq_offset_error <- abs(mean_dfreq - tag_row$dfreq)
  
  # Calculate component scores (all 0-1 range)
  timing_score <- max(0, 1 - (max_timing_error_ms / pulse_slop_ms))
  freq_score <- max(0, 1 - (freq_range / freq_slop_khz))
  sig_score <- max(0, 1 - (sig_range / sig_slop_db))
  offset_score <- max(0, 1 - (freq_offset_error / freq_slop_khz))
  
  # Weighted overall score (adjusted for SNR if available)
  if (!is.na(mean_snr)) {
    overall_score <- (timing_score * 0.3) + (freq_score * 0.15) + 
      (sig_score * 0.15) + (offset_score * 0.15) + (snr_score * 0.25)
  } else {
    overall_score <- (timing_score * 0.4) + (freq_score * 0.2) + 
      (sig_score * 0.2) + (offset_score * 0.2)
  }
  
  return(list(
    match = TRUE, 
    score = overall_score,
    timing_score = timing_score,
    freq_score = freq_score,
    sig_score = sig_score,
    offset_score = offset_score,
    snr_score = snr_score,
    timing_error_ms = max_timing_error_ms,
    freq_range = freq_range,
    sig_range = sig_range,
    snr_range = snr_range,
    mean_snr = mean_snr,
    freq_offset_error = freq_offset_error
  ))
}

#' Find best matching tag for a pulse sequence
#' @param pulse_subset Data frame with pulse data
#' @param tag_database Data frame with tag definitions
#' @param min_score Minimum score threshold for valid matches
#' @param ... Additional parameters passed to matches_tag_pattern
#' @return List with best match information or NULL if no good match
find_best_tag_match <- function(pulse_subset, tag_database, min_score = 0.7, ...) {
  
  if (nrow(pulse_subset) != 4) {
    return(NULL)
  }
  
  best_match <- list(tag_id = NA, score = 0, tag_row = NULL, details = NULL)
  
  # Test against each known tag pattern
  for (t in 1:nrow(tag_database)) {
    match_result <- matches_tag_pattern(pulse_subset, tag_database[t, ], ...)
    
    if (match_result$match && match_result$score > best_match$score) {
      best_match <- list(
        tag_id = tag_database$id[t],
        score = match_result$score,
        tag_row = tag_database[t, ],
        details = match_result
      )
    }
  }
  
  # Return best match only if it meets minimum score threshold
  if (!is.na(best_match$tag_id) && best_match$score >= min_score) {
    return(best_match)
  } else {
    return(NULL)
  }
}

#' Create detection object from matched pulse sequence
#' @param pulse_subset Data frame with 4 matched pulses
#' @param best_match Result from find_best_tag_match
#' @param pulse_indices Original indices of pulses in data
#' @return Detection object (list)
create_detection_object <- function(pulse_subset, best_match, pulse_indices) {
  
  intervals_s <- diff(pulse_subset$timestamp)
  
  detection <- list(
    tag_id = best_match$tag_id,
    tag_freq_mhz = best_match$tag_row$tagFreq,
    start_time = pulse_subset$timestamp[1],
    end_time = pulse_subset$timestamp[4],
    pulse_intervals_s = intervals_s,
    expected_intervals_s = c(best_match$tag_row$g1_s, 
                             best_match$tag_row$g2_s, 
                             best_match$tag_row$g3_s),
    mean_dfreq_khz = mean(pulse_subset$dfreq_khz),
    expected_dfreq_khz = best_match$tag_row$dfreq,
    mean_sig_db = mean(pulse_subset$sig_db),
    sig_range_db = max(pulse_subset$sig_db) - min(pulse_subset$sig_db),
    match_score = best_match$score,
    timing_score = best_match$details$timing_score,
    freq_score = best_match$details$freq_score,
    sig_score = best_match$details$sig_score,
    offset_score = best_match$details$offset_score,
    pulse_indices = pulse_indices,
    codeset = best_match$tag_row$codeset,
    project = best_match$tag_row$proj,
    # Timing accuracy metrics
    max_timing_error_ms = best_match$details$timing_error_ms,
    freq_range_khz = best_match$details$freq_range,
    freq_offset_error_khz = best_match$details$freq_offset_error
  )
  
  return(detection)
}

#' Validate detection quality
#' @param detection Detection object
#' @param strict_mode Use stricter validation criteria
#' @return TRUE if detection passes validation
validate_detection <- function(detection, strict_mode = FALSE) {
  
  # Basic score threshold
  min_score <- if (strict_mode) 0.8 else 0.7
  if (detection$match_score < min_score) {
    return(FALSE)
  }
  
  # Timing accuracy check
  max_timing_error <- if (strict_mode) 1.0 else 1.5  # ms
  if (detection$max_timing_error_ms > max_timing_error) {
    return(FALSE)
  }
  
  # Frequency consistency check
  max_freq_range <- if (strict_mode) 1.5 else 2.0  # kHz
  if (detection$freq_range_khz > max_freq_range) {
    return(FALSE)
  }
  
  # All component scores should be reasonable
  min_component_score <- if (strict_mode) 0.6 else 0.4
  if (detection$timing_score < min_component_score || 
      detection$freq_score < min_component_score) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Filter overlapping detections (greedy approach)
#' @param detections List of detection objects
#' @param max_overlap Maximum allowed pulse overlap
#' @return Filtered list of detections
filter_overlapping_detections <- function(detections, max_overlap = 1) {
  
  if (length(detections) <= 1) {
    return(detections)
  }
  
  # Sort detections by score (highest first) for greedy selection
  detection_scores <- sapply(detections, function(x) x$match_score)
  sorted_indices <- order(detection_scores, decreasing = TRUE)
  
  filtered_detections <- list()
  used_indices <- c()
  
  for (i in sorted_indices) {
    detection <- detections[[i]]
    detection_indices <- detection$pulse_indices
    
    # Check overlap with already accepted detections
    overlap_count <- length(intersect(detection_indices, used_indices))
    
    if (overlap_count <= max_overlap) {
      # Accept this detection
      filtered_detections <- append(filtered_detections, list(detection))
      used_indices <- c(used_indices, detection_indices)
    }
  }
  
  return(filtered_detections)
}