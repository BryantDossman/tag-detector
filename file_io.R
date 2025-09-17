# file_io.R
# Functions for reading and writing VHF detection files (TEXT FILES ONLY)

library(tools)

#' Read text detection file
#' @param filename Path to text detection file (.txt, .dat, .log)
#' @param verbose Print progress messages
#' @return Data frame with pulse data
read_detection_file <- function(filename, verbose = TRUE) {
  if (!file.exists(filename)) {
    stop("File not found: ", filename)
  }
  
  if (verbose) {
    cat("Reading detection file:", filename, "\n")
  }
  
  file_ext <- tolower(tools::file_ext(filename))
  
  if (file_ext %in% c("txt", "dat", "log")) {
    return(read_text_detection_file(filename, verbose))
  } else {
    stop("Unsupported file format. Only text files supported: .txt, .dat, .log")
  }
}

#' Read text-based detection file
#' @param filename Path to text file (.txt, .dat, .log)
#' @param verbose Print progress messages
#' @return Data frame with parsed pulse data
read_text_detection_file <- function(filename, verbose = TRUE) {
  lines <- readLines(filename, warn = FALSE)
  
  if (verbose) {
    cat("Read", length(lines), "lines from text file\n")
  }
  
  # Look for pulse data lines (format: p1,timestamp,dfreq,sig,noise)
  pulse_lines <- grep("^p1,", lines, value = TRUE)
  
  if (length(pulse_lines) == 0) {
    # Try other antenna patterns (p2, p3, etc.)
    pulse_lines <- grep("^p[0-9]+,", lines, value = TRUE)
  }
  
  if (length(pulse_lines) > 0) {
    if (verbose) {
      cat("Found", length(pulse_lines), "pulse data lines\n")
    }
    
    # Parse pulse lines into data frame
    pulse_data <- parse_pulse_lines(pulse_lines)
    
    if (nrow(pulse_data) == 0) {
      if (verbose) {
        cat("Warning: Found pulse lines but could not parse any valid data\n")
      }
    }
    
    return(pulse_data)
    
  } else {
    if (verbose) {
      cat("No pulse data lines found in expected format\n")
      cat("Expected format: pX,timestamp,dfreq_khz,sig_db,noise_db\n")
      cat("Sample of file content:\n")
      print(head(lines, 10))
    }
    return(data.frame())  # Return empty data frame
  }
}

#' Parse pulse data lines into data frame
#' @param pulse_lines Character vector of pulse data lines
#' @return Data frame with parsed pulse data
parse_pulse_lines <- function(pulse_lines) {
  valid_rows <- list()
  parse_errors <- 0
  
  for (i in seq_along(pulse_lines)) {
    line <- pulse_lines[i]
    parts <- strsplit(line, ",")[[1]]
    
    if (length(parts) >= 5) {
      tryCatch({
        # Parse each field with validation
        antenna <- trimws(parts[1])
        timestamp <- as.numeric(parts[2])
        dfreq_khz <- as.numeric(parts[3])
        sig_db <- as.numeric(parts[4])
        noise_db <- as.numeric(parts[5])
        
        # Handle optional SNR field (6th field)
        snr_db <- if (length(parts) >= 6) as.numeric(parts[6]) else NA
        
        # Basic validation
        if (is.na(timestamp) || is.na(dfreq_khz) || is.na(sig_db) || is.na(noise_db)) {
          parse_errors <- parse_errors + 1
          next
        }
        
        valid_rows[[length(valid_rows) + 1]] <- data.frame(
          antenna = antenna,
          timestamp = timestamp,
          dfreq_khz = dfreq_khz,
          sig_db = sig_db,
          noise_db = noise_db,
          snr_db = snr_db,
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        parse_errors <<- parse_errors + 1
      })
    } else {
      parse_errors <- parse_errors + 1
    }
  }
  
  if (length(valid_rows) == 0) {
    if (parse_errors > 0) {
      cat("Warning: Could not parse any of the", length(pulse_lines), "pulse lines\n")
      cat("Expected format: antenna,timestamp,dfreq_khz,sig_db,noise_db[,snr_db]\n")
      if (length(pulse_lines) > 0) {
        cat("Sample line:", pulse_lines[1], "\n")
      }
    }
    return(data.frame(antenna = character(0), timestamp = numeric(0), 
                      dfreq_khz = numeric(0), sig_db = numeric(0), 
                      noise_db = numeric(0), snr_db = numeric(0),
                      stringsAsFactors = FALSE))
  }
  
  pulse_data <- do.call(rbind, valid_rows)
  
  if (parse_errors > 0) {
    cat("Warning:", parse_errors, "lines could not be parsed\n")
  }
  
  return(pulse_data)
}

#' Validate text file format and provide diagnostic information
#' @param filename Path to text file
#' @param show_samples Show sample lines from file
#' @return List with validation results and diagnostics
validate_text_file_format <- function(filename, show_samples = TRUE) {
  if (!file.exists(filename)) {
    return(list(valid = FALSE, error = "File not found"))
  }
  
  lines <- readLines(filename, warn = FALSE, n = 100)  # Read first 100 lines for validation
  
  # Look for pulse data patterns
  pulse_patterns <- c(
    "^p1," = grep("^p1,", lines),
    "^p[0-9]+," = grep("^p[0-9]+,", lines),
    "timestamp" = grep("timestamp", lines, ignore.case = TRUE),
    "dfreq" = grep("dfreq", lines, ignore.case = TRUE)
  )
  
  pulse_line_count <- length(grep("^p[0-9]+,", lines))
  
  diagnostics <- list(
    total_lines_checked = length(lines),
    pulse_lines_found = pulse_line_count,
    file_size_bytes = file.info(filename)$size,
    patterns_found = pulse_patterns[lengths(pulse_patterns) > 0]
  )
  
  if (show_samples && length(lines) > 0) {
    cat("File format validation for:", basename(filename), "\n")
    cat("Total lines checked:", length(lines), "\n")
    cat("Pulse lines found:", pulse_line_count, "\n")
    
    if (pulse_line_count > 0) {
      cat("\nSample pulse lines:\n")
      sample_pulse_lines <- grep("^p[0-9]+,", lines, value = TRUE)[1:min(3, pulse_line_count)]
      for (line in sample_pulse_lines) {
        cat("  ", line, "\n")
      }
    } else {
      cat("\nNo pulse lines found. Sample file content:\n")
      for (i in 1:min(5, length(lines))) {
        cat("  ", lines[i], "\n")
      }
      cat("\nExpected format: pX,timestamp,dfreq_khz,sig_db,noise_db\n")
    }
  }
  
  return(list(
    valid = pulse_line_count > 0,
    pulse_count = pulse_line_count,
    diagnostics = diagnostics
  ))
}

#' Apply frequency filters to pulse data
#' @param pulse_data Data frame with pulse data
#' @param min_dfreq Minimum offset frequency (kHz)
#' @param max_dfreq Maximum offset frequency (kHz)
#' @param unsigned_dfreq Ignore frequency sign
#' @param verbose Print filtering messages
#' @return Filtered data frame
filter_pulse_data <- function(pulse_data, min_dfreq = -50, max_dfreq = 50, 
                              unsigned_dfreq = FALSE, verbose = TRUE, ...) {
  
  initial_count <- nrow(pulse_data)
  
  # Handle unsigned frequency option
  if (unsigned_dfreq) {
    pulse_data$dfreq_khz <- abs(pulse_data$dfreq_khz)
    if (verbose) {
      cat("Applied unsigned frequency conversion\n")
    }
  }
  
  # Apply frequency filters
  pulse_data <- pulse_data[pulse_data$dfreq_khz >= min_dfreq & 
                             pulse_data$dfreq_khz <= max_dfreq, ]
  
  filtered_count <- nrow(pulse_data)
  
  if (verbose && filtered_count < initial_count) {
    cat("Filtered", initial_count - filtered_count, "pulses outside frequency range [", 
        min_dfreq, ",", max_dfreq, "] kHz\n")
    cat("Remaining pulses:", filtered_count, "\n")
  }
  
  return(pulse_data)
}

#' Write detection results to CSV file
#' @param detections List of detection objects
#' @param filename Output CSV filename
#' @param verbose Print progress messages
write_detections_csv <- function(detections, filename, verbose = TRUE) {
  if (length(detections) == 0) {
    if (verbose) {
      cat("No detections to write\n")
    }
    return(invisible(NULL))
  }
  
  # Convert detections to data frame
  detection_df <- data.frame(
    detection_id = seq_along(detections),
    tag_id = sapply(detections, function(x) x$tag_id),
    tag_freq_mhz = sapply(detections, function(x) x$tag_freq_mhz),
    start_time = sapply(detections, function(x) x$start_time),
    datetime = as.POSIXct(sapply(detections, function(x) x$start_time), origin = "1970-01-01"),
    duration_s = sapply(detections, function(x) x$end_time - x$start_time),
    mean_dfreq_khz = sapply(detections, function(x) x$mean_dfreq_khz),
    mean_sig_db = sapply(detections, function(x) x$mean_sig_db),
    match_score = sapply(detections, function(x) x$match_score),
    codeset = sapply(detections, function(x) x$codeset),
    source_file = sapply(detections, function(x) ifelse(is.null(x$source_file), "", x$source_file)),
    stringsAsFactors = FALSE
  )
  
  write.csv(detection_df, filename, row.names = FALSE)
  
  if (verbose) {
    cat("Wrote", nrow(detection_df), "detections to:", filename, "\n")
  }
  
  return(invisible(detection_df))
}

#' Create file processing summary
#' @param results List containing processing results
#' @param output_file Optional filename for summary
#' @return Data frame with file processing summary
create_processing_summary <- function(results, output_file = NULL) {
  if (is.null(results$file_summaries) || nrow(results$file_summaries) == 0) {
    return(data.frame())
  }
  
  summary_df <- results$file_summaries
  
  # Add totals row
  totals <- data.frame(
    file_name = "TOTAL",
    file_index = NA,
    total_pulses = sum(summary_df$total_pulses, na.rm = TRUE),
    detections_found = sum(summary_df$detections_found, na.rm = TRUE),
    time_span_hours = sum(summary_df$time_span_hours, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  summary_df <- rbind(summary_df, totals)
  
  if (!is.null(output_file)) {
    write.csv(summary_df, output_file, row.names = FALSE)
    cat("Wrote processing summary to:", output_file, "\n")
  }
  
  return(summary_df)
}