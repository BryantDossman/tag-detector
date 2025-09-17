# batch_processing.R
# Functions for processing multiple detection files (TEXT FILES ONLY)

# Requires: file_io.R, detection_engine.R, pattern_matching.R

#' Process multiple detection files
#' @param file_pattern Pattern for files to process (e.g., "*.txt", "*.dat")
#' @param directory Directory containing files
#' @param tag_database Data frame with tag definitions
#' @param output_file Output CSV filename for combined results
#' @param validate_files Run file format validation before processing
#' @param verbose Print progress messages
#' @param ... Additional parameters passed to detect_known_tags
#' @return List containing all results and summaries
process_detection_files <- function(file_pattern = "*.txt", 
                                    directory = ".",
                                    tag_database = NULL,
                                    output_file = "tag_detections_summary.csv",
                                    validate_files = TRUE,
                                    verbose = TRUE,
                                    ...) {
  
  # Find files matching pattern
  if (grepl("\\*", file_pattern)) {
    files <- list.files(path = directory, pattern = glob2rx(file_pattern), full.names = TRUE)
  } else {
    files <- file.path(directory, file_pattern)
  }
  
  if (length(files) == 0) {
    stop("No files found matching pattern: ", file_pattern, " in directory: ", directory)
  }
  
  # Filter for supported text file extensions only
  valid_extensions <- c("txt", "dat", "log")
  files <- files[tolower(tools::file_ext(files)) %in% valid_extensions]
  
  if (length(files) == 0) {
    stop("No text files found with supported extensions (.txt, .dat, .log) matching pattern: ", 
         file_pattern, " in directory: ", directory)
  }
  
  if (verbose) {
    cat("Found", length(files), "text files to process:\n")
    for (f in files) {
      cat(" -", basename(f), "\n")
    }
    cat("\n")
  }
  
  # Optional file validation step
  if (validate_files && verbose) {
    cat("Validating file formats...\n")
    for (f in files) {
      validation <- validate_text_file_format(f, show_samples = FALSE)
      if (!validation$valid) {
        cat("Warning: ", basename(f), " - no pulse data found\n")
      } else {
        cat("✓ ", basename(f), " - ", validation$pulse_count, " pulse lines\n")
      }
    }
    cat("\n")
  }
  
  all_detections <- list()
  file_summaries <- list()
  
  # Process each file
  for (i in seq_along(files)) {
    file <- files[i]
    if (verbose) {
      cat("=== Processing file", i, "of", length(files), ":", basename(file), "===\n")
    }
    
    file_result <- process_single_file(file, tag_database, verbose, ...)
    
    # Add file information to detections
    if (length(file_result$detections) > 0) {
      for (j in seq_along(file_result$detections)) {
        file_result$detections[[j]]$source_file <- basename(file)
        file_result$detections[[j]]$file_index <- i
      }
      all_detections <- c(all_detections, file_result$detections)
    }
    
    # Store file summary
    file_summaries[[i]] <- file_result$summary
    
    if (verbose) {
      cat("File processed:", file_result$summary$total_pulses, "pulses,", 
          file_result$summary$detections_found, "detections\n\n")
    }
  }
  
  # Create combined results
  results <- compile_batch_results(all_detections, file_summaries, files, 
                                   output_file, verbose)
  
  return(results)
}

#' Process a single detection file
#' @param file_path Path to file
#' @param tag_database Tag database
#' @param verbose Print messages
#' @param ... Parameters for detection
#' @return List with detections and summary
process_single_file <- function(file_path, tag_database, verbose, ...) {
  
  tryCatch({
    # Read the detection file
    pulse_data <- read_detection_file(file_path, verbose = verbose)
    
    if (is.data.frame(pulse_data) && nrow(pulse_data) > 0) {
      # Apply basic filtering
      pulse_data <- filter_pulse_data(pulse_data, verbose = verbose, ...)
      
      # Run tag detection
      detections <- detect_known_tags(pulse_data, tag_database = tag_database, 
                                      verbose = verbose, ...)
      
      # Create file summary
      summary <- create_file_summary(file_path, pulse_data, detections)
      
      return(list(detections = detections, summary = summary))
      
    } else {
      if (verbose) {
        cat("Could not process file or no pulse data found\n")
      }
      
      summary <- create_file_summary(file_path, data.frame(), list(), 
                                     error = "No pulse data found")
      return(list(detections = list(), summary = summary))
    }
    
  }, error = function(e) {
    if (verbose) {
      cat("Error processing file:", e$message, "\n")
    }
    
    summary <- create_file_summary(file_path, data.frame(), list(), 
                                   error = e$message)
    return(list(detections = list(), summary = summary))
  })
}

#' Create summary for a single file
#' @param file_path File path
#' @param pulse_data Pulse data frame
#' @param detections List of detections
#' @param error Optional error message
#' @return Summary data frame
create_file_summary <- function(file_path, pulse_data, detections, error = NULL) {
  
  time_span_hours <- 0
  if (nrow(pulse_data) > 1) {
    time_span_hours <- (max(pulse_data$timestamp) - min(pulse_data$timestamp)) / 3600
  }
  
  summary <- data.frame(
    file_name = basename(file_path),
    total_pulses = nrow(pulse_data),
    detections_found = length(detections),
    time_span_hours = time_span_hours,
    processing_time = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(error)) {
    summary$error <- error
  }
  
  return(summary)
}

#' Compile results from batch processing
#' @param all_detections Combined list of detections
#' @param file_summaries List of file summary data frames
#' @param files Vector of file paths
#' @param output_file Output filename
#' @param verbose Print messages
#' @return Compiled results list
compile_batch_results <- function(all_detections, file_summaries, files, 
                                  output_file, verbose) {
  
  if (verbose) {
    cat("=== BATCH PROCESSING COMPLETE ===\n")
    cat("Total detections across all files:", length(all_detections), "\n")
  }
  
  # Combine file summaries
  combined_file_summaries <- do.call(rbind, file_summaries)
  
  # Create detection summary if we have detections
  if (length(all_detections) > 0) {
    detection_summary <- create_detection_summary(all_detections)
    
    # Save results
    write.csv(detection_summary, output_file, row.names = FALSE)
    if (verbose) {
      cat("Saved", nrow(detection_summary), "detections to:", output_file, "\n")
    }
    
    # Print summary statistics
    print_batch_summary(detection_summary, verbose)
    
    return(list(
      detections = all_detections,
      summary = detection_summary,
      file_summaries = combined_file_summaries,
      total_files = length(files)
    ))
    
  } else {
    if (verbose) {
      cat("No detections found in any files\n")
    }
    
    return(list(
      detections = list(),
      summary = data.frame(),
      file_summaries = combined_file_summaries,
      total_files = length(files)
    ))
  }
}

#' Create detection summary data frame
#' @param detections List of detection objects
#' @return Data frame with detection summary
create_detection_summary <- function(detections) {
  
  detection_summary <- data.frame(
    detection_id = seq_along(detections),
    source_file = sapply(detections, function(x) 
      ifelse(is.null(x$source_file), "", x$source_file)),
    tag_id = sapply(detections, function(x) x$tag_id),
    tag_freq_mhz = sapply(detections, function(x) x$tag_freq_mhz),
    start_time = sapply(detections, function(x) x$start_time),
    datetime = as.POSIXct(sapply(detections, function(x) x$start_time), 
                          origin = "1970-01-01"),
    duration_s = sapply(detections, function(x) x$end_time - x$start_time),
    mean_dfreq_khz = sapply(detections, function(x) x$mean_dfreq_khz),
    mean_sig_db = sapply(detections, function(x) x$mean_sig_db),
    match_score = sapply(detections, function(x) x$match_score),
    timing_score = sapply(detections, function(x) x$timing_score),
    freq_score = sapply(detections, function(x) x$freq_score),
    max_timing_error_ms = sapply(detections, function(x) x$max_timing_error_ms),
    codeset = sapply(detections, function(x) x$codeset),
    project = sapply(detections, function(x) 
      ifelse(is.null(x$project), NA, x$project)),
    stringsAsFactors = FALSE
  )
  
  return(detection_summary)
}

#' Print batch processing summary
#' @param detection_summary Detection summary data frame
#' @param verbose Print detailed summary
print_batch_summary <- function(detection_summary, verbose = TRUE) {
  
  if (!verbose) return(invisible())
  
  cat("\n=== BATCH SUMMARY ===\n")
  
  # Detections by tag ID
  cat("Detections by tag ID:\n")
  tag_counts <- table(detection_summary$tag_id)
  for (tag_id in names(tag_counts)) {
    cat(sprintf("  Tag %s: %d detections\n", tag_id, tag_counts[tag_id]))
  }
  
  # Detections by file
  cat("\nDetections by file:\n")
  file_counts <- table(detection_summary$source_file)
  for (file_name in names(file_counts)) {
    cat(sprintf("  %s: %d detections\n", file_name, file_counts[file_name]))
  }
  
  # Quality metrics
  cat("\nQuality metrics:\n")
  cat(sprintf("  Mean match score: %.3f\n", mean(detection_summary$match_score)))
  cat(sprintf("  Mean timing error: %.2f ms\n", mean(detection_summary$max_timing_error_ms)))
  cat(sprintf("  Score range: %.3f - %.3f\n", 
              min(detection_summary$match_score), max(detection_summary$match_score)))
  
  cat("\n")
}

#' Convenience wrapper for common batch processing scenarios
#' @param directory Directory containing detection files
#' @param pattern File pattern to match (e.g., "*.txt", "*.dat")
#' @param tag_csv_file Path to tag database CSV
#' @param output_file Output filename
#' @param validate_files Check file formats before processing
#' @param ... Additional detection parameters
#' @return Batch processing results
batch_process_detections <- function(directory = ".", 
                                     pattern = "*.txt",
                                     tag_csv_file = "tags.csv",
                                     output_file = "batch_detections.csv",
                                     validate_files = TRUE,
                                     ...) {
  
  # Load tag database
  if (file.exists(tag_csv_file)) {
    source("tag_database.R")
    tag_db <- load_tag_database(tag_csv_file)
  } else {
    source("tag_database.R") 
    tag_db <- create_default_tag_database()
    cat("Using default tag database (", tag_csv_file, "not found)\n")
  }
  
  # Load required functions
  source("file_io.R")
  source("pattern_matching.R")
  source("detection_engine.R")
  
  # Process all files
  results <- process_detection_files(
    file_pattern = pattern,
    directory = directory,
    tag_database = tag_db,
    output_file = output_file,
    validate_files = validate_files,
    ...
  )
  
  return(results)
}