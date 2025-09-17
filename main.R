# main.R
# Main script for VHF tag detection analysis (TEXT FILES ONLY)

# Load all required functions
source("tag_database.R")
source("file_io.R") 
source("pattern_matching.R")
source("detection_engine.R")
source("batch_processing.R")

#' Main function to run tag detection analysis
#' @param input_path Path to single file or directory
#' @param tag_db_file Path to tag database CSV file
#' @param output_file Output CSV filename
#' @param file_pattern Pattern for batch processing (if input_path is directory)
#' @param validate_files Run file format validation before processing
#' @param ... Additional parameters for detection algorithm
#' @return Analysis results
run_tag_analysis <- function(input_path,
                             tag_db_file = "tags.csv",
                             output_file = "tag_detections.csv",
                             file_pattern = "*.txt",
                             validate_files = TRUE,
                             ...) {
  
  cat("=== VHF TAG DETECTION ANALYSIS (TEXT FILES ONLY) ===\n")
  cat("Input:", input_path, "\n")
  cat("Tag database:", tag_db_file, "\n")
  cat("Output file:", output_file, "\n\n")
  
  # Load tag database
  if (file.exists(tag_db_file)) {
    tag_db <- load_tag_database(tag_db_file)
    display_tag_summary(tag_db)
  } else {
    cat("Tag database file not found, using default database\n")
    tag_db <- create_default_tag_database()
    display_tag_summary(tag_db)
  }
  
  # Determine if input is file or directory
  if (file.exists(input_path)) {
    if (dir.exists(input_path)) {
      # Directory - batch processing
      cat("Processing directory:", input_path, "\n")
      cat("File pattern:", file_pattern, "\n\n")
      
      results <- process_detection_files(
        file_pattern = file_pattern,
        directory = input_path,
        tag_database = tag_db,
        output_file = output_file,
        validate_files = validate_files,
        ...
      )
      
    } else {
      # Single file
      cat("Processing single file:", input_path, "\n\n")
      
      pulse_data <- read_detection_file(input_path)
      pulse_data <- filter_pulse_data(pulse_data, ...)
      
      detections <- detect_known_tags(pulse_data, tag_database = tag_db, ...)
      
      # Create results structure similar to batch processing
      detection_summary <- if (length(detections) > 0) {
        create_detection_summary(detections)
      } else {
        data.frame()
      }
      
      if (nrow(detection_summary) > 0) {
        write.csv(detection_summary, output_file, row.names = FALSE)
        cat("Saved", nrow(detection_summary), "detections to:", output_file, "\n")
      }
      
      results <- list(
        detections = detections,
        summary = detection_summary,
        file_summaries = create_file_summary(input_path, pulse_data, detections),
        total_files = 1
      )
    }
  } else {
    stop("Input path does not exist: ", input_path)
  }
  
  # Print final summary
  print_analysis_summary(results)
  
  return(results)
}

#' Print analysis summary
#' @param results Analysis results list
print_analysis_summary <- function(results) {
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Files processed:", results$total_files, "\n")
  cat("Total detections:", length(results$detections), "\n")
  
  if (length(results$detections) > 0) {
    # Tag breakdown
    tag_ids <- sapply(results$detections, function(x) x$tag_id)
    tag_counts <- table(tag_ids)
    
    cat("\nDetections by tag:\n")
    for (tag in names(tag_counts)) {
      cat(sprintf("  Tag %s: %d detections\n", tag, tag_counts[tag]))
    }
    
    # Quality summary
    scores <- sapply(results$detections, function(x) x$match_score)
    timing_errors <- sapply(results$detections, function(x) x$max_timing_error_ms)
    
    cat(sprintf("\nQuality summary:\n"))
    cat(sprintf("  Mean match score: %.3f (range: %.3f - %.3f)\n", 
                mean(scores), min(scores), max(scores)))
    cat(sprintf("  Mean timing error: %.2f ms (max: %.2f ms)\n",
                mean(timing_errors), max(timing_errors)))
    
    # Time span
    if (nrow(results$summary) > 0) {
      time_span <- max(results$summary$start_time) - min(results$summary$start_time)
      cat(sprintf("  Detection time span: %.2f hours\n", time_span / 3600))
    }
  }
  
  cat("\n")
}

# Example usage functions

#' Quick analysis with default parameters
#' @param input_path Path to file or directory
#' @return Analysis results
quick_analysis <- function(input_path) {
  return(run_tag_analysis(input_path))
}

#' Strict analysis with tighter parameters
#' @param input_path Path to file or directory
#' @return Analysis results  
strict_analysis <- function(input_path) {
  return(run_tag_analysis(
    input_path,
    pulse_slop_ms = 1.0,      # Tighter timing
    min_match_score = 0.8,    # Higher score threshold
    strict_mode = TRUE
  ))
}

#' Analysis for noisy environment
#' @param input_path Path to file or directory
#' @return Analysis results
noisy_environment_analysis <- function(input_path) {
  return(run_tag_analysis(
    input_path,
    max_pulse_rate = 50,      # Rate limiting
    pulse_rate_window = 30,   # 30 second windows
    min_match_score = 0.75,   # Slightly higher threshold
    sig_slop_db = 8           # Tighter signal consistency
  ))
}
