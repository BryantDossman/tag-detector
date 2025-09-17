#!/usr/bin/env Rscript
# process_receiver_data.R
# Script to process VHF tag detection data from receiver_data folder

# Load the main detection system
source("main.R")

# Configuration
INPUT_DIR <- "receiver_data"
OUTPUT_FILE <- "receiver_detections.csv"
FILE_PATTERN <- "*.txt"

# Detection parameters (can be adjusted based on data quality)
DETECTION_PARAMS <- list(
  pulse_slop_ms = 1.5,      # Timing tolerance
  freq_slop_khz = 2.0,      # Frequency tolerance
  sig_slop_db = 10,         # Signal strength tolerance
  min_match_score = 0.7,    # Minimum detection score
  validate_files = TRUE,    # Check file formats first
  max_pulse_rate = 0        # Disable rate limiting (0 = off)
)

# Function to run the analysis
run_receiver_analysis <- function() {
  cat("=== RECEIVER DATA PROCESSING ===\n")
  cat("Input directory:", INPUT_DIR, "\n")
  cat("File pattern:", FILE_PATTERN, "\n")
  cat("Output file:", OUTPUT_FILE, "\n\n")
  
  # Check if input directory exists
  if (!dir.exists(INPUT_DIR)) {
    stop("Input directory not found: ", INPUT_DIR)
  }
  
  # Check if tag database exists
  if (file.exists("tags.csv")) {
    cat("Using tag database: tags.csv\n\n")
  } else {
    cat("Tag database (tags.csv) not found - using default database\n\n")
  }
  
  # Run the analysis
  results <- do.call(run_tag_analysis, c(
    list(
      input_path = INPUT_DIR,
      output_file = OUTPUT_FILE,
      file_pattern = FILE_PATTERN
    ),
    DETECTION_PARAMS
  ))
  
  # Additional summary output
  cat("\n=== PROCESSING SUMMARY ===\n")
  if (length(results$detections) > 0) {
    cat("Successfully processed receiver data!\n")
    cat("Check", OUTPUT_FILE, "for detailed results\n")
    
    # Create file-by-file summary
    if (!is.null(results$file_summaries) && nrow(results$file_summaries) > 0) {
      summary_file <- "receiver_file_summary.csv"
      write.csv(results$file_summaries, summary_file, row.names = FALSE)
      cat("File processing summary saved to:", summary_file, "\n")
    }
  } else {
    cat("No tag detections found in the receiver data\n")
    cat("This could indicate:\n")
    cat("  - No tags were present during recording\n")
    cat("  - Detection parameters too strict\n") 
    cat("  - Data format issues\n")
    cat("Try running with looser parameters if needed\n")
  }
  
  return(results)
}

# Function to run with relaxed parameters for noisy data
run_relaxed_analysis <- function() {
  cat("Running analysis with relaxed parameters for noisy environment...\n\n")
  
  relaxed_params <- list(
    pulse_slop_ms = 3.0,      # More timing tolerance
    freq_slop_khz = 3.0,      # More frequency tolerance  
    sig_slop_db = 15,         # More signal tolerance
    min_match_score = 0.6,    # Lower score threshold
    validate_files = TRUE,
    max_pulse_rate = 50       # Rate limiting for noisy data
  )
  
  results <- do.call(run_tag_analysis, c(
    list(
      input_path = INPUT_DIR,
      output_file = "receiver_detections_relaxed.csv",
      file_pattern = FILE_PATTERN
    ),
    relaxed_params
  ))
  
  return(results)
}

# Function to validate data files before processing
validate_data_files <- function() {
  cat("=== DATA FILE VALIDATION ===\n")
  
  files <- list.files(INPUT_DIR, pattern = FILE_PATTERN, full.names = TRUE)
  
  if (length(files) == 0) {
    cat("No files found matching pattern:", FILE_PATTERN, "\n")
    return(FALSE)
  }
  
  cat("Found", length(files), "files to validate:\n\n")
  
  valid_files <- 0
  for (file in files) {
    validation <- validate_text_file_format(file, show_samples = TRUE)
    if (validation$valid) {
      valid_files <- valid_files + 1
    }
    cat("\n")
  }
  
  cat("Validation complete:", valid_files, "of", length(files), "files contain valid pulse data\n")
  return(valid_files > 0)
}

# Main execution
if (!interactive()) {
  # Script is being run from command line
  cat("Starting receiver data analysis...\n\n")
  
  # First validate the files
  if (validate_data_files()) {
    cat("\nProceeding with analysis...\n\n")
    results <- run_receiver_analysis()
  } else {
    cat("No valid data files found. Please check file formats.\n")
  }
} else {
  # Interactive mode - just load functions
  cat("Receiver data processing functions loaded.\n")
  cat("Available functions:\n")
  cat("  run_receiver_analysis()    - Process with standard parameters\n")
  cat("  run_relaxed_analysis()     - Process with relaxed parameters\n") 
  cat("  validate_data_files()      - Check file formats\n\n")
  cat("To start processing, run: results <- run_receiver_analysis()\n")
}