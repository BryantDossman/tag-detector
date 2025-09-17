# tag_database.R
# Functions for loading and managing VHF tag databases

#' Load tag database from CSV file
#' @param csv_file Path to CSV file containing tag definitions
#' @return Data frame with tag information including calculated gap patterns
load_tag_database <- function(csv_file = "tags.csv") {
  if (!file.exists(csv_file)) {
    stop("Tag database file not found: ", csv_file)
  }
  
  tag_db <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Validate required columns
  required_cols <- c("proj", "id", "tagFreq", "fcdFreq", "g1", "g2", "g3", "bi", "dfreq")
  missing_cols <- setdiff(required_cols, names(tag_db))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in tag database: ", paste(missing_cols, collapse = ", "))
  }
  
  # Calculate 4-gap patterns (convert milliseconds to seconds)
  tag_db$g1_s <- tag_db$g1 / 1000
  tag_db$g2_s <- tag_db$g2 / 1000  
  tag_db$g3_s <- tag_db$g3 / 1000
  tag_db$g4_s <- tag_db$bi - (tag_db$g1_s + tag_db$g2_s + tag_db$g3_s)
  
  cat("Loaded", nrow(tag_db), "tags from database:", csv_file, "\n")
  return(tag_db)
}

#' Create hardcoded tag database (fallback when no CSV available)
#' @return Data frame with hardcoded tag information
create_default_tag_database <- function() {
  tag_db <- data.frame(
    proj = c(975, 975, 975),
    id = c(127, 233, 243),
    tagFreq = c(166.38, 166.38, 166.38),
    fcdFreq = c(166.376, 166.376, 166.376),
    g1 = c(46.381, 70.795, 70.796),      # ms
    g2 = c(107.412, 63.471, 122.06),     # ms  
    g3 = c(87.883, 24.411, 83),          # ms
    bi = c(38.8951988, 38.8953018, 38.8958015), # seconds
    dfreq = c(0.0004, -0.0001, -0.0006), # kHz
    codeset = c("Lotek6M", "Lotek6M", "Lotek6M"),
    stringsAsFactors = FALSE
  )
  
  # Calculate gap patterns
  tag_db$g1_s <- tag_db$g1 / 1000
  tag_db$g2_s <- tag_db$g2 / 1000
  tag_db$g3_s <- tag_db$g3 / 1000
  tag_db$g4_s <- tag_db$bi - (tag_db$g1_s + tag_db$g2_s + tag_db$g3_s)
  
  return(tag_db)
}

#' Display tag database summary
#' @param tag_db Tag database data frame
display_tag_summary <- function(tag_db) {
  cat("Tag Database Summary:\n")
  cat("====================\n")
  
  for (i in 1:nrow(tag_db)) {
    cat(sprintf("Tag %d (%.2f MHz): gaps [%.3f, %.3f, %.3f, %.3f] s, burst interval %.3f s\n",
                tag_db$id[i], tag_db$tagFreq[i], 
                tag_db$g1_s[i], tag_db$g2_s[i], tag_db$g3_s[i], tag_db$g4_s[i],
                tag_db$bi[i]))
  }
  cat("\n")
}

#' Get expected gap pattern for a specific tag
#' @param tag_db Tag database data frame
#' @param tag_id Tag ID to look up
#' @return Vector of gap intervals in seconds, or NULL if not found
get_tag_gaps <- function(tag_db, tag_id) {
  tag_row <- tag_db[tag_db$id == tag_id, ]
  if (nrow(tag_row) == 0) {
    return(NULL)
  }
  return(c(tag_row$g1_s, tag_row$g2_s, tag_row$g3_s))
}

#' Validate tag database integrity
#' @param tag_db Tag database data frame
#' @return TRUE if valid, otherwise stops with error message
validate_tag_database <- function(tag_db) {
  # Check for duplicate tag IDs
  if (any(duplicated(tag_db$id))) {
    stop("Duplicate tag IDs found in database")
  }
  
  # Check for reasonable gap values
  if (any(tag_db$g1_s <= 0 | tag_db$g2_s <= 0 | tag_db$g3_s <= 0)) {
    stop("Invalid gap values (must be positive)")
  }
  
  # Check for reasonable burst intervals
  if (any(tag_db$bi <= 0 | tag_db$bi > 3600)) {
    stop("Invalid burst intervals (must be between 0 and 3600 seconds)")
  }
  
  return(TRUE)
}
