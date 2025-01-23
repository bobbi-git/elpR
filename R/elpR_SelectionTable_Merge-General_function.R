# merge selection tables with differing column names, order, and number
# can be in one folder or subfolders
# bje37@cornell.edu
# Updated Jan 2025

# This function will
# - merge selection tables that are in a single folder or subfolders
# - selection tables can have differing columns and column order - the script will standardize across tables
# - this script will merge all selections into one table and will rename the Selection ID so they are all unique
# - This script expects Raven Sound Selection Tables with columns: Selection, Begin Time (s),
#        End Time (s), High Freq (s), Low Freq (s), File Offset (s), Begin File, Begin Path
# - output file is .txt selection table



merge_selection_tables <- function(path, recursive = TRUE) {
  # Check if path exists
  if (!dir.exists(path)) {
    stop("The specified directory does not exist")
  }

  # Function to get all txt files, including those in subfolders
  get_txt_files <- function(path, recursive = TRUE) {
    if(recursive) {
      # Get files from main folder and subfolders
      files <- list.files(path = path,
                          pattern = "\\.txt$",
                          recursive = TRUE,
                          full.names = TRUE)
    } else {
      # Get files only from main folder
      files <- list.files(path = path,
                          pattern = "\\.txt$",
                          recursive = FALSE,
                          full.names = TRUE)
    }
    return(files)
  }

  # Get list of all .txt files
  file_names <- get_txt_files(path, recursive)

  if(length(file_names) == 0) {
    stop("No .txt files found in specified location")
  }

  # Define column types
  numeric_cols <- c("Begin Time (s)", "End Time (s)", "File Offset (s)",
                    "Low Freq (Hz)", "High Freq (Hz)")
  character_cols <- c("Begin Clock Time", "Begin Date", "Begin Hour",
                      "Begin File", "Notes", "Class", "View", "Channel")

  # Create empty list to store all dataframes
  all_tables <- list()
  valid_tables <- 0

  # Read and process each file
  for (i in 1:length(file_names)) {
    # Read each table with careful handling of quotes and special characters
    table <- try(read.table(file_names[i],
                            header=TRUE,
                            sep="\t",
                            check.names=FALSE,
                            quote="",
                            fill=TRUE,
                            stringsAsFactors=FALSE))

    # Only process if table has rows
    if (!inherits(table, "try-error") && nrow(table) > 0) {
      valid_tables <- valid_tables + 1

      # Convert numeric columns
      for(col in numeric_cols) {
        if(col %in% names(table)) {
          table[[col]] <- as.numeric(as.character(table[[col]]))
        }
      }

      # Convert and clean character columns
      for(col in character_cols) {
        if(col %in% names(table)) {
          table[[col]] <- as.character(table[[col]])
          table[[col]] <- gsub("\r|\n|\t", " ", table[[col]])
        }
      }

      # Add filename as a column (use only the file name, not full path)
      table$Selection_Table <- basename(file_names[i])

      # Extract filename from Begin Path
      if("Begin Path" %in% names(table)) {
        table$`Begin File` <- basename(as.character(table$"Begin Path"))
      } else {
        cat("\nNo Begin Path column found - skipping filename extraction\n")
      }


      # Store in list
      all_tables[[valid_tables]] <- table
      cat("File:", basename(file_names[i]), "- rows:", nrow(table), "\n")
    }
  }

  if(valid_tables == 0) {
    stop("No valid tables found to process")
  }

  # Combine all tables
  merged_df <- bind_rows(all_tables)

  # Check for duplicates based on key columns
  duplicate_check <- merged_df %>%
    group_by(`Begin File`, `File Offset (s)`,`End Time (s)`, `High Freq (Hz)`,`Low Freq (Hz)`) %>%
    filter(n() > 1)

  cat("\nDuplicate entries found:", nrow(duplicate_check), "\n")

  if(nrow(duplicate_check) > 0) {
    cat("\nSample of duplicates:\n")
    print(head(duplicate_check))

    # Remove duplicates
    merged_df <- merged_df %>%
      distinct(`Begin File`, `File Offset (s)`,`End Time (s)`, `High Freq (Hz)`,`Low Freq (Hz)`, .keep_all = TRUE)
  }

  # Sort by Begin File and File Offset
  merged_df <- merged_df[order(merged_df$"Begin File", merged_df$"File Offset (s)"),]

  # Add new Selection column
  merged_df$Selection <- 1:nrow(merged_df)

  # Move Selection to first column
  merged_df <- merged_df %>% select(Selection, everything())

  # Create output filename with timestamp
  output_file <- file.path(path,
                           paste0("combined_selection_tables_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"),
                                  ".txt"))

  # Write the combined table
  write.table(merged_df,
              output_file,
              sep="\t",
              row.names=FALSE,
              quote=FALSE,
              fileEncoding="UTF-8")

  # Print summary
  cat("\nOriginal number of rows:", nrow(bind_rows(all_tables)), "\n")
  cat("Final number of rows after removing duplicates:", nrow(merged_df), "\n")
  cat("Number of columns:", ncol(merged_df), "\n")
  cat("\nOutput file saved as:", output_file, "\n")

}


