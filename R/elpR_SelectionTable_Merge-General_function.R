# merge selection tables with differing column names, order, and number
# can be in one folder or subfolders
# bje37@cornell.edu
# Updated Jan 2025

# This function will
# - merge selection tables that are in a single folder or subfolders
# - selection tables can have differing columns and column order - the script will standardize across tables
# - this script will merge all selections into one table and will rename the Selection ID so they are all unique
# - This script expects Raven Sound Selection Tables with columns: Selection, Begin Time (s),
#        End Time (s), High Freq (s), Low Freq (s), File Offset (s), Begin Path
# - output file is .txt selection table

#' @name merge_selection_tables
#' @title Merge Selection Tables
#' @description Merge selection tables together. They can be in sub-folders or all in one folder.
#' @param path Character string specifying the directory containing selection table files
#' @param recursive Logical; if TRUE, searches for files recursively in subdirectories
#' @return A data frame containing the merged selection tables with standardized columns:
#' \itemize{
#'   \item Selection - New sequential selection numbers
#'   \item Original Selection ID - Original selection numbers from source files
#'   \item Source Selection Table - Name of the original file
#'   \item Begin Time (s) - Start time in seconds
#'   \item End Time (s) - End time in seconds
#'   \item Low Freq (Hz) - Lower frequency bound
#'   \item High Freq (Hz) - Upper frequency bound
#'   \item File Offset (s) - Time offset in source file
#'   \item Begin Path - Original file path
#'   \item Begin File - Filename from Begin Path
#'   \item Begin Clock Time - Calculated datetime of the selection
#' }
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Reads all .txt files in the specified directory
#'   \item Preserves original selection IDs
#'   \item Removes duplicate entries
#'   \item Standardizes column formats
#'   \item Adds source file information
#'   \item Calculates clock times using selection_datetime function
#'   \item Creates a new output file with timestamp in name
#' }
#' @import dplyr
#' @export
#' @importFrom dplyr bind_rows distinct select everything
#' @seealso \code{\link{selection_datetime}} for datetime calculations
#' @note The function expects selection table files in tab-delimited format



merge_selection_tables <- function(path, recursive = TRUE) {
  library(dplyr)

  if (!dir.exists(path)) {
    stop("The specified directory does not exist")
  }

  # Determine which folders we’ll process
  if (recursive) {
    folders_to_process <- list.dirs(path, recursive = FALSE)
    folders_to_process <- folders_to_process[folders_to_process != path]  # skip top-level
    if (length(folders_to_process) == 0) {
      cat("No subfolders found in", path, "\n")
      return(invisible(NULL))
    }
  } else {
    folders_to_process <- path
  }

  # Loop over each folder
  for (folder_path in folders_to_process) {
    folder_prefix <- if (recursive) basename(folder_path) else NULL

    priority_cols <- c(
      "Selection",
      "Original Selection ID",
      "Source Selection Table",
      "Begin Time (s)",
      "End Time (s)",
      "Low Freq (Hz)",
      "High Freq (Hz)",
      "File Offset (s)",
      "Begin Path",
      "Begin File"
    )

    file_names <- list.files(path = folder_path,
                             pattern = "\\.txt$",
                             recursive = FALSE,
                             full.names = TRUE)

    if (length(file_names) == 0) {
      cat("No .txt files found in", folder_path, "\n")
      next
    }

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    if (!is.null(folder_prefix)) {
      output_filename <- paste0(folder_prefix, "_combined_selection_tables_", timestamp, ".txt")
    } else {
      output_filename <- paste0("combined_selection_tables_", timestamp, ".txt")
    }
    output_file <- file.path(folder_path, output_filename)

    selection_counter <- 0
    first_write <- TRUE

    # Process each file in this folder
    for (fn in file_names) {
      cat("Reading file:", fn, "\n")
      flush.console()

      table <- tryCatch({
        read.table(fn,
                   header = TRUE,
                   sep = "\t",
                   check.names = FALSE,
                   quote = "",
                   fill = TRUE,
                   stringsAsFactors = FALSE)
      }, error = function(e) {
        cat("Error reading file:", fn, "-", e$message, "\n")
        return(NULL)
      })

      if (is.null(table) || nrow(table) == 0) next

      # Add metadata columns
      if ("Selection" %in% names(table)) {
        table$`Original Selection ID` <- table$Selection
      }
      table$`Source Selection Table` <- basename(fn)

      numeric_cols <- c("Begin Time (s)", "End Time (s)",
                        "Low Freq (Hz)", "High Freq (Hz)",
                        "File Offset (s)")

      for (col in names(table)) {
        if (col %in% numeric_cols) {
          table[[col]] <- suppressWarnings(as.numeric(as.character(table[[col]])))
        } else {
          table[[col]] <- as.character(table[[col]])
        }
      }

      if ("Begin Path" %in% names(table)) {
        table$`Begin File` <- basename(as.character(table$"Begin Path"))
      }

      # Sort columns: priority first, then others
      other_cols <- setdiff(names(table), priority_cols)
      sorted_other_cols <- sort(other_cols)
      all_cols <- c(priority_cols[priority_cols %in% names(table)], sorted_other_cols)
      table <- table[, all_cols, drop = FALSE]

      # Sort rows by file and offset
      table <- table[order(table$`Begin File`, table$`File Offset (s)`),]

      # Add continuous selection numbers across files
      rows <- nrow(table)
      table$Selection <- seq(selection_counter + 1, selection_counter + rows)
      selection_counter <- selection_counter + rows

      # Move Selection to first position
      table <- table %>%
        select(Selection, `Original Selection ID`, `Source Selection Table`, everything())

      # Write output — first file has headers, rest append without headers
      if (first_write) {
        write.table(table, output_file, sep = "\t", row.names = FALSE,
                    quote = FALSE, fileEncoding = "UTF-8", na = "")
        first_write <- FALSE
      } else {
        write.table(table, output_file, sep = "\t", row.names = FALSE,
                    quote = FALSE, fileEncoding = "UTF-8", na = "",
                    append = TRUE, col.names = FALSE)
      }
    }

    if (first_write) {
      cat("No valid tables processed in", folder_path, "\n")
    } else {
      cat("\nMerged file saved as:", output_file, "\n")
      cat("Total rows written:", selection_counter, "\n")
    }
  }
}


# merge_selection_tables <- function(path, recursive = TRUE) {
#   library(dplyr)
#   #library(lubridate)
#
#
#   # Check if path exists
#   if (!dir.exists(path)) {
#     stop("The specified directory does not exist")
#   }
#
#   # Get all txt files
#   file_names <- list.files(path = path,
#                            pattern = "\\.txt$",
#                            recursive = recursive,
#                            full.names = TRUE)
#
#   if(length(file_names) == 0) {
#     stop("No .txt files found in specified location")
#   }
#
#   # Define priority order for columns
#   priority_cols <- c(
#     "Selection",
#     "Original Selection ID",
#     "Source Selection Table",
#     "Begin Time (s)",
#     "End Time (s)",
#     "Low Freq (Hz)",
#     "High Freq (Hz)",
#     "File Offset (s)",
#     "Begin Path",
#     "Begin File"
#   )
#
#   # Create empty list for tables
#   all_tables <- list()
#   valid_tables <- 0
#
#   # Process each file
#   for (i in 1:length(file_names)) {
#     table <- try(read.table(file_names[i],
#                             header = TRUE,
#                             sep = "\t",
#                             check.names = FALSE,
#                             quote = "",
#                             fill = TRUE,
#                             stringsAsFactors = FALSE))
#
#     if (!inherits(table, "try-error") && nrow(table) > 0) {
#       valid_tables <- valid_tables + 1
#
#       # Save original Selection number
#       if("Selection" %in% names(table)) {
#         table$`Original Selection ID` <- table$Selection
#       }
#
#       # Add original filename
#       table$`Source Selection Table` <- basename(file_names[i])
#
#       # Convert known numeric columns
#       numeric_cols <- c("Begin Time (s)", "End Time (s)",
#                         "Low Freq (Hz)", "High Freq (Hz)",
#                         "File Offset (s)")
#
#       # Convert columns appropriately
#       for(col in names(table)) {
#         if(col %in% numeric_cols) {
#           table[[col]] <- as.numeric(as.character(table[[col]]))
#         } else {
#           table[[col]] <- as.character(table[[col]])
#         }
#       }
#
#       # Extract filename from Begin Path
#       if("Begin Path" %in% names(table)) {
#         table$`Begin File` <- basename(as.character(table$"Begin Path"))
#       }
#
#       # Sort columns: priority columns first, then remaining columns alphabetically
#       other_cols <- setdiff(names(table), priority_cols)
#       sorted_other_cols <- sort(other_cols)
#       all_cols <- c(
#         priority_cols[priority_cols %in% names(table)],
#         sorted_other_cols
#       )
#
#       table <- table[, all_cols]
#       all_tables[[valid_tables]] <- table
#       cat("File:", basename(file_names[i]), "- rows:", nrow(table), "\n")
#     }
#   }
#
#   if(valid_tables == 0) {
#     stop("No valid tables found to process")
#   }
#
#   # Combine tables
#   merged_df <- bind_rows(all_tables)
#
#   # # Check for duplicates using base R approach
#   # duplicate_check <- merged_df[duplicated(merged_df[, c("Begin File", "File Offset (s)",
#   #                                                       "End Time (s)", "High Freq (Hz)",
#   #                                                       "Low Freq (Hz)")]) |
#   #                                duplicated(merged_df[, c("Begin File", "File Offset (s)",
#   #                                                         "End Time (s)", "High Freq (Hz)",
#   #                                                         "Low Freq (Hz)")], fromLast = TRUE), ]
#   #
#   # cat("\nDuplicate entries found:", nrow(duplicate_check), "\n")
#   #
#   # # Remove duplicates if found
#   # if(nrow(duplicate_check) > 0) {
#   #   cat("\nSample of duplicates:\n")
#   #   print(head(duplicate_check))
#   #
#   #   merged_df <- merged_df %>%
#   #     distinct(`Begin File`, `File Offset (s)`, `End Time (s)`,
#   #              `High Freq (Hz)`, `Low Freq (Hz)`, .keep_all = TRUE)
#   # }
#
#   # Sort by Begin File and File Offset
#   merged_df <- merged_df[order(merged_df$"Begin File", merged_df$"File Offset (s)"),]
#
#   # Add new Selection numbers
#   merged_df$Selection <- 1:nrow(merged_df)
#
#   # Move Selection and Original Selection ID to first columns
#   merged_df <- merged_df %>%
#     select(Selection, `Original Selection ID`, `Source Selection Table`, everything())
#
#
#   # add Begin Clock Time column if it doesn't exist (test)
#   # merged_df$`Begin Clock Time` <- sapply(1:nrow(merged_df), function(i) {
#   #   time <- selection_datetime(merged_df$`Begin File`[i],
#   #                              merged_df$`File Offset (s)`[i])
#   #   format(time, "%Y-%m-%d %H:%M:%S")
#   # })
#   # merged_df$`Begin Clock Time` <- selection_datetime(merged_df$`Begin File`,
#   #                                                    merged_df$`File Offset (s)`)
#
#   # Create output filename
#   output_file <- file.path(path,
#                            paste0("combined_selection_tables_",
#                                   format(Sys.time(), "%Y%m%d_%H%M%S"),
#                                   ".txt"))
#
#   # Write output
#   write.table(merged_df,
#               output_file,
#               sep = "\t",
#               row.names = FALSE,
#               quote = FALSE,
#               fileEncoding = "UTF-8",
#               na = "")
#
#   # Print summary
#   cat("\nOriginal number of rows:", nrow(bind_rows(all_tables)), "\n")
#   cat("Final number of rows after removing duplicates:", nrow(merged_df), "\n")
#   cat("Number of columns:", ncol(merged_df), "\n")
#   cat("\nOutput file saved as:", basename(output_file), "\n")
#
#   return(merged_df)
# }


## TO DO
# check for duplicates and remove if desired? No necessary?
# For validation purposes, is it possible to print the total number of rows (excluding headers) that were read in as well as the total number of rows combined in the final table?
# clock time doesn't work
