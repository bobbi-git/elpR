# Detector performance evaluation
# inspired by the performance evaluation MATLAB tool that Michael Pitzrick created

# This script will
# - read in selection tables (test and truth)
# - check for overlap between selections in the tables (overlap can be defined by user)
# - plot precision-recall
# - plot recall vs FP/hr
# - produce performance metric by threshold

# Requirements
# - selection tables for manually annotated target signals
# - selection tables for detector
# - all selection tables require Begin Time (s), End Time (s), Begin File, File Offset (s)
# - the detector table requires Score. If this is called something else, you can rename it in the code below.
# - a Schedule file = a .txt file with Begin Date (m/d/y), Begin Time (hh:mm:ss), End Date (m/d/y), End Time (hh:mm:ss) in columns

### TO DO
# - allow for classification-specific performance metrics
# - allow for SNR-based performance metrics
# - test schedule file
# - allow for specific site/channel
# - report schedule used and time periods used
# - report overlap used and threshold bin used

# load all necessary packages
library(dplyr)
library(ggplot2)
library(PRROC)  # For ROC and PR curves
library(chron)

# truth tables and test tables
truth <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/detector_validation/truth_tables" # path to the manually annotated selection tables
truth_tables <- list.files(path = truth,recursive=T,full.names=TRUE, pattern = "*.txt")
truth_tables <- lapply(truth_tables, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})

detector <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/detector_validation/test_tables" # path to the detector generated selection tables
det_tables <-  list.files(path = detector,recursive=T,full.names=TRUE, pattern = "*.txt")
det_tables <- lapply(det_tables, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})

schedule_path <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/detector_validation/schedule.txt" # schedule file
score <- "Score" # name of your score column
min_threshold = 0.2
thresh_bin <- 0.1 # bins of scores to report

output <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/detector_validation/output"

#### Code ####

# check required columns in tables
check_required_columns <- function(table_list, list_name = "") {
  required_cols <- c("Begin File", "File Offset (s)", "Begin Time (s)", "End Time (s)")

  for(i in seq_along(table_list)) {
    df <- table_list[[i]]

    message("\nChecking table ", i, " in ", list_name, " list:")
    message("Found columns: ", paste(names(df), collapse = ", "))

    missing_cols <- required_cols[!required_cols %in% names(df)]

    if(length(missing_cols) > 0) {
      message("WARNING: Missing required columns: ", paste(missing_cols, collapse = ", "))
    } else {
      message("SUCCESS: All required columns present")
    }

    if(length(missing_cols) == 0) {
      na_counts <- sapply(df[required_cols], function(x) sum(is.na(x)))
      if(any(na_counts > 0)) {
        message("WARNING: Found NA values in required columns:")
        for(col in names(na_counts)) {
          if(na_counts[col] > 0) {
            message("  ", col, ": ", na_counts[col], " NA values")
          }
        }
      } else {
        message("SUCCESS: No NA values in required columns")
      }
    }
  }
}


# check input tables for required columns
print(check_required_columns(truth_tables, "truth"))
print(check_required_columns(det_tables, "detector"))


# function to standardize the tables
standardize_table <- function(df, is_detector = TRUE) {
  required_cols <- c("Begin File", "File Offset (s)", "Begin Time (s)", "End Time (s)")

  missing_cols <- required_cols[!required_cols %in% names(df)]
  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Print unique values in Begin File column
  message("Unique Begin File values:")
  print(unique(df[["Begin File"]]))

  std_df <- data.frame(
    File_Name = as.character(df[["Begin File"]]),  # Explicitly convert to character
    File_Offset = as.numeric(as.character(df[["File Offset (s)"]])),
    Begin_Time = as.numeric(as.character(df[["Begin Time (s)"]])),
    End_Time = as.numeric(as.character(df[["End Time (s)"]]))
  )

  # Print unique values after standardization
  message("Unique File_Name values after standardization:")
  print(unique(std_df$File_Name))

  if(is_detector) {
    if("Score" %in% names(df)) {
      std_df$Score <- as.numeric(as.character(df[["Score"]]))
      std_df$Score[is.na(std_df$Score)] <- 1.0
    } else {
      message("No Score column found in detector file. Using 1.0 as default score.")
      std_df$Score <- 1.0
    }
  }

  std_df$Duration <- std_df$End_Time - std_df$Begin_Time
  std_df$Abs_Start <- std_df$File_Offset
  std_df$Abs_End <- std_df$File_Offset + std_df$Duration

  return(std_df)
}


# enable temporal overlap by percentage
is_overlap <- function(det, truth, min_overlap_percent = 0) {
  # First check file names match
  if(is.na(det$File_Name) || det$File_Name == "NA" ||
     is.na(truth$File_Name) || truth$File_Name == "NA") {
    return(FALSE)
  }

  if(det$File_Name != truth$File_Name) {
    return(FALSE)
  }

  # Calculate overlap
  overlap_start <- max(det$Abs_Start, truth$Abs_Start)
  overlap_end <- min(det$Abs_End, truth$Abs_End)

  # If no overlap, return FALSE
  if(overlap_end <= overlap_start) {
    return(FALSE)
  }

  # Calculate overlap duration
  overlap_duration <- overlap_end - overlap_start

  # Calculate durations of both events
  det_duration <- det$Abs_End - det$Abs_Start
  truth_duration <- truth$Abs_End - truth$Abs_Start

  # Calculate time overlap percentages relative to both events
  det_overlap_percent <- (overlap_duration / det_duration) * 100
  truth_overlap_percent <- (overlap_duration / truth_duration) * 100

  # Return TRUE if either temporal overlap percentage meets the minimum requirement
  return(det_overlap_percent >= min_overlap_percent ||
           truth_overlap_percent >= min_overlap_percent)
}


# combine multiple tables
combine_tables <- function(table_list, is_detector = TRUE) {
  if(length(table_list) == 0) {
    stop("Empty table list provided")
  }

  combined_df <- do.call(rbind, lapply(seq_along(table_list), function(i) {
    df <- table_list[[i]]

    if(is.null(df) || nrow(df) == 0) {
      warning("Skipping empty table at index ", i)
      return(NULL)
    }

    tryCatch({
      df <- standardize_table(df, is_detector)
      df$Source_Table <- i
      return(df)
    }, error = function(e) {
      warning("Error processing table ", i, ": ", e$message)
      return(NULL)
    })
  }))

  combined_df <- combined_df[!is.null(combined_df), ]

  if(nrow(combined_df) == 0) {
    stop("No valid data after processing tables")
  }

  return(combined_df)
}


# check if events fall withint schedule file
is_within_schedule <- function(event, schedule) {
  # Convert event times to POSIXct
  event_file <- event$File_Name
  # Extract date from filename (assuming format contains date)
  event_date <- sub(".*_(\\d{8})_.*", "\\1", event_file)
  event_date <- as.Date(event_date, format="%Y%m%d")

  event_start <- as.POSIXct(paste(event_date,
                                  format(as.POSIXct(event$File_Offset, origin="1970-01-01"), "%H:%M:%S")))
  event_end <- as.POSIXct(paste(event_date,
                                format(as.POSIXct(event$File_Offset + event$Duration, origin="1970-01-01"), "%H:%M:%S")))

  # Check if event falls within any scheduled period
  for(i in 1:nrow(schedule)) {
    if(event_start >= schedule$Begin_DateTime[i] &&
       event_end <= schedule$End_DateTime[i] &&
       grepl(schedule$Channel[i], event_file)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


# performance evaluation function
evaluate_detections <- function(truth_tables, det_tables,
                                schedule_path,
                                score_thresholds = seq(min_threshold, 0.999, thresh_bin),
                                min_overlap_percent = 0,
                                output_dir = NULL) {
  # Create output directory if specified
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Create subdirectories for different types of tables
    truth_dir <- file.path(output_dir, "truth_tables")
    detector_dir <- file.path(output_dir, "detector_tables")
    dir.create(truth_dir, showWarnings = FALSE)
    dir.create(detector_dir, showWarnings = FALSE)
  }

  # Check if schedule file exists and read it
  if (!file.exists(schedule_path)) {
    stop("Schedule file not found at: ", schedule_path)
  }

  # Try to read schedule file
  tryCatch({
    schedule <- read.table(schedule_path, header=TRUE, sep="\t", check.names=FALSE)
    message("Successfully read schedule file")
  }, error = function(e) {
    stop("Error reading schedule file: ", e$message)
  })

  # Extract just the time portion from the Begin/End Time fields (NEED TO BE SPECIFIC TO SITE ID/CHANNEL)
  schedule$`Begin Time` <- chron(times = schedule$`Begin Time`)
  schedule$`End Time` <- chron(times = schedule$`End Time`)

  # Calculate total monitoring hours from schedule
  schedule$Begin_DateTime <- as.POSIXct(paste(schedule$`Begin Date`, schedule$`Begin Time`),
                                        format="%m/%d/%Y %H:%M:%S")
  schedule$End_DateTime <- as.POSIXct(paste(schedule$`End Date`, schedule$`End Time`),
                                      format="%m/%d/%Y %H:%M:%S")

  # Diagnostic messages for schedule file
  message("Schedule periods:")
  message("Start: ", schedule$Begin_DateTime)
  message("End: ", schedule$End_DateTime)

  # Calculate total hours
  total_hours <- sum(difftime(schedule$End_DateTime,
                              schedule$Begin_DateTime,
                              units="hours"))

  message("Total monitoring hours: ", round(total_hours, 2))
  message("\nSchedule summary:")
  message("Number of schedule periods: ", nrow(schedule))
  message("First period: ", min(schedule$Begin_DateTime), " to ", min(schedule$End_DateTime)) # need to make these more clear
  message("Last period: ", max(schedule$Begin_DateTime), " to ", max(schedule$End_DateTime)) # need to make these more clear
  message("Total hours calculated: ", round(as.numeric(total_hours), 2))

  if(as.numeric(total_hours) <= 0) {
    stop("Invalid total hours calculated from schedule. Check date/time formats.")
  }

  # Combine and standardize tables for evaluation
  truth_combined <- combine_tables(truth_tables, is_detector = FALSE)
  detector_combined <- combine_tables(det_tables, is_detector = TRUE)
  # crop the table to events that only fall within schedule file
  truth_combined <- truth_combined[sapply(1:nrow(truth_combined),
                                          function(i) is_within_schedule(truth_combined[i,], schedule)), ]
  detector_combined <- detector_combined[sapply(1:nrow(detector_combined),
                                                function(i) is_within_schedule(detector_combined[i,], schedule)), ]
  message("After schedule filtering:")
  message("Truth events remaining: ", nrow(truth_combined))
  message("Detector events remaining: ", nrow(detector_combined))
  results <- list()

  for(threshold in score_thresholds) {
    message("\nEvaluating threshold: ", threshold)

    # Create copies of original tables for threshold bins
    current_truth_tables <- lapply(truth_tables, function(x) {
      df <- x
      df$Classification <- "FN"  # Default all truth events to False Negative
      return(df)
    })

    current_det_tables <- lapply(det_tables, function(x) {
      df <- x
      df$Classification <- "FP"  # Default all detector events to False Positive
      df$Above_Threshold <- df$Score >= threshold  # Add column to track if detection meets threshold
      return(df)
    })

    valid_detections <- detector_combined[detector_combined$Score >= threshold, ]

    # If no detections meet the threshold
    if(nrow(valid_detections) == 0) {
      message("No detections above threshold ", threshold)

      # Save annotated tables
      if (!is.null(output_dir)) {
        thresh_str <- sprintf("%.2f", threshold)

        # Save truth tables
        for(i in seq_along(current_truth_tables)) {
          filename <- paste0("truth_table_", i, "_thresh_", thresh_str, ".txt")
          write.table(current_truth_tables[[i]],
                      file = file.path(truth_dir, filename),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }

        # Save detector tables
        for(i in seq_along(current_det_tables)) {
          filename <- paste0("detector_table_", i, "_thresh_", thresh_str, ".txt")
          write.table(current_det_tables[[i]],
                      file = file.path(detector_dir, filename),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
      }

      results[[as.character(threshold)]] <- list(
        threshold = threshold,
        total_truth = nrow(truth_combined),
        total_detections = 0,
        tp_truth = 0,
        fn_truth = nrow(truth_combined),
        tp_det = 0,
        fp_det = 0,
        precision = NA,
        recall = NA,
        fp_per_hour = NA,
        total_hours = as.numeric(total_hours)
      )

      next
    }

    true_positives <- 0
    false_positives <- 0
    matched_truth_indices <- numeric()

    for(i in 1:nrow(valid_detections)) {
      det <- valid_detections[i, ]
      found_match <- FALSE

      for(j in 1:nrow(truth_combined)) {
        if(!(j %in% matched_truth_indices)) {
          if(is_overlap(det, truth_combined[j,], min_overlap_percent)) {
            true_positives <- true_positives + 1
            matched_truth_indices <- c(matched_truth_indices, j)
            found_match <- TRUE

            # Update classification in original format tables
            # For detector tables
            for(k in seq_along(current_det_tables)) {
              matches <- current_det_tables[[k]][["Begin File"]] == det$File_Name &
                abs(as.numeric(current_det_tables[[k]][["File Offset (s)"]]) - det$File_Offset) < 1e-10 &
                abs(as.numeric(current_det_tables[[k]][["Begin Time (s)"]]) - det$Begin_Time) < 1e-10 &
                abs(as.numeric(current_det_tables[[k]][["End Time (s)"]]) - det$End_Time) < 1e-10
              if(any(matches)) {
                current_det_tables[[k]]$Classification[matches] <- "TP"
              }
            }

            # For truth tables
            truth_record <- truth_combined[j,]
            for(k in seq_along(current_truth_tables)) {
              matches <- current_truth_tables[[k]][["Begin File"]] == truth_record$File_Name &
                abs(as.numeric(current_truth_tables[[k]][["File Offset (s)"]]) - truth_record$File_Offset) < 1e-10 &
                abs(as.numeric(current_truth_tables[[k]][["Begin Time (s)"]]) - truth_record$Begin_Time) < 1e-10 &
                abs(as.numeric(current_truth_tables[[k]][["End Time (s)"]]) - truth_record$End_Time) < 1e-10
              if(any(matches)) {
                current_truth_tables[[k]]$Classification[matches] <- "TP"
              }
            }

            break
          }
        }
      }

      if(!found_match) {
        false_positives <- false_positives + 1
      }
    }

    false_negatives <- nrow(truth_combined) - true_positives

    precision <- ifelse(true_positives + false_positives > 0,
                        true_positives / (true_positives + false_positives),
                        0)
    recall <- ifelse(nrow(truth_combined) > 0,
                     true_positives / nrow(truth_combined),
                     0)

    monitoring_hours <- as.numeric(total_hours)
    fp_per_hour <- if(monitoring_hours > 0) {
      false_positives / monitoring_hours
    } else {
      0
    }

    results[[as.character(threshold)]] <- list(
      threshold = threshold,
      total_truth = nrow(truth_combined),
      total_detections = nrow(valid_detections),
      tp_truth = true_positives,
      fn_truth = false_negatives,
      tp_det = true_positives,
      fp_det = false_positives,
      precision = precision,
      recall = recall,
      fp_per_hour = fp_per_hour,
      total_hours = monitoring_hours
    )

    # Save annotated tables
    if (!is.null(output_dir)) {
      thresh_str <- sprintf("%.2f", threshold)

      # Save truth tables
      for(i in seq_along(current_truth_tables)) {
        filename <- paste0("truth_table_", i, "_thresh_", thresh_str, ".txt")
        write.table(current_truth_tables[[i]],
                    file = file.path(truth_dir, filename),
                    sep = "\t", row.names = FALSE, quote = FALSE)
      }

      # Save detector tables
      for(i in seq_along(current_det_tables)) {
        filename <- paste0("detector_table_", i, "_thresh_", thresh_str, ".txt")
        write.table(current_det_tables[[i]],
                    file = file.path(detector_dir, filename),
                    sep = "\t", row.names = FALSE, quote = FALSE)
      }
    }
  }

  # Convert results to data frames
  metrics_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      Threshold = x$threshold,
      Precision = if(is.na(x$precision)) NA else round(x$precision, 3),
      Recall = if(is.na(x$recall)) NA else round(x$recall, 3),
      FP_per_hour = round(x$fp_per_hour, 3)
    )
  }))

  summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      Threshold = x$threshold,
      Precision = if(is.na(x$precision)) NA else round(x$precision, 3),
      Recall = if(is.na(x$recall)) NA else round(x$recall, 3),
      FP_per_hour = round(x$fp_per_hour, 3),
      Total_Truth = x$total_truth,
      TP_Truth = x$tp_truth,
      FN_Truth = x$fn_truth,
      Total_Detections = x$total_detections,
      TP_Det = x$tp_det,
      FP_Det = x$fp_det,
      Total_Hours = round(x$total_hours, 2)
    )
  }))

  # Generate plots
  # First, create a filtered dataset without NA values for plotting
  plot_data <- metrics_df[!is.na(metrics_df$Precision) & !is.na(metrics_df$Recall), ]

  pr_plot <- ggplot(plot_data, aes(x = Recall, y = Precision)) +
    geom_line() +
    geom_point(aes(color = Threshold), size = 3) +
    geom_text(aes(label = sprintf("%.1f", Threshold)), nudge_y = 0.02, size = 3) +
    scale_color_gradient(low = "red", high = "green") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Precision-Recall Curve",
         x = "Recall",
         y = "Precision") +
    theme_minimal() +
    theme(legend.position = "none")

  fphr_plot <- ggplot(plot_data, aes(x = FP_per_hour, y = Recall)) +
    geom_line() +
    geom_point(aes(color = Threshold), size = 3) +
    geom_text(aes(label = sprintf("%.1f", Threshold)), nudge_y = 0.02, size = 3) +
    scale_color_gradient(low = "red", high = "green") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Recall vs False Positives per Hour",
         x = "False Positives per Hour",
         y = "Recall") +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(
    metrics = metrics_df,
    summary = summary_df,
    pr_plot = pr_plot,
    fphr_plot = fphr_plot
  ))
}


# save plots and tables
# (include in the function)



### USER TO RUN ###
# results

# # With default >0% overlap requirement
# results <- evaluate_detections(truth_tables, det_tables, schedule_path = schedule_path, output_dir = output)
# print(results$metrics)
# print(results$summary)
# print(results$pr_plot)
# print(results$fphr_plot)

# With modified % time overlap requirement
results_percentage <- evaluate_detections(truth_tables,
                                          det_tables,
                                          min_overlap_percent = 50,
                                          schedule_path = schedule_path,
                                          output_dir = output)
# View results
print(results_percentage$summary)
print(results_percentage$pr_plot)
print(results_percentage$fphr_plot)
write.table(results_percentage$summary,
            file=paste(output,"/","detPerf_",format(Sys.time(),"%Y%m%d-%H%M%S"),'.txt',sep=""),
            sep='\t',
            na="",
            col.names=TRUE,
            row.names=FALSE,
            quote=FALSE,
            append=FALSE)


