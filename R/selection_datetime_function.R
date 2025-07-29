# selection table date/time from file name and file offset
# bje37@cornell.edu

# function to extract and modify datetime
#' @name selection_datetime
#' @title Extract Date/Time from Selection Table
#' @description Extract and modify datetime from Begin File name and File offset (s)
#' @param filename A string containing the filename with format "YYYYMMDD_HHMMSS"
#' @param file_offset_seconds Numeric offset in seconds (Raven's File Offset (s))
#' @return POSIXct datetime object in new Begin Clock Time
#' @import lubridate stringr
#' @export
#' @importFrom lubridate seconds
#' @importFrom stringr str_extract

selection_datetime <- function(filename, file_offset_seconds) {
  library(lubridate)
  library(stringr)

  # Extract date and time using regex
  datetime_str <- str_extract(filename, "\\d{8}_\\d{6}")

  # Convert to POSIXct datetime object
  datetime <- ymd_hms(gsub("_", "", datetime_str))

  # Add offset
  new_datetime <- datetime + seconds(file_offset_seconds)

  return(new_datetime)
}

# # Apply to dataframe
# # Assuming your dataframe is called 'df' with columns 'filename' and 'File Offset (s)'
# df$event_datetime <- mapply(selection_datetime,
#                             df$filename,  # or whatever column contains your filenames
#                             df$`File Offset (s)`)
#
# # If you want to format the output in a specific way:
# df$event_datetime_formatted <- format(df$event_datetime, "%Y-%m-%d %H:%M:%S")
