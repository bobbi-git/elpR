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


path <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/temp" # directory where selection tables are saved (can have subfolders)

# For files in a single folder:
merge_selection_tables(path, recursive = FALSE)

# For files in folder and subfolders:
merge_selection_tables(path, recursive = TRUE)


