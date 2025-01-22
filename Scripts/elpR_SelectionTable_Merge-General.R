# merge selection tables with differing column names, order, and number
# can be in one folder or subfolders
# bje37@cornell.edu
# Updated Jan 2025


path <- "C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/temp" # directory where selection tables are saved (can have subfolders)


# For files in a single folder:
merge_selection_tables(path, recursive = FALSE)

# For files in folder and subfolders:
merge_selection_tables(path, recursive = TRUE)


