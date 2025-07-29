# global.R
required_packages <- c(
  "shiny", "plyr", "dplyr", "ggplot2", "bigreadr", "openxlsx", "stringr",
  "gsubfn", "lubridate", "filesstrings", "data.table", "warbleR",
  "tuneR", "tidyr", "devtools", "bslib", "shinyFiles"
)

# Install and load packages
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
