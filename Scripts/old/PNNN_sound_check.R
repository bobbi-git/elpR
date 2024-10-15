#1) Install packages (if they are not yet installed)
install.packages(c("warbleR","ggplot2","stringr","lubridate","data.table","tuneR","plyr","dlpyr","tidyr","openxlsx"))

rm(list = ls())

# 2) Load the packages (need to do this each time you open R)
library(warbleR)
library(stringr)
library(lubridate)
library(data.table)
library(tuneR)
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)

#3) Run the pnnn_sound_check_function.R function to check the sound files for problems

### UPDATE THESE FIELDS ####
sound_path <- "//ag-clo-159NAS105.ad.cornell.edu/L/ELP/Projects/Nouabale/dep15/testing_bje/sound" # choose top folder path to sound files

deployment_name <- "nn_202210_oct"
deployment_num <- "01" # update with current deployment number
disk_ID <- "07" # name of the disk where the sound files are stored

sample_rate <- 8000 # sample rate in Hz


# Run the function
sound.check(sound_path)

# Note: This script can take several minutes to complete.
# If you see the red stop sign at the top right of the Console, the script is running.
# If you see red script in the Console, contact Bobbi (bje37@cornell.edu)

# Once the script finishes running,
# 1) a preview of the sound_check file will open in R and summaries will be printed in the console below
# 2) a .txt file of the sound_check data will be saved to the \PNNNR\Files\sound_check folder
# 3) a .xlsx file of the sound_check reports will be saved to the \PNNNR\Files\sound_check folder

