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
sound_path <- "L:/ELP/Projects/Dzanga/2022_DSPA_PAM_project/dz_sounds/dz_202305_may_dep01-WAV-8kHz" # choose top folder path to sound files
deployment_name <- "dz_202305_may" # official name of the deployment
deployment_num <- "01" # update with current deployment number
disk_ID <- "01" # name of the disk where the sound files are stored (alphanumeric)
sample_rate <- 8000 # sample rate in Hz
fileDurationMin <- 60 # duration in minutes of the expected sound file duration (60 for 1 hr, 1440 for 1 day)
sound_file_ext <- ".wav" # the extension of the sound file (e.g., ".wav",".flac", or ".aiff"). FLAC files require a flac program installed on PC
sites <- "DSPA_DZ_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)

# Run the function
sound.check(sound_path)

# Note: This script can take several minutes to complete.
# If you see the red stop sign at the top right of the Console, the script is running.
# If you see red script in the Console, contact Bobbi (bje37@cornell.edu)

# Once the script finishes running,
# 1) a preview of the sound_check file will open in R and summaries will be printed in the console below
# 2) a .txt file of the sound_check data will be saved to the \PNNNR\Files\sound_check folder
# 3) a .xlsx file of the sound_check reports will be saved to the \PNNNR\Files\sound_check folder

