# Sound checking audio files doe wlwphant rumble analysis
# bje37
# bobbi.estabrook@cornell.edu

# Initialize the function in the function file

# Install packages (if not already installed)
## install.packages(c("devtools","warbleR","stringr","lubridate","data.table","tuneR","plyr","dplyr","tidyr","openxlsx","ggplot2","filesstrings","gsubfn"))

# clear the environment
rm(list = ls())

# Load lackage
library(devtools)
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
library(filesstrings)
library(gsubfn)


# Set fields
sound_path <- "G:/Kakum2024/Sound_Files/Kakum_2024" # choose top folder path to sound files
deployment_name <- "kk_202405_may" # official name of the deployment
deployment_num <- "02" # update with current deployment number
disk_ID <- "366" # name of the disk where the sound files are stored (alphanumeric)
sample_rate <- 8000 # sample rate in Hz
fileDurationMin <- 60 # duration in minutes of the expected sound file duration (60 for 1 hr, 1440 for 1 day)
sound_file_ext <- ".wav" # the extension of the sound file (e.g., ".wav",".flac", or ".aiff")
sites <- "Kakum_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
extra_sounds_folder <-"G:/Kakum2024/Sound_Files/Kakum2024_extra_sounds" # choose a folder to store the extra sound files to.


#### 1) SOUND CHECK ####
# This script will:
# 1) Check the duration of sound files and mark sounds < 1 min  with "y" for exclusion
# 2) Check the total daily duration of sound files and mark those with <23 hrs with "e" for exclusion from the elephant analysis
# 3) Check the sample rate of the sound file
# 4) Check expected duration of sound files
# 5) produce a report of the sound overview in an Excel file with multiple workbooks
sound.check(sound_path)

####  2) SOUND EXCLUDE ####
### DO NOT RUN THIS UNTIL YOU HAVE REVIEWED THE SOUND CHECK REPORT EXCEL SHEET AND CONFIRMED WHICH SOUND FILES SHOULD BE EXCLUDED
# This script will:
# 1) Sound files marked with y will be moved to the extra_sounds_folder defined above
# 2) The sound_check spreadsheet will be updated with excluded sound file statistics
# 3) All Swift Debug and Config files will be moved to \PNNNR\Files\swift_files if have_SwiftFiles is set to "y"

have_SwiftFiles <- "y" # type "y" (lower-case) if you have Swift files in the sound folders. Type "n" if you do not.
merge_swift_files <- "y" # type y if you want the swift files to be merged together into one file (this will not delete the single swift files). For this to work, all the swift files must have the same structure (columns names and order)

sound.exclude(sound_path,extra_sounds_folder)





