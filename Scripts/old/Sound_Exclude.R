# Exclude the bad files by moving them to a new folder.

# REQUIREMENTS:
  # This step requires that the Sound_Check_Reports excel sheet be update by marking all sound files to exclude with a 'y' in the Exclude (y) column of the Sounds tab
  # The sound.exclude function will find all files with a 'y' and move them from the sound folder to the 'extra_sounds_folder' that is assigned in the script below
  # CLOSE THE Sound_Check_Report.xlsx file before running this

# The sound.exclude function will:
# - read the excel table produced by the sound_check script
# - move all sounds marked with a 'y' in the "Exclude (y)" column of the sound_check.xlsx file to the excluded sounds folder defined in the script
# Sound files to exclude include files with less than 1 min of recording and bad sound files
# For Hori-Harm, excluded sounds are those with less than 22.95 hours or more than 24 hours (remove for HoriHarm)
# for each site, remove first and last sound file (move for HoriHarm) if they don't form a complete 24-hr day in the date
# - summarize the duration of the excluded files and save the info in the sound_check xlsx file
# - move the Swift config and debug .txt files to the swift_files folder


# 1) clear the environment
rm(list = ls())

## 2) Install packages (if they are not yet installed)
#install.packages(c("filesstrings","dplyr","openxlsx","gsubfn"))

# 3) Load the packages (need to do this each time you open R)
library(filesstrings)
library(dplyr)
library(openxlsx)
library(gsubfn)

# 4) Update the fields below
sound_path <- "L:/ELP/Projects/Nouabale/x_temp/bje_temp/test_sounds" # choose top folder path to sound files. If on the serve, the full server path is needed, not mapped letter
deployment_name <- "test" # official name of the deployment
deployment_num <- "0" # update with current deployment number
disk_ID <- "00" # name of the disk where the sound files are stored (alphanumeric)
sample_rate <- 8000 # sample rate in Hz
fileDurationMin <- 1440 # duration in minutes of the expected sound file duration (60 for 1 hr, 1440 for 1 day)
sound_file_ext <- ".wav" # the extension of the sound file (e.g., ".wav",".flac", or ".aiff")
sites <- "PNNN_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
extra_sounds_folder <-"L:/ELP/Projects/Nouabale/x_temp/bje_temp/extras" # choose a folder to store the extra sound files to.

# 5) run the sound.exclude function
sound.exclude(sound_path,extra_sounds_folder)


# After this scrips is run
# 1) Sound files marked with y will be moved to the extra_sounds_folder defined above
# 2) The sound_check spreadsheet will be updated with excluded sound file statistics
# 3) All Swift Debug and Config files will be moved to \PNNNR\Files\swift_files
