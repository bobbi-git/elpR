# PNNN gunshot detector output selection table restructuring for the Congo team

# bje37
# bobbi.estabrook@cornell.edu

# REQUIREMENTS
# - all detector output saved to one folder (no site sub-folders)
# - detector output from the gunshot data template detector
# - num_events file created by the DTD app

# 1) clear environment
rm(list = ls())

# 2) Install packages (if not already installed)
#install.packages(c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate","filesstrings"))

 #3) Load lackages
library(plyr)
library(dplyr)
library(ggplot2)
library(bigreadr)
library(openxlsx)
library(stringr)
library(filesstrings)
library(gsubfn)
library(lubridate)

# 4) Update fields below
deployment_num <- "15" # update with current deployment number (same as in step 1 (sound_check))
deployment_name <- "nn_202210_oct"
disk_ID <- "05" # name of the disk where the sound files are stored\  deployment_name <- "dz_202305_may" # official name of the deployment
sites <- "DSPA_DZ_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)


# 5) Run these lines of code, but do not change them
gun_selection_tables <- '~/R/Bobbi_Scripts/PNNNR/Files/HH_Tables/gunshot/raw' # directory with the raw detector selection tables (they can be in subfolders)
Filter_ScoreThreshold <- 0.53 # The score that the final processed tables should be filtered by
Detector_ScoreThreshold <- 0.53 # The score thereshold that the detector was run withgun_num_events <- '~/R/Bobbi_Scripts/PNNNR/Files/num_events/gunshot' # path to the num_events file saved by the detector

# 6) Run below for the hori-harm detector selection tables
guns8p53_Selection_Table_Restructure(gun_selection_tables)

# After all the files are complete:
# move the file (see list below) to the appropriate folders on the server, and delete the files from the raw, final, and processed folders
# - Move files from:
#   - List of empty selection tables: R\Bobbi_Scripts\PNNNR\Files\Empty_HH_Tables\gunshot
#   - Final selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\final
#   - intermediate selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\processed (delete?)
#   - Zero detection selection tables: R\Bobbi_Scripts\PNNNR\Files\zero_days_SSTs\gunshot
