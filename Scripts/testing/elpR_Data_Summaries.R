# ELP Detections and Sampling Effort

# This script reads in these data
# - Elephant rumble detection data (requires columns "Site", "Begin File","File Offset (s)","Rumble Count","Deployment Number","Sound Problems", "Notes")
# - Zero days data to account for sampling effort (requires columns 'Begin File')
# - sound check files
# - site information (Name, optional: Strata, Lat/Long, habitat)


# CAUTION
# Some of these files may not have been updated after changes were made to sound file names, times, subsequent discoveries in poor sound quality, etc.
# This script assumes all the tables are up-to-date. All changes should be updated in these files before they are loaded in .R
# this does not yet include bad dates from the SwiftOne gain setting issue

# bje37@cornell.edu
# updated: Sept 2023

# TO DO
# overlay plot and individual site plot (all sites)
# plot average per strata by unit time with redline (one strata per panel)
# mean monthly plot per strata
# leave gap in plot when no data, rather than connect line
# need to plot rumbles given sampling effort

rm(list = ls())

###### READ FROM BASE TABLES IN BASETABLES FOLDER ######
#
# library(dplyr)
# library(tidyverse)
# library(stringr)
# library(purrr)
# library(lubridate)
# library(readxl)

output <- "L:/ELP/Projects/Dzanga/2022_DSPA_PAM_project/dz_analysis/base_tables/merged_data/dep01-04" # where you want the merged data to be saved to
sound_checks <- "L:/ELP/Projects/Dzanga/2022_DSPA_PAM_project/dz_analysis/base_tables/sound_checks" # where all the sound check files are located
ele_tables<-"L:/ELP/Projects/Dzanga/2022_DSPA_PAM_project/dz_analysis/base_tables/ele" # where all the elephant selection tables are located
zero_txt <- "L:/ELP/Projects/Dzanga/2022_DSPA_PAM_project/dz_analysis/base_tables/HH_zero_detection_days" # where the zero-day (selection tables with dummy events on dates without rumbles) tables are located
# gunshot_tables <-

project_name <- "DzangaBai" # name of project (e.g., "PNNN", "DzangaBai", etc.)
deployment_num <- "01-04" # deployment(s) number (e.g., "01-12", "04")
detector_name <- "HHv6" # # name of the detector used for these data. For HoriHarm:HHv6, FruitPunchAI: FPv1, Stanford Detector: SDv1. For other, type the name. Do not include spaces or special characters

fileDurationMin <- 60 # duration in minutes of the expected sound file duration (60 for 1 hr, 1440 for 1 day)

site_lat_long <- read.table("C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/sites/DSPA_DZ_Sites.txt",
                            header = T, sep ="\t", check.names=FALSE,quote = "\"")# load general table for sites

rand_dates_needed <- "y" # type "y" if so, type "n" of not
ele_bad_sound_remove <- "y" # type "y" if sounds with <23 hrs of sound should be excluded. Type "n" if not
use_only_sites_provided <- "y" # choose "y" if you only want to include the sites that were listed in the site_lat_long file. This enables exclusion of other sites that may be in the selection tables.

data_summaries()

