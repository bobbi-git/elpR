# ELP Detections and Sampling Effort

# This script reads in these data
# - Elephant rumble detection data (requires columns "Site", "Begin File","File Offset (s)","Rumble Count","Deployment Number","Sound Problems", "Notes")
# - Zero days data to account for sampling effort (requires columns 'Begin File')
# - sound check files (.xlsx versions produced by the elpR package)
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
# documentation

rm(list = ls())

#
# library(dplyr)
# library(tidyverse)
# library(stringr)
# library(purrr)
# library(lubridate)
# library(readxl)

#### Choose Directories ####

#output <- "L:/ELP/Projects/Nouabale/nn_grid/nn_analyses/base tables/output" # where you want the merged data to be saved to
output<- rstudioapi::selectDirectory(caption = "Select folder for the output to be saved",label = "Select",path = getwd())

#sound_checks <- "L:/ELP/Projects/Nouabale/nn_grid/nn_analyses/base tables/sound_checks" # where all the sound check files are located
sound_checks <- rstudioapi::selectDirectory(caption = "Select folder containing 'sound check' files",label = "Select",path = getwd())

#ele_tables <-"L:/ELP/Projects/Nouabale/nn_grid/nn_analyses/base tables/ele" # where all the elephant selection tables are located
ele_tables <- rstudioapi::selectDirectory(caption = "Select folder containing selection tables",label = "Select",path = getwd())

#zero_txt <- "L:/ELP/Projects/Nouabale/nn_grid/nn_analyses/base tables/HH_zero_detection_days" # where the zero-day (selection tables with dummy events on dates without rumbles) tables are located
zero_txt <- rstudioapi::selectDirectory(caption = "Select folder containing 'zero days' selection tables",label = "Select",path = getwd())

# choose the site file with latitude, longitude, "vegetation Class" (optional), "strata" (optional)
site_lat_long <- read.table("C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/sites/PNNN_Sites_latLongStrata.txt",
                            header = T, sep ="\t", check.names=FALSE,quote = "\"")# load general table for sites

# empty tables?

# gunshot_tables <-

project_name <- "PNNN" # name of project (e.g., "PNNN", "DzangaBai", "Kakum", etc.)
deployment_num <- "01-20" # deployment(s) number (e.g., "01-12", "04")
detector_name <- "HHv6" # # name of the detector used for these data. For HoriHarm:HHv6, FruitPunchAI: FPv1, Stanford Detector: SDv1. For other, type the name. Do not include spaces or special characters

fileDurationMin <- 1440 # duration in minutes of the expected sound file duration (60 for 1 hr, 1440 for 1 day)
# need to figure out how to handle multi-duration files


sound_check_include <- "y" # if you have a sound_check file for this script to filter out bad sounds, choose "y", otherwise choose "n"
rand_dates_needed <- "y" # type "y" if so, type "n" of not
#zeroDays_table_exist <- "y" # if you used the rumble selection table restructure R script from the elpR package to restructure selection tables, a 'Zero Days' file was created. Select it from the folder above and choose "y". If not, choose "n"
ele_bad_sound_remove <- "y" # type "y" if sounds with <23 hrs of sound should be excluded. Type "n" if not
use_only_sites_provided <- "y" # choose "y" if you only want to include the sites that were listed in the site_lat_long file. This enables exclusion of other sites that may be in the selection tables.
# NOT IN YET: use_only_files_in_soundCheck <- "y" # do you want to exclude selections that do not occur on sound files listed in the sound check?
# create map of results <- "y" # do you want the data plottod on a map ("y" if yes)?

data_summaries()

