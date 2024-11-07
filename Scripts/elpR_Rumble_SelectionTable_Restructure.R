# PNNN detector output selection table restructuring
# WARNING: ONLY RUN THIS SCRIPT ON A *COPY* OF THE DETECTOR OUTPUT
# bje37
# bobbi.estabrook@cornell.edu

rm(list = ls())

#1) Initialize the function in the function file

#2) Install packages (if not already installed)
# install.packages(c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate",
# "filesstrings"))


#3) Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(bigreadr)
library(openxlsx)
library(stringr)
library(filesstrings)
library(gsubfn)
library(lubridate)

#4) Set fields
  # Update the fields below
  deployment_name <- "dz_202307_jul" # official name of the deployment
  deployment_num <- "02" # update with current deployment number
  disk_ID <- "406" # name of the disk where the sound files are stored (alphanumeric)
  sites <- "DSPA_DZ_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
  sample_rate <- "8kHz" # sample rate of the sound files that the detector was run on

#5) Provide Detector information
  Detector <- "HHv6" # For HoriHarm:HHv6, FruitPunchAI: FPv1, Stanford Detector: SDv1
  Detector_ScoreThreshold <- 0.2 # The score that the detector was run with
  Filter_ScoreThreshold <- 0.4 # The score that the final processed tables should be filtered by

#5) Run line below to restructure the gunshot detector selection tables
  Rumble_Selection_Table_Restructure(HH_selection_tables)


## TO DO ##
# add detection name and version as column in table
