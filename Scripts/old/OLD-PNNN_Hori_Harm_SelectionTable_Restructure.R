# PNNN Hori-Harm detector output selection table restructuring
# WARNING: ONLY RUN THIS SCRIPT ON A *COPY* OF THE DETECTOR OUTPUT
# bje37
# bobbi.estabrook@cornell.edu

rm(list = ls())

#1) Initialize the function in the function file

#2) Install packages (if not already installed)
#install.packages(c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate","filesstrings"))

# #3) Load lackages
library(plyr)
library(dplyr)
library(ggplot2)
library(bigreadr)
library(openxlsx)
library(stringr)
library(filesstrings)
library(gsubfn)
library(lubridate)

#4) Update the fields below
  sites <- "PNNN_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
  deployment_name <- "nn_202302_feb"
  deployment_num <- "16" # update with current deployment number
  disk_ID <- "02" # name of the disk where the sound files are stored (alphanumeric)

  # do not update these fields
  Filter_ScoreThreshold <- 0.4 # The score that the final processed tables should be filtered by
  Detector_ScoreThreshold <- 0.2 # The score that the detector was run with

#5) Run line below to restructure the gunshot detector selection tables
  HH_Selection_Table_Restructure(HH_selection_tables)


