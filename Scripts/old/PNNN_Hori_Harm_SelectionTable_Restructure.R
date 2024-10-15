# PNNN Hori-Harm detector output selection table restructuring
# WARNING: ONLY RUN THIS SCRIPT ON A *COPY* OF THE DETECTOR OUTPUT
# bje37
# bobbi.estabrook@cornell.edu

rm(list = ls())

#1) Initialize the function in the function file

#2) Install packages (if not already installed)
# install.packages(c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate",
# "filesstrings"))


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

#4) Set fields
  # Update the fields below
  deployment_name <- "an_202312_dec" # official name of the deployment
  deployment_num <- "01" # update with current deployment number
  disk_ID <- "00" # name of the disk where the sound files are stored (alphanumeric)
  sites <- "NatGeo_an_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)

  # do not update these fields
  Filter_ScoreThreshold <- 0.25 # The score that the final processed tables should be filtered by
  Detector_ScoreThreshold <- 0.25 # The score that the detector was run with

#5) Run line below to restructure the gunshot detector selection tables
  HH_Selection_Table_Restructure(HH_selection_tables)


