# PNNN gunshot detector output selection table restructuring for the Congo team

# bje37
# bobbi.estabrook@cornell.edu

# REQUIREMENTS
# - all detector output saved to one folder (no site sub-folders)
# - detector output from the gunshot data template detector
# - num_events file created by the DTD app


#1) Load package
library(elpR)

#2) Set fields
rm(list = ls())
# Update the fields below and run each line
deployment_name <- "dz_202311_nov" # official name of the deployment
deployment_num <- "03" # update with current deployment number
disk_ID <- "201" # name of the disk where the sound files are stored (alphanumeric)
sites <- "DSPA_Dz_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
sample_rate <- "8kHz" # sample rate of the sound files that the detector was run on

#5) Provide Detector information
Detector <- "DTDguns8" # For the gunshot data template detector:"DTDguns8"
Detector_ScoreThreshold <- 0.53 # The score that the detector was run with
Filter_ScoreThreshold <- 0.53 # The score that the final processed tables should be filtered by

gun_selection_tables <- '~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/gunshot/raw'

# 6) Run function below
gunshot_Selection_Table_Restructure(gun_selection_tables)

# After all the files are complete:
# move the file (see list below) to the appropriate folders on the server, and delete the files from the raw, final, and processed folders
# - Move files from:
#   - List of empty selection tables: R\Bobbi_Scripts\PNNNR\Files\Empty_HH_Tables\gunshot
#   - Final selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\final
#   - intermediate selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\processed (delete?)
#   - Zero detection selection tables: R\Bobbi_Scripts\PNNNR\Files\zero_days_SSTs\gunshot
