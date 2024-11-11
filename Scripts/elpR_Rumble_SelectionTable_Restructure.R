# PNNN detector output selection table restructuring
# WARNING: ONLY RUN THIS SCRIPT ON A *COPY* OF THE DETECTOR OUTPUT
# bje37
# bobbi.estabrook@cornell.edu

#1) Load library elpR
library(elpR)

#2) Set fields
rm(list = ls())
  # Update and run the fields below
  deployment_name <- "dz_202311_nov" # official name of the deployment
  deployment_num <- "03" # update with current deployment number
  disk_ID <- "202" # name of the disk where the sound files are stored (alphanumeric)
  sites <- "DSPA_DZ_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
  sample_rate <- "8kHz" # sample rate of the sound files that the detector was run on

#3) Provide Detector information
  Detector <- "HHv6" # For HoriHarm:HHv6, FruitPunchAI: FPv1, Stanford Detector: SDv1
  Detector_ScoreThreshold <- 0.2 # The score that the detector was run with
  Filter_ScoreThreshold <- 0.4 # The score that the final processed tables should be filtered by

  have_sound_check <- "y" # set this to "y" if you have a sound check report file (from the sound_check script). Set this to "n" if you do not have a sound check report file

#4) Run line below to restructure the gunshot detector selection tables
  Rumble_Selection_Table_Restructure(HH_selection_tables)

  # After all the files are complete:
  # move the file (see list below) to the appropriate folders on the server, and delete the files from the raw, final, and processed folders
  # - Move files from:
  #   - List of empty selection tables: R\Bobbi_Scripts\PNNNR\Files\Empty_HH_Tables\gunshot
  #   - Final selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\final
  #   - intermediate selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\gunshot\processed (delete?)
  #   - Zero detection selection tables: R\Bobbi_Scripts\PNNNR\Files\zero_days_SSTs\gunshot


## TO DO ##
# add detection name and version as column in table
