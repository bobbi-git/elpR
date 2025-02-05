# PNNN detector output selection table restructuring
# *** WARNING ***: ONLY RUN THIS SCRIPT ON A *COPY* OF THE DETECTOR OUTPUT, NOT THE ORIGINAL because it will modify the selection tables
# bje37
# bobbi.estabrook@cornell.edu

#1) Load library elpR
library(elpR)

#2) Set fields
rm(list = ls())
  # Update and run the fields below
  deployment_name <- "dz_202404_apr" # official name of the deployment
  deployment_num <- "04" # update with current deployment number
  disk_ID <- "201" # name of the disk where the sound files are stored (alphanumeric)
  sites <- "DSPA_DZ_Sites.txt" # Name of the text file in the 'sites' folder that has a list of the sites for this project, include file type extension in name (e.g., 'txt)
  sample_rate <- "8kHz" # sample rate of the sound files that the detector was run on

  three_rand_days <- "y" # if you want 3 random days per week (dates are pre-selected in the R\Bobbi_Scripts\Packages\elpR\Files\rand-days\Rand_dates.csv file), type "y", if not "n" (NOT ENABLED)
  min_23hrs <- "y" # if your project requires a minimum of 23 h per day of sound for elephant analysis, type "y", otherwise "n"
  score_column_name <- "Score" # the name of your score column. In BirdNET, for example, it's Confidence, while in Hori-Harm it's Score.
  # sound check file optional?

#3) Provide Detector information
  Detector <- "HHv6" # For HoriHarm:HHv6, FruitPunchAI: FPv1, Stanford Detector: SDv1
  Detector_ScoreThreshold <- 0.2 # The score that the detector was run with (0.2 for Hori-Harm rumbles)
  Filter_ScoreThreshold <- 0.4 # The score that the final processed tables should be filtered by (0.4 for Hori-harm rumbles)

#4) Run line below to restructure the gunshot detector selection tables
   Rumble_Selection_Table_Restructure(HH_selection_tables)

  # After all the files are complete:
  # move the files (see list below) to the appropriate folders on the server, and delete the files from the raw and final
  # - Move files from:
  #   - List of empty selection tables: R\Bobbi_Scripts\PNNNR\Files\Empty_HH_Tables\rumble
  #   - Final selection tables: R\Bobbi_Scripts\PNNNR\Files\HH_Tables\rumble\final
  #   - Zero detection selection tables: R\Bobbi_Scripts\PNNNR\Files\zero_days_SSTs\rumble
  # - Copy the sound check file to your deployment information table
