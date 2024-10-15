# rename files from Excel spreadsheet
# bje37
# Oct 2020

#### Rename sound file names using old and new names From an Excel sheet ####
# requires an pre-filled Excel sheet that contains your original file names and paths ("File Path"), and your new file names and paths ("New File Path"). 
# All New File Path values should be previously calculated in the spreadsheet prior to running this script

rm(list = ls())
library(readxl)

# set values
sound_path <- "L:/ELP/Projects/Nouabale/nn_analyses/Cluster_SECR/PNNN_Dep17/Multi-Channel_Files_7" # sound file parent folder location
rename_path <- "L:/ELP/Projects/Nouabale/nn_analyses/Cluster_SECR/PNNN_Dep17/PNNN_dep17_cluster_7-channel_sound_file_rename.xlsx" # path to the excel file that contains columns for the File Path (original path and names) and New Path (same path with new file names)
file_name<-as.data.frame(read_xlsx(rename_path,sheet = "Sheet1",col_names=TRUE)) # add sheet= if a specific sheet of the excel file needs to be used. This reads in the Excel sheet
current_path<-file_name$`File Path` # current path (including file name)
new_path<-file_name$`New File Path` # new path and name of sounds based on the pre-assigned 'New File Path' in the Excel sheet

# rename the sounds
setwd(sound_path)
for (i in 1:length(current_path)){
  file.rename(current_path[i],new_path[i]) #add line to save them in their site folders
}

### sketch space (testing zone) ###
# extract date and time from different file name formats
sounds <- c("5756.210415202958.wav","5756.210416212948.wav","5756.210417232938.wav")
structureA <- "\\d{12}" # file name ex.: project.YYMMDDHHMMSS.wav
formatA <- '%y%m%d%H%M%S'

dates <- as.Date(str_extract(sounds,structureA),format=formatA) # convert format of start date and time to proper data and time format
