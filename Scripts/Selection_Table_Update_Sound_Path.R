# change sound path or sound names in a selection table

# Two methods here:
# 1) Using new sound file names to replace old ones based on matching site, date and time
# 2) Recalculating date/time in file names in table

rm(list = ls())
library(dplyr)

###### A. Using sound files to match ######

setwd('L:/ELP/Projects/Nouabale/nn_sounds/nn_202101_jan')
path<-'L:/ELP/Projects/Nouabale/nn_sounds/nn_202101_jan'

# 1.a) index the sound files from the site subfolders
sound_paths<-dir(path=path,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".wav") #index all the files
sound_paths<-as.data.frame(sound_paths)
sound_paths$Site<-substr(sound_paths$sound_paths,1,5) # add site ID

# extract the date and time from each sound file
sound_paths$`File Date`<-str_extract(sound_paths$sound_paths,"\\d{8}.\\d{6}")
files$Incorrect_DateTime <- as.POSIXct(files$Incorrect,format='%Y%m%d_%H%M%S',tz="UTC")


###### 2.a) Calculating a new date-time to replace existing one #####

# Index the selection tables that need the sound file names to be updated
setwd("L:/ELP/Projects/Other/Ghana/kakum_logs/HoriHarm/Kakum_2000/test")
tables<-dir("L:/ELP/Projects/Other/Ghana/kakum_logs/HoriHarm/Kakum_2000/test",all.files=TRUE,pattern=".txt")
tables<-as.data.frame(tables)
# tables$Site<-substr(tables$tables,1,5) #extract site ID if needed

#isolate the sites to be updated (if needed)
#new<-tables %>% filter(Site %in% c("nn01b","nn01c","nn01e","nn01f","nn01g","nn02f","nn02g","nn03d","nn03e","nn03f","nn03g","nn04d","nn04e","nn04f","nn05c",
                                     "nn05d","nn05e","nn05f","nn06d","nn06e","nn06f","nn09a","nn09b","nn09c","nn10a","nn10b"))
#new2<-new[,1]
#new2<-tables

# read and modify each table separately
for(i in 1:length(new2)){
  table<-read.table(new2[i],header=TRUE,sep="\t", check.names=FALSE) # read each table
  table<-as.data.frame.matrix(table) # create a data frame with the table
  table$`File Date`<-str_extract(table$`Begin Path`,"\\d{8}.\\d{6}") # extract the date time info from file name
  table$`File Date` <- as.POSIXct(table$`File Date`,format='%Y%m%d_%H%M%S',tz="UTC") # convert the date time info into calendar and clock date/time
  table$`New File Date`<-table$`File Date`-12*60*60 # subtract 12 hours from the date/time
  table$`New File Name`<-paste(table$Site,
                               "_",
                               sprintf('%04d',year(table$`New File Date`)),
                               sprintf('%02d',month(table$`New File Date`)),
                               sprintf('%02d',day(table$`New File Date`)),
                                "_",
                               sprintf('%02d',hour(table$`New File Date`)),
                               sprintf('%02d',minute(table$`New File Date`)),
                               sprintf('%02d',second(table$`New File Date`)),
                               ".wav",sep="")# new file name
  table$`Begin File`<-table$`New File Name`# replace the old file name with the new file name
  table$`New Path`<-substr(table$`Begin Path`,1,nchar(table$`Begin Path`)-25)# create new file path with new begin file
  table$`New Path`<-paste(table$`New Path`,table$`New File Name`,sep="") 
  table$`Begin Path`<-table$`New Path` # replace old file name with new name in begin path
  table_new<-table[c(1:16)] # remove unwanted columns
  write.table(table_new,new2[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) # save the new table and replace the old
}

###### B. Getting file name from a .txt file instead of directory of sounds

# 1.b) or index new file names from .txt file instead of sound files in folder
sound_paths<-read.table("L:/ELP/Projects/Other/Ghana/kakum_sounds/kakum_2000/Sound_File_Rename/Renamed_sound_index.txt", sep="\t",header=TRUE,check.names=FALSE)
sound_paths$"Begin File"<-sound_paths$`First Rename`
sound_paths$Site<-substr(sound_paths$`First Rename`,1,8)

# 2.b) Index the selection tables that need the sound file names to be updated
setwd("L:/ELP/Projects/Other/Ghana/kakum_logs/HoriHarm/Kakum_2000/test")
tables<-dir("L:/ELP/Projects/Other/Ghana/kakum_logs/HoriHarm/Kakum_2000/test",all.files=TRUE,pattern=".txt",full.names = TRUE)

# 3.b) Cross-reference old file names with the indexed file names and file names in each selection table
for (i in length(tables)){
  table<-as.data.frame(read.table(tables[i],sep="\t",header=TRUE, check.names=FALSE)) # read in one table at a time in the loop
  table2<-merge(table,sound_paths,by=c("Begin File","Site"),all=TRUE) # merge the two tables based on site and old begin file name
  table3<-table2[!(is.na(table2$Selection)), ]# delete rows without a selection event (empty rows from the merge)
  table4<-table3[, !(colnames(table3) %in% c("Begin File","First Rename"))] # remove unwanted columns
  table4$`Begin Path`<-substr(table4$`Begin Path`,1,nchar(table4$`Begin Path`)-46) # paste the new name in the file path
  table4$`Begin Path`<-paste0(table4$`Begin Path`,table4$`Second Rename`)
  names(table4)[names(table4) == 'Second Rename'] <- 'Begin File' # replace Begin File with new Begin File
  table5<-table4[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                   "Begin Path", "File Offset (s)", "Begin File", "Site", "Score", "Tag 1", "Tag 2", "Notes", "Analyst")] #reorder columns for Raven
  write.table(table5,tables[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) # save the table
}


###### C. Update sound file path in selection tables from correct listfile

setwd('//ag-clo-159nas105.ad.cornell.edu/L/ELP/Detectors/Detector_Performance/Gunshot/Truth_Tables/Korup_4kHz')
path<-'//ag-clo-159nas105.ad.cornell.edu/L/ELP/Detectors/Detector_Performance/Gunshot/Truth_Tables/Korup_4kHz/correct_path'

listfile <- read.table("//ag-clo-159nas105.ad.cornell.edu/L/ELP/Detectors/Detector_Performance/Gunshot/Sounds/Korup/Korup_listfile.txt",header=TRUE,sep="\t", check.names=FALSE) # load listfile sound paths

tables<-dir("//ag-clo-159nas105.ad.cornell.edu/L/ELP/Detectors/Detector_Performance/Gunshot/Truth_Tables/Korup_4kHz",all.files=TRUE,pattern=".txt")
for(i in 1:length(tables)){
  table<-read.table(tables[i],header=TRUE,sep="\t", check.names=FALSE) # read each table
  table2 <- merge(table,listfile,by="Begin File",all.x=FALSE) # merge the listfile and table by begin file 
  names(table2)[names(table2) == 'Begin Path'] <- "Old Begin Path" # rename the old begin path
  names(table2)[names(table2) == 'New Begin Path'] <- "Begin Path" # rename the new begin path
  write.table(table2,tables[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) # save the table
}


###### D. Update sound file path in selection tables by replacing start of Begin File directory

setwd('L:/ELP/Projects/Nouabale/nn_logs/eles/hori-harm/by_deploy/nn_202201_jan_dep13/p4_rand_counted/temp')
tables_path<-'L:/ELP/Projects/Nouabale/nn_logs/eles/hori-harm/by_deploy/nn_202201_jan_dep13/p4_rand_counted/temp'

tables<-dir(path=tables_path,all.files=TRUE,pattern=".txt")
for(i in 1:length(tables)){
  table<-read.table(tables[i],header=TRUE,sep="\t", check.names=FALSE,fill=TRUE) # read each table
    for(j in 1:nrow(table)){
      table$'Begin Path'<-paste0("L:\\ELP\\Projects\\Nouabale\\nn_sounds\\",table$Site,"\\",table$'Begin File')
      }
  write.table(table,tables[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) # save the table
  }
