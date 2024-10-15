# use to move or copy multiple files within multiple folders to new locations
# BJE 2021

rm(list = ls())

### For specific files in a single folder path ###

#install.packages("filesstrings")
library(filesstrings)

setwd('L:/ELP/Detectors/Detector_Performance/NN_Rumbles') # parent folder containing all other folders
path<-"L:/ELP/Detectors/Detector_Performance/NN_Rumbles" # parent folder

folder<-list.dirs(path,recursive=F,full.names=TRUE)

for (j in 1:length(folder)){
  setwd(folder[j]) #so the script jumps into each subfolder
  files = dir(path=folder[j], full.names = TRUE,pattern="*.mat",recursive=TRUE) #make a list of all the files within the subfolder j
  #files = list.files(path=folder[j], full.names = TRUE,pattern="*.mat",recursive=TRUE) #make a list of all the files within the subfolder j (Haven't tested this method, but it may work as well as dir)
  file.move(files,folder[j]) #move indexed files to new folder location (file.copy to copy files instead)
}


#__________________________________________________________________________#

### For file paths in listfiles (a .txt file) ###
setwd('L:/ELP/Detectors/Detector_Performance/NN_Rumbles')
path<- 'L:/ELP/Detectors/Detector_Performance/NN_Rumbles/'
out<-  'L:/ELP/Detectors/Detector_Performance/Rumbles/Sounds/Nouabale-Ndoki_Grid'

listfiles<-dir(path=path,full.names=TRUE)

#merge all the listfiles into one .txt file
for (j in 1:length(listfiles)){
  all_txt_df <- lapply(listfiles, function(x) {read.table(file = x, header = F, sep ="\t", check.names=FALSE)})  # Read the listfiles in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  write.table(merge_df,file="Merged_listfile.txt",sep="\t",na="",col.names=FALSE,row.names=FALSE, quote=FALSE) #save table
}

# copy files using the paths in the listfile to a new folder
for (i in 1:nrow(merge_df)){
  file.copy(merge_df[i,],out) #copy indexed files to new folder location (file.copy to copy files instead)
}



#__________________________________________________________________________#

#### For file paths in a single listfile (a .txt file) ####
setwd('L:/ELP/Projects/Nouabale/dep15/testing_bje')
path<- 'L:/ELP/Projects/Nouabale/dep15/testing_bje/'
out<-  'L:/ELP/Projects/Nouabale/dep15/testing_bje/temp_sounds'

listfile<-read.table("sound_listfile.txt", header = F, check.names=FALSE)

# copy files using the paths in the listfile to a new folder
for (i in 1:nrow(listfile)){
  #file.copy(listfile[i,],out) #copy indexed files to new folder location
  file.move(listfile[i,],out) # move indexed files to new folder location
  }



#__________________________________________________________________________#


#### For files based on Selection table Begin file
# setwd
# set output path for files to be copied to
# for all selection tables
#read Begin Path column for all selection tables in wd

setwd('L:/ELP/Detectors/Detector_Performance/Gunshot/Truth_Tables/Nouabale-Ndoki_8kHz') # selection tables
path<- 'L:/ELP/Detectors/Detector_Performance/Gunshot/Truth_Tables/Nouabale-Ndoki_8kHz' # selection tables
out<-  'L:/ELP/Detectors/Detector_Performance/Gunshot/Sounds/Nouabale-Ndoki_8kHz'# location to copy sounds to

files<-dir(path=path,full.names=TRUE,pattern=".txt")

# for each selection table, copy sound files
for (j in 1:length(files)){
  file <- read.table(files[j], header = T, sep ="\t", check.names=FALSE)
  path <- unique(file$'Begin Path')
  for (i in 1:length(path)){
    file.copy(path[i],out) # copy indexed files to new folder location
  }
}

