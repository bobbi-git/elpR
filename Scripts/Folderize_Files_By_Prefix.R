# sort selection tables to site-specific folders
# BJE 2021

rm(list = ls())

### For selection tables containing multiple sites ###

library(filesstrings)
library(stringr)

setwd('~/PNNNR/Files/HH_Tables/gunshot/raw') # parent folder containing all site folders




# current file paths
old_paths <- dir(path=path, full.names = TRUE,pattern="*.wav",recursive=TRUE) # index paths

# new file paths to move to
new_paths <- substr(old_paths,1,65)

#perform file move
file.move(old_paths,new_paths,overwrite=FALSE)



# _____________________________________________________________________________________#

##### SKETCH SPACE ######

# the above method requires the use to calculate and input from the used when creating a new path
# It could be nicer to automate with with reference to folder names, rather than a substr and paste command
# testing that option below...


folder<-list.dirs(path,recursive=F,full.names=TRUE) # index folders

old_paths <- dir(path=folder, full.names = TRUE,pattern="*.wav",recursive=TRUE) # index paths
files <- basename(old_paths) # index file names

#perform file move
for(i in length(paths)){
  for(j in length(folder)){
    if(substr(basename(paths[i]),1,5) == substr(basename(folder[j]),1,5),
       file.move(paths[i],folder[j]))
}}


files = dir(path=folder, full.names = FALSE,pattern="*.wav",recursive=TRUE)
files = basename(list.files(path=path, full.names = FALSE,pattern="*.wav",recursive=TRUE))

for (j in 1:length(folder)){
  setwd(folder[j]) #so the script jumps into each subfolder
  files = dir(path=folder[j], full.names = TRUE,pattern="*.wav",recursive=TRUE) #make a list of all the files within the subfolder j
  #files = list.files(path=folder[j], full.names = TRUE,pattern="*.mat",recursive=TRUE) #make a list of all the files within the subfolder j (Haven't tested this method, but it may work as well as dir)
  #file.move(files,folder[j]) #move indexed files to new folder location (file.copy to copy files instead)
}


# Delete empty folders (in progress - USE FREECOMMANDER for now)
setwd('D:/Temp/TEST/Rtest') # parent folder containing all site folders
path<-'D:/Temp/TEST/Rtest' # parent folder containing all site folders

folder<-list.dirs(path,recursive=TRUE,full.names=TRUE) # index folders

folderInfo<-file.info(folder)
empty_folders <-folderInfo[which(folderInfo$size == 0),]
empty_folders <- which(file.info(list.dirs(path,recursive=TRUE,full.names=TRUE))$size == 0)
file_names[sapply(file_names, file.size) < 200]# lists presumed empty tables based on file size of 200 bytes (optional)
write.table(file_small, file='Empty_Selection_Table.csv', row.names = FALSE, col.names = FALSE) #save a .csv listing the files with no detections (optional)

sum(file.info(list.files(path,all.files=TRUE,recursive =TRUE))$size)


### For file paths in a listfile (a .txt file) ###
setwd('W:/projects/2015_NYSDEC_NYLongIsland_77766/77766_NYSDEC_Detector_Info/X_Detector_Evaluation/Performance/3Yr_Performance/listfiles/delete')
path<- 'W:/projects/2015_NYSDEC_NYLongIsland_77766/77766_NYSDEC_Detector_Info/X_Detector_Evaluation/Performance/3Yr_Performance/listfiles/delete'
out<-  'W:/projects/2015_NYSDEC_NYLongIsland_77766/77766_NYSDEC_Detector_Info/X_Detector_Evaluation/Performance/3Yr_Performance/Sounds'

listfiles<-dir(path=path,full.names=TRUE)

#merge all the listfiles into one .txt file
for (j in 1:length(listfiles)){
  all_txt_df <- lapply(listfiles, function(x) {read.table(file = x, header = F, sep ="\t", check.names=FALSE)})  # Read the listfiles in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  write.table(merge_df,file="Merged_listfile.txt",sep="\t",na="",col.names=FALSE,row.names=FALSE, quote=FALSE) #save table
}

# copy files using the paths in the listfile to a new folder
for (i in 1:nrow(merge_df)){
  file.copy(merge_df[i,],out) #move indexed files to new folder location (file.copy to copy files instead)
}

snapshot <- fileSnapshot(path, recursive= TRUE) # creates snapshot of info before moving
snapshot$info
