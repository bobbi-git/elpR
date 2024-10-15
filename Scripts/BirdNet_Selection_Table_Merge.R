#this script merges multiple Raven selection tables produced by BirdNet to prepare the files for analysis and performance evaluation
# bje37@cornell.edu
# Dec 2023

library(stringr)

# update:
setwd('L:/ELP/Detectors/Elephant/Detectors/BirdNetPlus/Detector_output/output_tempSpace') #parent folder of all files
path<-"L:/ELP/Detectors/Elephant/Detectors/BirdNetPlus/Detector_output/output_tempSpace" # parent folder of all files
HH_selection_tables <- 'L:/ELP/Detectors/Elephant/Detectors/BirdNetPlus/Detector_output/output_tempSpace'

# #### A) MERGE SELECTION TABLES WITHIN ONE FOLDER (NEEDS TO BE FIXED) ####
# file_names<-dir(path=HH_selection_tables,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt")
# #file_size<-file_names[sapply(file_names, file.size) > 117] #only read files greater than 117 bytes since those less are blank tables (this will change depending on column names, so update)
#
#   for (i in 1:length(file_names)){
#     table<-read.table(file_names[i],header=TRUE,sep="\t", check.names=FALSE) #read each table separately
#     table_df<-as.data.frame.matrix(table) #save each selection table as a data frame
#     table_df$Selection_Table <- file_names[i]
#     all_txt_df <- lapply(table_df, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
#     merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them
#     merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
#     if(nrow(merge_df>0)){
#       merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
#       write.table(table_df,file_names[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save each table wit same name into same directory
#     }
#   }

#### B) MERGE ALL SELECTION TABLES THAT ARE IN SUBFOLDERS ####
library(plyr)

folder<-list.dirs(HH_selection_tables,recursive=F,full.names=TRUE)
channels <- as.data.frame(read.table("L:/ELP/Detectors/Elephant/Datasets/Testing/Detector_Performance/NN_Channel_Assignments_DetEval.txt", sep="\t", header=T, check.names=FALSE,quote="\""))
colnames(channels) <- c("Channel-New", "Site")

for (j in 1:length(folder)){
  setwd(folder[j]) #so the script jumps into each subfolder
  list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
  #txt_size<-list_txt[sapply(list_txt, file.size) > 200] #ignore tables with less than 200 Bytes (tables less than 1 KB are assumed empty tables for the PNNN analysis)
  all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  merge_df$`Begin File` <-  str_match(merge_df$`Begin Path`,"nn\\d{2}(.*?).wav")[,1]# add begin file from Begin Path if it doesn't exist
  merge_df$`Begin File` <-  str_replace(merge_df$`Begin File`, pattern = "_\\d+\\.", replacement = ".") # remove the file offset value at the end of the file name
  merge_df$`File Offset (s) x` <- merge_df$`File Offset (s)` + as.numeric(gsub('^.|.$', '',(str_extract(merge_df$`Begin Path`, pattern = "_\\d+\\."))))# recalculate the actual file offset for the original sound file
  merge_df$`File Offset (s) OLD` <- merge_df$`File Offset (s)`
  merge_df$`File Offset (s)` <- merge_df$`File Offset (s) x`
  merge_df$ClassifierModel <- "rumble_overlapping_other" # add the name of the classifier that was used in the model for these detections
  merge_df$`Low Freq (Hz)` <- 20
  merge_df$`High Freq (Hz)` <- 100
  merge_df$Site <- substr(merge_df$`Begin File`,start=1,stop=5)
  #merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]} # order the table by Begin File and File Offset
  if(nrow(merge_df>0)){
    merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
    #write.table(merge_df,file=paste(folder[j],"_BirdNet.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table
  }
}

head(merge_df_nn04a)
names(merge_df_channels2)
unique(merge_df_nn04a$Site)

# additional custom steps
merge_df_channels <- merge(merge_df,channels,all.x=T) # add the artificial channel ID to run Mike's Detector Validation Tool (must be a better way to do this)
merge_df_channels2 <- merge_df_channels[,c("Selection", "View", "Channel-New", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                                          "Species Code", "Common Name", "Confidence", "Begin Path", "File Offset (s)", "Begin File",
                                          "ClassifierModel", "Site")]
colnames(merge_df_channels2)[3] <- "Channel"
write.table(merge_df_channels2,file=paste(folder[j],"_BirdNet.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table

# make a smaller dataset to test this process. Use just site nn04a for testing.
merge_df_nn04a <- subset(merge_df_channels2,Site == "nn04a")
write.table(merge_df_nn04a,file=paste(folder[j],"_nn04a_BirdNet.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table

