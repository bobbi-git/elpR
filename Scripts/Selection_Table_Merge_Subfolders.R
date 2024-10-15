#this script merged multiple Raven selection tables within subdirectories, saves the selection tables as one per folder
#bje37
# 28 Mar 2019

#### MERGE ALL SELECTION TABLES IN SUBFOLDERS ####
path<- "" # path to the parent folder of all the selection tables. This script expects them to be in subfolders.

folder<-list.dirs(path,recursive=F,full.names=TRUE)

library(plyr)

for (j in 1:length(folder)){
  setwd(folder[j]) #so the script jumps into each subfolder
  list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
  all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them
  merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
  if(nrow(merge_df>0)){
    merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
    write.table(merge_df,file=paste(folder[j],"_HHv6.2_p2.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table
  }
}
