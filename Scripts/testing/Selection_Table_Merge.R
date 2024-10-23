#this script merges multiple Raven selection tables within subdirectories, saves the selection tables as one
#bje37
# 28 Mar 2019

library(stringr)

# update:
setwd('C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/temp') #parent folder of all files
path<-"C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/temp" # parent folder of all files
HH_selection_tables <- 'C:/Users/bje37/Documents/R/Bobbi_Scripts/Packages/elpR/Files/temp'

#### A) MERGE SELECTION TABLES WITHIN ONE FOLDER (NEEDS TO BE FIXED) ####
file_names<-dir(path=HH_selection_tables,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt")
#file_size<-file_names[sapply(file_names, file.size) > 117] #only read files greater than 117 bytes since those less are blank tables (this will change depending on column names, so update)

  for (i in 1:length(file_names)){
    table<-read.table(file_names[i],header=TRUE,sep="\t", check.names=FALSE) #read each table separately
    table_df<-as.data.frame.matrix(table) #save each selection table as a data frame
    table_df$Selection_Table <- file_names[i]
    all_txt_df <- lapply(table_df, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
    merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them
    merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
    if(nrow(merge_df>0)){
      merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
     # write.table(table_df,file_names[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save each table wit same name into same directory
    }
  }

#### B) MERGE ALL SELECTION TABLES THAT ARE IN SUBFOLDERS ####
folder<-list.dirs(HH_selection_tables,recursive=F,full.names=TRUE)

library(plyr)

for (j in 1:length(folder)){
  setwd(folder[j]) #so the script jumps into each subfolder
  list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
  #txt_size<-list_txt[sapply(list_txt, file.size) > 200] #ignore tables with less than 200 Bytes (tables less than 1 KB are assumed empty tables for the PNNN analysis)
  all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  merge_df$`Begin File` <- str_match(merge_df$`Begin Path`,"nn\\d{2}(.*?).wav")[,1] # add begin file from Begin Path if it doesn't exist
  #merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]} # order the table by Begin File and File Offset
  if(nrow(merge_df>0)){
    merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
    write.table(merge_df,file=paste(folder[j],"_merged.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table
  }
}


library(data.table)
first <- data.table(index = c("a", "a", "b", "c", "c"),
                    type = 1:5,
                    value = 3:7)

second <- data.table(i2 = c("a", "a", "b", "c", "c"),
                     t2 = c(1:3, 7, 5),
                     value = 5:9)

second[first , on=c(i2="index", t2="type"), nomatch=0L]
