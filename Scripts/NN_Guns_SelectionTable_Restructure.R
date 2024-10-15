#this script formats multiple RavenX selection tables within subdirectories, saves the selection tables, filters the output by score and random days
#bje37
# 7 June 2021

#Steps
#1) Run RavenX selection tables through this R script (files can be in subfolders)
#2) Concatenate the tables
#3) Filter the saved selection tables by score and length using (keep copies of both the filtered and unfiltered files)



#set working directory
x <- selection_tables

HH_Selection_Table_Restructure <- function (x,y,z)
{
  setwd(x)

  #### Cross-reference sound files with known errors (see Sound_Quality_Check r script) ####
  sound_check <- read.xlsx(paste("~/R/Bobbi_Scripts/PNNNR/Files/sound_check/Sound_Check_Reports_dep",deployment_num,".xlsx",sep=""),sheet="Sounds",colNames=TRUE,check.names=FALSE,sep.names = " ") # read in the sound_check file that was created in the first step
  sound_check$'Sound Problems'<-paste(sound_check$`File Duration Check`, sound_check$'File Length Check',sound_check$`Sound Gap Check`,sound_check$"SampleRate", sound_check$"Deployment Notes",sound_check$"Sound Problems", sound_check$`Exclude (y)`,sep = "; ") # concatenate the sound problems into one column
  names(sound_check)[names(sound_check) == 'Current File Name'] <- 'Begin File' #change "begin hour" to "hour"
  sound_problem<-sound_check[c("Begin File", "Sound Problems","Exclude (y/e)")]


  file_names<-dir(path=x,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt") #index all the files
  file_size<-file_names[sapply(file_names, file.size) > 200] # index only files greater than 200 bytes since those <200 bytes are blank selection tables (the for loop code won't run on empty selection tables)

  # Record empty selection tables (those with less than 200 bytes)
  file_small<-as.data.frame(file_names[sapply(file_names, file.size) < 200])# lists (identifies) tables with no detections
  if(nrow(file_small) >0){
    file_small$dep <-deployment_num
    colnames(file_small) <- c("Empty Selection Table","Deployment")
    file_small$Site <- substring(sub("_20.*","",file_small$'Empty Selection Table'),4) #NEED TO UPDATE THIS FOR THE CLUSTER SITE NAMES
    file_small$Date <- substr(sub(".*Date_","",file_small$'Empty Selection Table'),start=1,stop=8)
    file_small$Date <- as.POSIXct(file_small$Date,format='%Y%m%d',origin = "1970-01-01",tz="Africa/Brazzaville") # convert format of start date and time to proper data and time format
    file_small$`Deployment Name` <- deployment_name
    file_small$Detection_Score <- Detector_ScoreThreshold
    write.table(file_small, file= paste('~/R/Bobbi_Scripts/PNNNR/Files/Empty_HH_Tables/gunshot/Empty_gunsDTD_Tables_p',sub("\\d.","",Detector_ScoreThreshold),"_dep",deployment_num,'.txt',sep=""),sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) #save a .csv listing the files with no detections
  }

  # Load raw detector output, add new columns and remove columns that are not needed
  for (i in 1:length(file_size)){
    gun_data<-read.table(file_size[i],header=TRUE,sep="\t", check.names=FALSE) #read each table from the 200+ byte list separately
    gun_data_df<-as.data.frame.matrix(gun_data) #save each individual selection table as a data frame
    gun_new<- gun_data_df[ , -which(names(gun_data_df) %in% c("Delta Time (s)","Delta Freq (Hz)", "Notes", "Tags", "SID", "Time Check"))] #remove unwanted columns
    names(gun_new)[names(gun_new) == 'Begin Hour'] <- 'Hour' #change "begin hour" to "hour"
    gun_new$`File Start DateTime` <- str_extract(gun_new$`Begin File`,"\\d{8}.\\d{6}") #  file start date and time from file name (_YYYYMMDD_HHMMSS)
    gun_new$`File Start DateTime` <- as.POSIXct(gun_new$`File Start DateTime`,format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") #tz = "UTC"
    gun_new$`File Start Date`<-format(as.Date(str_extract(gun_new$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y") # use this for date if you want the file date rather than Raven Begin Date
    gun_new$`Selection Begin DateTime` <- gun_new$`File Start DateTime`+ gun_new$`File Offset (s)` # add File Offset (s)  to calculate selection date-time
    gun_new$`Begin Date`<- as.Date(gun_new$'Selection Begin DateTime',"%Y%m%d",tz="Africa/Brazzaville") # different from File Name Date in that it is the date of the event
    gun_new$`Begin Clock Time` <- format(gun_new$`Selection Begin DateTime`,"%H:%M:%S") # Time of event
    gun_new$CFD<-NA #add gunshot coding annotation column
    gun_new$`Gun Type` <- NA
    gun_new$Notes<-NA #add "Notes" column
    gun_new$Analyst<-NA #add "Analyst" column
    gun_new$"Deployment"<-deployment_num # add a deployment number column (change for each deployment)
    gun_new$Score<-round(gun_new$Score,digits = 3) #round the score to 3 decimal places
    gun_new$"Site"<-sub("_.*","",gun_new$`Begin File`)# substr(gun_new$"Begin File",1,5) #add column with the Site ID, derived from the file path name
    gun_order<-gun_new[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                         "Begin Path", "File Offset (s)", "Begin File", "Begin Date","Begin Clock Time","Site", "Hour",
                         "File Start Date", "Score", "CFD","Gun Type","Notes", "Analyst", "Deployment")] #reorder columns
    #gun_order$"Begin Path"<-gsub("\\\\\\\\159nas\\\\L\\\\ELP\\\\","L:ELP\\\\",gun_order$"Begin Path")
    #elp_order$"Begin Path"<-substr(elp_order$'Begin Path',34,53) # change begin path from deployment folder to site folder
    gun_sort<-gun_order[order(gun_order$"File Offset (s)"),]# sort dataframe by file offset
    write.table(gun_sort,file_size[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) #save each table with same name into same directory
  }

  # MERGE ALL SELECTION TABLES
  folder<-list.dirs(x,recursive=F,full.names=TRUE)

  for (j in 1:length(folder)){
    setwd(folder[j]) #so the script jumps into each subfolder
    list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
    #txt_size<-list_txt[sapply(list_txt, file.size) > 200] #ignore tables with less than 200 Bytes (tables less than 1 KB are assumed empty tables)
    all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
    merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them
    #merge_df<-merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]
    merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
    merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
    det_table_name <- paste("guns8p",sub("\\d.","",Detector_ScoreThreshold),"_8k_brut.txt",sep="")
    write.table(merge_df,file=paste(folder[j],"_",deployment_name,"_dep",deployment_num,"_",det_table_name,sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save merged selection tables of the raw detector output (score threshold > 0.2)
  }

# Filter HH MERGED SELECTION TABLES BY SCORE
  files<- dir(path=x,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=paste(deployment_name,"_dep",deployment_num,"_",det_table_name,sep=""))
  setwd(x)

  for (q in 1:length(files)){
    gun_merged<-read.table(files[q],header=TRUE,sep="\t",check.names=FALSE) #read in the merged selection table
    gun_merged$"Score"<-as.numeric(gun_merged$"Score") #set the Score column values to numeric
    #names(gun_merged)[names(gun_merged) == 'Begin Date'] <- 'begin-date' # rename the "Begin Date"
    gun_th<-filter(gun_merged,gun_merged$"Score">=Filter_ScoreThreshold) #filter the merged table by threshold
    table_site <- sub("_.*","",gun_th$`Begin File`[1]) #strapplyc(files[q],"nn\\d{2}\\D", simplify = TRUE) # DOES NOT WORK FOR CLUSTER SITE NAMES. Need to fix
    write.table(gun_th,file=paste("~/R/Bobbi_Scripts/PNNNR/Files/HH_Tables/gunshot/processed/",deployment_name,"_dep",deployment_num,"_",table_site,"_","guns8p",sub("\\d.","",Filter_ScoreThreshold),"_8k_brut.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the filtered table
    } #if this doesn't run initially because it can't find the file, check that the "Begin Path" in the selection tables match

}

#### 4) Create Zero-Days Selection Table ####
# Create Selection Table with dummy events for dates that don't have detections above 0.53 (p53) to create list of 'zero days'

#prepare the RavenX files (every file represents the sound date that the detector was run on, even empty files)
ravenX <- as.data.frame(file_names) # the index of all RavenX detector files from the process above
ravenX$Date <- as.Date(str_extract(ravenX$file_names,"\\d{8}"),"%Y%m%d") # isolate date from file name
ravenX$Site <- sub(".*_nn","",ravenX$file_names) # substr(ravenX$file_names,4,8)
ravenX$Site <- sub("_.*","",ravenX$Site)
ravenX$Site <- paste("nn",ravenX$Site,sep="")

# combine all the processed detector files with the filtered score (these files represent dates with detections events above the score threshold)
dets_path<-("~/R/Bobbi_Scripts/PNNNR/Files/HH_Tables/gunshot/processed/") # this is the path to the processed (i.e., merged, filtered) selection tables
det_files<-list.files(dets_path, full.names = TRUE,pattern=paste("guns8p",sub("\\d.","",Filter_ScoreThreshold),"_8k_brut.txt",sep=""),recursive=TRUE) #make a list of all the files within the subfolder j
all_txt_files <- lapply(det_files, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files
merge_files <- do.call("rbind", lapply(all_txt_files, as.data.frame)) # Combine them into a data frame
sort_files<-merge_files[order(merge_files$"Begin File",merge_files$"File Offset (s)"),] # sort by begin file and file offset (s)
#sort_files$"Selection"<- seq.int(nrow(sort_files)) #renumber the selections for Raven
#write.table(sort_files,file=paste(path_p4_rand,"_elp_merged_ALL.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table of all events

dets_dates <- sort_files
dets_dates$"Threshold"<- Filter_ScoreThreshold
dets_dates$Date <- dets_dates$`Begin Date` #as.Date(substr(p4_days$'Begin File',7,14),"%Y%m%d")

# crossreference file dates with p4 detector file dates
#zero_days<-merge(ravenX,dets_dates,by=c("Date","Site"),all.x=T)

# create a new sound selection table with zero days

p4_zero<-zero_days[is.na(zero_days$Threshold),]
p4_zero$`Tag 1` = "-1"
p4_zero$`Begin Time (s)` = 20
p4_zero$`End Time (s)` = 1000
p4_zero$View = "Spectrogram"
p4_zero$Channel = 1
p4_zero$`Low Freq (Hz)` = 10
p4_zero$`High Freq (Hz)` = 1000
p4_zero$Notes = "No detections on this date and site at this threshold"
p4_zero$Selection = seq(1:nrow(p4_zero))
p4_zero$`File Offset (s)`= 20
p4_zero$'Begin Date' <- as.Date(p4_zero$Date,format = '%Y-%m-%d')
p4_zero$'Begin Date'<-format(p4_zero$'Begin Date',"%m/%d/%Y")
p4_zero <- p4_zero[,-which(names(p4_zero) %in% "Rand")] # remove the rand column to allow for the new

# isolate the rand dates and save the selection table
p4_zero_rand <- merge(p4_zero,elp_rand,by="Begin Date",all.x=T)
p4_zero_rand <- p4_zero_rand[ , which(names(p4_zero_rand) %in% c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                                                                 "Begin Path", "File Offset (s)", "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score",
                                                                 "Tag 1", "Tag 2", "Tag 3", "Notes", "Analyst","Rand", "Dep Num","Sound Problems"))]
p4_zero_rand <- p4_zero_rand[which(p4_zero_rand$Rand == "rand"),] # filter to only include Rand dates
p4_zero_rand <- p4_zero_rand[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                               "Begin Path", "File Offset (s)", "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score",
                               "Tag 1", "Tag 2", "Tag 3", "Notes", "Analyst","Rand", "Dep Num","Sound Problems")]

# Set Begin Path and Begin File from sound file names
sounds<-'L:/ELP/Projects/Nouabale/nn_sounds'
sound_path<- list.files(path=sounds,all.files=TRUE,full.names=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".wav",) #index all the files in the nn sounds folder and save in dara frame
sound_path2<-as.data.frame(sound_path)
names(sound_path2)[names(sound_path2) == 'sound_path'] <- 'Begin Path'
sound_path2$Site<-substr(sound_path2$`Begin Path`,67,71)
sound_path2$'Begin Date' <-
  #cross reference


write.table(p4_zero_rand,file="HoriHarm_sep2020_ZeroDays_Rand.txt",sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)


