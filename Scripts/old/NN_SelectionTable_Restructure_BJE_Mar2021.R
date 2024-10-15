#this script formats multiple RavenX selection tables within subdirectories, saves the selection tables, filters the output by score and random days
#bje37
# 1 March 2021

#Steps
#1) Run RavenX selection tables through this R script (files can be in subfolders)
#2) Concatenate the tables
#3) Filter the saved selection tables by score and size using (keep copies of both the filtered and unfiltered files)

rm(list = ls())

#set working directory
setwd('L://ELP//Projects//Nouabale//nn_logs//log_management//Selection_Table_Restructure//HH_tables')
path<-"L://ELP//Projects//Nouabale//nn_logs//log_management//Selection_Table_Restructure//HH_tables"
#output<-"E://Temp//ELP_NN2018_Apr_Tables/New_Tables"


#### 1) FORMAT THE SELECTION TABLES ######

########## A) ELP RANDOM DAYS PER WEEK ####
elp_rand<-read.table('L://ELP//Projects//Nouabale//nn_logs//log_management//Selection_Table_Restructure//rand-days//Rand_dates.csv',header=TRUE,sep="\t",row.names=NULL) #read in rand file that contains 3 random days per week of the survey
elp_rand$Date<-as.Date(elp_rand$Date,"%d-%b-%y") #tell R what the date structure is and read it as date and format the date to match Raven selection tables
elp_rand$Date<-format(elp_rand$Date,"%m/%d/%Y")
elp_rand$"Rand"<-"rand" #create a column that is populated with "rand" to merge with the selection tables
names(elp_rand)[names(elp_rand)=='Date']<-'Begin Date' #change the name of the Days column to "File Date" to match with the File Date column in the selection table

######### B) FORMAT RAVENX SELECTION TABLES ####
file_names<-dir(path=path,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt") #index all the files
file_size<-file_names[sapply(file_names, file.size) > 200] # index only files greater than 200 bytes since those <200 bytes are blank selection tables (the for loop code won't run on empty selection tables)

file_small<-file_names[sapply(file_names, file.size) < 200]# lists (identifies) tables with no detections
write.table(file_small, file='Empty_HH_Tables_List_p2_rand&nonrand.csv',row.names = FALSE) #save a .csv listing the files with no detections

for (i in 1:length(file_size)){ 
    elp_data<-read.table(file_size[i],header=TRUE,sep="\t", check.names=FALSE) #read each table from the 200+ byte list separately
    elp_data_df<-as.data.frame.matrix(elp_data) #save each individual selection table as a data frame
    elp_new<- elp_data_df[ , -which(names(elp_data_df) %in% c("Delta Time (s)","Delta Freq (Hz)", "Notes", "Tags", "SID", "Time Check"))] #remove unwanted columns
    names(elp_new)[names(elp_new) == 'Begin Hour'] <- 'Hour' #change "begin hour" to "hour"
    elp_new$"FileName Date"<-format(as.Date(substr(elp_new$"Begin File",7,14),"%Y%m%d"),"%m/%d/%Y") # use this for date if you want the file date rather than Raven Begin Date
    # elp_new$"Date Check"<-as.Date(elp_new$"Date Check","%m/%d/%Y") # Use this for date if you want the Raven X Date Check for date
    elp_new$"Event Date"<-as.Date(elp_new$"Begin Date","%m/%d/%Y")
    elp_new$"Count"<-NA #add "Count" (formerly "Tag 1") column
    elp_new$"Measurable"<-NA #add "measurable" (formerly "Tag 2") column
    elp_new$"Harmonics"<-NA #add "Harmonics column
    elp_new$"Ambiguous"<-NA #add "ambiguous" (formerly "Tag 3") column 
    elp_new$Notes<-NA #add "Notes" column
    elp_new$Analyst<-NA #add "Analyst" column
    elp_new$"Dep Num"<-"14" # add a deployment number column (change for each deployment)
    elp_new$Score<-round(elp_new$Score,digits = 3) #round the score to 3 decimal places
    elp_new$"Site"<-substr(elp_new$"Begin File",1,5) #add column with the Site ID, derived from the file path name
    elp_rand_data<-merge(elp_new,elp_rand,by="Begin Date",all.x=T) #merge the random days (see elp_rand section above) with selection tables to mark which days were randomly reviewed
    elp_order<-elp_rand_data[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", "Begin Path", "File Offset (s)", 
                               "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes", "Analyst","Rand", "Dep Num")] #reorder columns
    elp_order$"Begin Path"<-gsub("\\\\\\\\159nas\\\\L\\\\ELP\\\\","L:ELP\\\\",elp_order$"Begin Path")
    #elp_order$"Begin Path"<-substr(elp_order$'Begin Path',34,53) # change begin path from deployment folder to site folder
    #elp_order$"Begin Path"<-substr(elp_order$'Begin Path',40,11) # change begin path from deployment folder to site folder
    elp_sort<-elp_order[order(elp_order$"File Offset (s)"),]# sort dataframe by file offset
    elp_sort$'Call Criteria'<-"20210212" # Update this with the latest version of the Call Criteria
    write.table(elp_order,file_size[i],sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) #save each table with same name into same directory
  }

#### 2) MERGE ALL SELECTION TABLES #### there must be a way to merge in the previous step and save this whole step, but I think Liz wanted site-wise files, in which case, that wouldn't be useful
folder<-list.dirs(path,recursive=F,full.names=TRUE)

library(plyr)

  for (j in 1:length(folder)){
    setwd(folder[j]) #so the script jumps into each subfolder
    list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
    txt_size<-list_txt[sapply(list_txt, file.size) > 200] #ignore tables with less than 200 Bytes (tables less than 1 KB are assumed empty tables)
    all_txt_df <- lapply(txt_size, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
    #merge_df <- rbind.fill(all_txt_df, as.data.frame) # Combine them (this still crosses evens between sites, but doesn't skip the end) CAUSES THE "RAND" TO BE "1" FOR SOME REASON (NOT GOOD)
    merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
    merge_df<-merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]
    merge_df$"Selection"<- seq.int(nrow(merge_df)) #renumber the selections for Raven
    write.table(merge_df,file=paste(folder[j],"_HHv6.2_p2.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table
  }
# if: "Error in order(merge_df$"Begin File", merge_df$"File Offset (s)") : argument 1 is not a vector" check file sizes in folder (they are all likely 1 kb and the folder should be deleted, then rerun this section)



#### 3) Cross-reference sound files with known errors (see Sound_Quality_Check r script) #### *update for each deployment*
sound_check<-read.table("L:/ELP/Projects/Nouabale/nn_deployInfo/nn_202205_may_dep14/PNNN_sound_check_dep14.txt",sep="\t",header=TRUE,check.names=FALSE)# load in the sound check file
sound_check$'Sound Problems'<-paste(sound_check$`File Duration Check`, sound_check$'File Length Check',sound_check$`Sound Gap Check`,sound_check$"SampleRate", sound_check$"Deployment Notes",sound_check$"Sound Problems", sound_check$`Exclude (y)`,sep = "; ") # concatenate the sound problems into one column
names(sound_check)[names(sound_check) == 'Current File Name'] <- 'Begin File' #change "begin hour" to "hour"
sound_problem<-sound_check[c("Begin File", "Sound Problems")]
# the cross reference will occur in the next step (4)

#### 4) Filter HH MERGED SELECTION TABLES BY SCORE (>0.4 for Noubale-Ndoki, >0.6 for bais) ####
#install.packages('dplyr')
library('dplyr')
files<-list.files(path=path,pattern="_HHv6.2_p2.txt",all.files=TRUE, recursive=TRUE,full.names=FALSE) #use this so the for loop can jump into each file
setwd('L://ELP//Projects//Nouabale//nn_logs//log_management//Selection_Table_Restructure//HH_tables')

for (q in 1:length(files)){
  elp_merged<-read.table(files[q],header=TRUE,sep="\t",check.names=FALSE) #read in the merged selection table
  elp_merged$"Score"<-as.numeric(elp_merged$"Score") #set the Score column values to numeric
  names(elp_merged)[names(elp_merged) == 'Begin Date'] <- 'begin-date' # rename the "Begin Date" column because Raven won't read it
  elp_sound<-merge(elp_merged,sound_problem,by="Begin File",all.x=T)# cross-reference with sound problems (from step 3 above)
  elp_sound<-elp_sound[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", "Begin Path", "File Offset (s)", 
                             "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes", "Analyst","Rand", "Dep Num","Sound Problems")]
  #names(elp_sound)[names(elp_sound)=='Sound Problems.y']<-'Sound Problems' # DELETE THIS LINE
  elp_th<-filter(elp_sound,elp_sound$"Score">=0.4) #filter the merged table by threshold (the threshold will need to change)
  elp_th_rand<-filter(elp_th,elp_th$Rand == "rand") #make new dataframe for the random days
  elp_th_nonrand<- filter(elp_th,elp_th$Rand == "") #make new dataframe for the non-random days
  elp_th2_4_all<-filter(elp_sound,elp_sound$"Score"<0.4) # filter the random and non random days for events between 0.2 and 0.4 threshold
  # create new directory for files
  write.table(elp_th_rand,file=paste(substr(files[q],1,nchar(files[q])-7),"_p4_rand_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the random table
  write.table(elp_th_nonrand,file=paste(substr(files[q],1,nchar(files[q])-7),"_p4_nonrand_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the random table
  write.table(elp_th2_4_all,file=paste(substr(files[q],1,nchar(files[q])-7),"_p2-p4_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)
  } #if this doesn't run initially because it can't find the file, check that the "Begin Path" in the selection tables match

# Move the selection tables into the deployment folder on the server, then sort into folders
# create empty folders to copy to
# move files to folders

############## Merge ALL p4 rand selection tables (optional, not required if site-wise tables preferred) ##################
path_p4_rand<-("//ag-clo-159NAS105.ad.cornell.edu/L/ELP/Projects/Nouabale/nn_logs/eles/hori-harm/by_deploy/nn_202205_may_dep14/p4_rand_counted") # update this with the path to the files you want to merge (p4 rand). Copy of p4_ran_raw files need to be copied into this folder path to merge them
files<-list.files(path_p4_rand, full.names = TRUE,pattern="*p4_rand_raw.txt",recursive=TRUE) #make a list of all the files within the subfolder j
all_txt_files <- lapply(files, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
merge_files <- do.call("rbind", lapply(all_txt_files, as.data.frame)) # Combine them into a data frame
sort_files<-merge_files[order(merge_files$"Begin File",merge_files$"File Offset (s)"),]
sort_files$"Selection"<- seq.int(nrow(sort_files)) #renumber the selections for Raven
write.table(sort_files,file=paste(path_p4_rand,"_elp_merged_ALL.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table of all events
# delete the files from the folder and move the merged file into the merged raw folder

#quick check of the detector output
sort_files$Date<-as.Date(sort_files$'Event Date',format="%Y-%m-%d")

library(ggplot2)
library(dplyr)
detector_dates<-sort_files %>%
  group_by(Date) %>% tally()

detector_site<-sort_files %>%
  group_by(Site) %>% tally()

detector_summary<-sort_files %>%
  group_by(Site,Date) %>% tally()

ggplot(detector_summary,aes(x=as.factor(Site),y=n))+
  geom_boxplot(fill="slateblue",alpha=0.2)+
  xlab("n Detections")

plot(detector_dates$n~detector_dates$Date)
plot(detector_site$n~as.factor(detector_site$Site))

#split table in to 2000 events
library('bigreadr')
setwd("//ag-clo-159NAS105.ad.cornell.edu/L/ELP/Projects/Nouabale/nn_logs/eles/hori-harm/by_deploy/nn_202205_may_dep14/p4_rand_counted")
path<-"//ag-clo-159NAS105.ad.cornell.edu/L/ELP/Projects/Nouabale/nn_logs/eles/hori-harm/by_deploy/nn_202205_may_dep14/p4_rand_counted"

file<-list.files(path,pattern = ".txt")
#for (k in 1:length(file)){
#  split_file(file[k],2000,prefix_out = paste(file[k],"test",sep="_"),repeat_header = TRUE) #if this gives and error, check that the "_test.txt" file isn't " _test.txt" with a space. If so, delete the space
#}

# OR
for (k in 1:length(file)){
  split_file(file[k],2000,prefix_out = paste("nn_202205_may_HH6.2_p4_rand_raw"),repeat_header = TRUE) #if this gives and error, check that the "_test.txt" file isn't " _test.txt" with a space. If so, delete the space
}


#### 4) Create Zero-Days Selection Table ####
# Create Selection Table with dummy events for dates that don't have detections above 0.4 (p4) to create list of 'zero days'
library(stringr)

#prepare the RavenX files
path<-"L://ELP//Projects//Nouabale//nn_logs//log_management//Selection_Table_Restructure//HH_tables"
ravenx<-list.files(path=path,recursive = TRUE,include.dirs = FALSE, pattern=".txt") #index all the files from Raven-X
ravenx<-as.data.frame(ravenx)
ravenx$Date<-as.Date(substr(ravenx$ravenx,48,55),"%Y%m%d") # date from file name
#ravenx$Date<-as.Date(substr(ravenx$ravenx,92,99),"%Y%m%d") # date from file name
ravenx$Site<-substr(ravenx$ravenx,4,8)

# prepare the p4 files
p4_days<-sort_files #sort files comes from when the tables were merged
p4_days$"Threshold"<-"p4" # indicate that these are p4 score threshold events
#names(p4_days)[names(p4_days) == 'Event Date'] <- 'Date'

# cross-reference file dates with p4 detector file dates
zero_days<-merge(ravenx,p4_days,by=c("Date","Site"),all.x=T) 

# create a new sound selection table with zero days
library(dplyr)

p4_zero<-zero_days[is.na(zero_days$Threshold),]
p4_zero$`Count` = "NA"
p4_zero$Measurable = "NA"
p4_zero$Harmonics = ""
p4_zero$Ambiguous = ""
p4_zero$`Begin Time (s)` = 20
p4_zero$`End Time (s)` = 1000
p4_zero$View = "Spectrogram"
p4_zero$Channel = 1
p4_zero$`Low Freq (Hz)` = 10
p4_zero$`High Freq (Hz)` = 1000
p4_zero$Notes = "No Hori-Harm rumble detections on this date and site at threshold of p4"
p4_zero$Selection = ""
p4_zero$`File Offset (s)`= 20
p4_zero$'Begin Date' <- as.Date(p4_zero$Date,format = '%Y-%m-%d')
p4_zero$'Begin Date'<-format(p4_zero$'Begin Date',"%m/%d/%Y")
p4_zero$"Event Date"<-p4_zero$"Begin Date"
p4_zero <- p4_zero[,-which(names(p4_zero) %in% "Rand")] # remove the rand column to allow for the new
p4_zero$'Sound Problems' <- "" # merge these with sound problems table 

# isolate the rand dates and save the selection table
p4_zero_rand <- merge(p4_zero,elp_rand,by="Begin Date",all.x=T)
p4_zero_rand <- p4_zero_rand[ , which(names(p4_zero_rand) %in% c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", 
                                                                 "Begin Path", "File Offset (s)", "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score", 
                                                                 "Count", "Measurable", "Harmonics","Ambiguous", "Notes", "Analyst","Rand", "Dep Num","Sound Problems"))]
p4_zero_rand <- p4_zero_rand[which(p4_zero_rand$Rand == "rand"),] # filter to only include Rand dates
p4_zero_rand <- p4_zero_rand[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", 
                "Begin Path", "File Offset (s)", "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score", 
                "Count", "Measurable", "Harmonics","Ambiguous", "Notes", "Analyst","Rand", "Dep Num","Sound Problems")]

# Set Begin Path and Begin File from sound file names
sounds<-'L:/ELP/Projects/Nouabale/x_Sounds_deploy_temp/202205_may_dep14'
sound_path<- list.files(path=sounds,all.files=TRUE,full.names=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".wav",) #index all the files in the nn sounds folder and save in data frame
sound_path2<-as.data.frame(sound_path)
names(sound_path2)[names(sound_path2) == 'sound_path'] <- 'Begin Path'
sound_path2$'Begin File' <- substr(sound_path2$'Begin Path',nchar(sound_path2$`Begin Path`)-24,nchar(sound_path2$`Begin Path`))
sound_path2$Site<-substr(sound_path2$`Begin File`,1,5)
sound_path2$"Event Date"<- as.Date(substr(sound_path2$'Begin File',7,14),"%Y%m%d")
sound_path2$"Event Date"<- format(sound_path2$'Event Date',"%m/%d/%Y")

p4_zero_rand_sound <- merge(p4_zero_rand,sound_path2,by= c("Event Date","Site"),all.x=T)#cross reference with the zero days table. Only keep 
p4_zero_rand_sound_new<- p4_zero_rand_sound[ , -which(names(p4_zero_rand_sound) %in% c("Begin Path.x","Begin File.x"))] # remove begin.path.x and begin.file.x
names(p4_zero_rand_sound_new)[names(p4_zero_rand_sound_new) == 'Begin Path.y'] <- 'Begin Path'# change names of begin.path.y and begin.file.y
names(p4_zero_rand_sound_new)[names(p4_zero_rand_sound_new) == 'Begin File.y'] <- 'Begin File'

p4_zero_rand_sound_all <- merge(p4_zero_rand_sound_new,sound_problem,by= c("Begin File"),all.x=T) #add sound problem info
p4_zero_rand_sound_all<- p4_zero_rand_sound_all[ , -which(names(p4_zero_rand_sound_all) %in% c("Sound Problems.x"))] # remove sound problems.x
names(p4_zero_rand_sound_all)[names(p4_zero_rand_sound_all) == 'Sound Problems.y'] <- 'Sound Problems'# change column name

p4_zero_rand_sound_new <- p4_zero_rand_sound_all[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", 
                               "Begin Path", "File Offset (s)", "Begin File", "Site", "Hour", "FileName Date","Event Date", "Score", 
                               "Count", "Measurable", "Harmonics","Ambiguous", "Notes", "Analyst","Rand", "Dep Num","Sound Problems")]
#p4_zero_days<-# rather than zero-files (as the script currently reports) convert to zero calendar days
p4_zero_rand_sound_new$Selection<-seq(1:nrow(p4_zero_rand_sound_new))

write.table(p4_zero_rand_sound_new,file="HoriHarm_202205may_dep14_ZeroDays_Rand_p4.txt",sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)


############## Filter merged table by site (OPTIONAL; incomplete code) ##############
#Site<-list(unique(merge_files$Site)) #list the sites from the merged table
#make a for loop that goes through the list of sites and creates a new dataframe based on the unique site, then saves tables for each dataframe
#for (k in 1:length(mylist)){
#  site_df <- subset(mylist,mylist[k])#subset each site data table as a dataframe
#  write.table(site_df,file=paste(path,"Site[k]_elp_merged_ALL.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save the dataframe as a table
#}



