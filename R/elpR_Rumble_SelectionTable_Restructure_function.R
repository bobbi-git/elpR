#this script formats multiple Hori-Harm selection tables within subdirectories, saves the selection tables, filters the output by score and random days
#bje37
# 1 March 2021

# This script will:
# - read in all selection tables from HoriHarm detector output (required that they are in site-folders)
# - restructure those tables
# - merge the detector tables into site-wise tables
# - add sound_check info to the selection tables (sound check should have been run BEFORE this step)
# - add the random days designation to the 3 randomly selected days per week
# - ignore detections on sound files that were marked with 'y' (bad sound file) or 'e' (bad sound file for elephant analysis) in the sound check excel sheet
# - save selection tables that are filtered by score and random dates with maximum of 2000 selections per table (in the Packages/elpR\File\Rumble\Final folder)
# - count the total number of detections per site and day by score 0.2, 0.4, and 0.4 random days and sound file duration (added to the sound check excel sheet as a tab, saved in the Packages/elpR\Files|sound_check folder)
# - calculates the total number of detections per hour for each sound file
# - plot the total detections per score of 0.2 and 0.4 rand as .png files (saved in num_events folder)
# - count the total number of days without detections at 0.2 and 0.4
# - save a selection table of sound files without detections at score 0.4 for random dates (saved in the Packages/elpR\Fileszero_days_SSTs\rumble folder)

# TO DO
# check that all selections for have a unique selection ID

HH_Selection_Table_Restructure <- function (x) {


  # # install and load necessary packages
  # sel_table_struct <- c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate","filesstrings")
  # if(!require(sel_table_struct)){
  #   install.packages(sel_table_struct)}
  # lapply(sel_table_struct, library, character.only=TRUE)

 #### PRE-CREATE SOME FILEDS FOR SCRIPT ####

  # default names
  raw_table_name <-paste(deployment_name,"_",deployment_num,"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_",sample_rate,"_brut",sep="") #default raw output table name
  filtered_table_name <-  paste(deployment_name,"_",deployment_num,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_8k_brut",sep="") # default filtered table name
  standard_name_disk <- paste(deployment_name,"_dep",deployment_num,"_d",disk_ID, sep="") # output file names (deployment name, number, disk)

  # Hori_Harm folders
  processed_HH <- "~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/"
  path_p4_rand <- "~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p4_rand_raw/"
  raw_selection_tables <- '~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/raw' # directory with the raw detector selection tables (they can be in subfolders)


  # create empty sub-folders to receive the selection tables in the processed folder
  HH_folders <- basename(list.dirs(raw_selection_tables,recursive=F))
  for(z in 1:length(HH_folders)){
   if (!dir.exists(paste(processed_HH,HH_folders[z],sep=""))) dir.create(paste(processed_HH,HH_folders[z],sep=""))
    }
  # HH_folders <- read.table(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sites/",sites, sep=""),header=FALSE,row.names=NULL)
  # for(z in 1:nrow(HH_folders)){
  #  if (!dir.exists(paste(processed_HH,HH_folders[z,],sep=""))) dir.create(paste(processed_HH,HH_folders[z,],sep=""))
  #   }

  setwd(raw_selection_tables)


  #### ELP RANDOM DAYS PER WEEK ####
  elp_rand<-read.table('~/R/Bobbi_Scripts/Packages/elpR/Files/rand-days/Rand_dates.csv',header=TRUE,sep="\t",row.names=NULL) #read in rand file that contains 3 random days per week of the survey
  elp_rand$Date<-format(as.Date(elp_rand$Date,"%d-%b-%y"),"%m/%d/%Y") #tell R what the date structure is and read it as date and format the date to match Raven selection tables
  elp_rand$"Rand"<-"rand" #create a column that is populated with "rand" to merge with the selection tables
  names(elp_rand)[names(elp_rand)=='Date']<-'Begin Date' #change the name of the Days column to "File Date" to match with the File Date column in the selection table


  #### Cross-reference sound files with known errors (see Sound_Quality_Check r script) ####
  sound_check <- read.xlsx(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),
                           sheet="Sounds",colNames=TRUE,check.names=FALSE,sep.names = " ") # read in the sound_check file that was created in the first step
  sound_check$'Sound Problems'<-paste(sound_check$`File Duration Check`, sound_check$'File Length Check',sound_check$`Sound Gap Check`,sound_check$"SampleRate",
                                      sound_check$"Deployment Notes",sound_check$"Sound Problems", sound_check$`Exclude (y/e)`,sep = "; ") # concatenate the sound problems into one column
  names(sound_check)[names(sound_check) == 'Current File Name'] <- 'Begin File'
  sound_problem<-sound_check[c("Begin File", "Sound Problems","Exclude (y/e)")]
  # the cross reference will occur in the next step (4)

  # Identify empty selection tab
  file_names <-dir(path=raw_selection_tables,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt") #index all the files
  file_size <-file_names[sapply(file_names, file.size) > 200] # index only files greater than 200 bytes since those <200 bytes are blank selection tables (the for loop code won't run on empty selection tables)

  # Record empty selection tables (those with less than 200 bytes) in a .txt file
  file_small <-as.data.frame(file_names[sapply(file_names, file.size) < 200])# lists (identifies) tables with no detections
  if(nrow(file_small) >0){
    file_small$dep <-deployment_num
    colnames(file_small) <- c("Empty Selection Table","Deployment")
    file_small$Site <- substring(sub("_20.*","",file_small$'Empty Selection Table'),4) #NEED TO UPDATE THIS FOR THE CLUSTER SITE NAMES
    file_small$Date <- substr(sub(".*Date_","",file_small$'Empty Selection Table'),start=1,stop=8)
    file_small$Date <- as.POSIXct(file_small$Date,format='%Y%m%d',origin = "1970-01-01",tz="Africa/Brazzaville") # convert format of start date and time to proper data and time format
    file_small$`Deployment Name` <- deployment_name
    file_small$'Detection Score Threshold' <- Detector_ScoreThreshold
    write.table(file_small, file=paste('~/R/Bobbi_Scripts/Packages/elpR/Files/Empty_Tables/rumble/',standard_name_disk,"_Empty_Tables_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),'.txt',sep=""),
                sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) #save a .csv listing the files with no detections
  }

######### FORMAT Detector SELECTION TABLES ####
  for (i in 1:length(file_size)){
      elp_data<-read.table(file_size[i],header=TRUE,sep="\t", check.names=FALSE,quote="\"") #read each table from the 200+ byte list separately
      elp_new<-as.data.frame.matrix(elp_data) #save each individual selection table as a data frame
      elp_new$`Begin File` <- basename(elp_new$`Begin Path`) # str_match(elp_new$`Begin Path`,"nn\\d{2}(.*?).wav")[,1]
      elp_new$`File Start DateTime` <- str_extract(elp_new$`Begin File`,"\\d{8}.\\d{6}") #  file start date and time from file name (_YYYYMMDD_HHMMSS)
      elp_new$`File Start DateTime` <- as.POSIXct(elp_new$`File Start DateTime`,format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") #tz = "UTC"
      elp_new$`File Start Date`<-format(as.Date(str_extract(elp_new$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y") # use this for date if you want the file date rather than Raven Begin Date
      elp_new$`Selection Begin DateTime` <- elp_new$`File Start DateTime`+ elp_new$`File Offset (s)` # add File Offset (s)  to calculate selection date-time
      elp_new$`Begin Date`<- format(as.Date(elp_new$'Selection Begin DateTime',"%Y%m%d",tz="Africa/Brazzaville"),"%m/%d/%Y") # different from File Name Date in that it is the date of the event
      elp_new$`Begin Clock Time` <-format(elp_new$`Selection Begin DateTime`,"%H:%M:%S") # Time of event
      elp_new$`Begin Hour` <- format(as.POSIXct( elp_new$`Begin Clock Time` ,format="%H:%M:%S"),"%H")# hour(elp_new$`Begin Clock Time`)
      elp_new$"Count"<-NA #add "Count" (formerly "Tag 1") column
      elp_new$"Measurable"<-NA #add "measurable" (formerly "Tag 2") column
      elp_new$"Harmonics"<-NA #add "Harmonics column
      elp_new$"Ambiguous"<-NA #add "ambiguous" (formerly "Tag 3") column
      elp_new$Notes<-NA #add "Notes" column
      elp_new$Analyst<-NA #add "Analyst" column
      elp_new$"Deployment"<-deployment_num # add a deployment number column (change for each deployment)
      elp_new$Disk <- disk_ID
      elp_new$Score<-as.numeric(round(elp_new$Score,digits = 3)) #round the score to 3 decimal places
      #elp_new$Site<-sub("_.*","",elp_new$`Begin File`)# substr(elp_new$"Begin File",1,5) #add column with the Site ID, derived from the file path name
      #elp_new$Site <- substr(str_extract(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}_*"),1,nchar(str_extract(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}_*"))-1)
      elp_new$Site <- substr(str_match(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_20")[,1],1,
                             nchar(str_match(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_20")[,1])-3)
      elp_rand_data<-merge(elp_new,elp_rand,by="Begin Date",all.x=TRUE) #merge the random days (see elp_rand section above) with selection tables to mark which days were randomly reviewed
      elp_order <- elp_rand_data[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)",
                                 "Low Freq (Hz)", "High Freq (Hz)", "Begin Path", "File Offset (s)",
                                 "Begin File", "Site", "Begin Hour", "File Start Date","Begin Date",
                                 "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes",
                                 "Analyst","Rand", "Deployment","Disk")] #reorder columns
      elp_order$"Begin Path"<-gsub("\\\\\\\\159nas\\\\L\\\\ELP\\\\","L:ELP\\\\",elp_order$"Begin Path")
      elp_sort<-elp_order[order(elp_order$"File Offset (s)"),]# sort dataframe by file offset
      elp_sort$'Call Criteria'<-"20210212" # Update this with the latest version of the Call Criteria
      elp_rand_sound<-merge(elp_sort,sound_problem,by="Begin File",all.x=T)# cross-reference with sound problems if the table is empty and create dummy values
      elp_rand_sound$`Exclude (y/e)`[is.na(elp_rand_sound$`Exclude (y/e)`)|elp_rand_sound$`Exclude (y/e)`==""] <-"Good"
      elp_rand_sound_exclude <- elp_rand_sound[(elp_rand_sound$`Exclude (y/e)` == "Good"),]
      write.table(elp_rand_sound_exclude,
                      paste(processed_HH,file_size[i], sep=""),
                      sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE) #save each table with same name into same directory
      }

#### 2) MERGE ALL SELECTION TABLES BY SITE #### (for site-foldered structure)
  # Note: The section below also works, so this chunk may be able to be deleted
# folder<-list.dirs(processed_HH,recursive=F,full.names=TRUE)
#   #merge
#   for (j in 1:length(folder)){
#     setwd(folder[j]) #so the script jumps into each subfolder
#     list_txt = list.files(path=folder[j], full.names = TRUE,pattern="*.txt") #make a list of all the files within the subfolder j
#     all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
#     merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
#     merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
#     merge_df$"Selection"<- if(nrow(merge_df)>0){seq.int(nrow(merge_df))} #renumber the selections for Raven
#     write.table(merge_df,file=paste(folder[j],"/",standard_name_disk,"_HHv6p2_p",sub("\\d.","",Detector_ScoreThreshold),"_8k_rand_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save tables
#     #write.table(merge_df,file=paste(folder[j],standard_name_disk,"_HHv6p2_p",sub("\\d.","",Detector_ScoreThreshold),"_8k_rand_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save tables
#     }


# if: "Error in order(merge_df$"Begin File", merge_df$"File Offset (s)") : argument 1 is not a vector" check file sizes in folder (they are all likely 1 kb and the folder should be deleted, then rerun this section)

  #### MERGE ALL SELECTION TABLES (this section should work for files that are and are not already in site-wise folders)####
  setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/")
  list_txt = list.files(path="~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/", full.names = TRUE,pattern="*.txt",recursive = TRUE) #make a list of all the files within the subfolder j
  all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
  merge_df$"Selection"<- if(nrow(merge_df)>0){seq.int(nrow(merge_df))} #renumber the selections for Raven

  # separate tables into sites
  siteWise_list <- split(merge_df, f = merge_df$Site)
  for (m in seq(siteWise_list)){
    write.table(siteWise_list[[m]],file=paste(processed_HH,standard_name_disk,"_",names(siteWise_list)[[m]],"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_8k_rand_raw.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save tables
  }

#### Filter HH MERGED SELECTION TABLES BY SCORE (>0.4 for Noubale-Ndoki, >0.6 for bais) ####
setwd(processed_HH)
files <- list.files(path=processed_HH,pattern=paste(standard_name_disk,sep=""),all.files=TRUE, recursive=TRUE,full.names=FALSE) #use this so the for loop can jump into each file
file_size <- files[sapply(files, file.size) > 200]

# create directories to receive the processed selection tables
if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p", sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",sep=""))){
       dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",sep=""))}
if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/",sep=""))){
       dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/",sep=""))}
if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))){
       dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))}

# filter tables and move them to cooresponding folders in the final folder
for (q in 1:length(file_size)){
  elp_merged <- read.table(file_size[q],header=TRUE,sep="\t",check.names=FALSE) #read in the merged selection table
  elp_sound_table <- if(nrow(elp_merged)>0){
            elp_sound <- elp_merged[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                                   "Begin Path", "File Offset (s)", "Begin File", "Site", "Begin Hour", "File Start Date","Begin Date",
                                   "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes", "Analyst","Rand", "Deployment",
                                   "Sound Problems","Call Criteria","Disk")]
            elp_th<-filter(elp_sound,elp_sound$"Score">=Filter_ScoreThreshold) #filter the merged table by score threshold
            elp_th_rand<-filter(elp_th,elp_th$Rand == "rand") #make new dataframe for the random days for the filtered score
            elp_th_nonrand<- filter(elp_th,elp_th$Rand == "") #make new dataframe for the non-random days
            elp_th2_all<-filter(elp_sound,elp_sound$"Score"<Filter_ScoreThreshold) # filter the random and non random days for events between initial detection score and new filtered score threshold

            # write the random table with selections that have score > the threshold
             write.table(elp_th_rand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                              sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",# folder name
                                              substr(file_size[q],1,nchar(file_size[q])-7),"p", sub("\\d.","",Filter_ScoreThreshold),"_rand_raw.txt",sep=""), # file name
                       sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the random table for score > threshold)


            # write the non random table with selections that have score > the threshold
              write.table(elp_th_nonrand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                              sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/", # folder name
                                              substr(file_size[q],1,nchar(file_size[q])-7),"p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw.txt",sep=""), # file name
                    sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the nonrandom dates table for score > threshold

            # write a rand and non rand table with score below selection
              write.table(elp_th2_all,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                             sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/", # folder name
                                             substr(file_size[q],1,nchar(file_size[q])-7),sub("\\d.","p",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw.txt",sep=""), # file name
                    sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) # save all other detections < threshold
    } #if this doesn't run initially because it can't find the file, check that the "Begin Path" in the selection tables match
}
# move the remaining selection tables to the final p2_raw folder
for (h in 1:length(files)){
 file.move(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/",files[h],sep=""),"~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw/",overwrite = TRUE)
}

# ##### Merge ALL p4 rand selection tables and split into 2000 events (optional, not required if site-wise tables preferred) ####
#   files3<-list.files(path_p4_rand, full.names = TRUE,pattern="*p4_rand_raw.txt",recursive=TRUE) #make a list of all the files within the subfolder j
#   all_txt_files <- lapply(files3, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
#   merge_files <- do.call("rbind", lapply(all_txt_files, as.data.frame)) # Combine them into a data frame
#   sort_files<-merge_files[order(merge_files$"Begin File",merge_files$"File Offset (s)"),] # sort by Begin File and File Offset
#   sort_files$"Selection"<- seq.int(nrow(sort_files)) #renumber the selections for Raven
#   write.table(sort_files,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p4_rand_counted/",deployment_name,"_dep",deployment_num,"_elp_merged_ALL.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save table of all events
#
# # # #split table in to 2000 events
# setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p4_rand_counted/")
#   merged_p4_tables <- list.files("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p4_rand_counted/",pattern = "_elp_merged_ALL.txt")
#    for (k in 1:length(merged_p4_tables)){
#     split_file(merged_p4_tables[k],2000,prefix_out = paste(deployment_name,"_dep",deployment_num,"_HH6p2_p",sub("\\d.","",Filter_ScoreThreshold),"_rand_raw_XXX",sep=""),repeat_header = TRUE) #if this gives and error, check that the "_test.txt" file isn't " _test.txt" with a space. If so, delete the space
#    }

#### Summarize sampling effort and # detections per site day ####
  # sound check sound list
  ele_sounds <- sound_check
  ele_sounds$`Exclude (y/e)`[is.na(ele_sounds$`Exclude (y/e)`)|ele_sounds$`Exclude (y/e)`==""] <-"Good"
  #ele_sounds <- ele_sounds[(ele_sounds$`Exclude (y/e)` == "Good"),]#
  ele_sounds2 <- ele_sounds[c("Site", "Begin File", "File Duration (s)", "Duration Minutes","Exclude (y/e)","Sound Problems", "File Path")]
  names(ele_sounds2)[names(ele_sounds2) == "File Path"] <- "Begin Path"
  ele_sounds2$Date <- format(as.Date(str_extract(ele_sounds2$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")

  # detections
  setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw")
  det_files <- list.files(path="~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw",
                          pattern=standard_name_disk,
                          all.files=TRUE, recursive=TRUE,full.names=FALSE)
  det_files <- lapply(det_files, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  det_files_Detector_ScoreThreshold <- do.call("rbind", lapply(det_files, as.data.frame)) # Combine them into a data frame
  # det_files_p2 <- do.call("rbind", lapply(det_files, as.data.frame)) # Combine them into a data frame
  det_files_Filter_ScoreThreshold_dets <- det_files_Detector_ScoreThreshold[(det_files_Detector_ScoreThreshold$Score >=as.numeric(Filter_ScoreThreshold)),]
  # det_files_p4 <- det_files_p2[(det_files_p2$Score >.4),]
  det_files_Filter_ScoreThreshold_dets_rand <- det_files_Filter_ScoreThreshold_dets[(det_files_Filter_ScoreThreshold_dets$Rand == "rand"),]
  # det_files_p4_rand <- det_files_p4[(det_files_p4$Rand == "rand"),]

  det_files_Detector_ScoreThreshold_summary <- det_files_Detector_ScoreThreshold %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for lowest score threshold (Detector_ScoreThreshold from script)
  # dets_files_p2 <- det_files_p2 %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for lowest score threshold (Detector_ScoreThreshold from script)
  colnames(det_files_Detector_ScoreThreshold_summary) <- c("Begin File","Site", "Date","Sum Rumbles > Detector Threshold")
  #colnames(det_files_Detector_ScoreThreshold_summary) <- c("Begin File","Site", "Date",paste0("Number of Rumble Detections >",Detector_ScoreThreshold,sep=""))
  det_files_Filter_ScoreThreshold_dets_summary <- det_files_Filter_ScoreThreshold_dets %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for filtered score thresholds (Filter_ScoreThreshold from script)
  colnames(det_files_Filter_ScoreThreshold_dets_summary) <- c("Begin File","Site","Date" ,"Sum Rumbles > Filtered Detector Threshold")
  #colnames(det_files_Filter_ScoreThreshold_dets_summary) <- c("Begin File","Site","Date" ,paste0("Number of Rumble Detections >",Filter_ScoreThreshold,sep=""))
  det_files_Filter_ScoreThreshold_dets_rand_summary <- det_files_Filter_ScoreThreshold_dets_rand %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and random day for filtered score thresholds (Filter_ScoreThreshold from script)
  colnames(det_files_Filter_ScoreThreshold_dets_rand_summary) <- c("Begin File","Site","Date" ,"Sum Rumbles for Rand Days > Filtered Detector Threshold")
  #colnames(det_files_Filter_ScoreThreshold_dets_rand_summary) <- c("Begin File","Site","Date" ,paste0("Number of Rand Rumble Detections >",Filter_ScoreThreshold,sep=""))

  # merge detections and sound files
  ele_sound_dets <- merge(ele_sounds2,det_files_Detector_ScoreThreshold_summary,all.x=T)
  ele_sound_dets <- merge(ele_sound_dets,det_files_Filter_ScoreThreshold_dets_summary,all.x=T)
  ele_sound_dets <- merge(ele_sound_dets,det_files_Filter_ScoreThreshold_dets_rand_summary,all.x=T)
  ele_sound_dets <- merge(ele_sound_dets,elp_rand,all.x=T,by.x="Date", by.y="Begin Date")
  ele_sound_dets$Rand[is.na(ele_sound_dets$Rand)] <- "non-rand"
  for(f in 1:nrow(ele_sound_dets)) {
    if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
      ele_sound_dets$`Sum Rumbles > Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles > Detector Threshold`[f])] <-0
    }
    if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
      ele_sound_dets$`Sum Rumbles > Filtered Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles > Filtered Detector Threshold`[f])] <-0
    }
    if(ele_sound_dets$`Exclude (y/e)`[f] == "Good" && ele_sound_dets$Rand[f] == "rand" ) {
      ele_sound_dets$`Sum Rumbles for Rand Days > Filtered Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles for Rand Days > Filtered Detector Threshold`[f])] <-0
    }
  }
  # for(f in 1:nrow(ele_sound_dets)) {
  #   if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
  #       ele_sound_dets$`Number of p2 Rumble Detections`[f][is.na(ele_sound_dets$`Number of p2 Rumble Detections`[f])] <-0
  #     }
  #   if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
  #     ele_sound_dets$`Number of p4 Rumble Detections`[f][is.na(ele_sound_dets$`Number of p4 Rumble Detections`[f])] <-0
  #   }
  #   if(ele_sound_dets$`Exclude (y/e)`[f] == "Good" && ele_sound_dets$Rand[f] == "rand" ) {
  #     ele_sound_dets$`Number of p4 Rand Rumble Detections`[f][is.na(ele_sound_dets$`Number of p4 Rand Rumble Detections`[f])] <-0
  #   }
  # }

  # Summarize by site and dates, with total duration and # detections p2 and p4 rand
  ele_sound_dates <- aggregate(`Duration Minutes`~Site+Date,data=ele_sound_dets,FUN=sum)
  ele_sound_dates_detectorTHreshold <- aggregate(`Sum Rumbles > Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
  #ele_sound_dates_detectorTHreshold <- aggregate(`Number of p2 Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)
  ele_sound_dates_scoreThreshold <- aggregate(`Sum Rumbles > Filtered Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
  #ele_sound_dates_p4 <- aggregate(`Number of p4 Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)
  ele_sound_dates_ScoreThreshold_rand <- aggregate(`Sum Rumbles for Rand Days > Filtered Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
  #ele_sound_dates_p4_rand <- aggregate(`Number of p4 Rand Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)

  ele_sound_dets_sum <- merge(ele_sound_dets,ele_sound_dates_detectorTHreshold,all.x=T)
  ele_sound_dets_sum <- merge(ele_sound_dets_sum,ele_sound_dates_scoreThreshold,all.x=T)
  ele_sound_dets_sum <- merge(ele_sound_dets_sum,ele_sound_dates_ScoreThreshold_rand,all.x=T)
  ele_sound_dets_sum$`Duration (Hrs)` <- round(ele_sound_dets_sum$`File Duration (s)`/3600,digit=2)
  ele_sound_dets_sum$`File Duration (s)` <- round(ele_sound_dets_sum$`File Duration (s)`,digit = 2)
  ele_sound_dets_sum$`Duration Minutes` <- round(ele_sound_dets_sum$`Duration Minutes`,digit=2)
  ele_sound_dets_sum$`Dets_per_Hr` <- round(ele_sound_dets_sum$`Sum Rumbles > Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  ele_sound_dets_sum$`Dets_per_Hr_ScoreThreshold` <- round(ele_sound_dets_sum$`Sum Rumbles > Filtered Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  ele_sound_dets_sum$`Dets_per_Hr_ScoreThreshold_rand` <- round(ele_sound_dets_sum$`Sum Rumbles for Rand Days > Filtered Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  ele_sound_dets_sum <- ele_sound_dets_sum[order(ele_sound_dets_sum$"Begin File"),] # sort by begin file
  ele_sound_dets_sum <- ele_sound_dets_sum[c("Date","Site","Rand","Begin File","File Duration (s)","Duration Minutes","Duration (Hrs)","Exclude (y/e)",
                                             "Sum Rumbles > Detector Threshold" ,"Sum Rumbles > Filtered Detector Threshold","Sum Rumbles for Rand Days > Filtered Detector Threshold",
                                             "Dets_per_Hr","Dets_per_Hr_ScoreThreshold","Dets_per_Hr_ScoreThreshold_rand")]
  write.table(ele_sound_dets_sum,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/num_events/rumble/",standard_name_disk,"_",Detector,"_Detection_Summaries.txt",sep=""),
              sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE)


# save summaries as a new tab in sound_check
  proj_sites <- as.data.frame(read.table(paste('~/R/Bobbi_Scripts/Packages/elpR/Files/sites/',sites,sep=""),header=FALSE,sep="\t", check.names=FALSE))
  ele_det_sites <- data.frame(proj_sites)

  ele_det_ScoreThreshold_rand_sites_sum <- aggregate(ele_sound_dets_sum$`Sum Rumbles for Rand Days > Filtered Detector Threshold`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE)
  ele_det_ScoreThreshold_sites_sum <- aggregate(ele_sound_dets_sum$`Sum Rumbles > Filtered Detector Threshold`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE)
  ele_det_DetectorThreshold_sites_sum <- aggregate(ele_sound_dets_sum$`Sum Rumbles > Detector Threshold`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE)
  ele_det_duration_sites_sum <- aggregate(ele_sound_dets_sum$`Duration (Hrs)`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE)

  rumble_sound_stats <- cbind(ele_det_duration_sites_sum$Site,ele_det_duration_sites_sum$x,ele_det_DetectorThreshold_sites_sum$x,ele_det_ScoreThreshold_sites_sum$x,ele_det_ScoreThreshold_rand_sites_sum$x)
  colnames(rumble_sound_stats) <- c("Site","Duration (Hrs)",
                                    paste0("Number of Rumble Detections >",Detector_ScoreThreshold,sep=""),
                                    paste0("Number of Rumble Detections >",Filter_ScoreThreshold,sep=""),
                                    paste0("Number of Rand Rumble Detections >",Filter_ScoreThreshold,sep=""))
  #colnames(rumble_sound_stats) <- c("Site","Duration (Hrs)","Sum Detections","Sum p4 Detections","Sum p4 Rand Detections")
  colnames(ele_det_sites) <- c("Site")
  rumble_site_sound_stats <- merge(ele_det_sites,rumble_sound_stats,by = "Site",all=TRUE)# merge the sound stats and sites together

  wb <- loadWorkbook(file = paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""))
  if("Rumble Detector Summaries" %in% names(wb)){
    removeWorksheet(wb, "Rumble Detector Summaries")
    saveWorkbook(wb, paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),overwrite=TRUE)
  }
  addWorksheet(wb, sheetName = "Rumble Detector Summaries",tabColour='green')
  writeData(wb, sheet = "Rumble Detector Summaries", x = rumble_site_sound_stats)
  saveWorkbook(wb,paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),returnValue=FALSE,overwrite=TRUE)

# plot to detector summaries (total detections by sound file)
  det_sum_DetectorThreshold <- aggregate(`Sum Rumbles > Detector Threshold`~Date+Site, data=ele_sound_dets_sum,sum,na.rm=TRUE)
  det_sum_DetectorThreshold$Date <- as.Date(det_sum_DetectorThreshold$Date,format="%m/%d/%Y")
  det_sum_DetectorThreshold <- det_sum_DetectorThreshold[order(det_sum_DetectorThreshold$Site,det_sum_DetectorThreshold$Date),]
  det_sum_ScoreThreshold_rand <-aggregate(`Sum Rumbles for Rand Days > Filtered Detector Threshold`~Date+Site, data=ele_sound_dets_sum,sum,na.rm=TRUE)
  det_sum_ScoreThreshold_rand$Date <- as.Date(det_sum_ScoreThreshold_rand$Date,format="%m/%d/%Y")
  det_sum_ScoreThreshold_rand <- det_sum_ScoreThreshold_rand[order(det_sum_ScoreThreshold_rand$Site,det_sum_ScoreThreshold_rand$Date),]

  setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/num_events/rumble")
  det_sum_DetectorThreshold_plot <- ggplot(det_sum_DetectorThreshold, aes(x = Date, y =  `Sum Rumbles > Detector Threshold`, colour = Site)) +
          geom_point(show.legend = FALSE)+
          scale_y_continuous(trans='log10')+
          facet_wrap( ~ Site)+
          ylab(paste0("Number of Rumble Detections >",Detector_ScoreThreshold," Score",sep=""))
  ggsave(plot = det_sum_DetectorThreshold_plot,paste0(standard_name_disk,"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_plot.png",sep=""),width =10, height =10 )

  det_sum_ScoreThreshold_rand_plot <- ggplot(det_sum_ScoreThreshold_rand, aes(x = Date, y = `Sum Rumbles for Rand Days > Filtered Detector Threshold`, colour = Site)) +
          geom_point(show.legend = FALSE) +
          scale_y_continuous(trans='log10')+
          facet_wrap( ~ Site)+
          ylab(paste0("Number of Rumble Detections >",Filter_ScoreThreshold," Score",sep=""))
  ggsave(plot = det_sum_ScoreThreshold_rand_plot,paste(standard_name_disk,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_plot.png",sep=""),width =10, height =10)

  #print(det_sum_p2_plot)
  #print(det_sum_p4_rand_plot)


# #### Create Zero-Days Selection Table ####

  sounds_det_ScoreTHreshold_rand <- ele_sound_dets[(ele_sound_dets$`Exclude (y/e)` == "Good"),]
  sounds_det_ScoreTHreshold_rand <- sounds_det_ScoreTHreshold_rand[(sounds_det_ScoreTHreshold_rand$Rand == "rand"),]
  sounds_det_ScoreTHreshold_rand <- sounds_det_ScoreTHreshold_rand[(sounds_det_ScoreTHreshold_rand$`Sum Rumbles for Rand Days > Filtered Detector Threshold` == 0),]

  ScoreThreshold_zero <- sounds_det_ScoreTHreshold_rand[c("Date","Site","Begin File","Rand","Sound Problems")]
  ScoreThreshold_zero$`Count` = "NA"
  ScoreThreshold_zero$Measurable = "NA"
  ScoreThreshold_zero$Harmonics = ""
  ScoreThreshold_zero$Ambiguous = ""
  ScoreThreshold_zero$`Begin Time (s)` = 20
  ScoreThreshold_zero$`End Time (s)` = 100
  ScoreThreshold_zero$View = "Spectrogram"
  ScoreThreshold_zero$Channel = 1
  ScoreThreshold_zero$`Low Freq (Hz)` = 10
  ScoreThreshold_zero$`High Freq (Hz)` = 1000
  ScoreThreshold_zero$Notes = "No Hori-Harm rumble detections on this date and site at threshold of p4"
  ScoreThreshold_zero$Selection = seq.int(nrow(ScoreThreshold_zero))
  ScoreThreshold_zero$`File Offset (s)`= 20
  ScoreThreshold_zero$'Begin Date' <- as.Date(ScoreThreshold_zero$Date,format = '%m/%d/%Y')
  ScoreThreshold_zero$'Begin Date'<-format(ScoreThreshold_zero$'Begin Date',"%m/%d/%Y")
  ScoreThreshold_zero$"Event Date"<-ScoreThreshold_zero$"Begin Date"
  ScoreThreshold_zero$`Deployment` <- deployment_num
  ScoreThreshold_zero$Score <- "NA"
  ScoreThreshold_zero$`Begin Path` <- ""
  ScoreThreshold_zero$Analyst <- "NA"
  ScoreThreshold_zero$`Call Criteria` <- "NA"
  ScoreThreshold_zero$`Begin Hour` <- ""
  ScoreThreshold_zero$"File Start Date" <- ScoreThreshold_zero$'Begin Date'

  ScoreThreshold_zero_rand <- ScoreThreshold_zero[c("Selection", "View", "Channel", "Begin Time (s)",
                            "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                            "Begin Path", "File Offset (s)", "Begin File", "Site",
                            "Begin Hour", "File Start Date","Begin Date",
                            "Score", "Count", "Measurable", "Harmonics", "Ambiguous",
                            "Notes", "Analyst","Rand", "Deployment",
                            "Sound Problems","Call Criteria")]

  write.table(ScoreThreshold_zero_rand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/zero_days_SSTs/rumble/",standard_name_disk,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_rand_ZeroDets.txt",sep=""),
              sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)
}


