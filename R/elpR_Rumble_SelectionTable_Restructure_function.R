#bje37
# 1 March 2021

## TO DO
# check that all selections have a unique selection ID
# exclude dates outside of deployment period from selection table. (need to add deployment start and end to sites file (optional))
# for FruitPunch detections - automatically annotate overlapping detections with "Duplicate" in Notes. Delete?

#' @title Rumble Selection Table Restructure
#' @author Bobbi J. Estabrook <bobbi.estabrook@cornell.edu>
#'
#' @description
#' This function will:
#' \itemize{
#'  \item read in all selection tables from elephant rumble detector output in the 'Packages/elpR/File/Rumble/raw' folder (required that they are in site-folders)
#'  \item restructure those tables by merging, cleaning, and adding new columns
#'  \item merge the detector tables into site-wise selection tables
#'  \item add 'sound check' info to the selection tables (the 'sound_check' function should have been run on associated sound files BEFORE this step)
#'  \item add the random days designation to the 3 randomly selected days per week (user defined by three_rand_days <- "y" or "n")
#'  \item ignore detections on sound files that were marked with 'y' (bad sound file) in the 'Exclude (y/n)' column of the 'Sound Check' Excel file
#'  \item if min_23hrs <- "y" then ignore detections on sound files that were marked with 'e' (bad sound file for elephant analysis) in the 'Exclude (y/n)' column of the 'Sound Check' Excel file
#'  \item if three_rand_days <- "y" then summarize and produce selections tables for the 3 random days per week
#'  \item save selection tables that are filtered by score per table (in the elpR/File/Rumble/Final folder)
#'  \item count the total number of detections per site and day by score and sound file duration (added to the 'sound check' Excel sheet as a tab, saved in the elpR/Files/sound_check folder)
#'  \item calculates the total number of detections per sound file saved as a .txt file in the elpR/File/Num_events folder
#'  \item plot the total detections per score as .png files (saved in elpR/File/Num_events folder)
#'  \item count the total number of days without detections
#'  \item save a selection table of sound files without detections at threshold score (user defined) for random dates (saved in the elpR/Fileszero_days_SSTs/rumble folder)
#'  \item if the detector specified is "FPv1", the script will reset the minimum frequency of the detections to 10 Hz, because the FruitPunch detector (FPv1) generates minimum frequencies that are higher than the detected signal.
#'}
#' @note
#' * The user needs to update the fields in the script.
#' * input detector files require Raven Pro columns:
#'   \itemize{
#'   \item Begin File
#'   \item File Offset (s)
#'   \item Begin Time (s)
#'   \item End Time (s)
#'   \item High Freq (Hz)
#'   \item Low Freq (Hz)
#'   \item Score (can be named something else, but must be indicated by user in script)}
#' The 'sound check' file is required for this function. The name of the file should be unchanged from when it was created. Use the same variable names between the two scripts for each project, deployment, and disk
#' This script will only generate selection tables and summaries for sites that are included in the corresponding 'sound check' file.
#' WARNING: New files will overwrite old files with the same name. Be sure to move files of the same name out of the 'Packages/elpR/File/Rumble/Final' folder before running this script if you don't want to overwrite them
#'
#' @param HH_selection_tables Do not change this value
#'
#' @return Output files
#' \itemize{
#'  \item updated sound check file with new tab called Rumble Detector Summaries (/elpR/Files/sound_check)
#'    \item Duration of good sounds per site
#'    \item Total detections at lowest score per site
#'    \item Total detections above score threshold per site
#'    \item Total detections above score threshold and on random dates per site (will be blank if random dates are not enabled by user)
#'  \item Raven Pro sound selection tables (/elpR/Files/Selection_Tables/rumble/final) in folders by score
#'  \item Empty selection tables. These are dummy selections tables that have 1 selection per sound file which did not have a detection (elpR/Files/Empty_Tables/rumble) greater than the initial detector score
#'  \item Summary of the number of events per sound file (elpR/Files/num_events/rumble)
#'  \item Plots for the number of detections per day per site by the detector score and the filtered score (elpR/Files/num_events/rumble)
#'  \item Dummy selection tables with selections on sound files that did not have a detection above the filtered score (elpR/Files/zero_days_SSTs/rumble)
#'  }
#'
#' @importFrom plyr rbind.fill
#' @importFrom dplyr filter select mutate group_by tally rename %>%
#' @importFrom ggplot2 ggplot geom_point scale_y_continuous facet_wrap aes
#' @importFrom bigreadr fread2
#' @importFrom openxlsx read.xlsx loadWorkbook addWorksheet writeData saveWorkbook removeWorksheet
#' @importFrom stringr str_extract str_match
#' @importFrom gsubfn strapplyc
#' @importFrom lubridate hour
#' @importFrom filesstrings file.move
#' @importFrom utils write.table read.table
#' @importFrom stats aggregate
#' @importFrom graphics plot
#'
#' @export

# every time the description is updated, run the code below.
# detach("package:elpR", unload = TRUE)  # If it's loaded
# remove.packages("elpR")
# .rs.restartR()   # Restart R session
# devtools::document() # enable this ever time you make description changes.
# then build/install package


Rumble_Selection_Table_Restructure <- function (x) {

  # install and load necessary packages
  sel_table_struct <- c("plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr","gsubfn","lubridate","filesstrings")
  options(warn = -1)
  for (i in sel_table_struct){
    if (!require(i, quietly = TRUE, character.only = TRUE)){
      install.packages(i)
      library(i)
    }
  }

 #### PRE-CREATE SOME FILEDS FOR SCRIPT ####

  # default names
  raw_table_name <-paste(deployment_name,"_",deployment_num,"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_",sample_rate,"_brut",sep="") #default raw output table name
  filtered_table_name <-  paste(deployment_name,"_",deployment_num,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_",sample_rate,"_brut",sep="") # default filtered table name
  standard_name_disk <- paste(deployment_name,"_dep",deployment_num,"_d",disk_ID, sep="") # output file names (deployment name, number, disk)

  # Hori_Harm folders
  processed_HH <- "~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/"
  #path_p4_rand <- "~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p4_rand_raw/" # REMOVE?
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
  # if (soundcheck_file == "y"){} # impliment option for sound check file
  sound_check <- openxlsx::read.xlsx(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),
                           sheet="Sounds",colNames=TRUE,check.names=FALSE,sep.names = " ") # read in the sound_check file that was created in the first step
  sound_check$'Sound Problems'<-paste(sound_check$`File Duration Check`, sound_check$'File Length Check',sound_check$`Sound Gap Check`,sound_check$"SampleRate",
                                      sound_check$"Deployment Notes",sound_check$"Sound Problems", sound_check$`Exclude (y/e)`,sep = "; ") # concatenate the sound problems into one column
  names(sound_check)[names(sound_check) == 'Current File Name'] <- 'Begin File'
  sound_problem<-sound_check[c("Begin File", "Sound Problems","Exclude (y/e)")]
  sound_problem$`Exclude (y/e)`[is.na(sound_problem$`Exclude (y/e)`)|sound_problem$`Exclude (y/e)`==""] <-"Good" # if the sounds were not excluded, mark as "good"
  if (min_23hrs == "n"){
    sound_problem$`Exclude (y/e)`[sound_problem$`Exclude (y/e)`=="e"] <-"Good"
  } # if 23 h per day is not required for analysis, mark "e" as "Good", otherwise, leave it as "e"

  # the cross reference will occur in the next step (4)

  # Identify empty selection tab
  file_names <-dir(path=raw_selection_tables,
                   all.files=TRUE,
                   include.dirs=TRUE,
                   recursive = TRUE,
                   pattern=".txt",
                   full.names=TRUE) #index all the files
  file_size <-file_names[sapply(file_names, file.size) > 200] # index only files greater than 200 bytes since those <200 bytes are blank selection tables (the for loop code won't run on empty selection tables

  # Record empty selection tables (those with less than 200 bytes) in a .txt file
  file_small <-as.data.frame(file_names[sapply(file_names, file.size) < 200])# lists (identifies) tables with no detections
  if(nrow(file_small) >0){
    file_small$dep <-deployment_num
    colnames(file_small) <- c("Empty Selection Table","Deployment Number")
    file_small$`Empty Selection Table` <- file_small$`Empty Selection Table`
    #file_small$Site <- substring(sub("_20.*","",file_small$'Empty Selection Table'),4) #NEED TO UPDATE THIS FOR THE CLUSTER SITE NAMES
    file_small$Site <- substr(str_match(file_small$'Empty Selection Table',"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_")[,1],1,
                              nchar(str_match(file_small$'Empty Selection Table',"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_")[,1])-1)
    #file_small$Date <- substr(sub(".*Date_","",file_small$'Empty Selection Table'),start=1,stop=8)
    file_small$Date <-  substr(str_match(file_small$'Empty Selection Table',"\\d{8}")[,1],1,
                               nchar(str_match(file_small$'Empty Selection Table',"\\d{8}")[,1]))
    file_small$Date <- as.POSIXct(file_small$Date,format='%Y%m%d',origin = "1970-01-01",tz="Africa/Brazzaville") # convert format of start date and time to proper data and time format
    file_small$`Deployment Name` <- deployment_name
    file_small$'Detection Score Threshold' <- Detector_ScoreThreshold
    file_small$"Begin File" <- ""
    file_small$Rand <- ""
    file_small$`Sound Problems` <- ""
    file_small$`Count` = "NA"
    file_small$Measurable = "NA"
    file_small$Harmonics = "NA"
    file_small$Ambiguous = "NA"
    file_small$`Begin Time (s)` = 20
    file_small$`End Time (s)` = 100
    file_small$View = "Spectrogram"
    file_small$Channel = 1
    file_small$`Low Freq (Hz)` = 10
    file_small$`High Freq (Hz)` = 1000
    file_small$Notes = paste("No Hori-Harm rumble detections on this date and site at score threshold of",Detector_ScoreThreshold,sep=" ")
    file_small$Selection = seq.int(nrow(file_small))
    file_small$`File Offset (s)`= 20
    file_small$'Begin Date' <- as.Date(file_small$Date,format = '%m/%d/%Y')
    file_small$'Begin Date'<-format(file_small$'Begin Date',"%m/%d/%Y")
    file_small$"Event Date"<-file_small$"Begin Date"
    file_small$`Deployment Number` <- deployment_num
    file_small$Score <- Detector_ScoreThreshold
    file_small$`Begin Path` <- ""
    file_small$Analyst <- "NA"
    file_small$`Call Criteria` <- "NA"
    file_small$`Begin Hour` <- ""
    file_small$"File Start Date" <- file_small$'Begin Date'

    write.table(file_small, file=paste('~/R/Bobbi_Scripts/Packages/elpR/Files/Empty_Tables/rumble/',
                                       standard_name_disk,
                                       "_Empty_Tables_",
                                       Detector,
                                       "_p",
                                       sub("\\d.",
                                           "",
                                           Detector_ScoreThreshold),
                                       '.txt',
                                       sep=""),
                sep='\t',na="",
                col.names=TRUE,
                row.names=FALSE,
                quote=FALSE,
                append=FALSE) #save a .csv listing the files with no detections
  }

######### FORMAT Detector SELECTION TABLES ####
  for (i in 1:length(file_size)){
      elp_data<-read.table(file_size[i],
                           header=TRUE,
                           sep="\t",
                           check.names=FALSE,
                           quote="\"") #read each table from the 200+ byte list separately
      elp_new<-as.data.frame.matrix(elp_data) #save each individual selection table as a data frame
      elp_new$`Begin File` <- basename(elp_new$`Begin Path`) # str_match(elp_new$`Begin Path`,"nn\\d{2}(.*?).wav")[,1]
      elp_new$`File Start DateTime` <- str_extract(elp_new$`Begin File`,"\\d{8}.\\d{6}") #  file start date and time from file name (_YYYYMMDD_HHMMSS)
      elp_new$`File Start DateTime` <- as.POSIXct(elp_new$`File Start DateTime`,
                                                  format='%Y%m%d_%H%M%S',
                                                  origin = "1970-01-01",
                                                  tz="Africa/Brazzaville") #tz = "UTC"
      elp_new$`File Start Date`<-format(as.Date(str_extract(elp_new$'Begin File' ,"\\d{8}.\\d{6}"),
                                                "%Y%m%d"),
                                        "%m/%d/%Y") # use this for date if you want the file date rather than Raven Begin Date
      elp_new$`Selection Begin DateTime` <- elp_new$`File Start DateTime`+ elp_new$`File Offset (s)` # add File Offset (s)  to calculate selection date-time
      elp_new$`Begin Date`<- format(as.Date(elp_new$'Selection Begin DateTime',
                                            "%Y%m%d",
                                            tz="Africa/Brazzaville"),
                                    "%m/%d/%Y") # different from File Name Date in that it is the date of the event
      elp_new$`Begin Clock Time` <-format(elp_new$`Selection Begin DateTime`,"%H:%M:%S") # Time of event
      elp_new$`Begin Hour` <- format(as.POSIXct( elp_new$`Begin Clock Time` ,format="%H:%M:%S"),"%H")# hour(elp_new$`Begin Clock Time`)
      elp_new$"Count"<-NA #add "Count" (formerly "Tag 1") column
      elp_new$"Measurable"<-NA #add "measurable" (formerly "Tag 2") column
      elp_new$"Harmonics"<-NA #add "Harmonics column
      elp_new$"Ambiguous"<-NA #add "ambiguous" (formerly "Tag 3") column
      elp_new$Notes<-NA #add "Notes" column
      elp_new$Analyst<-NA #add "Analyst" column
      elp_new$"Deployment Number"<-deployment_num # add a deployment number column (change for each deployment)
      elp_new$Disk <- disk_ID
      if (Detector == "SDv1"){
        elp_new$Score <- NA
      } # if Stanford detector was used, add a Score column
      elp_new <- elp_new %>%
        dplyr::rename(Score = !!sym(score_column_name))
      elp_new$Score<-as.numeric(round(elp_new$Score,digits = 3)) #round the score to 3 decimal places
      elp_new$Site <- substr(str_match(elp_new$`Begin File`,"[a-zA-Z]{2}\\d{2}[a-zA-Z]{1}.")[,1],1,
                              nchar(str_match(elp_new$`Begin File`,"[a-zA-Z]{2}\\d{2}[a-zA-Z]{1}.")[,1])-1)
      elp_rand_data<-merge(elp_new,elp_rand,by="Begin Date",all.x=TRUE) #merge the random days (see elp_rand section above) with selection tables to mark which days were randomly reviewed
      elp_order <- elp_rand_data[c("Selection",
                                   "View",
                                   "Channel",
                                   "Begin Time (s)",
                                   "End Time (s)",
                                   "Low Freq (Hz)",
                                    "High Freq (Hz)",
                                    "Begin Path",
                                    "File Offset (s)",
                                   "Begin File",
                                   "Site",
                                   "Begin Hour",
                                   "Begin Clock Time",
                                   "File Start Date",
                                   "Begin Date",
                                   "Score",
                                   "Count",
                                   "Measurable",
                                   "Harmonics",
                                   "Ambiguous",
                                   "Notes",
                                   "Analyst",
                                   "Rand",
                                   "Deployment Number",
                                   "Disk")] #reorder columns
      elp_order$"Begin Path"<-gsub("\\\\\\\\159nas\\\\L\\\\ELP\\\\","L:ELP\\\\",elp_order$"Begin Path") # add begin path
      elp_sort<-elp_order[order(elp_order$"File Offset (s)"),]# sort dataframe by file offset # sort by file and file offset
      elp_sort$'Call Criteria'<-"20210212" # Update this with the latest version of the Call Criteria
      elp_rand_sound<-merge(elp_sort,
                            sound_problem,
                            by="Begin File",
                            all.x=T)# cross-reference with sound problems if the table is empty and create dummy values
      elp_rand_sound$`Exclude (y/e)`[is.na(elp_rand_sound$`Exclude (y/e)`)|elp_rand_sound$`Exclude (y/e)`==""] <-"Good" # if the sounds were not excluded, mark as "good"
      elp_rand_sound_exclude <- elp_rand_sound[(elp_rand_sound$`Exclude (y/e)` == "Good"),] # filter selection table only by good sounds and exclude bad sounds
      # if(nrow(elp_rand_sound_exclude) >0){ # if table doesn't have good sounds then remove it, if it does, then save the table
      #     write.table(elp_rand_sound_exclude,
      #                     paste(processed_HH,file_size[i], sep=""),
      #                     sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE), #save each table with same name into same directory
      #   } else {file.move(file_size[i],"~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/excluded")
      #   }
      if (nrow(elp_rand_sound_exclude) > 0) {
        if (Detector == "FPv1"){
          elp_rand_sound_exclude$`Low Freq (Hz)` <- 10
          # remove overlapping detections
        }# if FruitPunch FPv1 detector was used, remove overlapping detections and decrease the min frequency
        write.table(elp_rand_sound_exclude,
                    paste(processed_HH,file.path(basename(dirname(file_size[i])), basename(file_size[i])) , sep=""),
                    sep="\t",
                    na="",
                    col.names=TRUE,
                    row.names=FALSE,
                    quote=FALSE,
                    append=FALSE) # If table has detections in it, save the table
      }
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
  # sum(sapply(all_txt_df, nrow))
  merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
  merge_df<-if(!is.null(merge_df)){merge_df[order(merge_df$"Begin File",merge_df$"File Offset (s)"),]}
  merge_df$"Selection"<- if(nrow(merge_df)>0){seq.int(nrow(merge_df))} #renumber the selections for Raven
  # nrow(merge_df)

  # separate tables into sites
  siteWise_list <- split(merge_df, f = merge_df$Site)
  for (m in seq(siteWise_list)){
    write.table(siteWise_list[[m]],
                file=paste(processed_HH,standard_name_disk,"_",names(siteWise_list)[[m]],"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_",sample_rate,".txt",sep=""),
                sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE) #save tables
  }
  # sum(sapply(siteWise_list, nrow))

#### Filter HH MERGED SELECTION TABLES BY SCORE (>0.4 for Noubale-Ndoki, >0.6 for bais) ####
setwd(processed_HH)
files <- list.files(path=processed_HH,pattern=paste(standard_name_disk,sep=""),all.files=TRUE, recursive=TRUE,full.names=FALSE) #use this so the for loop can jump into each file
file_size <- files[sapply(files, file.size) > 200]

# create directories to receive the processed selection tables
if(three_rand_days == "y"){
  if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p", sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",sep=""))){
         dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",sep=""))}
  if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/",sep=""))){
         dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/",sep=""))}
  if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))){
         dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))}
}

if(three_rand_days == "n"){
  if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p", sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))){
    dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))}
  if(!dir.exists(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))){
    dir.create(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/",sep=""))}
}


# filter tables and move them to corresponding folders in the final folder
# check if randon days needed
if(three_rand_days == "y"){
  for (q in 1:length(file_size)){
    elp_merged <- read.table(file_size[q],header=TRUE,sep="\t",check.names=FALSE) #read in the merged selection table
    elp_sound_table <- if(nrow(elp_merged)>0){
              elp_sound <- elp_merged[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                                     "Begin Path", "File Offset (s)", "Begin File", "Site", "Begin Hour", "Begin Clock Time", "File Start Date","Begin Date",
                                     "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes", "Analyst","Rand", "Deployment Number",
                                     "Sound Problems","Call Criteria","Disk")]
              elp_th<-filter(elp_sound,elp_sound$"Score">=Filter_ScoreThreshold) #filter the merged table by score threshold
              elp_th_rand<-filter(elp_th,elp_th$Rand == "rand") #make new dataframe for the random days for the filtered score
              elp_th_nonrand<- filter(elp_th,elp_th$Rand == "") #make new dataframe for the non-random days
              elp_th2_all<-filter(elp_sound,elp_sound$"Score"<Filter_ScoreThreshold) # filter the random and non random days for events between initial detection score and new filtered score threshold

              # write the random table with selections that have score > the threshold
               write.table(elp_th_rand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                                sub("\\d.","",Filter_ScoreThreshold),"_rand_raw/",# folder name
                                                substr(file_size[q],1,nchar(file_size[q])-4),"_p", sub("\\d.","",Filter_ScoreThreshold),"_rand_raw.txt",sep=""), # file name
                         sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the random table for score > threshold)


              # write the non random table with selections that have score > the threshold
                write.table(elp_th_nonrand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                                sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw/", # folder name
                                                substr(file_size[q],1,nchar(file_size[q])-4),"_p",sub("\\d.","",Filter_ScoreThreshold),"_nonrand_raw.txt",sep=""), # file name
                      sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the nonrandom dates table for score > threshold

              # write a rand and non rand table with score below selection
                write.table(elp_th2_all,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                               sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/", # folder name
                                               substr(file_size[q],1,nchar(file_size[q])-4),"_",sub("\\d.","p",Detector_ScoreThreshold),
                                               "-p",sub("\\d.","",Filter_ScoreThreshold),"_raw.txt",sep=""), # file name
                      sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) # save all other detections < threshold
      } #if this doesn't run initially because it can't find the file, check that the "Begin Path" in the selection tables match
  }
}

if(three_rand_days == "n"){
  for (q in 1:length(file_size)){
    elp_merged <- read.table(file_size[q],header=TRUE,sep="\t",check.names=FALSE) #read in the merged selection table
    elp_sound_table <- if(nrow(elp_merged)>0){
      elp_sound <- elp_merged[c("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                                "Begin Path", "File Offset (s)", "Begin File", "Site", "Begin Hour", "Begin Clock Time", "File Start Date","Begin Date",
                                "Score", "Count", "Measurable", "Harmonics", "Ambiguous", "Notes", "Analyst","Rand", "Deployment Number",
                                "Sound Problems","Call Criteria","Disk")]
      elp_th<-filter(elp_sound,elp_sound$"Score">=Filter_ScoreThreshold) #filter the merged table by score threshold
      elp_th2_all<-filter(elp_sound,elp_sound$"Score"<Filter_ScoreThreshold) # filter events between initial detection score and new filtered score threshold

      # write the table with selections that have score > the threshold
      write.table(elp_th,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                            sub("\\d.","",Filter_ScoreThreshold),"_raw/", # folder name
                                            substr(file_size[q],1,nchar(file_size[q])-4),"_p",sub("\\d.","",Filter_ScoreThreshold),"_raw.txt",sep=""), # file name
                  sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) #save the table for score > threshold

      # write table with score below threshold
      write.table(elp_th2_all,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p",
                                         sub("\\d.","",Detector_ScoreThreshold),"-p",sub("\\d.","",Filter_ScoreThreshold),"_raw/", # folder name
                                         substr(file_size[q],1,nchar(file_size[q])-4),"_",sub("\\d.","p",Detector_ScoreThreshold),
                                         "-p",sub("\\d.","",Filter_ScoreThreshold),"_raw.txt",sep=""), # file name
                  sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE) # save all other detections < threshold
    } #if this doesn't run initially because it can't find the file, check that the "Begin Path" in the selection tables match
  }
}

#
# move the remaining selection tables to the final p2_raw folder
for (h in 1:length(files)){
 file.move(paste("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/processed/",files[h],sep=""),
           "~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw/",overwrite = TRUE)
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
  # prepare sound check sound list of good sounds
  ele_sounds <- sound_check
  if (min_23hrs == "n"){
    ele_sounds$`Exclude (y/e)`[ele_sounds$`Exclude (y/e)`=="e"] <-"Good"
  } # if 23 h per day is not required for analysis, mark "e" as "Good", otherwise, leave it as "e"
  ele_sounds$`Exclude (y/e)`[is.na(ele_sounds$`Exclude (y/e)`)|ele_sounds$`Exclude (y/e)`==""] <-"Good" # if minimum of 23 hrs per day required, mark the blank cells in Exclude (y/n) column as good. Leave "y" and "e" alone.
  #ele_sounds <- ele_sounds[(ele_sounds$`Exclude (y/e)` == "Good"),]#
  ele_sounds2 <- ele_sounds[c("Site", "Begin File", "File Duration (s)", "Duration Minutes","Exclude (y/e)","Sound Problems", "File Path")]
  names(ele_sounds2)[names(ele_sounds2) == "File Path"] <- "Begin Path"
  ele_sounds2$Date <- format(as.Date(str_extract(ele_sounds2$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")

  # detection summaries
  # filter selection tables by rand and score
  setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw")
  det_files <- list.files(path="~/R/Bobbi_Scripts/Packages/elpR/Files/Selection_Tables/rumble/final/p2_raw",
                          pattern=standard_name_disk,
                          all.files=TRUE, recursive=TRUE,full.names=FALSE)
  det_files <- lapply(det_files, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
  # sum(sapply(det_files, nrow))
  det_files_Detector_ScoreThreshold <- do.call("rbind", lapply(det_files, as.data.frame)) # Combine them into a data frame
  # det_files_p2 <- do.call("rbind", lapply(det_files, as.data.frame)) # Combine them into a data frame
  det_files_Filter_ScoreThreshold_dets <- det_files_Detector_ScoreThreshold[(det_files_Detector_ScoreThreshold$Score >=as.numeric(Filter_ScoreThreshold)),] # filter by score (user defined)
  # det_files_p4 <- det_files_p2[(det_files_p2$Score >.4),]
  if(three_rand_days == "y") {
    det_files_Filter_ScoreThreshold_dets_rand <- det_files_Filter_ScoreThreshold_dets[(det_files_Filter_ScoreThreshold_dets$Rand == "rand"),] # filter by random days only if rand days required (user defined)
    # det_files_p4_rand <- det_files_p4[(det_files_p4$Rand == "rand"),]
  }

  # summarize filtered detections by file name, date, and site
  det_files_Detector_ScoreThreshold_summary <- det_files_Detector_ScoreThreshold %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for lowest score threshold (Detector_ScoreThreshold from script)
  # dets_files_p2 <- det_files_p2 %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for lowest score threshold (Detector_ScoreThreshold from script)
  colnames(det_files_Detector_ScoreThreshold_summary) <- c("Begin File","Site", "Date","Sum Rumbles > Detector Threshold")
  #colnames(det_files_Detector_ScoreThreshold_summary) <- c("Begin File","Site", "Date",paste0("Number of Rumble Detections >",Detector_ScoreThreshold,sep=""))
  det_files_Filter_ScoreThreshold_dets_summary <- det_files_Filter_ScoreThreshold_dets %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and day for filtered score thresholds (Filter_ScoreThreshold from script)
  colnames(det_files_Filter_ScoreThreshold_dets_summary) <- c("Begin File","Site","Date" ,"Sum Rumbles > Filtered Detector Threshold")
  #colnames(det_files_Filter_ScoreThreshold_dets_summary) <- c("Begin File","Site","Date" ,paste0("Number of Rumble Detections >",Filter_ScoreThreshold,sep=""))
  if(three_rand_days == "y"){
    det_files_Filter_ScoreThreshold_dets_rand_summary <- det_files_Filter_ScoreThreshold_dets_rand %>% group_by(`Begin File`,Site,`Begin Date`) %>% tally() # summarize detections per site and random day for filtered score thresholds (Filter_ScoreThreshold from script)
    colnames(det_files_Filter_ScoreThreshold_dets_rand_summary) <- c("Begin File","Site","Date" ,"Sum Rumbles for Rand Days > Filtered Detector Threshold")
    #colnames(det_files_Filter_ScoreThreshold_dets_rand_summary) <- c("Begin File","Site","Date" ,paste0("Number of Rand Rumble Detections >",Filter_ScoreThreshold,sep=""))
  }


  # merge detections and sound file info (sound check)
  ele_sound_dets <- merge(ele_sounds2,det_files_Detector_ScoreThreshold_summary,all.x=T)
  ele_sound_dets <- merge(ele_sound_dets,det_files_Filter_ScoreThreshold_dets_summary,all.x=T)
  # mark non-rand dates if random dates are required
  if(three_rand_days == "y"){
    ele_sound_dets <- merge(ele_sound_dets,det_files_Filter_ScoreThreshold_dets_rand_summary,all.x=T)
    ele_sound_dets <- merge(ele_sound_dets,elp_rand,all.x=T,by.x="Date", by.y="Begin Date")
    ele_sound_dets$Rand[is.na(ele_sound_dets$Rand)] <- "non-rand"
  }

  for(f in 1:nrow(ele_sound_dets)) {
    if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
      ele_sound_dets$`Sum Rumbles > Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles > Detector Threshold`[f])] <-0
    }
    if(ele_sound_dets$`Exclude (y/e)`[f] == "Good") {
      ele_sound_dets$`Sum Rumbles > Filtered Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles > Filtered Detector Threshold`[f])] <-0
    }
    if(three_rand_days == "y"){
      if(ele_sound_dets$`Exclude (y/e)`[f] == "Good" && ele_sound_dets$Rand[f] == "rand" ) {
        ele_sound_dets$`Sum Rumbles for Rand Days > Filtered Detector Threshold`[f][is.na(ele_sound_dets$`Sum Rumbles for Rand Days > Filtered Detector Threshold`[f])] <-0
      }
    }
  }

  # Summarize by site and dates, with total duration and # detections p2 and p4 rand
  ele_sound_dates <- aggregate(`Duration Minutes`~Site+Date,data=ele_sound_dets,FUN=sum)
  ele_sound_dates_detectorTHreshold <- aggregate(`Sum Rumbles > Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
  #ele_sound_dates_detectorTHreshold <- aggregate(`Number of p2 Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)
  ele_sound_dates_scoreThreshold <- aggregate(`Sum Rumbles > Filtered Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
  #ele_sound_dates_p4 <- aggregate(`Number of p4 Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)
  if(three_rand_days == "y"){
    ele_sound_dates_ScoreThreshold_rand <- aggregate(`Sum Rumbles for Rand Days > Filtered Detector Threshold`~Site+Date,data=ele_sound_dets,FUN=sum)
    #ele_sound_dates_p4_rand <- aggregate(`Number of p4 Rand Rumble Detections`~Site+Date,data=ele_sound_dets,FUN=sum)
  }

  ele_sound_dets_sum <- merge(ele_sound_dets,ele_sound_dates_detectorTHreshold,all.x=T)
  ele_sound_dets_sum <- merge(ele_sound_dets_sum,ele_sound_dates_scoreThreshold,all.x=T)
  if(three_rand_days == "y"){
    ele_sound_dets_sum <- merge(ele_sound_dets_sum,ele_sound_dates_ScoreThreshold_rand,all.x=T)
  }
  ele_sound_dets_sum$`Duration (Hrs)` <- round(ele_sound_dets_sum$`File Duration (s)`/3600,digit=2)
  ele_sound_dets_sum$`File Duration (s)` <- round(ele_sound_dets_sum$`File Duration (s)`,digit = 2)
  ele_sound_dets_sum$`Duration Minutes` <- round(ele_sound_dets_sum$`Duration Minutes`,digit=2)
  ele_sound_dets_sum$`Dets_per_Hr` <- round(ele_sound_dets_sum$`Sum Rumbles > Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  ele_sound_dets_sum$`Dets_per_Hr_ScoreThreshold` <- round(ele_sound_dets_sum$`Sum Rumbles > Filtered Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  if(three_rand_days == "y"){
    ele_sound_dets_sum$`Dets_per_Hr_ScoreThreshold_rand` <- round(ele_sound_dets_sum$`Sum Rumbles for Rand Days > Filtered Detector Threshold`/ele_sound_dets_sum$`Duration (Hrs)`,digit=3)
  }
  ele_sound_dets_sum <- ele_sound_dets_sum[order(ele_sound_dets_sum$"Begin File"),] # sort by begin file
  if(three_rand_days == "y"){
    ele_sound_dets_sum <- ele_sound_dets_sum[c("Date","Site","Rand","Begin File","File Duration (s)","Duration Minutes","Duration (Hrs)","Exclude (y/e)",
                                             "Sum Rumbles > Detector Threshold" ,"Sum Rumbles > Filtered Detector Threshold","Sum Rumbles for Rand Days > Filtered Detector Threshold",
                                             "Dets_per_Hr","Dets_per_Hr_ScoreThreshold","Dets_per_Hr_ScoreThreshold_rand")]
  }
  if(three_rand_days == "n"){
    ele_sound_dets_sum <- ele_sound_dets_sum[c("Date","Site","Begin File","File Duration (s)","Duration Minutes","Duration (Hrs)","Exclude (y/e)",
                                               "Sum Rumbles > Detector Threshold" ,"Sum Rumbles > Filtered Detector Threshold",
                                               "Dets_per_Hr","Dets_per_Hr_ScoreThreshold")]
  }
  write.table(ele_sound_dets_sum,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/num_events/rumble/",standard_name_disk,"_",Detector,"_Detection_Summaries.txt",sep=""),
              sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE)


# save summaries as a new tab in sound_check
  # reports summarize based on sound files in sound check. If detections occur on dates not in sound check,
  # these summaries won't reflect those detections, but the processed selection tables will still have the detections
  # unless the option for filtering detections by begin and end date is selected
  proj_sites <- read.table(paste('~/R/Bobbi_Scripts/Packages/elpR/Files/sites/',sites,sep=""),header=TRUE,sep="\t", check.names=FALSE)
  ele_det_sites <- data.frame(Site = unique(proj_sites$Site))

  ele_det_duration_sites_sum <- aggregate(
    subset(ele_sound_dets_sum, `Exclude (y/e)` == "Good")$`Duration (Hrs)`,
    by = list(Site=subset(ele_sound_dets_sum, `Exclude (y/e)` == "Good")$Site),
    FUN=sum,
    na.rm = TRUE)

  if(three_rand_days == "y") {
    ele_det_ScoreThreshold_rand_sites_sum <- aggregate(
      ele_sound_dets_sum$`Sum Rumbles for Rand Days > Filtered Detector Threshold`,
      by = list(Site=ele_sound_dets_sum$Site),
      FUN=sum,
      na.rm = TRUE)
  } else {
    # Define what should happen if three_rand_days is not "y"
    ele_det_ScoreThreshold_rand_sites_sum <- data.frame(
      Site = unique(ele_sound_dets_sum$Site),
      x = NA
    )
  }

  ele_det_ScoreThreshold_sites_sum <- aggregate(
    ele_sound_dets_sum$`Sum Rumbles > Filtered Detector Threshold`,
    by = list(Site=ele_sound_dets_sum$Site),
    FUN=sum,
    na.rm = TRUE)

  ele_det_DetectorThreshold_sites_sum <- aggregate(
    ele_sound_dets_sum$`Sum Rumbles > Detector Threshold`,
    by = list(Site=ele_sound_dets_sum$Site),
    FUN=sum,
    na.rm = TRUE)

  # ifelse(three_rand_days == "y",
  #     ele_det_ScoreThreshold_rand_sites_sum <- aggregate(ele_sound_dets_sum$`Sum Rumbles for Rand Days > Filtered Detector Threshold`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE),
  #     ele_det_ScoreThreshold_rand_sites_sum <- )
  #ele_det_duration_sites_sum <- aggregate(ele_sound_dets_sum$`Duration (Hrs)`, by = list(Site=ele_sound_dets_sum$Site),FUN=sum, na.rm = TRUE)

  ele_det_duration_sites_sum <- merge(ele_det_sites, ele_det_duration_sites_sum, by = "Site", all.x = TRUE)
  ele_det_ScoreThreshold_sites_sum <- merge(ele_det_sites, ele_det_ScoreThreshold_sites_sum, by = "Site", all.x = TRUE)
  ele_det_DetectorThreshold_sites_sum <- merge(ele_det_sites, ele_det_DetectorThreshold_sites_sum, by = "Site", all.x = TRUE)
  if(three_rand_days == "y") {
    ele_det_ScoreThreshold_rand_sites_sum <- merge(ele_det_sites, ele_det_ScoreThreshold_rand_sites_sum, by = "Site", all.x = TRUE)
  }

  rumble_sound_stats <- cbind(ele_det_duration_sites_sum$Site,
                              ele_det_duration_sites_sum$x,
                              ele_det_DetectorThreshold_sites_sum$x,
                              ele_det_ScoreThreshold_sites_sum$x,
                              ele_det_ScoreThreshold_rand_sites_sum$x)

  colnames(rumble_sound_stats) <- c("Site","Duration (Hrs)",
                                    paste0("Number of Rumble Detections >",Detector_ScoreThreshold,sep=""),
                                    paste0("Number of Rumble Detections >",Filter_ScoreThreshold,sep=""),
                                    paste0("Number of Rand Rumble Detections >",Filter_ScoreThreshold,sep=""))
  #colnames(rumble_sound_stats) <- c("Site","Duration (Hrs)","Sum Detections","Sum p4 Detections","Sum p4 Rand Detections")

  wb <- loadWorkbook(file = paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""))
  if("Rumble Detector Summaries" %in% names(wb)){
    removeWorksheet(wb, "Rumble Detector Summaries")
    saveWorkbook(wb, paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),overwrite=TRUE)
  }
  addWorksheet(wb, sheetName = "Rumble Detector Summaries",tabColour='green')
  writeData(wb, sheet = "Rumble Detector Summaries", x = rumble_sound_stats)
  saveWorkbook(wb,paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),returnValue=FALSE,overwrite=TRUE)

# plot to detector summaries (total detections by sound file)
  det_sum_DetectorThreshold <- aggregate(`Sum Rumbles > Detector Threshold`~Date+Site, data=ele_sound_dets_sum,sum,na.rm=TRUE)
  det_sum_DetectorThreshold$Date <- as.Date(det_sum_DetectorThreshold$Date,format="%m/%d/%Y")
  det_sum_DetectorThreshold <- det_sum_DetectorThreshold[order(det_sum_DetectorThreshold$Site,det_sum_DetectorThreshold$Date),]
  if(three_rand_days == "y"){
    det_sum_ScoreThreshold_rand <-aggregate(`Sum Rumbles for Rand Days > Filtered Detector Threshold`~Date+Site, data=ele_sound_dets_sum,sum,na.rm=TRUE)
    det_sum_ScoreThreshold_rand$Date <- as.Date(det_sum_ScoreThreshold_rand$Date,format="%m/%d/%Y")
    det_sum_ScoreThreshold_rand <- det_sum_ScoreThreshold_rand[order(det_sum_ScoreThreshold_rand$Site,det_sum_ScoreThreshold_rand$Date),]
  }
  setwd("~/R/Bobbi_Scripts/Packages/elpR/Files/num_events/rumble")
  det_sum_DetectorThreshold_plot <- ggplot(det_sum_DetectorThreshold, aes(x = Date, y =  `Sum Rumbles > Detector Threshold`, colour = Site)) +
          geom_point(show.legend = FALSE)+
          scale_y_continuous(trans='log10')+
          facet_wrap( ~ Site)+
          ylab(paste0("Number of Rumble Detections >",Detector_ScoreThreshold," Score",sep=""))
  ggsave(plot = det_sum_DetectorThreshold_plot,paste0(standard_name_disk,"_",Detector,"_p",sub("\\d.","",Detector_ScoreThreshold),"_plot.png",sep=""),width =10, height =10 )

  if(three_rand_days == "y"){
    det_sum_ScoreThreshold_rand_plot <- ggplot(det_sum_ScoreThreshold_rand, aes(x = Date, y = `Sum Rumbles for Rand Days > Filtered Detector Threshold`, colour = Site)) +
            geom_point(show.legend = FALSE) +
            scale_y_continuous(trans='log10')+
            facet_wrap( ~ Site)+
            ylab(paste0("Number of Rumble Detections >",Filter_ScoreThreshold," Score",sep=""))
    ggsave(plot = det_sum_ScoreThreshold_rand_plot,
           paste("~/R/Bobbi_Scripts/Packages/elpR/Files/num_events/rumble/",standard_name_disk,"_",
                 Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_plot.png",sep=""),width =10, height =10)
  }

  #print(det_sum_p2_plot)
  #print(det_sum_p4_rand_plot)


# #### Create Zero-Days Selection Table ####
  # create dummy selection table with list of sound files from selection tables that have no detections at the filtered detector score threshold, but had detection above the original detector score.
  # rand only
  if(three_rand_days == "y"){
    sounds_det_ScoreTHreshold_rand <- ele_sound_dets[(ele_sound_dets$`Exclude (y/e)` == "Good"),]
    sounds_det_ScoreTHreshold_rand <- sounds_det_ScoreTHreshold_rand[(sounds_det_ScoreTHreshold_rand$Rand == "rand"),]
    sounds_det_ScoreTHreshold_rand <- sounds_det_ScoreTHreshold_rand[(sounds_det_ScoreTHreshold_rand$`Sum Rumbles for Rand Days > Filtered Detector Threshold` == 0),]
    ScoreThreshold_zero_rand <- sounds_det_ScoreTHreshold_rand[c("Date","Site","Begin File","Rand","Sound Problems")]
    ScoreThreshold_zero_rand$`Count` = "NA"
    ScoreThreshold_zero_rand$Measurable = "NA"
    ScoreThreshold_zero_rand$Harmonics = ""
    ScoreThreshold_zero_rand$Ambiguous = ""
    ScoreThreshold_zero_rand$`Begin Time (s)` = 20
    ScoreThreshold_zero_rand$`End Time (s)` = 100
    ScoreThreshold_zero_rand$View = "Spectrogram"
    ScoreThreshold_zero_rand$Channel = 1
    ScoreThreshold_zero_rand$`Low Freq (Hz)` = 10
    ScoreThreshold_zero_rand$`High Freq (Hz)` = 1000
    ScoreThreshold_zero_rand$Notes = paste("No Hori-Harm rumble detections on this date and site at threshold of",Filter_ScoreThreshold,sep = " ")
    ScoreThreshold_zero_rand$Selection = seq.int(nrow(ScoreThreshold_zero_rand))
    ScoreThreshold_zero_rand$`File Offset (s)`= 20
    ScoreThreshold_zero_rand$'Begin Date' <- as.Date(ScoreThreshold_zero_rand$Date,format = '%m/%d/%Y')
    ScoreThreshold_zero_rand$'Begin Date'<-format(ScoreThreshold_zero_rand$'Begin Date',"%m/%d/%Y")
    ScoreThreshold_zero_rand$"Event Date"<-ScoreThreshold_zero_rand$"Begin Date"
    ScoreThreshold_zero_rand$`Deployment Number` <- deployment_num
    ScoreThreshold_zero_rand$Score <- "NA"
    ScoreThreshold_zero_rand$`Begin Path` <- ""
    ScoreThreshold_zero_rand$Analyst <- "NA"
    ScoreThreshold_zero_rand$`Call Criteria` <- "NA"
    ScoreThreshold_zero_rand$`Begin Hour` <- ""
    ScoreThreshold_zero_rand$"File Start Date" <- ScoreThreshold_zero_rand$'Begin Date'

    ScoreThreshold_zero_rand <- ScoreThreshold_zero_rand[c("Selection", "View", "Channel", "Begin Time (s)",
                              "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
                              "Begin Path", "File Offset (s)", "Begin File", "Site",
                              "Begin Hour", "File Start Date","Begin Date",
                              "Score", "Count", "Measurable", "Harmonics", "Ambiguous",
                              "Notes", "Analyst","Rand", "Deployment Number",
                              "Sound Problems","Call Criteria")]

    write.table(ScoreThreshold_zero_rand,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/zero_days_SSTs/rumble/",standard_name_disk,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_rand_ZeroDets.txt",sep=""),
                sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)
  }

  # save table for all detections (rand and non-rand) and for filtered score threshold
    sounds_det_ScoreTHreshold <- ele_sound_dets[(ele_sound_dets$`Exclude (y/e)` == "Good"),]
    sounds_det_ScoreTHreshold <- sounds_det_ScoreTHreshold[(sounds_det_ScoreTHreshold$`Sum Rumbles > Filtered Detector Threshold` == 0),]
    ScoreThreshold_zero <- sounds_det_ScoreTHreshold[c("Date","Site","Begin File","Sound Problems")]
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
    ScoreThreshold_zero$Notes = paste("No Hori-Harm rumble detections on this date and site at threshold of",Filter_ScoreThreshold,sep = " ")
    ScoreThreshold_zero$Selection = seq.int(nrow(ScoreThreshold_zero))
    ScoreThreshold_zero$`File Offset (s)`= 20
    ScoreThreshold_zero$'Begin Date' <- as.Date(ScoreThreshold_zero$Date,format = '%m/%d/%Y')
    ScoreThreshold_zero$'Begin Date'<-format(ScoreThreshold_zero$'Begin Date',"%m/%d/%Y")
    ScoreThreshold_zero$"Event Date"<-ScoreThreshold_zero$"Begin Date"
    ScoreThreshold_zero$`Deployment Number` <- deployment_num
    ScoreThreshold_zero$Score <- "NA"
    ScoreThreshold_zero$`Begin Path` <- ""
    ScoreThreshold_zero$Analyst <- "NA"
    ScoreThreshold_zero$`Call Criteria` <- "NA"
    ScoreThreshold_zero$`Begin Hour` <- ""
    ScoreThreshold_zero$"File Start Date" <- ScoreThreshold_zero$'Begin Date'

    ScoreThreshold_zero <- ScoreThreshold_zero[c("Selection",
                                                 "View",
                                                 "Channel",
                                                 "Begin Time (s)",
                                                 "End Time (s)",
                                                 "Low Freq (Hz)",
                                                 "High Freq (Hz)",
                                                 "Begin Path",
                                                 "File Offset (s)",
                                                 "Begin File",
                                                 "Site",
                                                 "Begin Hour",
                                                 "File Start Date",
                                                 "Begin Date",
                                                 "Score",
                                                 "Count",
                                                 "Measurable",
                                                 "Harmonics",
                                                 "Ambiguous",
                                                 "Notes",
                                                 "Analyst",
                                                 "Deployment Number",
                                                 "Sound Problems",
                                                 "Call Criteria")]

    write.table(ScoreThreshold_zero,file=paste("~/R/Bobbi_Scripts/Packages/elpR/Files/zero_days_SSTs/rumble/",standard_name_disk,"_",Detector,"_p",sub("\\d.","",Filter_ScoreThreshold),"_ZeroDets.txt",sep=""),
                sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)

#### delete all folders in the Processed folder ####
  delete_all_folders <- function(path) {
    # List all directories in the given path
    dirs <- list.dirs(processed_HH, full.names = TRUE, recursive = FALSE)
    # Remove each directory
    sapply(dirs, unlink, recursive = TRUE)
    cat("Deleted", length(dirs), "folders from", path, "\n")
  }
  delete_all_folders(processed_HH)

  setwd('~/R/Bobbi_Scripts/Packages/elpR')
}

# BJE added "Begin Clock Time" to output file. If breaks, check this.

# library(devtools)
# document()
# build()
