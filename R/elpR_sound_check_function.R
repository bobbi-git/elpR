# This function performs the sound quality check on the Congo PNNN dataset and produces a report
# written by BJE
# bje37@cornell.edu
# Feb 2023

#### TO DO ####
## remove NA from sound check tab of spreadsheet ("NAs introduced by coercion")
## Flag start times that begin outside of expected hour (7-19) to indicate improper time stamp
## mark BAD SOUND as y in exclude column. It's in the script, but not working. Needs to be fixed.

sound.check <- function (x)
  {

  #setwd(x)
  # x <- sound_path

  # install and load necessary packages
  sound_check_packages <- c("shiny","plyr","dplyr","ggplot2","bigreadr","openxlsx","stringr",
                            "gsubfn","lubridate","filesstrings","data.table","warbleR",
                            "tuneR","tidyr","devtools")
  options(warn = -1)
  for (i in sound_check_packages){
    if (!require(i, quietly = TRUE, character.only = TRUE)){
      install.packages(i)
      library(i)
    }
  }

  standard_name_disk <- paste(deployment_name,"_dep",deployment_num,"_d",disk_ID, sep="") # output file names (deployment name, number, disk)

  # List all the sound files and their info
  file_paths<-list.files(path=x,all.files=TRUE,full.names = TRUE,include.dirs=TRUE,recursive = TRUE, pattern = sound_file_ext) #index file paths
  filesize<-file.size(file_paths) # file size (bytes)
  filenames<-basename(list.files(x,pattern = sound_file_ext,all.files=FALSE,full.names=FALSE,recursive = TRUE,include.dirs=FALSE))
  #fileduration<-duration_wavs(file_paths)$duration # sound file duration (s) - may need to change duration_wavs to wav_dur (DOESNT WORK when sounds are 0 bytes - remove before!)

  # Create the data frame with required sound check columns
  check <- data.frame(file_paths,filenames,filesize) #make new data frame with existing data from above
  for(k in 1:nrow(check)){
    check$duration[k] <- ifelse(check$filesize[k] == 0,"0",
                                ifelse(readChar(check$file_paths[k], 4, useBytes=TRUE)=="RIFF"|readChar(check$file_paths[k], 4, useBytes=TRUE)=="fLaC",
                                       duration_sound_files(files = check$file_paths[k], file.format = "\\.wav$|\\.flac$")$duration, # need to install FLAC program on computer for this to work. Not practical for all users.
                                       #duration_wavs(check$file_paths[k])$duration,
                                "BAD SOUND")) # calculate sound file duration (seconds). If corrupted (has a bad header) call it "Bad sound".
  } # This typically processes 1200 sounds per minute

  check$duration<-as.numeric(check$duration)
  colnames(check) <- c("File Path","Current File Name","File Size (Bytes)","File Duration (s)")  # name columns

  check$'File Size (GB)' <- round(check$`File Size (Bytes)`/1073741824,4) #  file size in GB
  check$Site <- sub("\\_","",str_extract(check$`Current File Name`,"[a-z]{2}\\d{2}[a-z]{1}."))
  #check$Site <- substr(str_extract(check$`Current File Name`,"[a-z]{2}\\d{2}[a-z]{1}_*"),1,nchar(str_extract(check$`Current File Name`,"[a-z]{2}\\d{2}[a-z]{1}_*"))-1) # currently does not interpret the cluster sites ("nn09c1" etc) names correctly because of extra character
  check$"Site Name Length" <- nchar(check$Site) # mark sound files that are longer or shorter than the expected file name length (25 characters, update as needed)
  check$"Site Name Length Check" <- ifelse(check$`Site Name Length`>5,"Site name too long",
                                           ifelse(check$`Site Name Length`<5,"Site name too short","Site name good")) # check the character length of sound name
  #check$FolderName <- ifelse(check$Site == substr(check$'Current File Name',1,5),"","Sound file in wrong folder") # Check that the site of the sound file matches the site of the folder (NEED TO DO)
  check$"Current File Start DateTime" <- str_extract(check$'Current File Name' ,"\\d{8}.\\d{6}") #  file start date and time from file name (_YYYYMMDD_HHMMSS)
  check$"Current File Start DateTime" <- as.POSIXct(check$`Current File Start DateTime`,format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert format of start date and time to proper data and time format
  check$"File Begin Hour" <- hour(check$`Current File Start DateTime`) #  the begin hour
  check$"Duration Hours" <- round(check$`File Duration (s)`/3600,digits = 2) #  total time in hours
  check$"Duration Minutes" <-round(check$`File Duration (s)`/60,digits=2) # total time in minutes
  check$"File Duration Check"<-ifelse(check$`File Duration (s)`>((fileDurationMin*60)+1),"File longer than expected",
                                      ifelse(check$`File Duration (s)`<((fileDurationMin*60)*0.98),"File shorter than expected","")) # Check if file is longer or shorter than 24 hours
  check$'Calculated End DateTime' <- as.POSIXct(check$`Current File Start DateTime`+
                                                  check$`File Duration (s)`,origin="1970-01-01",tz="Africa/Brazzaville") # add file duration to start time
  check$"Year Check"<-ifelse(as.character(year(check$`Current File Start DateTime`))=="2000","Wrong Year","") # check if sound file year is 2000 (this indicated a problem with recording time). See if these can be renamed if there is an announcement at the end of the recording period.
  check$"Expected File Start DateTime"<-NA
  check$`Begin Date` <- as.Date(check$"Current File Start DateTime",tz="Africa/Brazzaville")

  # read the sample rate of each file (takes several minutes, depending on number of files)
  check$SampleRate <- ""
  for(j in 1:nrow(check)){
    check$SampleRate[j] <- ifelse(check$'File Size (Bytes)'[j] ==0,"No Data",
                                  ifelse(is.na(check$`File Duration (s)` [j]),"BAD SOUND",
                                         read_wave(check$'File Path'[j],from=0,to=0.05)@samp.rate))
    print(paste0(j," of ", nrow(check)," complete"," ",Sys.time()," ",check$SampleRate[j]," Hz"))
  } # This typically processes 1200 sounds per minute
  check$Sample_Rate_Check <- ifelse(check$SampleRate == sample_rate, "",
                                    ifelse(check$SampleRate == "No Data","No Data",
                                           ifelse(check$SampleRate == "BAD SOUND", "BAD SOUND", "Wrong Sample Rate")
                                    ))

  # calculate total duration of sounds per site and date
  #detach(package:plyr,unload=TRUE)
  #site_date <- check %>% group_by(Site,`Begin Date`) %>% summarise("Total Duration per Date"= sum(`Duration Hours`))
  site_date <- aggregate(check$`Duration Hours`,by=list(Site=check$Site,`Begin Date`=check$`Begin Date`),FUN=sum)
  colnames(site_date) <- c("Site","Begin Date","Total Hours in Date per Site")

  ## Calculate expected start time
  # split data frame into sites-wise data frames
  for(i in unique(check$Site)) {
    nam <- paste("check", i, sep = ".")
    assign(nam, check[check$Site==i,])
  }

  # make a list of all the site data frames
  site_dataframes <-lapply(ls(pattern=paste("check.",substr(deployment_name, start=1, stop=2), sep="")), function(x) get(x))

  # for each site data frame, calculate expected file start time based on length of previous file within the site
  for(h in 1:length(site_dataframes)){
    if(nrow(site_dataframes[[h]])>1){
      for (l in 2:nrow(site_dataframes[[h]])){
        site_dataframes[[h]]$`Expected File Start DateTime`[l] <-
          ifelse(site_dataframes[[h]]$`File Duration (s)`[l]>86400,
                 as.numeric(site_dataframes[[h]]$`Calculated End DateTime`[l-1]),
                 as.numeric(site_dataframes[[h]]$`Calculated End DateTime`[l-1])+(60*3))
      }
    }
  }

  # create new data frame with the calculated time end
  sound_check<-rbindlist(site_dataframes)
  sound_check$`Duration (DHMS)` <- round(seconds_to_period(sound_check$'File Duration (s)'),digits=2) # File duration in Days, hours, minutes, seconds
  sound_check$`Expected File Start DateTime` <- ifelse(is.na(sound_check$'Expected File Start DateTime'),
                                                       sound_check$`Current File Start DateTime`,
                                                       sound_check$`Expected File Start DateTime`)# start time of the first sound file
  sound_check$`Expected File Start DateTime` <- as.POSIXct(sound_check$'Expected File Start DateTime', origin = "1970-01-01", tz="UTC") # convert to date/time
  sound_check$`Expected File Start Difference`<-round(seconds_to_period(abs(sound_check$'Current File Start DateTime'-sound_check$'Expected File Start DateTime')),digits=2)
  sound_check$`Expected File Start Difference minutes`<-round(period_to_seconds(sound_check$`Expected File Start Difference`)/60,digits=2)
  sound_check$`Sound Gap Check`<-ifelse(seconds_to_period(sound_check$'Current File Start DateTime'-sound_check$'Expected File Start DateTime')>300,"Sound Gap",
                                        ifelse(seconds_to_period(sound_check$"Current File Start DateTime"-sound_check$"Expected File Start DateTime")<(-300),"Sound Overlap",""))
  sound_check$`File Length Check`<-ifelse(sound_check$`File Duration (s)`==0,"No Data","") # mark sound files with length 0 as "No Data"
  sound_check$Notes <- ""
  sound_check$`Deployment Number` <- deployment_num
  sound_check <- merge(sound_check,site_date,all.x=T)
  sound_check$`Exclude (y/e)`<-ifelse(sound_check$'File Duration (s)'<60,"y",
                                    ifelse(sound_check$`Total Hours in Date per Site`<22.95,"e",
                                           ifelse(sound_check$`SampleRate`=="BAD SOUND","y",""))) # mark sounds that are less than 1 minute (60 s) with a 'y' to exclude

  sound_check$`Date` <- as.Date(sound_check$`Current File Start DateTime`)
  View(sound_check) # look at the table

  # restructure table to cleaner output format
  sound_check_output<-sound_check[,c("Site","File Path","Current File Name","Site Name Length Check", "Current File Start DateTime","Calculated End DateTime","Begin Date","File Duration (s)","Duration Minutes","Duration Hours",
                                     "File Duration Check","Duration (DHMS)","Total Hours in Date per Site","File Length Check","File Size (GB)","SampleRate", "Sample_Rate_Check","Year Check","Expected File Start Difference minutes",
                                     "Sound Gap Check","Exclude (y/e)","Notes","Deployment Number")]
  #View(sound_check_output) #look at the formatted output table

# Create the output for the .xlsx summary workbook
  check_name_length<-sound_check %>% group_by(Site,`Site Name Length Check`) %>% tally() # check file name
  colnames(check_name_length) <- c("Site","File Name Length Issue","Total Files with Issue")
  check_file_duration<-sound_check %>% group_by(Site,`File Duration Check`) %>% tally() # check file length
  colnames(check_file_duration) <- c("Site","File Duration Issue","Total Files with Issue")
  check_wrong_Year<-sound_check %>% group_by(Site,`Year Check`) %>% tally() # check file year
  colnames(check_wrong_Year) <- c("Site","Wrong Year","Total Files with Issue")
  check_sound_Gap<-sound_check %>% group_by(Site,`Sound Gap Check`) %>% tally() # check for sound gaps or overlap
  colnames(check_sound_Gap) <- c("Site","Sound Gap Issue","Total Files with Issue")
  check_0s_duration <- sound_check %>% group_by(Site,`File Length Check`) %>% tally() # check that duration of file is not 0s
  colnames(check_0s_duration) <- c("Site","File Duration (s) Issue","Total Files with Issue")
  #check_file_size<- sound_check %>% group_by(Site,``) %>% tally() # check that file is not 0kb - this won't work. Files of 0 kb break the code and wouldn't make it this far
  sum_sounds <- sound_check %>% group_by(Site) %>% tally() # number of sound files per site
  colnames(sum_sounds) <- c("Site","Total Files")
  sample_rate_check<-sound_check %>% group_by(Site,`SampleRate`) %>% tally() # check file name
  colnames(sample_rate_check) <- c("Site","Sample Rate (Hz)","Sum Sample Rate Files")
  sum_GB <- aggregate(sound_check$"File Size (GB)", by=list(Site=sound_check$Site), FUN=sum)# Total file size in GB
  avg_GB <- aggregate(sound_check$"File Size (GB)", by=list(Site=sound_check$Site), FUN=mean) # Average file size in GB
  sum_duration_Hrs <- aggregate(sound_check$"Duration Hours", by=list(Site=sound_check$Site), FUN=sum) # Total duration in hours
  avg_duration_Hrs <- aggregate(sound_check$"Duration Hours", by=list(Site=sound_check$Site), FUN=mean) # Average duration in hours

  sound_stats <- cbind(avg_GB,sum_GB$x,avg_duration_Hrs$x,sum_duration_Hrs$x)
  colnames(sound_stats) <- c("Site","Average Size (GB)","Sum Size (GB)","Average Duration (Hours)","Sum Duration (Hours)")
  proj_sites <- as.data.frame(read.table(paste('~/R/Bobbi_Scripts/Packages/elpR/Files/sites/',sites,sep=""),header=TRUE,sep="\t", check.names=FALSE))
  # colnames(proj_sites) <- c("Site")
  site_sound_stats <- merge(sound_stats,proj_sites,by = "Site",all=TRUE)# merge the sound stats and sites together


  list_of_reports <- list("Sounds" = sound_check_output,
                          "File Name Length" = check_name_length,
                          "File Duration" = check_file_duration,
                          "Wrong Year" = check_wrong_Year,
                          "Sound Gap-Overlap" = check_sound_Gap,
                          "Duration 0s" = check_0s_duration,
                          "Sound File Count" = sum_sounds,
                          "Sample_Rate" = sample_rate_check,
                          "Sound Stats" = site_sound_stats)

  sound_check_file <- "~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/"

  write.xlsx(list_of_reports,file=paste(sound_check_file,"Sound_Check_Reports_",standard_name_disk,".xlsx",sep=""),asTable=TRUE,na="")
  #write.table(sound_check,file=paste(sound_check_file,"Sound_Check_",standard_name_disk,".txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE)

  return(c(sound_stats))

  # return(
  #   ggplot(data = duration_by_date_hrs) +
  #   geom_line(mapping = aes(x = Date, y = (x/60/60))) +
  #   facet_wrap(~ Site, nrow = 2) +
  #   ylab("File Duration (Hours)")# plot total recording duration per file by time per site
  # )
}
