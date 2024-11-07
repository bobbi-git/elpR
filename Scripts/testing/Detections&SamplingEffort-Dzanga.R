# ELP Detections and Sampling Effort

# This script reads in three data-types
   # - Elephant rumble detection data (requires columns 'Begin File', 'File Offset', 'Count' and 'Dep Num')
   # - Gunshot detection data (requires columns 'Begin File', 'File Offset', 'Count' and 'Dep Num')
   # - Zero days data to account for sampling effort (requires columns 'Begin File')
   # - sound check files


# CAUTION
# Some of these files may not have been updated after changes were made to sound file names, times, subsequent discoveries in poor sound quality, etc.
# This script assumes all the tables are up-to-date. All changes should be updated in these files before they are loaded in .R
# this does not yet include bad dates from the SwiftOne gain setting issue

# bje37@cornell.edu
# updated: Sept 2023

## ADD site strata ()
## Add night and day cat
# overlay plot and individual site plot (all sites)
# plot average per strata by unit time with rendline (one strata per panel)
# daily summaries
# mean monthly plot per strata
# add lat long

rm(list = ls())

###### READ FROM BASE TABLES IN BASETABLES FOLDER ######

library(dplyr)
library(tidyverse)
library(stringr)
library(purrr)
library(lubridate)
library(readxl)

output <- setwd("L://ELP//Projects//Dzanga//2022_DSPA_PAM_project//dz_analysis//base_tables//merged_data")

sound_checks <- "L://ELP//Projects//Dzanga//2022_DSPA_PAM_project//dz_analysis//base_tables//sound_checks//" # where all the sound check files are saved
ele_tables<-"L://ELP//Projects//Dzanga//2022_DSPA_PAM_project//dz_analysis//base_tables//ele//" # where all the elephant selection tables are saved
zero_txt <- "L://ELP//Projects//Dzanga//2022_DSPA_PAM_project//dz_analysis//base_tables//HH_zero_detection_days//" # where the zero-day (selection tables with dummy events on dates without rumbles) tables are saved

project_name <- "DSPA_PAM" # name of proejct (e.g., "PNNN", "DzangaBai", etc.)
deployment_num <- "01" # deployment(s) number (e.g., "01-12", "04")
detector_name <- "hori-harm" # name of the detector used for these data (e.g., "Hori-harm", "hand", "DTD")


# file names
proj_dep_name <-paste(project_name,deployment_num,detector_name,sep="_") #default raw output table name


##### Sound Check (list of all sound files) #####
  sound_check_list <- list.files(path = sound_checks,recursive=T,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
  sound_check_list <- lapply(sound_check_list, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator
  sound_check_list <- rapply(sound_check_list, as.character, how = "replace") #make all columns character types before merging
  sound_check_merge_df <- dplyr::bind_rows(sound_check_list) # merge all sound check files together
  cols.nums <- c("Duration Hours",'File Duration (s)',"SampleRate")
  sound_check_merge_df[cols.nums] <- sapply(sound_check_merge_df[cols.nums],as.numeric) # set specific columns as numeric
  sound_check_merge_df$`File Start DateTime` <- as.POSIXct(str_extract(sound_check_merge_df$`Current File Name`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
  sound_check_merge_df$`File Start Date` <- format(as.Date(str_extract(sound_check_merge_df$'Current File Name' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
  sound_check_merge_df$`File End DateTime` <- sound_check_merge_df$`File Start DateTime`+ sound_check_merge_df$`File Duration (s)`
  sound_check_merge_df$`File End Date` <- format(as.Date(sound_check_merge_df$`File End DateTime`,"%Y%m%d",tz="Africa/Brazzaville"),"%m/%d/%Y")
  sound_check_merge_df$Detector <- "Sound Check"
  sound_check_merge_df$'Sound Check Count' <- "sound check"
  sound_check_merge_df <- rename(sound_check_merge_df, "File Name" = "Current File Name")
  sound_check_merge_df <- rename(sound_check_merge_df, "Deployment Number" = "Deployment")

  sound_check_merge_df2 <- sound_check_merge_df[,c("Site", "File Name","File Duration (s)","File Start DateTime","File Start Date","File End DateTime",
                                                   "File End Date","Sound Check Count","Exclude (y/e)","Notes",
                                                   "Deployment Number")] # create new dataframe with specific columns
  sound_check_merge_df2 <- rename(sound_check_merge_df2, "Date" = "File Start Date")
  sound_check_merge_df2$`File Start DateTime` <- as.POSIXct(str_extract(sound_check_merge_df2$'File Name',"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
  sound_check_merge_df2$`File Start Date` <- format(as.Date(str_extract(sound_check_merge_df2$'File Name' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
  sound_check_merge_df2$`File End DateTime` <- sound_check_merge_df2$`File Start DateTime`+ sound_check_merge_df2$`File Duration (s)`
  sound_check_merge_df2$`File End Date` <- format(as.Date(sound_check_merge_df2$`File End DateTime`,"%Y%m%d",tz="Africa/Brazzaville"),"%m/%d/%Y")
  sound_check_merge_df2$Hour <- hour(sound_check_merge_df2$`File Start DateTime`)
  sound_check_merge_df2$Date <- as.Date(sound_check_merge_df$`File Start Date`,format="%m/%d/%Y")

  #View(sound_check_merge_df2)
  #names(sound_check_merge_df2)
  #dim(sound_check_merge_df2)
  #str(sound_check_merge_df2)
  min(sound_check_merge_df2$'File Start DateTime') # 2017-10-02 WAT
  max(sound_check_merge_df2$'File Start DateTime') # 2023-07-07 WAT


##### Rand Dates (3 random dates per week for elephant data, beginning in deployment 4) #####
  elp_rand<-read.table('~/R/Bobbi_Scripts/PNNNR/Files/rand-days/Rand_dates.csv',header=TRUE,sep="\t",row.names=NULL) #read in rand file that contains 3 random days per week of the survey
  elp_rand$Date<-format(as.Date(elp_rand$Date,"%d-%b-%y"), "%m/%d/%Y") #tell R what the date structure is and read it as date and format the date to match Raven selection tables
  elp_rand$Date <- as.Date(elp_rand$Date,"%m/%d/%Y")
  elp_rand$"Rand"<-"rand" #create a column that is populated with "rand" to merge with the selection tables
  # add rand dates from the first 3 deployments so we can include all that data


##### Hori-Harm Selection Tables (elephant detections (0+)) #####
  ele_tables_list<-list.files(path = ele_tables,recursive=T,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
  ele_tables_list <- lapply(ele_tables_list, function(x) {
    read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator (except the one for dep1-3 since we load in one for deps 1-8)
  #names(ele_tables_list) <- basename(ele_tables[c(1:6,8)]) # name the objects in list as the file names
  ele_tables_list <- rapply(ele_tables_list, as.character, how = "replace") #make all columns character types before merging
  ele_merge_df <- dplyr::bind_rows(ele_tables_list) # merge all tables together
  #list2env(ele_tables_list ,.GlobalEnv)
  #names(ele_merge_df)
  ele_merge_df <- rename(ele_merge_df, "Deployment Number" = "Deployment")
  cols.num <- c("Count","Deployment Number",'File Offset (s)')
  ele_merge_df[cols.num] <- sapply(ele_merge_df[cols.num],as.numeric)
  #ele_merge_df$RumbleCounts <- rowSums(ele_merge_df[,c("Tag 1","count","Count")],na.rm=TRUE)# add all of tag1, count and Count columns for one rumble count column
  ele_merge_df <- rename(ele_merge_df, "Rumble Count" = "Count")
  #ele_merge_df$'Deployment Number' <- rowSums(ele_merge_df[,c("dep","Dep Num")],na.rm=TRUE)

  ele_dets_df <- ele_merge_df[,c("Site", "Begin File","File Offset (s)","Rumble Count","Deployment Number","Sound Problems", "Notes")]
  ele_dets_df$`File Start DateTime` <- as.POSIXct(str_extract(ele_dets_df$`Begin File`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
  ele_dets_df$`File Start Date` <- format(as.Date(str_extract(ele_dets_df$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
  ele_dets_df$`Detection DateTime` <- ele_dets_df$`File Start DateTime`+ ele_dets_df$`File Offset (s)`
  ele_dets_df$`Date` <- as.Date(ele_dets_df$`Detection DateTime`,"%Y%m%d",tz="Africa/Brazzaville")
  ele_dets_df$Hour <- hour(ele_dets_df$`File Start DateTime`)#format(as.POSIXct(ele_dets_df$`Detection DateTime`), format = "%H")
  ele_dets_df$Detector <- "Hori-Harm"
  ele_dets_df <- rename(ele_dets_df, "File Name" = "Begin File")

  #View(ele_dets_df)
  dim(ele_dets_df) #31854      13
  min(ele_dets_df$`File Start DateTime`) # 2017-12-15 WAT
  max(ele_dets_df$`File Start DateTime`) # 2023-03-04 WAT
  sum(ele_dets_df$`Rumble Count`, na.rm = TRUE) # 2382 rumbles


##### Zero-Days Tables (sound files with 0 hori-harm detections) #####
  zero_days<-list.files(path = zero_txt,recursive=T,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
  zero_days_list <- lapply(zero_days, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator
  names(zero_days_list) <- basename(zero_days) # name the objects in list as the file names
  zero_days_df <- dplyr::bind_rows(zero_days_list) # merge all tables together
  zero_days_df<- rename(zero_days_df, "Deployment Number" = "Deployment")
  #View(zero_days_df)
  #names(zero_days_df)

  zero_days_df2 <- zero_days_df[,c("Site", "Begin File","File Offset (s)","Count","Deployment Number","Sound Problems", "Notes", "Rand")]
  zero_days_df2 <- rename(zero_days_df2, "Rumble Count" = "Count")
  zero_days_df2 <- rename(zero_days_df2, "File Name" = "Begin File")
  zero_days_df2$'Rumble Count' <- 0
  zero_days_df2$`File Start DateTime` <- as.POSIXct(str_extract(zero_days_df2$`File Name`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
  zero_days_df2$`File Start Date` <- format(as.Date(str_extract(zero_days_df2$'File Name' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
  zero_days_df2$`Detection DateTime` <- zero_days_df2$`File Start DateTime`+ zero_days_df2$`File Offset (s)`
  zero_days_df2$`Date` <- as.Date(zero_days_df2$`Detection DateTime`,"%Y%m%d",tz="Africa/Brazzaville")
  zero_days_df2$`Hour` <- hour(zero_days_df2$`File Start DateTime`)
  zero_days_df2$Detector <- "Hori-Harm"

  #str(zero_days_df2)
  #View(zero_days_df2)
  min(zero_days_df2$'File Start DateTime') # 2017-12-15 WAT
  max(zero_days_df2$'File Start DateTime') # 2023-02-26 WAT



#### gunshot selection tables ####



#### HOURLY SUMMARIES ####
#create table for every hour from the first to last in detector files
  smpl_hrs <- as.data.frame(seq(min(ele_dets_df$`File Start DateTime`,na.rm = TRUE), max(ele_dets_df$`File Start DateTime`,na.rm = TRUE),by = "hour")) #dataframe of sampled dates
  smpl_hrs <- rename(smpl_hrs, "DateTime" = "seq(min(ele_dets_df$`File Start DateTime`, na.rm = TRUE), max(ele_dets_df$`File Start DateTime`, na.rm = TRUE), by = \"hour\")") # rename column
  smpl_hrs$Date <- format(smpl_hrs$DateTime, "%Y-%m-%d") # add date
  smpl_hrs$Hour <- hour(smpl_hrs$DateTime) # add hour
  nrow(smpl_hrs) #2424

  sites <- as.data.frame(sort(unique(sound_check_merge_df2$Site))) # create dataframe of the sites
  sites <- rename(sites,"Site" = "sort(unique(sound_check_merge_df2$Site))") # don't know if this line is needed
  nrow(sites) #21

  # repeat the sampled hours for each site
  smpl_hrs2 <- do.call("rbind", replicate(nrow(sites), smpl_hrs, simplify = FALSE)) # repeat dateTime for all hours and site
  nrow(smpl_hrs2) #50904

  # repeat the sites for the number of hours *** There must be a better way!!!***
  sites2 <- do.call("rbind", replicate(nrow(smpl_hrs), sites, simplify = FALSE)) # repeat the site list n (50904 = number of hours for survey) times
  sites2$Site <- sites2[order(sites2$Site),] # order dataframe by site
  nrow(sites2) #50904

#combine sites and hourly times
  smpl_hrs_site <- cbind(smpl_hrs2,sites2) # combine the sites table with the hourly sample table
  dim(smpl_hrs_site) #50904  x 4
  max(smpl_hrs_site$DateTime) # "2023-08-04 WAT"
  min(smpl_hrs_site$DateTime) # "2023-04-26 WAT"

# combine with rand dates
  smpl_hrs_site_rand <- merge(smpl_hrs_site,elp_rand,all.x=TRUE) # merge with the rand dates
  smpl_hrs_site_rand$Date <- as.Date(smpl_hrs_site_rand$Date)
  min(smpl_hrs_site_rand$Date) # "2017-12-15"
  max(smpl_hrs_site_rand$Date) # "2023-03-04"
  dim(smpl_hrs_site_rand) #50904 x 5
  unique(smpl_hrs_site_rand$Rand) # NA "rand"

# combing elephant rumbles and zero days
  ele_dets_df_zero <- merge(ele_dets_df,zero_days_df2, all = TRUE)
  dim(ele_dets_df_zero) #  42620      14
  sum(ele_dets_df_zero$`Rumble Count`) # 2382
  #names(ele_dets_df_zero)
  min(ele_dets_df_zero$Date) # "NA" should be a date
  max(ele_dets_df_zero$Date) # "NA" should be a date

# summarize rumble count to hourly
  ele_dets_df_zero_hr <- ele_dets_df_zero %>%
    group_by(Date, Hour, Site) %>%
    summarize("Sum Rumbles" = sum(`Rumble Count`))# for each hour date site
  sum(ele_dets_df_zero_hr$`Sum Rumbles`) # 57339
  dim(ele_dets_df_zero_hr) #45051      4
  min(ele_dets_df_zero_hr$Date) #"2017-12-15"
  max(ele_dets_df_zero_hr$Date) #"2023-03-04"

# summarize hourly sound check bad data
  soundCheck_exclude_hr <-  sound_check_merge_df2 %>%
            group_by(Date, Hour, Site, `Exclude (y/e)`) %>%
            filter(`Exclude (y/e)` != "") %>% # remove files to be excluded
            summarize("Sum Bad Sounds (s)" = sum(`File Duration (s)`))# for each hour date site

# hourly summary rumbles
  ele_hrly1 <- merge(smpl_hrs_site_rand,ele_dets_df_zero_hr, all.x=TRUE) # merge sites with detections and zero days
  sum(ele_hrly1$`Sum Rumbles`,na.rm = TRUE) # 57339
  dim(ele_hrly1) #2468934       6
  min(ele_hrly1$Date) # "2017-12-15"
  max(ele_hrly1$Date) # "2023-03-04"

  dim(ele_hrly1[duplicated(ele_hrly1$`Sum Rumbles`),]) #2468814       6

  ele_hrly <- merge(ele_hrly1,soundCheck_exclude_hr,all.x=TRUE) # merge rumbles with sound check
  sum(ele_hrly$`Sum Rumbles`,na.rm = TRUE) # 57339
  dim(ele_hrly) #2468934       8
  min(ele_hrly$Date) # "2017-12-15"
  max(ele_hrly$Date) # "2023-03-04"
  unique(ele_hrly$`Exclude (y)`)
  sum(ele_hrly$`Sum Bad Sounds (s)`, na.rm=TRUE) #34555682 s
# should keep deployment in here

# merge with gunshots data
# merge with bad gain times

  setwd('L:/ELP/Projects/Nouabale/nn_analyses/base tables/merged_data')
  write.table(ele_hrly,paste(proj_dep_name,"Rumbles_ZeroDays_badSounds_randNonRand_hourly.txt",sep="_"),
              sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

# plot total rumbles and total bad sounds per hour (not working)
  ggplot(data = ele_hrly, aes(x = DateTime, y = "Sum Rumbles")) +
    geom_point()+
    #scale_y_continuous(limits=c(0, 300))+
    facet_wrap( ~ Site)


# remove non-rand dates and bad sound files that are to be excluded
# Note: Deployments 1-3 are excluded because they did not have 3 random days per week
  ele_hrly_rand_excludedSound <-  ele_hrly %>%
    filter(is.na(`Exclude (y/e)`)) %>% # remove files to be excluded
    filter(`Rand` != "" | !(is.na(`Rand`))) # remove nonRand dates
  min(ele_hrly_rand_excludedSound$Date) # "2018-05-01"
  max(ele_hrly_rand_excludedSound$Date) # "2023-03-04"
  sum(ele_hrly_rand_excludedSound$`Sum Rumbles`,na.rm = TRUE) #36656 total rumbles
  sum(ele_hrly_rand_excludedSound$`Sum Bad Sounds (s)`,na.rm = TRUE) #0 total bad sound days
  dim(ele_hrly_rand_excludedSound) #991538      8

  write.table(ele_hrly_rand_excludedSound,paste(proj_dep_name,
                                                "Rumbles_ZeroDays_soundExclude_rand_hourly.txt",sep="_"),
                                                sep='\t',na="",col.names=TRUE,row.names=FALSE,
                                                quote=FALSE, append=FALSE)

# plot hourly rumbles over time
  str(ele_hrly_rand_excludedSound)
  ele_hrly_rand_excludedSound$`Sum Rumbles` <- as.numeric(ele_hrly_rand_excludedSound$`Sum Rumbles`)
  ggplot(data = ele_hrly_rand_excludedSound, aes(x = DateTime, y = "Sum Rumbles")) +
    geom_point()+
    scale_y_continuous(limits=c(0, NA),expand = )+
    facet_wrap( ~ Site)

  ele_hrly_rand_excludedSound %>%
    group_by(Date) %>%
    summarise(meanRumbles = mean(`Sum Rumbles`)) %>%
    ggplot(aes(x = Date, y = "Sum Rumbles")) +
    geom_point()+
    scale_y_continuous(limits=c(0, 300))+
    facet_wrap( ~ Site)


#### DAILY SUMMARIES ####
# dataframe with all dates since start to present
  smpl_daily_site <- smpl_hrs_site[,c(2,4)] # extract date and site from the hourly sampled data
  smpl_daily_site <- smpl_daily_site[!(duplicated(smpl_daily_site)), ] # remove any duplicates from that
  smpl_daily_site$Date <- as.Date(smpl_daily_site$Date)
  dim(smpl_daily_site) #102924      2

# number of rumbles per date and site
  ele_daily1 <- ele_hrly_rand_excludedSound %>% # ele_hrly
    group_by(Date,Site) %>%
    summarise(`Sum Rumbles` = if(all(is.na(`Sum Rumbles`))) NA else
      sum(`Sum Rumbles`, na.rm = TRUE))
  sum(ele_daily1$`Sum Rumbles`, na.rm=TRUE) #36656
  dim(ele_daily1) #41395     3

# sum of bad sounds per date and site
  ele_daily2 <- ele_hrly %>%
    group_by(Date,Site) %>%
    summarize("Sum_Bad_Sounds_s" = sum(`Sum Bad Sounds (s)`, na.rm = FALSE))
  sum(ele_daily2$Sum_Bad_Sounds_s, na.rm=TRUE) #4536157
  dim(ele_daily2) #102924      3

  ele_daily <- merge(smpl_daily_site,ele_daily1, all.x=T) # merge the rumble dates with all sampled dates
  ele_daily <- merge(ele_daily,ele_daily2, all.x=T) # merge sum of bad sounds with previous
  sum(ele_daily$`Sum Rumbles`, na.rm = TRUE) #36656
  sum(ele_daily$Sum_Bad_Sounds_s, na.rm=TRUE) #4536157
  dim(ele_daily) #102924      4

# ele_daily$Sum_Rumbles <- as.character(ele_daily$Sum_Rumbles)
# is.na(ele_daily$Sum_Rumbles)

# add columns of interest
  ele_daily$Sum_Bad_Sounds_h <- ele_daily$Sum_Bad_Sounds_s/60/60
  ele_daily$Sum_Good_Sounds_h <- ifelse(is.na(ele_daily$`Sum Rumbles`),NA,
                                        (24 - ele_daily$Sum_Bad_Sounds_h))
  ele_daily$`Rumbles per Hour` <- format(round(as.numeric(ele_daily$`Sum Rumbles`/ele_daily$Sum_Good_Sounds_h),digits=3))

  ele_daily_randNonrand <- merge(ele_daily,elp_rand,all.x=TRUE) # merge with the rand dates
  ele_daily_randNonrand$Week <- sprintf("%02d", as.integer(format(ele_daily_randNonrand$Date, "%U"))+1)
  ele_daily_randNonrand$Month <- sprintf("%02d",month(ele_daily_randNonrand$Date))
  ele_daily_randNonrand$Year <- year(ele_daily_randNonrand$Date)
  ele_daily_randNonrand$YearMonth <- paste(ele_daily_randNonrand$Year,ele_daily_randNonrand$Month,sep="-")
  ele_daily_randNonrand$YearWeek <- paste(ele_daily_randNonrand$Year,ele_daily_randNonrand$Week,sep="-")
  dim(ele_daily_randNonrand) # 102924      13
  sum(ele_daily_randNonrand$`Sum_Bad_Sounds_s`, na.rm = TRUE) #4536157
  sum(ele_daily_randNonrand$`Sum Rumbles`, na.rm = TRUE) #36656

# add continuous month sample number
  YearMonthNum <- as.numeric(seq(1:length(unique(ele_daily_randNonrand$YearMonth))))
  YearMonth <- unique(ele_daily_randNonrand$YearMonth)
  sampMonth <- cbind(YearMonth,YearMonthNum)
  ele_daily_randNonrand_months <- merge(ele_daily_randNonrand,sampMonth,all.X=TRUE)
  sum(ele_daily_randNonrand_months$`Sum Rumbles`, na.rm=T) #36656

# add latitude and longitude
  lat_long <- read_excel("L:/ELP/Projects/Nouabale/nn_deployInfo/nn_all deploys/SiteInfo/siteinfo.xlsx")
  ele_daily_randNonrand_latLong <- merge(ele_daily_randNonrand_months,lat_long, all.x=T)

  write.table(ele_daily_randNonrand_latLong,"PNNN_Rumbles_ZeroDays_soundExclude_randNonRand_daily.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

# replace dates that were not sampled with NA for sum rumbles
  ele_daily_rand_latLong <- ele_daily_randNonrand_latLong
  ele_daily_rand_latLong$`Sum Rumbles` <- ifelse(ele_daily_rand_latLong$Rand =="rand", ele_daily_rand_latLong$`Sum Rumbles`,NA)
  sum(ele_daily_rand_latLong$`Sum Rumbles`,na.rm=TRUE) # 36656
  sum(ele_daily_rand_latLong$`Sum_Bad_Sounds_s`, na.rm = TRUE) #4536157

# remove dates from weeks that did not have 3 random days sampled
## count the number of non NA samples per year-week into a separate table
  smplsPerWk <- ele_daily_rand_latLong %>%
    group_by(YearWeek,Site) %>%
    filter(`Rand` == "rand") %>% # remove nonRand dates)
    count()
## then merge it back in
  ele_daily_rand_latLong <- merge(ele_daily_rand_latLong,smplsPerWk, all.x=TRUE)
  sum(ele_daily_rand_latLong$`Sum Rumbles`,na.rm=TRUE) # 36656
  sum(ele_daily_rand_latLong$`Sum_Bad_Sounds_s`, na.rm = TRUE) #4536157
  dim(ele_daily_rand_latLong) #102924     26
## convert sum rumbles to NA for weeks that didn't have 3 random dates
  ele_daily_rand_latLong$`Sum Rumbles` <- ifelse(ele_daily_rand_latLong$n >2,
                                                 ele_daily_rand_latLong$`Sum Rumbles`,NA)
  sum(ele_daily_rand_latLong$`Sum Rumbles`,na.rm=TRUE) # 36236

  write.table(ele_daily_rand_latLong,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_daily.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

# remove cluster analysis sites (nn09c1, nn09c2, nn09c3, nn09c4)
  ele_daily_rand_latLong_PNNNGrid <- ele_daily_rand_latLong[!(ele_daily_rand_latLong$Site %in% c("nn09c1","nn09c2","nn09c3","nn09c4")),]
  unique(ele_daily_rand_latLong_PNNNGrid$Site)
  sum(ele_daily_rand_latLong_PNNNGrid$`Sum Rumbles`,na.rm=T) #36236

###### average rumbles per month #####
# need to preserve NAs in sum rumbles
  rumble_monthly_means <- ele_daily_rand_latLong_PNNNGrid %>%
    group_by(YearMonth,YearMonthNum) %>%
    summarise(
      MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
                            else mean(`Sum Rumbles`, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
                            else sd(`Sum Rumbles`, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
                            else max(`Sum Rumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
                            else min(`Sum Rumbles`, na.rm=TRUE),
      sumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
                            else sum(`Sum Rumbles`, na.rm=TRUE)
    )
  sum(rumble_monthly_means$sumRumbles, na.rm=T) #36236

  ggplot(rumble_monthly_means, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
    geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
    geom_line()+
    geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    geom_smooth(method='lm')

  # linear regression of mean rumbles per day
  lm_rumbles <- lm(MeanDailyRumbles~as.numeric(YearMonthNum), data=rumble_monthly_means, na.action = na.exclude )
  summary(lm_rumbles)

  plot(rumble_monthly_means$MeanDailyRumbles~as.numeric(rumble_monthly_means$YearMonthNum))+
    abline(lm_rumbles)

##### average rumbles per month per site #####
  rumble_monthly_means_site <- ele_daily_rand_latLong_PNNNGrid %>%
    group_by(YearMonth,YearMonthNum,Site,stratum,"Vegitation Class") %>%
    summarise(
      MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else mean(`Sum Rumbles`, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else sd(`Sum Rumbles`, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else max(`Sum Rumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else min(`Sum Rumbles`, na.rm=TRUE),
      sumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else sum(`Sum Rumbles`, na.rm=TRUE)
    )
  sum(rumble_monthly_means_site$sumRumbles, na.rm=T) #36236

  #plot
  ggplot(rumble_monthly_means_site, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
    #geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
    geom_line()+
    #geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    #geom_smooth(method='lm')+
    facet_wrap(~Site)+
    scale_y_continuous()

##### average rumbles per month per stratum #####
  rumble_monthly_means_stratum <- ele_daily_rand_latLong_PNNNGrid %>%
    group_by(YearMonth,YearMonthNum,stratum) %>%
    summarise(
      MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else mean(`Sum Rumbles`, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else sd(`Sum Rumbles`, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else max(`Sum Rumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else min(`Sum Rumbles`, na.rm=TRUE),
      sumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
      else sum(`Sum Rumbles`, na.rm=TRUE)
    )
  sum(rumble_monthly_means_stratum$sumRumbles, na.rm=T) #36236
  write.table(rumble_monthly_means_stratum,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_dailyMean_Stratum.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)


  #plot stratum separate
  ggplot(rumble_monthly_means_stratum, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
    geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
    geom_line(color =  "dark blue")+
    geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    geom_smooth(method='lm')+
    facet_wrap(~stratum)+
    scale_y_continuous()

  # plot stratum together (note that month 38 has 0 detections. Check sound check for this month)
  ggplot(rumble_monthly_means_stratum, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles,group=stratum,color=stratum))+
    geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
    geom_line(color =  "dark blue")+
    geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    geom_smooth(method='lm',se=TRUE)+
    #facet_wrap(~stratum)+
    scale_y_continuous()

  # lm model
  library(broom)

  lm_rumbles_stratum <-
    rumble_monthly_means_stratum %>%
    nest(data = -stratum) %>%
    mutate(model = map(data, ~lm(MeanDailyRumbles~as.numeric(YearMonthNum),na.action = na.exclude, data = .)), tidied = map(model, tidy)) %>%
    unnest(tidied)

  plot(rumble_monthly_means_stratum$MeanDailyRumbles~as.numeric(rumble_monthly_means_stratum$YearMonthNum),
       col=c("red", "green","blue")[factor(rumble_monthly_means_stratum$stratum)])
    abline(lm_rumbles_stratum)


#### WEEKLY SUMMARIES ####
  # (need to finish writing script)

# by site
  ele_weekly_rand_latLong_PNNNGrid_sites <- ele_daily_rand_latLong_PNNNGrid %>%
    group_by(Site, Latitude, Longiture, YearWeek, YearMonth, YearMonthNum, Month, Week,`Vegetation Class`) %>%
    summarise(
      SumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
        else sum(`Sum Rumbles`, na.rm = T),
      SumBadSounds_h = if(all(is.na(Sum_Bad_Sounds_h))) NA_real_
        else sum(Sum_Bad_Sounds_h, na.rm = T),
      MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
        else mean(`Sum Rumbles`, na.rm = T),
      sdDailyrumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
        else sd(`Sum Rumbles`, na.rm = T),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
        else max(`Sum Rumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
        else min(`Sum Rumbles`, na.rm=TRUE)
      )
  sum(ele_weekly_rand_latLong_PNNNGrid_sites$SumRumbles, na.rm=T) #36284

  #plot
  ggplot(ele_weekly_rand_latLong_PNNNGrid_sites, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
    geom_point()

#### MONTHLY SUMMARIES ####
  #(need to finish writing script)

# by site
  ele_monthly_rand_latLong_PNNNGrid_sites <- ele_weekly_rand_latLong_PNNNGrid_sites %>%
    group_by(Site, Latitude, Longiture, YearMonth, YearMonthNum, Month, `Vegetation Class`) %>%
    summarise(
      SumRumbles = if(all(is.na(SumRumbles))) NA_real_
        else sum(SumRumbles, na.rm = T),
      SumBadSounds_h = if(all(is.na(SumBadSounds_h))) NA_real_
        else sum(SumBadSounds_h, na.rm = T),
      MeanWeeklyRumbles = if(all(is.na(SumRumbles))) NA_real_
        else mean(SumRumbles, na.rm = T),
      sdWeeklyrumbles = if(all(is.na(SumRumbles))) NA_real_
        else sd(SumRumbles, na.rm = T),
      n = n(),
      seWeeklyRumbles = sdWeeklyrumbles/sqrt(n),
      maxWeeklyRumbles = if(all(is.na(SumRumbles))) NA_real_
        else max(SumRumbles, na.rm=TRUE),
      minWeeklyRumbles = if(all(is.na(SumRumbles))) NA_real_
        else min(SumRumbles, na.rm=TRUE)
    )
  sum(ele_monthly_rand_latLong_PNNNGrid_sites$SumRumbles, na.rm=T) #36284

# plot mean daily rumbles per month
  ggplot(data = ele_monthly_rand_latLong_PNNNGrid_sites, aes(x = YearMonthNum, y = MeanWeeklyRumbles)) +
    geom_errorbar(aes(ymin=MeanWeeklyRumbles-seWeeklyRumbles, ymax=MeanWeeklyRumbles+seWeeklyRumbles), width=.1) +
    geom_line()+
    geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    geom_smooth(method='lm')
    #scale_y_continuous(limits=c(0, 300))+
    #facet_wrap( ~ Site)

# all sites
  ele_monthly_rand_latLong_PNNNGrid <- ele_weekly_rand_latLong_PNNNGrid_sites %>%
    group_by(YearMonth, YearMonthNum, Month) %>%
    summarise(
      SumRumbles = if(all(is.na(SumRumbles))) NA_real_
        else sum(SumRumbles, na.rm = T),
      SumBadSounds_h = if(all(is.na(SumBadSounds_h))) NA_real_
        else sum(SumBadSounds_h, na.rm = T),
      MeanWeeklyRumbles = if(all(is.na(SumRumbles))) NA_real_
        else mean(SumRumbles, na.rm = T),
      sdWeeklyrumbles = if(all(is.na(SumRumbles))) NA_real_
        else sd(SumRumbles, na.rm = T),
    )
  sum(ele_monthly_rand_latLong_PNNNGrid$SumRumbles, na.rm=T) #36284

#plot

#### SEASONALS SUMMARIES ####
  #(need to finish writing script)


#### YEARLY SUMMARIES ####
  #(need to finish writing script)



#### Summarize by site type, day and night ####
  # rumbles per day per week


#### Summarize by deployment ####


