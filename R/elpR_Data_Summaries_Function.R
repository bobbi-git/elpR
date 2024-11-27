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

## TO DO ##
# overlay plot and individual site plot (all sites)
# plot average per strata by unit time with redline (one strata per panel)
# daily summaries
# mean monthly plot per strata
# Enable option for rand dates
# enable option for rumble sound exclude (e)
# enable option for gunshot tables? Or just just one detector folder instead

data_summaries <- function (x) {

setwd(output)

  # install and load necessary packages
  det_sum_packages <- c("shiny","dplyr", "tidyverse","stringr","purrr","lubridate","readxl")
    options(warn = -1)
  for (i in det_sum_packages){
    if (!require(i, quietly = TRUE, character.only = TRUE)){
      install.packages(i)
      library(i)
    }
  }

# file names
proj_dep_name <-paste(project_name,"_dep",deployment_num,"_",detector_name,sep="") #default raw output table name

#### LOAD IN ALL DATA ####

##### Sound Check Files #####
# Should read in excel tables, not .txt files since those aren't frequently updated
# add if statement here if user has sound check...
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
sound_check_merge_df$`Deployment Number` <- as.numeric(sound_check_merge_df$`Deployment Number`)

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
#unique(sound_check_merge_df2$`Exclude (y/e)`, na.rm=T)
print(paste("Minimum Sound Check Date",min(sound_check_merge_df2$'File Start DateTime'),sep=" ")) # 2017-10-02 WAT
print(paste("Maximum Sound Check Date",max(sound_check_merge_df2$'File Start DateTime'),sep = " ")) # 2023-11-27 WAT
print(paste("Total hours of sounds in this dataset:",(sum(sound_check_merge_df2$`File Duration (s)`, na.rm=TRUE))/60/60,sep= " "))



##### Rand Dates  #####
#(3 random dates per week for elephant data, beginning in deployment 4)
elp_rand<-read.table('~/R/Bobbi_Scripts/PNNNR/Files/rand-days/Rand_dates.csv',header=TRUE,sep="\t",row.names=NULL) #read in rand file that contains 3 random days per week of the survey
elp_rand$Date<-format(as.Date(elp_rand$Date,"%d-%b-%y"), "%m/%d/%Y") #tell R what the date structure is and read it as date and format the date to match Raven selection tables
elp_rand$Date <- as.Date(elp_rand$Date,"%m/%d/%Y")
elp_rand$"Rand"<-"rand" #create a column that is populated with "rand" to merge with the selection tables


##### Hori-Harm Selection Tables #####
# (elephant detections (0+))
ele_tables_list<-list.files(path = ele_tables,recursive=F,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
ele_tables_list <- lapply(ele_tables_list, function(x) {
  read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "", fill=T)})  # Read the files in, assuming tab separator (except the one for dep1-3 since we load in one for deps 1-8)
#names(ele_tables_list) <- basename(ele_tables[c(1:6,8)]) # name the objects in list as the file names
ele_tables_list <- rapply(ele_tables_list, as.character, how = "replace") #make all columns character types before merging
ele_merge_df <- dplyr::bind_rows(ele_tables_list) # merge all tables together
#list2env(ele_tables_list ,.GlobalEnv)
#names(ele_merge_df)
cols.num <- c("Count","Deployment Number",'File Offset (s)')
ele_merge_df[cols.num] <- sapply(ele_merge_df[cols.num],as.numeric)
ele_merge_df <- rename(ele_merge_df, "Rumble Count" = "Count")
#stem(as.numeric(ele_merge_df$`Begin Hour`))
#unique(ele_merge_df$`Begin Hour`)

ele_dets_df <- ele_merge_df[,c("Site", "Begin File","File Offset (s)","Rumble Count","Deployment Number","Sound Problems", "Notes")]
ele_dets_df$`File Start DateTime` <- as.POSIXct(str_extract(ele_dets_df$`Begin File`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
ele_dets_df$`File Start Date` <- format(as.Date(str_extract(ele_dets_df$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
ele_dets_df$`Detection DateTime` <- ele_dets_df$`File Start DateTime`+ ele_dets_df$`File Offset (s)`
ele_dets_df$`Date` <- as.Date(ele_dets_df$`Detection DateTime`,"%Y%m%d",tz="Africa/Brazzaville")
ele_dets_df$Hour <- hour(ele_dets_df$`Detection DateTime`)#format(as.POSIXct(ele_dets_df$`Detection DateTime`), format = "%H") # get hour of selection
ele_dets_df$Detector <- detector_name
ele_dets_df <- rename(ele_dets_df, "File Name" = "Begin File")

# View(ele_dets_df)
# dim(ele_dets_df) #31854      13
# stem(ele_dets_df$Hour)
# unique(ele_dets_df$Hour)
# names(ele_dets_df)
print(paste("Minimum rumble selection table date:",min(ele_dets_df$`File Start DateTime`),sep=" "))
print(paste("Maximum rumble selection table date:",max(ele_dets_df$`File Start DateTime`),sep=" "))
print(paste("Total detected rumbles:",sum(ele_dets_df$`Rumble Count`, na.rm = TRUE),sep=" ")) # 2382 rumbles


##### Zero-Days Tables  #####
# selection tables with 0 hori-harm detections at filtered score threshold on rand dates
zero_days<-list.files(path = zero_txt,recursive=T,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
zero_days_list <- lapply(zero_days, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator
names(zero_days_list) <- basename(zero_days) # name the objects in list as the file names
zero_days_df <- dplyr::bind_rows(zero_days_list) # merge all tables together
#zero_days_df<- rename(zero_days_df, "Deployment Number" = "Deployment")
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
zero_days_df2$Detector <- detector_name

#### Empty Tables ####
# selection tables that had 0 hori-harm detections at the lowest score threshold

# Site information (optional)
site_info_names <- names(site_lat_long)

#### gunshot selection tables ####



#### HOURLY SUMMARIES ####

#create table for every hour from the first to last in detector files
# get unique date-hours from detection tables (make sure all sound check dates are included)
# smpl_hrs <- as.data.frame(seq(min(ele_dets_df$`File Start DateTime`,na.rm = TRUE), max(ele_dets_df$`File Start DateTime`,na.rm = TRUE),by = "hour")) #dataframe of sampled dates
# smpl_hrs <- rename(smpl_hrs, "DateTime" = "seq(min(ele_dets_df$`File Start DateTime`, na.rm = TRUE), max(ele_dets_df$`File Start DateTime`, na.rm = TRUE), by = \"hour\")") # rename column
# smpl_hrs$Date <- format(smpl_hrs$DateTime, "%Y-%m-%d") # add date
# smpl_hrs$Hour <- hour(smpl_hrs$DateTime) # add hour
# #nrow(smpl_hrs) #2424
#
# # get unique site IDs from sound check file
# sites <- as.data.frame(sort(unique(sound_check_merge_df2$Site))) # create dataframe of the sites
# sites <- rename(sites,"Site" = "sort(unique(sound_check_merge_df2$Site))") # don't know if this line is needed
# #nrow(sites) #21
#
# # repeat the sampled hours for each site
# smpl_hrs2 <- do.call("rbind", replicate(nrow(sites), smpl_hrs, simplify = FALSE)) # repeat dateTime for all hours and site
# #nrow(smpl_hrs2) #50904
#
# # repeat the sites for the number of hours *** There must be a better way ***
# sites2 <- do.call("rbind", replicate(nrow(smpl_hrs), sites, simplify = FALSE)) # repeat the site list n (50904 = number of hours for survey) times
# sites2$Site <- sites2[order(sites2$Site),] # order dataframe by site
# #nrow(sites2) #50904
#
# #combine sites and hourly times
# smpl_hrs_site <- cbind(smpl_hrs2,sites2) # combine the sites table with the hourly sample table
# #dim(smpl_hrs_site) #50904  x 4
# #min(smpl_hrs_site$DateTime) # "2023-08-04 WAT"
# #max(smpl_hrs_site$DateTime) # "2023-04-26 WAT"
#
# # combine sites and hour with rand dates
# smpl_hrs_site_rand <- merge(smpl_hrs_site,elp_rand,all.x=TRUE) # merge with the rand dates
# smpl_hrs_site_rand$Date <- as.Date(smpl_hrs_site_rand$Date)
# #min(smpl_hrs_site_rand$Date) # "2017-12-15"
# #max(smpl_hrs_site_rand$Date) # "2023-03-04"
# #dim(smpl_hrs_site_rand) #50904 x 5
# #unique(smpl_hrs_site_rand$Rand) # NA "rand"

# combing elephant rumbles and zero days
ele_dets_df_zero <- full_join(ele_dets_df,zero_days_df2)
print(paste("Total rumbles in merged table:",sum(ele_dets_df_zero$`Rumble Count`,na.rm=T), sep = " ")) # 2382
print(paste("Min date in merged table:",min(ele_dets_df_zero$Date), sep = " "))
print(paste("Max date in merged table:",max(ele_dets_df_zero$Date), sep = " "))
# stem(ele_dets_df_zero$Hour)

# add sound check (if user requested)
if(ele_bad_sound_remove == "y"){
       ele_dets_df_zero <- full_join(ele_dets_df_zero,sound_check_merge_df2) # merge det df with sound check
       ele_dets_df_zero <- ele_dets_df_zero %>%
         filter(is.na(`Exclude (y/e)`) | !(`Exclude (y/e)` %in% c("y", "e")))
      }
# sum(ele_det_sound_check$`Rumble Count`, na.rm = T)
# unique(ele_dets_df_zero$`Exclude (y/e)`,na.rm=T)

# add rand dates (if user requested) and remove detections on non rand dates if requested
# Note: For Deployments 2-3, 3 random dates were selected to be consistent with the other deployments (dep 01 is missing)
if(rand_dates_needed == "y"){
      ele_dets_df_zero <- ele_dets_df_zero[,-14]
      ele_dets_df_zero <- left_join(ele_dets_df_zero,elp_rand, by = "Date")
      ele_dets_df_zero <- ele_dets_df_zero %>% filter(`Rand` == "rand")
           # remove nonRand dates
      }
# unique(ele_dets_df_zero$Rand)
# sum(ele_dets_df_zero$`Rumble Count`, na.rm = T)
# names(ele_dets_df_zero)

# summarize rumble count to hourly
ele_dets_df_zero_hr <- ele_dets_df_zero %>%
  group_by(Date, Hour, Site,`Deployment Number`,Notes, `File Name`) %>%
  summarize("Sum Rumbles" = sum(`Rumble Count`))# for each hour date site
print(paste("Total Rumbles in merged hourly table:",sum(ele_dets_df_zero_hr$`Sum Rumbles`, na.rm=T),sep = " ")) # 57339
#dim(ele_dets_df_zero_hr) #45051      4
#min(ele_dets_df_zero_hr$Date) #"2017-12-15"
#max(ele_dets_df_zero_hr$Date) #"2023-03-04"

## hourly summary rumbles ##
# get unique date/sites from detection df
ele_date_site <- ele_dets_df_zero_hr[,c(-2,-4,-5,-7)]
ele_date_site <- ele_date_site %>%
  select(Site, Date, `File Name`) %>%
  distinct()
#dim(ele_date_site)

# Create a complete hour sequence for each surveyed Site-Date combination
ele_date_site_hr <- ele_date_site %>%
  mutate(Hour = list(0:23)) %>%
  unnest(Hour)

# merge the hourly dets df with the hourly df
ele_hrly1 <- left_join(ele_date_site_hr,ele_dets_df_zero_hr, by = c("Site", "Date", "Hour","File Name"))
ele_hrly1$`Sum Rumbles`[is.na(ele_hrly1$`Sum Rumbles`)] <- 0
print(paste("Total Rumbles in merged hourly table:",sum(ele_hrly1$`Sum Rumbles`, na.rm=T),sep = ""))
#unique(ele_hrly$Hour)

#ele_hrly1 <- merge(smpl_hrs_site_rand,ele_dets_df_zero_hr, all.x=TRUE) # merge sites with detections and zero days
# sum(ele_hrly1$`Sum Rumbles`,na.rm = TRUE) # 57339
# dim(ele_hrly1) #2468934       6
# min(ele_hrly1$Date) # "2017-12-15"
# max(ele_hrly1$Date) # "2023-03-04"
#dim(ele_hrly1[duplicated(ele_hrly1$`Sum Rumbles`),]) #2468814       6

# # summarize hourly sound check bad data
# # add optional
# soundCheck_hr <-  sound_check_merge_df2 %>%
#   group_by(Date, Hour, Site, `Exclude (y/e)`)
# soundCheck_hr$`Deployment Number` <- as.numeric(soundCheck_hr$`Deployment Number`)

# ele_hrly <- full_join(ele_hrly1,sound_check_merge_df2, by = c("File Name")) # merge rumbles with sound check
# add total bad sounds? It won't reflect earlier deployments that we don't have sound check files for
# print(paste("Total Rumbles in merged hourly table:",sum(ele_hrly$`Sum Rumbles`,na.rm = TRUE),sep=" ")) # 57339
# dim(ele_hrly) #2468934       8
# min(ele_hrly$Date) # "2017-12-15"
# max(ele_hrly$Date) # "2023-03-04"
# unique(ele_hrly$`Exclude (y/e)`) # NA  "e" "y"


# merge with gunshots data (needs development)
# merge with bad gain times (needs to be determined by noise levels)

# add strata, lat, and long
ele_hrly <- full_join(ele_hrly1,site_lat_long)
ele_hrly$Month <- month(ele_hrly$Date)
ele_hrly$Year <- year(ele_hrly$Date)

# day vs night
  #library(suncalc)
    # use lat long of each site to determine daily day and night hours
ele_hrly$DayNight<-ifelse(ele_hrly$Hour<6,"Night",
                        ifelse(ele_hrly$Hour<18,"Day","Night"))
#ele_hrly$JulianWeek <- round(julian(ele_hrly$Date)/7,digits=0)# add week of the dataset
ele_hrly$WeekDate <- floor_date(ele_hrly$Date, 'week') + 1 #first date of the week

# remove any sites that aren't in the user provided Site list (e.g., cluster analysis sites (nn09c1, nn09c2, nn09c3, nn09c4, etc))
if(use_only_sites_provided == "y"){
  ele_hrly <- ele_hrly[(ele_hrly$Site %in% unique(site_lat_long$Site)),]
}
#unique(ele_hrly$Site)
#sum(ele_hrly$`Sum Rumbles`,na.rm=T) #36236

write.table(ele_hrly,paste(output,"/",proj_dep_name,"Rumbles_ZeroDays_ExcludeBadSounds_Rand_hourly.txt",sep=""),
           sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

nightRumbles <- sum(ele_hrly[ele_hrly$DayNight == "Night",7],na.rm=TRUE)  #38791
dayRumbles <- sum(ele_hrly[ele_hrly$DayNight == "Day",7],na.rm=TRUE) #2246
proportionNightRumbles <- nightRumbles/(nightRumbles+dayRumbles) #0.945
print(paste("Proportion of night-time rumbles: ",round(proportionNightRumbles,digits=3),sep=" "))


  # ele_hrly_rand_excludedSound <-  ele_hrly %>%
  #   filter(is.na(`Exclude (y/e)`)) %>% # remove files to be excluded (e and y)
  #   filter(`Rand` != "" | !(is.na(`Rand`)))

  #min(ele_hrly_rand_excludedSound$Date) # "2018-05-01"
  #max(ele_hrly_rand_excludedSound$Date) # "2023-03-04"
  #sum(ele_hrly_rand_excludedSound$`Sum Rumbles`,na.rm = TRUE) #36656 total rumbles
  #dim(ele_hrly_rand_excludedSound) #991538      8

# write.table(ele_hrly_rand_excludedSound,
#             paste(output,"/",proj_dep_name,"_Rumbles_ZeroDays_BadsoundExclude_rand_hourly.txt",sep=""),
#             sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

# plot hourly rumbles over date-time
  # str(ele_hrly_rand_excludedSound)
  # ele_hrly_rand_excludedSound$`Sum Rumbles` <- as.numeric(ele_hrly_rand_excludedSound$`Sum Rumbles`)
  # ggplot(data = ele_hrly_rand_excludedSound, aes(x = DateTime, y = `Sum Rumbles`)) +
  #   geom_point()+
  #   scale_y_continuous(limits=c(0, 300))+
  #   facet_wrap( ~ Site)

# ele_hrly_rand_excludedSound %>%
#   group_by(Date) %>%
#   summarise(meanRumbles = mean(`Sum Rumbles`)) %>%
#   ggplot(aes(x = Date, y = "Sum Rumbles")) +
#   geom_point()+
#   scale_y_continuous(limits=c(0, 300))+
#   facet_wrap( ~ Site)

# plot day vs night rumbles (better of 0s removed)
  # ele_hrly_rand_excludedSound %>%
  #     #group_by(DayNight,Site) %>%
  #     ggplot(aes(x = DayNight, y = `Sum Rumbles`)) +
  #     geom_boxplot(fill = "blue",alpha = 0.2)+
  #     # scale_y_continuous(limits=c(0, 300))+
  #     facet_wrap( ~ Site)

# nightRumbles <- sum(ele_hrly_rand_excludedSound[ele_hrly_rand_excludedSound$DayNight == "Night",8],na.rm=TRUE)  #38791
# dayRumbles <- sum(ele_hrly_rand_excludedSound[ele_hrly_rand_excludedSound$DayNight == "Day",8],na.rm=TRUE) #2246
# proportionNightRumbles <- nightRumbles/(nightRumbles+dayRumbles) #0.945
# print(paste("Proportion of night-time rumbles: ",round(proportionNightRumbles,digits=3),sep=" "))

# # summarize hourly
# ele_hrly3 <- ele_hrly_rand_excludedSound %>%
#   group_by(Date, Hour, Site,DayNight,WeekDate,Latitude,Longitude,Strata, `Vegetation Class`,`Deployment Number`) %>%
#   summarize("Sum Rumbles" = sum(`Sum Rumbles`))# for each hour date site
# print(paste("Total Rumbles in merged hourly table:",sum(ele_hrly3$`Sum Rumbles`,na.rm = T),sep = " ")) # 57339
# print(paste("Total night time rumbles from hourly data: ",sum(ele_hrly3$`Sum Rumbles`[ele_hrly3$DayNight == "Night"],na.rm=TRUE),sep = ""))

## plot hourly data
# rumbles per date
ele_hrly_Date_plot<- ggplot(ele_hrly, aes(x = Date, y = `Sum Rumbles`)) +
                        geom_line() +
                        theme_bw() +
                        facet_wrap(~Site)
print(ele_hrly_Date_plot)

# rumbles day vs night
ele_hrly_nightday_plot <- ggplot(ele_hrly, aes(x = DayNight, y = `Sum Rumbles`)) +
                              geom_boxplot() +
                              theme_bw()
print(ele_hrly_nightday_plot)
ggsave(output,"/",proj_dep_name,"_ele_hrly_nightday_plot.png", ele_hrly_nightday_plot, width = 10, height = 6, dpi = 300)
ggsave(output,"/",proj_dep_name,"_ele_hrly_nightday_plot.eps", ele_hrly_nightday_plot, width = 10, height = 6, device = "eps")

# rumbles per hour by site
ele_hr_site_plot<- ggplot(ele_hrly, aes(x = Hour, y = `Sum Rumbles`)) +
                  stat_summary(fun = mean, geom = "bar") +
                  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
                  theme_bw()+
                  facet_wrap(~Site)
print(ele_hr_site_plot)
ggsave(output,"/",proj_dep_name,"_ele_hr_site_plot.png", ele_hr_site_plot, width = 10, height = 6, dpi = 300)
ggsave(output,"/",proj_dep_name,"_ele_hr_site_plot.eps", ele_hr_site_plot, width = 10, height = 6, device = "eps")

# rumbles per hour by site
ele_hr_plot<- ggplot(ele_hrly, aes(x = Hour, y = `Sum Rumbles`)) +
                  stat_summary(fun = mean, geom = "bar") +
                  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
                  theme_bw()
print(ele_hr_plot)
ggsave(output,"/",proj_dep_name,"_ele_hr_plot.png", ele_hr_plot, width = 10, height = 6, dpi = 300)
ggsave(output,"/",proj_dep_name,"_ele_hr_plot.eps", ele_hr_plot, width = 10, height = 6, device = "eps")

#### DAILY SUMMARIES ####
# summarize daily rumbles from hourly table
daily_site_summary <- ele_hrly %>%
  group_by(Date, Site,`WeekDate`,across(all_of(site_info_names))) %>%
  summarise(
    total_rumbles = sum(`Sum Rumbles`, na.rm = TRUE),
    n_hours = n(),
    night_total = sum(`Sum Rumbles`[DayNight == "Night"], na.rm = TRUE),
    prop_night = night_total/total_rumbles,
    prop_day = (total_rumbles-night_total)/total_rumbles
  ) %>%
  ungroup()
print(paste("Total rumbles per day for all dates:",sum(daily_site_summary$total_rumbles, na.rm=TRUE),sep=" ")) #36656

# ele_daily <-  ele_hrly %>%
#   group_by(Date, Site,Latitude,Longitude,`Vegetation Class`,Strata,`WeekDate`) %>%
#   summarize("Sum Rumbles" = if(all(is.na(`Sum Rumbles`))) NA_real_ else sum(`Sum Rumbles`, na.rm = TRUE)) %>%
#   #summarize("Sum Rumbles" = sum(`Sum Rumbles`,na.rm = TRUE)) %>%
#   ungroup()
# print(paste("Total rumbles per day for all dates:",sum(ele_daily$`Sum Rumbles`, na.rm=TRUE),sep=" ")) #36656
# #dim(ele_daily)
#
# # summarize night rumbles from hourly table and merge back into the daily rumbles table
# ele_daily_night <-  ele_hrly3 %>%
#   filter(DayNight == "Night") %>% # Filter by only nightTime rumbles
#   group_by(Date,Site) %>%
#   summarize("Sum Night Rumbles" = if(all(is.na(`Sum Rumbles`))) NA_real_ else sum(`Sum Rumbles`, na.rm = TRUE)) %>%
#   #summarize("Sum Night Rumbles" = sum(`Sum Rumbles`,na.rm = TRUE)) %>%
#   ungroup()
# print(paste("Total Night-Time Rumbles for all dates:",sum(ele_daily_night$`Sum Night Rumbles`, na.rm=TRUE),sep=" ")) #36656
# #dim(ele_daily_night)
#
# ele_daily_2 <-merge(ele_daily,ele_daily_night,all.x=TRUE)
# ele_daily_2$`Proportion Night Rumbles` <- round(ele_daily_2$`Sum Night Rumbles`/ele_daily_2$`Sum Rumbles`,digits=4)
# stem(ele_daily_2$`Proportion Night Rumbles`) # check the distribution of night rumble proportion
# print(paste("Total Daily Rumbles for all dates:",sum(ele_daily_2$`Sum Rumbles`, na.rm=TRUE),sep=" ")) #36656
# print(paste("Total Night-Time Rumbles for all dates:",sum(ele_daily_2$`Sum Night Rumbles`, na.rm=TRUE),sep=" "))
#
daily_site_summary$Month <- month(daily_site_summary$Date)
daily_site_summary$Year <- year(daily_site_summary$Date)
daily_site_summary$Week <- week(daily_site_summary$Date)

write.table(daily_site_summary,
            paste(output,"/",proj_dep_name,"_Rumbles_perDay.txt",sep=""),
            sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

## Plot daily data
# Time series of total rumbles per site
rumbles_daily_site_plot <- ggplot(daily_site_summary, aes(x = Date, y = total_rumbles)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~Site) +
  labs(title = "Total Rumbles Per Day by Site",
       y = "Total Rumbles",
       x = "Date")
print(rumbles_daily_site_plot)
ggsave(output,"/",proj_dep_name,"_ele_daily_site_plot.png", rumbles_daily_site_plot, width = 10, height = 6, dpi = 300)
ggsave(output,"/",proj_dep_name,"_ele_daily_site_plot.eps", rumbles_daily_site_plot, width = 10, height = 6, device = "eps")

# plot rumlbes per day per strata (if strata is present)
if("Strata" %in% names(daily_site_summary)){
  rumbles_daily_strata_plot <- ggplot(daily_site_summary, aes(x = Date, y = total_rumbles)) +
    geom_line() +
    theme_bw() +
    facet_wrap(~Strata) +
    scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y")+
    theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour="white", size=0.5))+
    labs(title = "Total Rumbles Per Day by Strata",
         y = "Total Rumbles",
         x = "Date")
  print(rumbles_daily_strata_plot)
  ggsave(output,"/",proj_dep_name,"_ele_daily_strata_plot.png", rumbles_daily_strata_plot, width = 10, height = 6, dpi = 300)
  ggsave(output,"/",proj_dep_name,"_ele_daily_strata_plot.eps", rumbles_daily_strata_plot, width = 10, height = 6, device = "eps")
}

# plot rumbles per day per vegetation class (if vegetation class is present)
if("Vegetation Class" %in% names(daily_site_summary)){
  rumbles_daily_vegetation_plot <- ggplot(daily_site_summary, aes(x = Date, y = total_rumbles)) +
    geom_line() +
    theme_bw() +
    facet_wrap(~`Vegetation Class`) +
    labs(title = "Total Rumbles Per Day by Vegetation Class",
         y = "Total Rumbles",
         x = "Date")
  print(rumbles_daily_vegetation_plot)
  ggsave(output,"/",proj_dep_name,"_ele_daily_vegetation_plot.png", rumbles_daily_vegetation_plot, width = 10, height = 6, dpi = 300)
  ggsave(output,"/",proj_dep_name,"_ele_daily_vegetation_plot.eps", rumbles_daily_vegetation_plot, width = 10, height = 6, device = "eps")
}

# save table of daily rumbles per strata
ele_daily_strata_table <-  daily_site_summary %>%
  group_by(Strata) %>%
  summarize(
    "Total Rumbles" = sum(total_rumbles,na.rm = TRUE),
    "Sum days sampled" = n(),
    "Average rumbles per day" = mean(total_rumbles, na.rm=TRUE),
    "Maximum rumbles per day" = max(total_rumbles, na.rm=TRUE),
    "Total night rumbles" = sum(night_total, na.rm=TRUE),
    "Average night time rumbles" = mean(night_total,na.rm=TRUE),
    "Maximum night rumbles" = max(night_total,na.rm=TRUE),
    "Proportion night time rumbles" = round(`Total night rumbles`/`Total Rumbles`,digits = 3)) %>%
  ungroup()
View(ele_daily_strata_table)
write.table(ele_daily_strata_table,
            paste(output,"/",proj_dep_name,"_Rumbles_perDay_perStrata.txt",sep=""),
            sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)


#### WEEKLY SUMMARIES ####
rumble_weekly_means_site <- daily_site_summary %>%
  group_by(Site,WeekDate,across(all_of(site_info_names))) %>%
  summarise(
    MeanDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
    else mean(`total_rumbles`, na.rm=TRUE),
    sdDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
    else sd(`total_rumbles`, na.rm=TRUE),
    n = n(),
    seDailyRumbles = sdDailyRumbles/sqrt(n),
    maxDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
    else max(`total_rumbles`, na.rm=TRUE),
    minDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
    else min(`total_rumbles`, na.rm=TRUE),
    sumRumbles = if(all(is.na(`total_rumbles`))) NA_real_
    else sum(`total_rumbles`, na.rm=TRUE),
    sumRumblesNight = if(all(is.na(`night_total`))) NA_real_
    else sum(`night_total`, na.rm=TRUE),
    SumRumblesSampleEffort = round(sumRumbles/n,digit=3),
    SumNightRumblesSampleEffort = round(sumRumblesNight/n,digit=3)
  )
#sum(rumble_weekly_means_site$sumRumbles, na.rm=T) #36236
rumble_weekly_means_site$`Proportion Night Rumbles` <- round(rumble_weekly_means_site$sumRumblesNight/rumble_weekly_means_site$sumRumbles,digits=4)
rumble_weekly_means_site$`Proportion Night Rumbles`[is.nan(rumble_weekly_means_site$`Proportion Night Rumbles`)] <- NA # remove NaN from proportion calc.
#stem(rumble_weekly_means_site$`Proportion Night Rumbles`) # look at the distribution of night rumble proportions
# stem(rumble_weekly_means_site$n) # looks like some weeks were sampled more than 3 times. *** Should check on this ***

write.table(rumble_weekly_means_site,paste(output,"/",proj_dep_name,
                                            "_Rumbles_Site_Weekly_Summaries.txt",sep=""),
            sep='\t',na="",col.names=TRUE,row.names=FALSE,
            quote=FALSE, append=FALSE)

# Plot average rumbles per week with linear regression line per strata
rumbleWklyStrata<-ggplot(rumble_weekly_means_site, aes(x=as.Date(WeekDate),y=MeanDailyRumbles))+
        #geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
        geom_line()+
        geom_point()+
        geom_errorbar(aes(ymin = MeanDailyRumbles - sd_rumbles,
                          ymax = MeanDailyRumbles + sd_rumbles),
                      width = 0.2)
        #stat_summary(fun.data=mean_cl_normal)+
        geom_smooth(method='lm')+
        facet_wrap(~Strata)+
        scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y")+
        theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_line(colour="white", size=0.5))+
        xlab("Month-Year")+
        ylab("Average Daily Rumbles per Week")+
        ggtitle("Average Daily Elephant Rumbles per Week by Strata")
 print(rumbleWklyStrata)
 ggsave(plot = rumbleWklyStrata,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_plot.png",sep=""),width =10, height =10)
 ggsave(plot = rumbleWklyStrata,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_plot.eps",sep=""),width =10, height =10)

# Plot Proportional rumbles (accounting for sampling effort)
 plot3 <- ggplot(weekly_summary, aes(x = WeekDate, y = prop_rumbles)) +
   geom_line(aes(group = sequence(rle(!is.na(prop_rumbles))$lengths))) +
   geom_point() +
   theme_bw() +
   labs(title = "Proportional Rumbles per Week",
        x = "Week",
        y = "Rumbles per Day")

# Summary table by Strata
if("Strata" %in% names(rumble_weekly_means_site)){
  rumble_daily_means_per_week_strata <- rumble_weekly_means_site %>%
    group_by(Strata) %>%
    summarise(
      MeanDailyRumbles = if(all(is.na(`sumRumbles`))) NA_real_
      else mean(`sumRumbles`, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(`sumRumbles`))) NA_real_
      else sd(`sumRumbles`, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`sumRumbles`))) NA_real_
      else max(`sumRumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`sumRumbles`))) NA_real_
      else min(`sumRumbles`, na.rm=TRUE),
      sumRumbles = if(all(is.na(`sumRumbles`))) NA_real_
      else sum(`sumRumbles`, na.rm=TRUE),
      sumRumblesNight = if(all(is.na(`sumRumblesNight`))) NA_real_
      else sum(`sumRumblesNight`, na.rm=TRUE)
    )
  View(rumble_daily_means_per_week_strata)
  write.table(rumble_daily_means_per_week_strata,paste(proj_dep_name,
                                                       "Rumbles_Strata_Daily_Summaries.txt",sep="_"),
              sep='\t',na="",col.names=TRUE,row.names=FALSE,
              quote=FALSE, append=FALSE)
}




# lm model
  # library(broom)
  #
  # lm_rumbles_stratum <-
  #   rumble_weekly_means_site %>%
  #   nest(data = -Strata) %>%
  #   mutate(model = map(data, ~lm(MeanDailyRumbles~as.numeric(JulianWeek),na.action = na.exclude, data = .)), tidied = map(model, tidy)) %>%
  #   unnest(tidied)
  #
  # plot(rumble_weekly_means_site$MeanDailyRumbles~as.numeric(rumble_weekly_means_site$JulianWeek),
  #      col=c("red", "green","blue")[factor(rumble_weekly_means_site$Strata)])
  # abline(lm_rumbles_stratum)


# linear regression of mean rumbles per day
  # lm_rumbles <- lm(MeanDailyRumbles~as.numeric(JulianWeek), data=rumble_weekly_means_site, na.action = na.exclude )
  # summary(lm_rumbles)
  #
  # plot(rumble_weekly_means_site$MeanDailyRumbles~as.numeric(rumble_weekly_means_site$JulianWeek))+
  #   abline(lm_rumbles)

##### average rumbles per month per site #####


# #plot
# ggplot(rumble_monthly_means_site, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
#   #geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
#   geom_line()+
#   #geom_point()+
#   stat_summary(fun.data=mean_cl_normal)+
#   #geom_smooth(method='lm')+
#   facet_wrap(~Site)+
#   scale_y_continuous()
#
# ##### average rumbles per month per stratum #####
# rumble_monthly_means_stratum <- ele_daily_rand_latLong_PNNNGrid %>%
#   group_by(YearMonth,YearMonthNum,stratum) %>%
#   summarise(
#     MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else mean(`Sum Rumbles`, na.rm=TRUE),
#     sdDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else sd(`Sum Rumbles`, na.rm=TRUE),
#     n = n(),
#     seDailyRumbles = sdDailyRumbles/sqrt(n),
#     maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else max(`Sum Rumbles`, na.rm=TRUE),
#     minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else min(`Sum Rumbles`, na.rm=TRUE),
#     sumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else sum(`Sum Rumbles`, na.rm=TRUE)
#   )
# sum(rumble_monthly_means_stratum$sumRumbles, na.rm=T) #36236
# write.table(rumble_monthly_means_stratum,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_dailyMean_Stratum.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)
#
#
# #plot stratum separate
# ggplot(rumble_monthly_means_stratum, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
#   geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
#   geom_line(color =  "dark blue")+
#   geom_point()+
#   stat_summary(fun.data=mean_cl_normal)+
#   geom_smooth(method='lm')+
#   facet_wrap(~stratum)+
#   scale_y_continuous()
#
# # plot stratum together (note that month 38 has 0 detections. Check sound check for this month)
# ggplot(rumble_monthly_means_stratum, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles,group=stratum,color=stratum))+
#   geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
#   geom_line(color =  "dark blue")+
#   geom_point()+
#   stat_summary(fun.data=mean_cl_normal)+
#   geom_smooth(method='lm',se=TRUE)+
#   #facet_wrap(~stratum)+
#   scale_y_continuous()
#
# # lm model
# library(broom)
#
# lm_rumbles_stratum <-
#   rumble_monthly_means_stratum %>%
#   nest(data = -stratum) %>%
#   mutate(model = map(data, ~lm(MeanDailyRumbles~as.numeric(YearMonthNum),na.action = na.exclude, data = .)), tidied = map(model, tidy)) %>%
#   unnest(tidied)
#
# plot(rumble_monthly_means_stratum$MeanDailyRumbles~as.numeric(rumble_monthly_means_stratum$YearMonthNum),
#      col=c("red", "green","blue")[factor(rumble_monthly_means_stratum$stratum)])
# abline(lm_rumbles_stratum)
#
#
# #### WEEKLY SUMMARIES ####
# # (need to finish writing script)
#
# # by site
# ele_weekly_rand_latLong_PNNNGrid_sites <- ele_daily_rand_latLong_PNNNGrid %>%
#   group_by(Site, Latitude, Longiture, YearWeek, YearMonth, YearMonthNum, Month, Week,`Vegetation Class`) %>%
#   summarise(
#     SumRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else sum(`Sum Rumbles`, na.rm = T),
#     SumBadSounds_h = if(all(is.na(Sum_Bad_Sounds_h))) NA_real_
#     else sum(Sum_Bad_Sounds_h, na.rm = T),
#     MeanDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else mean(`Sum Rumbles`, na.rm = T),
#     sdDailyrumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else sd(`Sum Rumbles`, na.rm = T),
#     n = n(),
#     seDailyRumbles = sdDailyRumbles/sqrt(n),
#     maxDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else max(`Sum Rumbles`, na.rm=TRUE),
#     minDailyRumbles = if(all(is.na(`Sum Rumbles`))) NA_real_
#     else min(`Sum Rumbles`, na.rm=TRUE)
#   )
# sum(ele_weekly_rand_latLong_PNNNGrid_sites$SumRumbles, na.rm=T) #36284
#
# #plot
# ggplot(ele_weekly_rand_latLong_PNNNGrid_sites, aes(x=as.numeric(YearMonthNum),y=MeanDailyRumbles))+
#   geom_point()
#
#


}
