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
# plot mean rumbles per sampled date to account for sampling effort
# enable option for gunshot tables? Or just just one detector folder instead
# remove dates that are not listed in sound check file? (optional) - shouldn't this already be happening by only marking 'good' dates from rumble SSTs?

data_summaries <- function (x) {
    graphics.off()  # close all plot devices
    gc()            # collect garbage

    if(sound_check_include == "y"){
      print("you indicated that you have a sound_check file for this script to filter out bad sounds")
    } else {print("you indicated that you DO NOT have a sound_check file for this script to filter out bad sounds")
        }
    if(rand_dates_needed == "y"){
      print("you indicated that you would like to filter the selections by those that fall on a pre-selected random date")
    } else { print("you indicated that you DO NOT want to filter the selections by those that fall on a pre-selected random date. All dates will be included in results")
      }
    if(ele_bad_sound_remove == "y"){
      print("you want to remove detections that occur on sound files with <23 hrs of sound")
    } else {print("you DO NOT want to remove detections that occur on sound files with <23 hrs of sound. Detections that occur on partial sound dates will be included in results")
        }
    if(use_only_sites_provided == "y"){
      print("you only want to include the sites that were listed in the site_lat_long file. This might result in detections being excluded if they occur on sites not included in the site_lat_long file")
    } else {print("you want to include all sites that are in the merged selection tables, and will not filter only by sites listed in the site_lat_long file.")
        }


  setwd(output)

    # install and load necessary packages
    det_sum_packages <- c("shiny","dplyr", "tidyverse","stringr","purrr","lubridate","readxl","stargazer","broom","ggplot2")
      options(warn = -1)
    for (i in det_sum_packages){
      if (!require(i, quietly = TRUE, character.only = TRUE)){
        install.packages(i)
        library(i)
      }
    }

   # Define helper to not kill function if saving fails
    safe_ggsave <- function(filename, plot, ...) {
      tryCatch(
        ggsave(filename, plot = plot, ...),
        error = function(e) {
          warning("ggsave failed for ", filename, ": ", conditionMessage(e))
        }
      )
    }

    # Make any error drop us into seeing where it came from
    options(error = rlang::entrace)  # requires rlang


  # file names
  proj_dep_name <-paste(project_name,"_dep",deployment_num,"_",detector_name,sep="") #default raw output table name

  #### LOAD IN ALL DATA ####

  ##### Sound Check Files #####
    # Should read in excel tables, not .txt files since those aren't frequently updated
    # add if statement here if user has sound check...
    if(sound_check_include == "y"){
      sound_check_list <- list.files(path = sound_checks,
                                     recursive=T,
                                     full.names=TRUE,
                                     pattern = "*.xlsx") # list all the selection tables in the ele_tables folder
      #sound_check_list <- lapply(sound_check_list, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator
      #sound_check_list <- rapply(sound_check_list, as.character, how = "replace") #make all columns character types before merging
      #sound_check_merge_df <- do.call("rbind", lapply(sound_check_list_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
      # sum(sapply(all_txt_df, nrow))
      sound_check_list_df <- lapply(sound_check_list, read_xlsx,
                                    sheet = "Sounds",
                                    col_names = TRUE)  # Read the files in
      sound_check_list_df <- lapply(sound_check_list_df, function(df) {
        if("SampleRate" %in% names(df)) {
          df$SampleRate <- as.numeric(as.character(df$SampleRate))
        }
        return(df)
      })
      sound_check_merge_df <- dplyr::bind_rows(sound_check_list_df) # merge all sound check files together
      cols.nums <- c("Duration Hours",'File Duration (s)',"SampleRate")
      sound_check_merge_df[cols.nums] <- sapply(sound_check_merge_df[cols.nums],as.numeric) # set specific columns as numeric
      sound_check_merge_df$`File Start DateTime` <- as.POSIXct(str_extract(sound_check_merge_df$`Current File Name`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
      sound_check_merge_df$`File Start Date` <- format(as.Date(str_extract(sound_check_merge_df$'Current File Name' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
      sound_check_merge_df$`File End DateTime` <- sound_check_merge_df$`File Start DateTime`+ sound_check_merge_df$`File Duration (s)`
      sound_check_merge_df$`File End Date` <- format(as.Date(sound_check_merge_df$`File End DateTime`,"%Y%m%d",tz="Africa/Brazzaville"),"%m/%d/%Y")
      sound_check_merge_df$Detector <- "Sound Check"
      sound_check_merge_df$'Sound Check Count' <- "sound check"
      sound_check_merge_df <-dplyr::rename(sound_check_merge_df,`File Name` = `Current File Name`) # rename to, from
      #sound_check_merge_df <-rename(sound_check_merge_df,`Deployment Number` = `Deployment`) # rename to, from
      # Check if "Deployment" exists and "Deployment Number" doesn't
      if ("Deployment" %in% names(sound_check_merge_df) &&
          !("Deployment Number" %in% names(sound_check_merge_df))) {
        sound_check_merge_df <- dplyr::rename(sound_check_merge_df,
                                       `Deployment Number` = `Deployment`)
      }
      sound_check_merge_df$`Deployment Number` <- as.numeric(sound_check_merge_df$`Deployment Number`)
      sound_check_merge_df2 <- sound_check_merge_df[,c("Site", "File Name","File Duration (s)","File Start DateTime","File Start Date","File End DateTime",
                                                       "File End Date","Sound Check Count","Exclude (y/e)","Notes",
                                                       "Deployment Number")] # create new dataframe with specific columns
      sound_check_merge_df2 <- dplyr::rename(sound_check_merge_df2, "Date" = "File Start Date")
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
      write.table(sound_check_merge_df2,paste(output,"/",proj_dep_name,"_soundCheck_merged.txt",sep=""),sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)
      print(paste("Minimum Sound Check Date",min(sound_check_merge_df2$'File Start DateTime', na.rm = TRUE),sep=" ")) # 2017-10-02 WAT
      print(paste("Maximum Sound Check Date",max(sound_check_merge_df2$'File Start DateTime', na.rm = TRUE),sep = " ")) # 2023-11-27 WAT
      print(paste("Total hours of sounds in this dataset:",round(sum(sound_check_merge_df2$`File Duration (s)`, na.rm=TRUE)/60/60, digits = 4),sep= " "))
      print(paste("Total years of sounds in this dataset:",round(sum(sound_check_merge_df2$`File Duration (s)`, na.rm=TRUE)/60/60/24/365, digits = 4),sep= " "))
    }


  ##### Rand Dates  #####
    #(3 random dates per week for elephant data, beginning in deployment 4)
    if(rand_dates_needed == "y"){
      elp_rand<-read.table('~/R/Bobbi_Scripts/Packages/elpR/Files/rand-days/Rand_dates.csv',header=TRUE,sep="\t",row.names=NULL) #read in rand file that contains 3 random days per week of the survey
      elp_rand$Date<-format(as.Date(elp_rand$Date,"%d-%b-%y"), "%m/%d/%Y") #tell R what the date structure is and read it as date and format the date to match Raven selection tables
      elp_rand$Date <- as.Date(elp_rand$Date,"%m/%d/%Y")
      elp_rand$"Rand"<-"rand" #create a column that is populated with "rand" to merge with the selection tables
    }

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
  #ele_merge_df <- rename(ele_merge_df, "Rumble Count" = "Count")
  if ("Count" %in% names(ele_merge_df) &&
      !("Rumble Count" %in% names(ele_merge_df))) {
    ele_merge_df <- dplyr::rename(ele_merge_df, "Rumble Count" = "Count")
  }
  ele_merge_df$Site <- substr(str_match(ele_merge_df$`Begin File`,"[a-zA-Z]{2}\\d{2}[a-zA-Z]{1}.")[,1],1,
                              nchar(str_match(ele_merge_df$`Begin File`,"[a-zA-Z]{2}\\d{2}[a-zA-Z]{1}.")[,1])-1)
  #stem(as.numeric(ele_merge_df$`Begin Hour`))
  #unique(ele_merge_df$`Begin Hour`)

  ele_dets_df <- ele_merge_df[,c("Site", "Begin File","File Offset (s)","Rumble Count","Deployment Number","Sound Problems", "Notes")]
  ele_dets_df$`File Start DateTime` <- as.POSIXct(str_extract(ele_dets_df$`Begin File`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
  ele_dets_df$`File Start Date` <- format(as.Date(str_extract(ele_dets_df$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
  ele_dets_df$`Detection DateTime` <- ele_dets_df$`File Start DateTime`+ ele_dets_df$`File Offset (s)`
  ele_dets_df$`Date` <- as.Date(ele_dets_df$`Detection DateTime`,"%Y%m%d",tz="Africa/Brazzaville")
  ele_dets_df$Hour <- hour(ele_dets_df$`Detection DateTime`)#format(as.POSIXct(ele_dets_df$`Detection DateTime`), format = "%H") # get hour of selection
  ele_dets_df$Detector <- detector_name
  ele_dets_df <- dplyr::rename(ele_dets_df, "File Name" = "Begin File")
  ele_dets_df$Month <- month(ele_dets_df$Date)
  ele_dets_df$Year <- year(ele_dets_df$Date)

  # View(ele_dets_df)
  # dim(ele_dets_df) #31854      13
  # stem(ele_dets_df$Hour)
  # unique(ele_dets_df$Hour)
  # names(ele_dets_df)
  print(paste("Minimum rumble selection table date:",min(ele_dets_df$`File Start DateTime`, na.rm = TRUE),sep=" "))
  print(paste("Maximum rumble selection table date:",max(ele_dets_df$`File Start DateTime`, na.rm = TRUE),sep=" "))
  print(paste("Total detected rumbles:",sum(ele_dets_df$`Rumble Count`, na.rm = TRUE),sep=" ")) # 2382 rumbles
  write.table(ele_dets_df,paste(output,"/",proj_dep_name,"_ele_SSTs_merged.txt",sep=""),sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)


  ##### Zero-Days Tables  #####
  # selection tables with 0 hori-harm detections at filtered score threshold on rand dates
    if(exists(zero_txt)){
      zero_days<-list.files(path = zero_txt,recursive=T,full.names=TRUE, pattern = "*.txt") # list all the selection tables in the ele_tables folder
      zero_days_list <- lapply(zero_days, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "")})  # Read the files in, assuming tab separator
      names(zero_days_list) <- basename(zero_days) # name the objects in list as the file names
      zero_days_df <- dplyr::bind_rows(zero_days_list) # merge all tables together
      zero_days_df$Rand <- ""
      #zero_days_df<- rename(zero_days_df, "Deployment Number" = "Deployment")
      #View(zero_days_df)
      #names(zero_days_df)

      zero_days_df2 <- zero_days_df[,c("Site", "Begin File","File Offset (s)","Count",
                                       "Deployment Number","Sound Problems", "Notes", "Rand")]
      zero_days_df2 <- dplyr::rename(zero_days_df2, "Rumble Count" = "Count")
      zero_days_df2 <- dplyr::rename(zero_days_df2, "File Name" = "Begin File")
      zero_days_df2$'Rumble Count' <- 0
      zero_days_df2$`File Start DateTime` <- as.POSIXct(str_extract(zero_days_df2$`File Name`,"\\d{8}.\\d{6}"),format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz="Africa/Brazzaville") # convert the data and time charaters to real date and time with the correct time zone
      zero_days_df2$`File Start Date` <- format(as.Date(str_extract(zero_days_df2$'File Name' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
      zero_days_df2$`Detection DateTime` <- zero_days_df2$`File Start DateTime`+ zero_days_df2$`File Offset (s)`
      zero_days_df2$`Date` <- as.Date(zero_days_df2$`Detection DateTime`,"%Y%m%d",tz="Africa/Brazzaville")
      zero_days_df2$`Hour` <- hour(zero_days_df2$`File Start DateTime`)
      zero_days_df2$Detector <- detector_name
    }
  #### Empty Tables ####
  # selection tables that had 0 hori-harm detections at the lowest score threshold

  # Site information
    if(exists("site_lat_long")){
      site_info_names <- names(site_lat_long)
    }

  #### FILTER BY ANALYSIS DATES ####

  #### gunshot selection tables ####
  # ideally make this usable for all types of selection tables and flexible


  #create table for every hour from the first to last in detector files
  # combing elephant rumbles and zero days
    if(exists(zero_txt)){
      ele_dets_df_zero <- full_join(ele_dets_df,zero_days_df2)
      print(paste("Total rumbles in merged table:",sum(ele_dets_df_zero$`Rumble Count`,na.rm=T), sep = " ")) # 2382
      print(paste("Min date in merged table:",min(ele_dets_df_zero$Date), sep = " "))
      print(paste("Max date in merged table:",max(ele_dets_df_zero$Date), sep = " "))
    } else {
      ele_dets_df_zero <- ele_dets_df
    }
  # stem(ele_dets_df_zero$Hour)
  # ele_dets_df_zero$Month <- month(ele_dets_df_zero$Date)
  # ele_dets_df_zero$Year <- year(ele_dets_df_zero$Date)


  # add sound check (if user requested)
    if(ele_bad_sound_remove == "y"| sound_check_include == "y"){
           ele_dets_df_zero <- full_join(ele_dets_df_zero,sound_check_merge_df2) # merge det df with sound check
           ele_dets_df_zero <- ele_dets_df_zero %>%
             filter(is.na(`Exclude (y/e)`) | !(`Exclude (y/e)` %in% c("y", "Y","e")))
          }
  # sum(ele_dets_df_zero$`Rumble Count`, na.rm = T)
  # unique(ele_dets_df_zero$`Exclude (y/e)`,na.rm=T)

  # add rand dates (if user requested) and remove detections on non rand dates if requested
  # Note: For Deployments 2-3, 3 random dates were selected to be consistent with the other deployments (dep 01 is missing)
    if(rand_dates_needed == "y"){
          #ele_dets_df_zero <- ele_dets_df_zero[,-14] # WHY IS THE  14TH COLUMN REMOVED HERE?
          ele_dets_df_zero <- left_join(ele_dets_df_zero,elp_rand, by = "Date") # combine rand dates with rumble table
          ele_dets_df_zero <- ele_dets_df_zero %>% filter(`Rand` == "rand")# remove nonRand dates
    }
    ele_dets_df_zero <- ele_dets_df_zero %>%
      mutate(
        `Rumble Count` = as.numeric(`Rumble Count`)
      )
  # unique(ele_dets_df_zero$Rand)
  # sum(ele_dets_df_zero$`Rumble Count`, na.rm = T)
  # names(ele_dets_df_zero)
  # zero_days_df2 %>% filter(Date == "2021-06-05", Site == "nn03g") %>%  nrow() > 0 # check for unintended dates


  #### HOURLY SUMMARIES ####
  # summarize rumble count to hourly
    ele_dets_df_zero_hr <- ele_dets_df_zero %>%
      # Group by these variables
      dplyr::group_by(Date, Hour, Site, `Deployment Number`,`File Name`) %>%
      # Summarize Rumble Count for each group
      dplyr::summarise(
        "Sum Rumbles" = if(all(is.na(`Rumble Count`))) {
          NA_real_  # If all values in group are NA, keep it NA
        } else {
          sum(`Rumble Count`, na.rm = TRUE)  # Otherwise sum non-NA values
        }
      ) %>%
      ungroup()
    print(paste("Total Rumbles in merged hourly table:",sum(ele_dets_df_zero_hr$`Sum Rumbles`, na.rm=T),sep = " ")) # 57339
    #dim(ele_dets_df_zero_hr) #45051      4
    min(ele_dets_df_zero_hr$Date)
    max(ele_dets_df_zero_hr$Date)


  ## hourly summary rumbles ##
  # get unique date/sites from detection df
    ele_date_site <- ele_dets_df_zero_hr[,c(-2,-4,-6)]
    ele_date_site <- ele_date_site %>%
      select(Site, Date, `File Name`) %>%
      distinct()
    #dim(ele_date_site)

  # Create a complete hour sequence for each surveyed Site-Date combination (so every hour sampled is represented, which they are missing from the SST)
    ele_date_site_hr <- ele_date_site %>%
      mutate(Hour = list(0:23)) %>%
      unnest(Hour)

  # merge the hourly dets df with the hourly df
    ele_hrly1 <- left_join(ele_date_site_hr,ele_dets_df_zero_hr, by = c("Site", "Date", "Hour","File Name"))
    ele_hrly1$`Sum Rumbles`[is.na(ele_hrly1$`Sum Rumbles`)] <- 0
    # ele_hrly1$Month <- month(ele_hrly1$Date)
    # ele_hrly1$Year <- year(ele_hrly1$Date)
    print(paste("Total Rumbles in merged hourly table: ",sum(ele_hrly1$`Sum Rumbles`, na.rm=T),sep = ""))
    #unique(ele_hrly$Hour)

  # merge with gunshots data (needs development)
  # merge with bad sound quality times (needs to be determined by noise levels) - should be incorporated in the sound check file instead!

  # add strata, lat, and long information from the sites txt file
    if(exists("site_lat_long")){
      ele_hrly <- full_join(ele_hrly1,site_lat_long)
    } else {
      ele_hrly <- ele_hrly1
    }
    ele_hrly$Month <- month(ele_hrly$Date)
    ele_hrly$Year <- year(ele_hrly$Date)
    ele_hrly$`Sum Rumbles`<- as.numeric(ele_hrly$`Sum Rumbles`)

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

  write.table(ele_hrly,paste(output,"/",proj_dep_name,"_Rumbles_ZeroDays_ExcludeBadSounds_Rand_hourly.txt",sep=""),
             sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

  nightRumbles <- sum(ele_hrly$`Sum Rumbles`[ele_hrly$DayNight == "Night"],na.rm=TRUE)  #38791
  dayRumbles <- sum(ele_hrly$`Sum Rumbles`[ele_hrly$DayNight == "Day"],na.rm=TRUE) #2246
  proportionNightRumbles <- nightRumbles/(nightRumbles+dayRumbles) #0.945
  print(paste("Proportion of night-time rumbles: ",round(proportionNightRumbles,digits=3),sep=" "))


  ## plot hourly data
  ##### total rumbles per date by site #####
  ele_hrly_Date_plot<- ggplot(as.data.frame(ele_hrly), aes(x = Date, y = `Sum Rumbles`)) +
                          geom_line(na.rm=FALSE) +
                          theme_bw() +
                          facet_wrap(~Site)+
                          labs(title="Total Rumbles per Hour")
  #print(ele_hrly_Date_plot)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hrly_plot.png",sep=""), ele_hrly_Date_plot, width = 10, height = 6, dpi = 300)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hrly_plot.eps", sep=""), ele_hrly_Date_plot, width = 10, height = 6, device = "eps")
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hrly_plot.png")),
              ele_hrly_nightday_plot,
              width = 10, height = 6, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hrly_plot.png")),
              ele_hrly_nightday_plot,
              width = 10, height = 6, device = "eps")

  ##### rumbles day vs night #####
  ele_hrly_nightday_plot <- ggplot(as.data.frame(ele_hrly), aes(x = DayNight, y = `Sum Rumbles`)) +
                                geom_boxplot() +
                                theme_bw()+
                            labs(title="Total Rumbles per Day and Night")
  # print(ele_hrly_nightday_plot)
  # Save plot
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hrly_nightday_plot.png",sep=""), ele_hrly_nightday_plot, width = 10, height = 6, dpi = 300)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hrly_nightday_plot.eps", sep=""), ele_hrly_nightday_plot, width = 10, height = 6, device = "eps")
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hrly_nightday_plot.png")),
              ele_hrly_nightday_plot,
              width = 10, height = 6, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hrly_nightday_plot.eps")),
              ele_hrly_nightday_plot,
              width = 10, height = 6, device = "eps")

  ##### rumbles per hour by site #####
  ele_hr_site_plot<- ggplot(as.data.frame(ele_hrly), aes(x = Hour, y = `Sum Rumbles`)) +
                    stat_summary(fun = mean, geom = "bar") +
                    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
                    theme_bw()+
                    facet_wrap(~Site)+
                    labs(title = "Mean (±SE) Rumbles per Hour")
  # print(ele_hr_site_plot)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hr_site_plot.png",sep=""), ele_hr_site_plot, width = 10, height = 6, dpi = 300)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hr_site_plot.eps",sep=""), ele_hr_site_plot, width = 10, height = 6, device = "eps")
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hr_site_plot.png")),
              ele_hr_site_plot,
              width = 10, height = 6, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hr_site_plot.eps")),
              ele_hr_site_plot,
              width = 10, height = 6, device = "eps")

  ##### rumbles per hour (improve axis) #####
  ele_hr_plot<- ggplot(as.data.frame(ele_hrly), aes(x = Hour, y = `Sum Rumbles`)) +
                    stat_summary(fun = mean, geom = "bar") +
                    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
                    theme_bw()+
                    labs(title = "Mean Rumbles Per Hour",
                         y = "Mean Rumbles (±SE)",
                         x = "Hour")
  # print(ele_hr_plot)
  # ggsave(paste(output,"/",proj_dep_name, "_ele_hr_plot.png",sep=""),
  #        ele_hr_plot,width = 10,height = 6,dpi = 300)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_hr_plot.eps",sep=""),
  #        ele_hr_plot,width = 10,height = 6,device = "eps")
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hr_plot.png")),
              ele_hr_plot,
              width = 10, height = 6, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_hr_plot.eps")),
              ele_hr_plot,
              width = 10, height = 6, device = "eps")

  # clear memory from the plots
  # dev.off()
  # gc()

  #### DAILY SUMMARIES ####
  # summarize daily rumbles from hourly table
  daily_site_summary <- ele_hrly %>%
    dplyr::group_by(Date, Site, WeekDate, across(all_of(site_info_names))) %>%
    dplyr::summarise(
      total_rumbles = if(sum(!is.na(`Sum Rumbles`)) == 0) NA_real_  # If all values are NA
      else sum(`Sum Rumbles`, na.rm = TRUE),
      n_hours = dplyr::n(),
      n_valid_hours = sum(!is.na(`Sum Rumbles`)),  # Count of non-NA hours
      night_total = if(sum(!is.na(`Sum Rumbles`[DayNight == "Night"])) == 0) NA_real_
      else sum(`Sum Rumbles`[DayNight == "Night"], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Calculate proportions after summarizing
    mutate(
      prop_night = night_total/total_rumbles,
      prop_day = (total_rumbles-night_total)/total_rumbles
    )

  daily_site_summary$Month <- month(daily_site_summary$Date)
  daily_site_summary$Year <- year(daily_site_summary$Date)
  daily_site_summary$Week <- week(daily_site_summary$Date)
  print(paste("Total rumbles per day for all dates:",sum(daily_site_summary$total_rumbles, na.rm=TRUE),sep=" ")) #36656

  write.table(daily_site_summary,
              paste(output,"/",proj_dep_name,"_Rumbles_perDay.txt",sep=""),
              sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)


  # Add all dates to the daily summary dataframe and fill them with NA so they have gaps in time in the plot
  # allows gaps in plots (if random dates enabled, then there will be many gaps)
  # create a complete sequence of dates
  date_range <- seq(min(daily_site_summary$Date),
                    max(daily_site_summary$Date),
                    by="day")
  # Create a data frame with all combinations of dates and strata
  complete_dates <- expand.grid(
    Date = date_range,
    Site = unique(daily_site_summary$Site))
  # Join this with your original data
  daily_site_summary_complete <- left_join(complete_dates, daily_site_summary)
  #names(daily_site_summary_complete)
  daily_site_summary_complete <- daily_site_summary_complete %>%
    mutate(
      total_rumbles = as.numeric(total_rumbles),
      night_total   = as.numeric(night_total)
    )

  ##### daily per site #####
  # Time series of total rumbles per site
  rumbles_daily_site_plot <- ggplot(as.data.frame(daily_site_summary_complete), aes(x = Date, y = total_rumbles)) +
    geom_line(na.rm=TRUE) +
    theme_bw() +
    facet_wrap(~Site) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")+
    theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour="white", size=0.5))+
    labs(title = "Total Elephant Rumbles per Date by Site",
         y = "Total Rumbles",
         x = "Date")
  #print(rumbles_daily_site_plot)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_site_plot.png",sep=""), rumbles_daily_site_plot, width = 10, height = 6, dpi = 300)
  # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_site_plot.eps",sep=""), rumbles_daily_site_plot, width = 10, height = 6, device = "eps")
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_site_plot.png")),
              rumbles_daily_site_plot,
              width = 10, height = 6, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_site_plot.eps")),
              rumbles_daily_site_plot,
              width = 10, height = 6, device = "eps")

  ##### daily per strata #####
  if("Strata" %in% names(daily_site_summary)){
    daily_strata_summary <- ele_hrly %>%
      group_by(Date,`WeekDate`,Strata) %>%
      dplyr::summarise(
        total_rumbles = sum(`Sum Rumbles`, na.rm = FALSE),
        n_hours = n(),
        night_total = sum(`Sum Rumbles`[DayNight == "Night"], na.rm = FALSE),
        prop_night = night_total/total_rumbles,
        prop_day = (total_rumbles-night_total)/total_rumbles
      ) %>%
      ungroup()
    print(paste("Total rumbles per day for all dates:",sum(daily_strata_summary$total_rumbles, na.rm=TRUE),sep=" ")) #36656
    complete_dates2 <- expand.grid(
      Date = date_range,
      Strata = unique(daily_strata_summary$Strata))
    # Join this with your original data
    daily_strata_summary_complete <- left_join(complete_dates2, daily_strata_summary)
    rumbles_daily_strata_plot <- ggplot(as.data.frame(daily_strata_summary_complete), aes(x = Date, y = total_rumbles, group = Strata)) +
      geom_line(na.rm=TRUE) +
      theme_bw() +
      facet_wrap(~Strata) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
      scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y")+
      theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(colour="white", size=0.5))+
      labs(title = "Total Rumbles per Date by Strata",
           y = "Total Rumbles",
           x = "Date")
    # print(rumbles_daily_strata_plot)
    # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_strata_plot.png",sep=""), rumbles_daily_strata_plot, width = 10, height = 6, dpi = 300)
    # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_strata_plot.eps",sep=""), rumbles_daily_strata_plot, width = 10, height = 6, device = "eps")
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_strata_plot.png")),
                rumbles_daily_strata_plot,
                width = 10, height = 6, dpi = 300)
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_strata_plot.eps")),
                rumbles_daily_strata_plot,
                width = 10, height = 6, device = "eps")

    # save table
    ele_daily_strata_table <-  daily_site_summary %>%
      dplyr::group_by(Strata) %>%
      dplyr::summarize(
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

  }


  ##### daily per vegetation class #####
  if("Vegetation Class" %in% names(daily_site_summary)){
    daily_veg_summary <- ele_hrly %>%
      dplyr::group_by(Date,`WeekDate`,`Vegetation Class`) %>%
      dplyr::summarise(
        total_rumbles = sum(`Sum Rumbles`, na.rm = FALSE),
        n_hours = n(),
        night_total = sum(`Sum Rumbles`[DayNight == "Night"], na.rm = FALSE),
        prop_night = night_total/total_rumbles,
        prop_day = (total_rumbles-night_total)/total_rumbles
      ) %>%
      ungroup()
    print(paste("Total rumbles per day for all dates:",sum(daily_veg_summary$total_rumbles, na.rm=TRUE),sep=" ")) #36656
    complete_dates3 <- expand.grid(
      Date = date_range,
      `Vegetation Class` = unique(daily_veg_summary$`Vegetation Class`)) %>%
      arrange(`Vegetation Class`, Date)
    # Join this with your original data
    daily_veg_summary_complete <- left_join(complete_dates3, daily_veg_summary)
    #print(paste("Total rumbles per day for all dates:",sum(daily_veg_summary_complete$total_rumbles, na.rm=TRUE),sep=" ")) #36656
    rumbles_daily_veg_plot <- ggplot(as.data.frame(daily_veg_summary_complete), aes(x = Date, y = total_rumbles, group = "Vegetation Class")) +
      geom_line(na.rm=TRUE) +
      theme_bw() +
      facet_wrap(~`Vegetation Class`) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
      scale_x_date(date_breaks = "4 month", date_labels = "%b-%Y")+
      theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(colour="white", size=0.5))+
      labs(title = "Total Rumbles Per Day by Vegetation Class",
           y = "Total Rumbles",
           x = "Date")
    # print(rumbles_daily_veg_plot)
    # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_vegetation_plot.png",sep=""), rumbles_daily_veg_plot, width = 10, height = 6, dpi = 300)
    # ggsave(paste(output,"/",proj_dep_name,"_ele_daily_vegetation_plot.eps",sep=""), rumbles_daily_veg_plot, width = 10, height = 6, device = "eps")
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_vegetation_plot.png")),
                rumbles_daily_veg_plot,
                width = 10, height = 6, dpi = 300)
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_ele_daily_vegetation_plot.eps")),
                rumbles_daily_veg_plot,
                width = 10, height = 6, device = "eps")

    write.table(daily_veg_summary_complete,
                paste(output,"/",proj_dep_name,"_Rumbles_perDay_perVegClass.txt",sep=""),
                sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)
    }

  # clear memory from the plots
  # dev.off()
  # gc()

  #### WEEKLY SUMMARIES ####
  ##### Weekly by site #####
  # summarize weekly presence
  rumble_weekly_means_site <- daily_site_summary %>%
    dplyr::group_by(Site,WeekDate,across(all_of(site_info_names))) %>%
    dplyr::summarise(
      MeanDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
      else mean(`total_rumbles`, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
      else sd(`total_rumbles`, na.rm=TRUE),
      n = dplyr::n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
      else max(`total_rumbles`, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
      else min(`total_rumbles`, na.rm=TRUE),
      sumRumbles = if(all(is.na(`total_rumbles`))) NA_real_
      else sum(`total_rumbles`, na.rm=TRUE),
      sumRumblesNight = if(all(is.na(`night_total`))) NA_real_
      else sum(`night_total`, na.rm=TRUE),
      meanRumblesNight = if(all(is.na(`night_total`))) NA_real_
      else mean(`night_total`, na.rm=TRUE),
      sdRumblesNight = if(all(is.na(`night_total`))) NA_real_
      else sd(`night_total`, na.rm=TRUE),
      seRumblesNight = sdRumblesNight/sqrt(n),
      PropNightRumbles = round(sumRumblesNight/sumRumbles, digit = 3),
      SumRumblesPerDaysSampled = round(sumRumbles/n,digit=3),
      SumNightRumblesPerDaysSampled = round(sumRumblesNight/n,digit=3)
    )
  #sum(rumble_weekly_means_site$sumRumbles, na.rm=T) #36236
  rumble_weekly_means_site$PropNightRumbles[is.nan(rumble_weekly_means_site$PropNightRumbles)] <- NA # remove NaN from proportion calc.
  #stem(rumble_weekly_means_site$PropNightRumbles) # look at the distribution of night rumble proportions

  write.table(rumble_weekly_means_site,paste(output,"/",proj_dep_name,
                                              "_Rumbles_Site_Weekly_Summaries.txt",sep=""),
              sep='\t',na="",col.names=TRUE,row.names=FALSE,
              quote=FALSE, append=FALSE)

  # Create a complete week timeseries and identify gaps in data so they are not plotted
  week_range <- seq(min(rumble_weekly_means_site$WeekDate), max(rumble_weekly_means_site$WeekDate), by="7 days")
  # Create a data frame with all combinations of dates and strata
  complete_weeks <- expand.grid(
    WeekDate = week_range,
    Site = unique(rumble_weekly_means_site$Site))
  # Join this with your original data
  rumble_weekly_means_site_complete <- left_join(complete_weeks, rumble_weekly_means_site, by = c("Site","WeekDate"))

  # plot the mean rumbles per day per week by site
  rumbleWklySite<-ggplot(as.data.frame(rumble_weekly_means_site_complete), aes(x=as.Date(WeekDate),y=MeanDailyRumbles))+
    geom_line(na.rm = TRUE)+
    geom_point()+
    geom_errorbar(aes(ymin = MeanDailyRumbles - seDailyRumbles,
                      ymax = MeanDailyRumbles + seDailyRumbles),
                  width = 0.2)+
    #stat_summary(fun.data=mean_cl_normal)+
    geom_smooth(method='lm', se = TRUE, size = 1.2, linetype = "dashed", aes(color = Strata))+
    theme_bw() +
    facet_wrap(~Site)+
    scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y") +
    scale_y_continuous(
      limits = c(0, max(rumble_weekly_means_site_complete$MeanDailyRumbles +
                          rumble_weekly_means_site_complete$seDailyRumbles, na.rm = TRUE)),
      breaks = seq(0, max(rumble_weekly_means_site_complete$MeanDailyRumbles +
                            rumble_weekly_means_site_complete$seDailyRumbles, na.rm = TRUE),
                   by = 50),
      minor_breaks = NULL,
      expand = c(0,0))+  # This removes the space below 0
    theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour="white", size=0.5))+
    labs(title = "Mean Daily Elephant Rumbles per Week by Site",
         x = "Week (month-year)",
         y = "Mean Daily Rumbles per Week (±SE)")
  # print(rumbleWklySite)
  # ggsave(plot = rumbleWklySite,paste(output,"/",proj_dep_name,"_RumbleWeeklySite_plot.png",sep=""),width =10, height =10)
  # ggsave(plot = rumbleWklySite,paste(output,"/",proj_dep_name,"_RumbleWeeklySite_plot.eps",sep=""),width =10, height =10)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklySite_plot.png")),
              rumbleWklySite,
              width = 10, height = 10, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklySite_plot.eps")),
              rumbleWklySite,
              width = 10, height = 10, device = "eps")


  ##### weekly per strata #####
  # DOES NOT ALLOW FOR GAPS IN THE DATA
  if("Strata" %in% names(daily_site_summary)){
    # group by strata instead of site and get means per week
    rumble_weekly_means_strata <- daily_site_summary %>%
      dplyr::group_by(Strata,WeekDate) %>%
      dplyr::summarise(
        MeanDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else mean(`total_rumbles`, na.rm=TRUE),
        sdDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else sd(`total_rumbles`, na.rm=TRUE),
        n = dplyr::n(),
        seDailyRumbles = sdDailyRumbles/sqrt(n),
        maxDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else max(`total_rumbles`, na.rm=TRUE),
        minDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else min(`total_rumbles`, na.rm=TRUE),
        sumRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else sum(`total_rumbles`, na.rm=TRUE),
        sumRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else sum(`night_total`, na.rm=TRUE),
        meanRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else mean(`night_total`, na.rm=TRUE),
        sdRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else sd(`night_total`, na.rm=TRUE),
        seRumblesNight = sdRumblesNight/sqrt(n),
        PropNightRumbles = round(sumRumblesNight/sumRumbles, digit = 3),
        SumRumblesPerDaysSampled = round(sumRumbles/n,digit=3),
        SumNightRumblesPerDaysSampled = round(sumRumblesNight/n,digit=3)
      )
    # combine these data with the complete list of weeks to allow for gaps in plot
    complete_weeks_strat <- expand.grid(
      WeekDate = week_range,
      Strata = unique(rumble_weekly_means_strata$Strata))
    rumble_weekly_means_strata_complete <- left_join(complete_weeks_strat, rumble_weekly_means_strata, by = c("Strata","WeekDate"))
    #save table
    #View(rumble_weekly_means_strata_complete)
    write.table(rumble_weekly_means_strata_complete,paste(proj_dep_name,
                                                          "Rumbles_Strata_Weekly_Summaries.txt",sep="_"),
                sep='\t',na="",col.names=TRUE,row.names=FALSE,
                quote=FALSE, append=FALSE)

    # plot
      rumbleWklyStrata<-ggplot(as.data.frame(rumble_weekly_means_strata_complete), aes(x=as.Date(WeekDate),y=MeanDailyRumbles))+
              geom_line(na.rm = TRUE)+
              geom_point()+
              geom_errorbar(aes(ymin = MeanDailyRumbles - seDailyRumbles,
                                ymax = MeanDailyRumbles + seDailyRumbles),
                            width = 0.2)+
              #stat_summary(fun.data=mean_cl_normal)+
              geom_smooth(method='lm', se = TRUE, size = 1.2, linetype = "dashed", aes(color = Strata))+
              theme_bw() +
              facet_wrap(~Strata)+
              scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y") +
              scale_y_continuous(
                #name = "Mean Daily Rumbles per Week",
                limits = c(0, max(rumble_weekly_means_strata_complete$MeanDailyRumbles +
                                    max(rumble_weekly_means_strata_complete$seDailyRumbles, na.rm = TRUE), na.rm = TRUE)+5),
                breaks = seq(0, max(rumble_weekly_means_strata_complete$MeanDailyRumbles +
                                      rumble_weekly_means_strata_complete$seDailyRumbles, na.rm = TRUE)+5,
                             by = 10),
                minor_breaks = NULL,
                expand = c(0,0))+  # This removes the space below 0
              theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_line(colour="white", size=0.5))+
              labs(title = "Mean Daily Elephant Rumbles per Week by Strata",
                   x = "Week (month-year)",
                   y = "Mean Daily Rumbles per Week (±SE)")
       # print(rumbleWklyStrata)
       # ggsave(plot = rumbleWklyStrata,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_plot.png",sep=""),width =10, height =10)
       # ggsave(plot = rumbleWklyStrata,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_plot.eps",sep=""),width =10, height =10)
       safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyStrata_plot.png")),
                   rumbleWklyStrata,
                   width = 10, height = 10, dpi = 300)
       safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyStrata_plot.eps")),
                   rumbleWklyStrata,
                   width = 10, height = 10, device = "eps")

    # plot weekly mean rumbles with secondary weekly night mean rumbles axis
     rumbleWklyStrata_WithNight <- ggplot(as.data.frame(rumble_weekly_means_strata_complete)) +
       # Primary axis elements
       geom_line(aes(x = WeekDate, y = MeanDailyRumbles)) +
       geom_point(aes(x = WeekDate, y = MeanDailyRumbles)) +
       geom_errorbar(aes(x = WeekDate,
                         ymin = MeanDailyRumbles - seDailyRumbles,
                         ymax = MeanDailyRumbles + seDailyRumbles),
                     width = 0.2) +
       geom_smooth(aes(x = WeekDate, y = MeanDailyRumbles, color = Strata),
                   method = 'lm', se = TRUE, size = 1.2, linetype = "dashed") +
       # Secondary axis elements for night rumbles
       geom_line(aes(x = WeekDate, y = meanRumblesNight),
                 color = "red", linetype = "dotted") +
       geom_point(aes(x = WeekDate, y = meanRumblesNight),
                  color = "red") +
       # Facets and themes
       facet_wrap(~Strata) +
       theme_bw() +
       # Axis formatting
       scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y") +
       scale_y_continuous(
         name = "Mean Daily Rumbles per Week (±SE)",
         limits = c(0, max(rumble_weekly_means_strata_complete$MeanDailyRumbles +
                             rumble_weekly_means_strata_complete$seDailyRumbles, na.rm = TRUE)+5),
         breaks = seq(0, max(rumble_weekly_means_strata_complete$MeanDailyRumbles +
                               rumble_weekly_means_strata_complete$seDailyRumbles, na.rm = TRUE)+5,
                      by = 10),  # Adjust this value to change break frequency
         expand = c(0,0),
         sec.axis = sec_axis(~.,
                             name = "Mean Night Rumbles per Week (±SE)",
                             breaks = seq(0, max(rumble_weekly_means_strata_complete$MeanDailyRumbles +
                                                   rumble_weekly_means_strata_complete$seDailyRumbles, na.rm = TRUE)+5,
                                          by = 10))  # Adjust this value to match primary axis
       ) +
       # Theme elements
       theme(axis.text.x = element_text(angle = 60, hjust = 1),
             plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_line(colour = "gray90"),  # Lighter grid lines
             panel.grid.minor = element_blank(),
             axis.title.y.right = element_text(color = "red"),
             axis.text.y.right = element_text(color = "red")) +
       # Labels
       labs(title = "Mean Daily Elephant Rumbles per Week by Strata",
            x = "Week (month-year)",
            color = "Strata")
     # print(rumbleWklyStrata_WithNight)
     # ggsave(plot = rumbleWklyStrata_WithNight,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_withNight_plot.png",sep=""),width =10, height =10)
     # ggsave(plot = rumbleWklyStrata_WithNight,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_withNight_plot.eps",sep=""),width =10, height =10)
     safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyStrata_withNight_plot.png")),
                 rumbleWklyStrata_WithNight,
                 width = 10, height = 10, dpi = 300)
     safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyStrata_withNight_plot.eps")),
                 rumbleWklyStrata_WithNight,
                 width = 10, height = 10, device = "eps")
  }

  ##### weekly per Vegetation Class #####
  # WORKING ON THIS - Mean daily (per site) rather than weekly across all sites is better
  # zero introduced on non-sampled dates?
  if("Vegetation Class" %in% names(daily_site_summary)){
    # group by strata instead of site and get means per week
    rumble_weekly_means_veg <- daily_site_summary %>%
      dplyr::group_by(`Vegetation Class`,WeekDate) %>%
      dplyr::summarise(
        MeanDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else mean(`total_rumbles`, na.rm=TRUE),
        sdDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else sd(`total_rumbles`, na.rm=TRUE),
        n = dplyr::n(),
        seDailyRumbles = sdDailyRumbles/sqrt(n),
        maxDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else max(`total_rumbles`, na.rm=TRUE),
        minDailyRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else min(`total_rumbles`, na.rm=TRUE),
        sumRumbles = if(all(is.na(`total_rumbles`))) NA_real_
        else sum(`total_rumbles`, na.rm=TRUE),
        sumRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else sum(`night_total`, na.rm=TRUE),
        meanRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else mean(`night_total`, na.rm=TRUE),
        sdRumblesNight = if(all(is.na(`night_total`))) NA_real_
        else sd(`night_total`, na.rm=TRUE),
        seRumblesNight = sdRumblesNight/sqrt(n),
        PropNightRumbles = round(sumRumblesNight/sumRumbles, digit = 3),
        SumRumblesPerDaysSampled = round(sumRumbles/n,digit=3),
        SumNightRumblesPerDaysSampled = round(sumRumblesNight/n,digit=3)
      )
    # combine with complete weeks
    complete_weeks_veg <- expand.grid(
      WeekDate = week_range,
      `Vegetation Class` = unique(rumble_weekly_means_veg$`Vegetation Class`))
    rumble_weekly_means_veg_complete <- left_join(complete_weeks_veg, rumble_weekly_means_veg, by = c("Vegetation Class","WeekDate"))

    # plot
    rumbleWklyVeg<-ggplot(as.data.frame(rumble_weekly_means_veg_complete), aes(x=as.Date(WeekDate),y=MeanDailyRumbles))+
      geom_line(na.rm = TRUE)+
      geom_point()+
      geom_errorbar(aes(ymin = MeanDailyRumbles - seDailyRumbles,
                        ymax = MeanDailyRumbles + seDailyRumbles),
                    width = 0.2)+
      #stat_summary(fun.data=mean_cl_normal)+
      geom_smooth(method='lm', se = TRUE, size = 1.2, linetype = "dashed", aes(color = `Vegetation Class`))+
      theme_bw() +
      facet_wrap(~`Vegetation Class`)+
      scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y") +
      scale_y_continuous(
        #name = "Mean Daily Rumbles per Week",
        limits = c(0, max(rumble_weekly_means_veg_complete$MeanDailyRumbles +
                            max(rumble_weekly_means_veg_complete$seDailyRumbles, na.rm = TRUE), na.rm = TRUE)+5),
        breaks = seq(0, max(rumble_weekly_means_veg_complete$MeanDailyRumbles +
                              rumble_weekly_means_veg_complete$seDailyRumbles, na.rm = TRUE)+5,
                     by = 50),
        minor_breaks = NULL,
        expand = c(0,0))+  # This removes the space below 0
      theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(colour="white", size=0.5))+
      labs(title = "Mean Daily Elephant Rumbles per Week by Vegetation Class",
           x = "Week (month-year)",
           y = "Mean Daily Rumbles per Week (±SE)")
    #add the sample size to the plot
    # print(rumbleWklyVeg)
    # ggsave(plot = rumbleWklyVeg,paste(output,"/",proj_dep_name,"_RumbleWeeklyVeg_plot.png",sep=""),width =10, height =10)
    # ggsave(plot = rumbleWklyVeg,paste(output,"/",proj_dep_name,"_RumbleWeeklyVeg_plot.eps",sep=""),width =10, height =10)
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyVeg_plot.png")),
                rumbleWklyVeg,
                width = 10, height = 10, dpi = 300)
    safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleWeeklyVeg_plot.eps")),
                rumbleWklyVeg,
                width = 10, height = 10, device = "eps")

    write.table(rumble_weekly_means_veg_complete,paste(proj_dep_name,
                                              "Mean_Daily_Rumbles_VegClass_Weekly_Summaries.txt",sep="_"),
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

  # clear memory from the plots
  # dev.off()
  # gc()

  #### MONTHLY SUMMARIES ####
   # add number of days sampled per month
   # add number of rumbles per sampled date value
   # plot proportion of night rumbles per month


  ##### mean daily per month by site #####
  rumble_monthly_means_site <- daily_site_summary %>%
    dplyr::group_by(Year,Month,Site) %>%
    dplyr::summarise(
      MeanDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else mean(total_rumbles, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else sd(total_rumbles, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else max(total_rumbles, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else min(total_rumbles, na.rm=TRUE),
      sumRumbles = if(all(is.na(total_rumbles))) NA_real_
      else sum(total_rumbles, na.rm=TRUE),
      sumNightRumbles = if(all(is.na(night_total))) NA_real_
      else sum(night_total,na.rm=TRUE),
      meanNightRumbles = if(all(is.na(night_total))) NA_real_
      else mean(night_total,na.rm=TRUE),
      proportionNightRumbles = sumNightRumbles/sumRumbles
    )
  sum(rumble_monthly_means_site$sumRumbles, na.rm=T) #36236
  rumble_monthly_means_site$`Year-Month` <- as.Date(paste(rumble_monthly_means_site$Year, rumble_monthly_means_site$Month, "01", sep = "-"))
  # create a grouping variable for consecutive months
  rumble_monthly_means_site <- rumble_monthly_means_site %>%
    arrange(Site, `Year-Month`) %>%
    dplyr::group_by(Site) %>%
    mutate(month_group = cumsum(c(TRUE, diff(as.numeric(`Year-Month`)) > 31))) %>%
    ungroup()
  write.table(rumble_monthly_means_site,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Site.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

  # plot monthly means by site
  rumble_monthly_means_site_plot <- ggplot(as.data.frame(rumble_monthly_means_site),aes(x=`Year-Month`,y=MeanDailyRumbles))+
    geom_line(aes(group = month_group))+
    #geom_point()+
    stat_summary(fun.data=mean_cl_normal)+
    facet_wrap(~Site)+
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b-%Y")+
    scale_y_continuous(trans = "pseudo_log")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_blank())+
    labs(title = "Average Number of Rumbles per Day per Month by Site",
         x = "Date (Month-Year)",
         y = "Log-Transformed Average Number of Rumbles per Day (+/- SE)")
  # print(rumble_monthly_means_site_plot)
  # ggsave(plot = rumble_monthly_means_site_plot,
  #        paste(output,"/",proj_dep_name,"_RumbleDailySite_perMonth_plot.png",sep=""),
  #        width =10, height =10)
  # ggsave(plot = rumble_monthly_means_site_plot,
  #        paste(output,"/",proj_dep_name,"_RumbleDailySite_perMonth_plot.eps",sep=""),
  #        width =10, height =10)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailySite_perMonth_plot.png")),
              rumble_monthly_means_site_plot,
              width = 10, height = 10, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailySite_perMonth_plot.eps")),
              rumble_monthly_means_site_plot,
              width = 10, height = 10, device = "eps")

  ##### average rumbles per day by month #####
  rumble_monthly_means <- daily_site_summary %>%
    dplyr::group_by(Year,Month) %>%
    dplyr::summarise(
      MeanDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else mean(total_rumbles, na.rm=TRUE),
      sdDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else sd(total_rumbles, na.rm=TRUE),
      n = n(),
      seDailyRumbles = sdDailyRumbles/sqrt(n),
      maxDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else max(total_rumbles, na.rm=TRUE),
      minDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
      else min(total_rumbles, na.rm=TRUE),
      sumRumbles = if(all(is.na(total_rumbles))) NA_real_
      else sum(total_rumbles, na.rm=TRUE),
      sumNightRumbles = if(all(is.na(night_total))) NA_real_
      else sum(night_total,na.rm=TRUE),
      meanNightRumbles = if(all(is.na(night_total))) NA_real_
      else mean(night_total,na.rm=TRUE),
      proportionNightRumbles = sumNightRumbles/sumRumbles
    )
  sum(rumble_monthly_means$sumRumbles, na.rm=T) #36236
  rumble_monthly_means$`Year-Month` <- as.Date(paste(rumble_monthly_means$Year, rumble_monthly_means$Month, "01", sep = "-"))
  write.table(rumble_monthly_means,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)

  # Create complete time series
  all_dates <- seq.Date(
    from = min(rumble_monthly_means$`Year-Month`),
    to = max(rumble_monthly_means$`Year-Month`),
    by = "1 month"
  )
  # Create complete dataset with all months
  monthly_means_complete <- tibble(`Year-Month` = all_dates) %>%
    left_join(rumble_monthly_means, by = "Year-Month")
  # add days since start to table for lm
  monthly_means_complete <- monthly_means_complete %>%
    mutate(days_since_start = as.numeric(`Year-Month` - min(`Year-Month`)))
  # plot monthly means
  rumble_monthly_means_plot <- ggplot() +
    geom_errorbar(data = monthly_means_complete,
                  aes(x = `Year-Month`,
                      ymin = MeanDailyRumbles - seDailyRumbles,
                      ymax = MeanDailyRumbles + seDailyRumbles),
                  width = 10,
                  size = 0.5,
                  alpha = 0.7) +
    geom_line(data = monthly_means_complete,
              aes(x = `Year-Month`, y = MeanDailyRumbles),
              alpha = 0.5) +
    geom_point(data = monthly_means_complete,
               aes(x = `Year-Month`, y = MeanDailyRumbles),
               color = "blue",
               size = 2) +
    geom_smooth(data = monthly_means_complete,
                aes(x = `Year-Month`, y = MeanDailyRumbles),
                method = "lm",
                se = TRUE,
                color = "blue") +
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b-%Y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour = "gray90"),
          panel.grid.minor = element_blank()) +
    labs(title = "Average Daily Rumbles per Month",
         subtitle = "Blue points show monthly averages (± SE)",
         x = "Month-Year",
         y = "Average Daily Rumbles (± SE)")
  # print(rumble_monthly_means_plot)
  # ggsave(plot = rumble_monthly_means_plot,paste(output,"/",proj_dep_name,"_RumbleDailyMean_perMonth_plot.png",sep=""),width =10, height =10)
  # ggsave(plot = rumble_monthly_means_plot,paste(output,"/",proj_dep_name,"_RumbleDailyMean_perMonth_plot.eps",sep=""),width =10, height =10)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyMean_perMonth_plot.png")),
              rumble_monthly_means_plot,
              width = 10, height = 10, dpi = 300)
  safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyMean_perMonth_plot.eps")),
              rumble_monthly_means_plot,
              width = 10, height = 10, device = "eps")


  # Fit the linear model for monthly means
  lm_fit <- lm(MeanDailyRumbles ~ days_since_start, data = monthly_means_complete)
  print(cat("Monthly Means Linear Regression Summary:\n"))
  print(summary(lm_fit))
  stargazer(lm_fit,
            type = "text",
            title = "Monthly Means Linear Regression Summary",
            covariate.labels = "Time",
            report = "vc*spt",
            out = paste(output,"/",proj_dep_name,"_monthly_means_regression_summary.txt",sep=""))



  ##### mean daily per month by stratum #####
  if("Strata" %in% names(daily_site_summary)){
   rumble_monthly_means_stratum <- daily_site_summary %>%
     dplyr::group_by(Year,Month,Strata) %>%
     dplyr::summarise(
       MeanDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
       else mean(total_rumbles, na.rm=TRUE),
       sdDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
       else sd(total_rumbles, na.rm=TRUE),
       n = n(),
       seDailyRumbles = sdDailyRumbles/sqrt(n),
       maxDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
       else max(total_rumbles, na.rm=TRUE),
       minDailyRumbles = if(all(is.na(total_rumbles))) NA_real_
       else min(total_rumbles, na.rm=TRUE),
       sumRumbles = if(all(is.na(total_rumbles))) NA_real_
       else sum(total_rumbles, na.rm=TRUE),
       sumNightRumbles = if(all(is.na(night_total))) NA_real_
       else sum(night_total,na.rm=TRUE),
       meanNightRumbles = if(all(is.na(night_total))) NA_real_
       else mean(night_total,na.rm=TRUE),
       proportionNightRumbles = sumNightRumbles/sumRumbles
     )
   sum(rumble_monthly_means_stratum$sumRumbles, na.rm=T) #36236
   rumble_monthly_means_stratum$`Year-Month` <- as.Date(paste(rumble_monthly_means_stratum$Year, rumble_monthly_means_stratum$Month, "01", sep = "-"))
   write.table(rumble_monthly_means_stratum,"PNNN_Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Stratum.txt",sep='\t',na="",col.names=TRUE,row.names=FALSE, quote=FALSE, append=FALSE)
  }

   #plot stratum separate
  # if("Strata" %in% names(rumble_monthly_means_stratum)){
  #    rumble_monthly_means_stratum_plot <- ggplot(rumble_monthly_means_stratum, aes(x=`Year-Month`,y=MeanDailyRumbles))+
  #                    geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles, ymax=MeanDailyRumbles+seDailyRumbles), width=.1) +
  #                    geom_line(color =  "dark blue")+
  #                    geom_point()+
  #                    stat_summary(fun.data=mean_cl_normal)+
  #                    geom_smooth(method='lm')+
  #                    facet_wrap(~Strata)+
  #                    scale_x_date(date_breaks = "6 months",
  #                                 date_labels = "%b-%Y")+
  #                    scale_y_continuous()+
  #                    theme(axis.text.x = element_text(angle = 60, hjust = 1),
  #                          plot.title = element_text(hjust = 0.5),
  #                          panel.grid.major = element_line(colour = "white"),  # Lighter grid lines
  #                          panel.grid.minor = element_blank())+
  #                    labs(title = "Average Number of Rumbles per Week per Month by Stratum",
  #                         x = "Date (Month-Year)",
  #                         y = "Average Number of Rumbles per Week (+/- SE)")
  #   print(rumble_monthly_means_stratum_plot)
  #   ggsave(plot = rumble_monthly_means_stratum_plot,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_perMonth_plot.png",sep=""),width =10, height =10)
  #   ggsave(plot = rumble_monthly_means_stratum_plot,paste(output,"/",proj_dep_name,"_RumbleWeeklyStrata_perMonth_plot.eps",sep=""),width =10, height =10)
  # }

  # plot monthly means per stratum
   if("Strata" %in% names(daily_site_summary)){
     # First create a grouping variable for consecutive months
     rumble_monthly_means_stratum <- rumble_monthly_means_stratum %>%
       arrange(Strata, `Year-Month`) %>%
       group_by(Strata) %>%
       mutate(month_group = cumsum(c(TRUE, diff(as.numeric(`Year-Month`)) > 31))) %>%
       ungroup()

     rumble_monthly_means_stratum_plot <- ggplot(as.data.frame(rumble_monthly_means_stratum),
                                                 aes(x=`Year-Month`,y=MeanDailyRumbles))+
       geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles,
                         ymax=MeanDailyRumbles+seDailyRumbles),
                     width=.1) +
       # Modified geom_line to use the grouping variable
       geom_line(aes(group = month_group), color = "dark blue")+
       geom_point()+
       stat_summary(fun.data=mean_cl_normal)+
       geom_smooth(method='lm', aes(color = Strata)) +
       scale_color_viridis_d()+
       facet_wrap(~Strata)+
       scale_x_date(date_breaks = "6 months",
                    date_labels = "%b-%Y")+
       scale_y_continuous()+
       theme(axis.text.x = element_text(angle = 60, hjust = 1),
             plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_line(colour = "white"),
             panel.grid.minor = element_blank())+
       labs(title = "Average Number of Rumbles per Day per Month by Stratum",
            x = "Date (Month-Year)",
            y = "Average Number of Rumbles per Day (+/- SE)")
     # print(rumble_monthly_means_stratum_plot)
     # ggsave(plot = rumble_monthly_means_stratum_plot,
     #        paste(output,"/",proj_dep_name,"_RumbleDailyStrata_perMonth_plot.png",sep=""),
     #        width =10, height =10)
     # ggsave(plot = rumble_monthly_means_stratum_plot,
     #        paste(output,"/",proj_dep_name,"_RumbleDailyStrata_perMonth_plot.eps",sep=""),
     #        width =10, height =10)
     safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyStrata_perMonth_plot.png")),
                 rumble_monthly_means_stratum_plot,
                 width = 10, height = 10, dpi = 300)
     safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyStrata_perMonth_plot.eps")),
                 rumble_monthly_means_stratum_plot,
                 width = 10, height = 10, device = "eps")
   }

  # plot monthly means with stratum together (note that month 38 has 0 detections. Check sound check for this month)
    if("Strata" %in% names(daily_site_summary)){
        # Create complete time series
        all_dates <- seq.Date(
          from = min(rumble_monthly_means_stratum$`Year-Month`),
          to = max(rumble_monthly_means_stratum$`Year-Month`),
          by = "month"
        )
        all_strata <- unique(rumble_monthly_means_stratum$Strata)
        complete_grid <- expand.grid(
          `Year-Month` = all_dates,
          Strata = all_strata
        ) %>% as_tibble()
        rumble_monthly_complete <- left_join(
          complete_grid,
          rumble_monthly_means_stratum,
          by = c("Year-Month", "Strata")
        )

        # Create the plot with complete data
        rumble_monthly_means_stratum_plot_overlay <- ggplot(as.data.frame(rumble_monthly_complete),
                                                            aes(x=`Year-Month`, y=MeanDailyRumbles, color=Strata))+
          geom_errorbar(aes(ymin=MeanDailyRumbles-seDailyRumbles,
                            ymax=MeanDailyRumbles+seDailyRumbles),
                        width=.1, alpha=0.5) +
          geom_line() +
          geom_point()+
          stat_summary(fun.data=mean_cl_normal)+
          geom_smooth(method='lm', aes(fill = Strata), alpha = 0.2) +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          scale_x_date(date_breaks = "6 months",
                       date_labels = "%b-%Y")+
          scale_y_continuous()+
          theme_bw() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_line(colour = "gray90"),
                panel.grid.minor = element_blank(),
                legend.position = "top")+
          labs(title = "Average Number of Rumbles per Day per Month by Stratum",
               x = "Date (Month-Year)",
               y = "Average Number of Rumbles per Day (+/- SE)")
        # print(rumble_monthly_means_stratum_plot_overlay)
        # ggsave(plot = rumble_monthly_means_stratum_plot_overlay,
        #        paste(output,"/",proj_dep_name,"_RumbleDailyStrata_perMonth_plot_overlay.png",sep=""),
        #        width =10, height =7)
        # ggsave(plot = rumble_monthly_means_stratum_plot_overlay,
        #        paste(output,"/",proj_dep_name,"_RumbleDailyStrata_perMonth_plot_overlay.eps",sep=""),
        #        width =10, height =7)
        safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyStrata_perMonth_plot_overlay.png")),
                    rumble_monthly_means_stratum_plot_overlay,
                    width = 10, height = 10, dpi = 300)
        safe_ggsave(file.path(output, paste0(proj_dep_name, "_RumbleDailyStrata_perMonth_plot_overlay.eps")),
                    rumble_monthly_means_stratum_plot_overlay,
                    width = 10, height = 10, device = "eps")
    }

   # Regression model monthly means per strata
   if("Strata" %in% names(daily_site_summary)){
        # convert dates to a numeric value (days since start)
        rumble_monthly_complete <- rumble_monthly_complete %>%
          mutate(days_since_start = as.numeric(`Year-Month` - min(`Year-Month`)))
        # Create models using the same date format as ggplot
        strata_models <- rumble_monthly_complete %>%
          group_by(Strata) %>%
          nest() %>%
          mutate(
            model = map(data, ~lm(MeanDailyRumbles ~ days_since_start, data = .x))
          )
        model_list <- strata_models$model
        names(model_list) <- strata_models$Strata
        stargazer(model_list,
                  type = "text",
                  title = "Regression Results by Stratum",
                  column.labels = strata_models$Strata,
                  intercept.bottom = FALSE,
                  report = "vc*spt",
                  out = paste0(output,"/",proj_dep_name,"_stratum_regression_results.txt"))
    }

  # # check lm output (compare)
  #  # Open a connection to a text file
  #  sink(paste0(output,"/",proj_dep_name,"_monthly_stratum_lm_results.txt"))
  #  # For each stratum
  #  for(strata in unique(rumble_monthly_complete$Strata)) {
  #    # Subset data for this stratum
  #    strata_data <- rumble_monthly_complete %>%
  #      filter(Strata == strata)
  #    # Fit model
  #    model <- lm(MeanDailyRumbles ~ days_since_start, data = strata_data)
  #    # Print results
  #    cat("\n\nResults for stratum:", strata, "\n")
  #    print(summary(model))
  #  }
  #  # Close the connection
  #  sink()


  # # check lm output (compare)
  #  # Open a connection to a text file
  #  sink(paste0(output,"/",proj_dep_name,"_monthly_lm_results.txt"))
  #  # For each stratum
  #  for(strata in unique(rumble_monthly_complete$Strata)) {
  #    # Subset data for this stratum
  #    strata_data <- rumble_monthly_complete %>%
  #      filter(Strata == strata)
  #    # Fit model
  #    model <- lm(MeanDailyRumbles ~ days_since_start, data = strata_data)
  #    # Print results
  #    cat("\n\nResults for stratum:", strata, "\n")
  #    print(summary(model))
  #  }
  #  # Close the connection
  #  sink()

  # # lm model
  # library(broom)
  #
  # lm_rumbles <-
  #   rumble_monthly_means %>%
  #   mutate(model = map(data, ~lm(MeanDailyRumbles~`Year-Month`,na.action = na.exclude, data = .)), tidied = map(model, tidy)) %>%
  #   unnest(tidied)
  #
  # plot(rumble_monthly_means_stratum$MeanDailyRumbles~rumble_monthly_means_stratum$`Year-Month`,
  #      col=c("red", "green","blue")[factor(rumble_monthly_means_stratum$Strata)])
  # abline(lm_rumbles_stratum)


  # clear memory from the plots
  # dev.off()
  # gc()


  #### SITE SUMMARIES (MAP) ####
  # indevelopment
    # if(all(c("Latitude", "Longitude") %in% colnames(site_lat_long))) { # if Lat and Lon exits in the sites_lat_long df, then map the data
    #   # df <- read.table(file = "L:/ELP/Projects/Other/Ghana/kakum_Analysis/base_tables/kk_2024/merged_data/Kakum_dep02_HHv6Rumbles_ZeroDays_ExcludeBadSounds_Rand_hourly.txt", header = T, sep ="\t", check.names=FALSE,quote = "\"",comment.char = "", fill=T)
    #   # names(df)
    #   # head(df)
    #
    #   ele_site <- ele_hrly %>%
    #     group_by(Site) %>%
    #     summarize(
    #       Latitude = round(first(Latitude),5),
    #       Longitude = round(first(Longitude),5),
    #       "Sum Rumbles" = sum(`Sum Rumbles`, na.rm = TRUE)
    #       )
    #
    #   #map the data
    #   # if latitude and longitude are present, the map the data
    #   install.packages("leaflet")
    #   library(leaflet)
    #   install.packages(c("leaflet.providers", "sf"))
    #   library(leaflet.providers)
    #
    #   # Basic map with multiple layer options
    #   leaflet(ele_site) %>%
    #     # Add layer control to toggle between base maps
    #     addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    #     addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
    #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    #
    #   # Create an interactive map
    #   leaflet(df2) %>%
    #     addTiles() %>%  # Add default OpenStreetMap tiles
    #     addCircleMarkers(
    #       lng = ~Longitude,
    #       lat = ~Latitude,
    #       radius = ~sqrt(`Sum Rumbles`) * 3,  # Adjust the multiplier to change circle sizes
    #       popup = ~paste("Site:", Site, "<br>Sum Rumbles:", `Sum Rumbles`),
    #       color = "red",
    #       fillOpacity = 0.7,
    #       group = "Sites"
    #     ) %>%
    #
    #     # Set the view to center on your study area
    #     setView(lng = 1.32887, lat = 5.62806, zoom = 10)  # Adjust zoom level as needed
    #
    #     # Add layer control
    #     addLayersControl(
    #       baseGroups = c("OpenStreetMap", "Satellite", "Topo"), # "Satellite"
    #       overlayGroups = c("Sites"),
    #       options = layersControlOptions(collapsed = FALSE)
    #     )
    # }
  # clear memory from the plots
}
