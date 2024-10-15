# This script finds the site name, date and time based on ELP PNNN selection table and sound file naming convention


sound <-  # sound file, including file name and directory
TimeZone <- "" #reference the list of time zones for code :https://en.wikipedia.org/wiki/List_of_tz_database_time_zones


sound$Site <- strapplyc(sound$`Begin File`,"nn\\d\\d\\D", simplify = TRUE) # based on site names 'nn01a', DOES NOT WORK FOR CLUSTER SITES (nn09c1)
sound$Site <- sub("_.*","",check$`Current File Name`)
elp_new$Site <- substr(str_match(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_20")[,1],1,
                       nchar(str_match(elp_new$`Begin File`,"[a-z]{2}\\d{2}[a-z]{1}\\s*(.*?)\\s*_20")[,1])-3)


# get sound file start date and time
sound$`File Start DateTime` <-  str_extract(gun_new$`Begin File`,"\\d{8}.\\d{6}") #  extract date and time characters from file name (_YYYYMMDD_HHMMSS)
sound$`File Start DateTime` <- as.POSIXct(gun_new$`File Start DateTime`,format='%Y%m%d_%H%M%S',origin = "1970-01-01",tz=TimeZone) # convert the data and time charaters to real date and time with the correct time zone
sound$`File Start Date` <- format(as.Date(str_extract(gun_new$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y")
sound$`File Start Time` <-
  elp_new$`File Start Date`<-format(as.Date(str_extract(elp_new$'Begin File' ,"\\d{8}.\\d{6}"),"%Y%m%d"),"%m/%d/%Y") # use this for date if you want the file date rather than Raven Begin Date


# Calcualte selection event date and time (requires File Offset (s))
sound$`Begin DateTime` <- sound$`File Start DateTime`+ sound$`File Offset (s)`
sound$`Begin Date` <- as.Date(sound$`Begin DateTime`,"%Y%m%d",tz=TimeZone)
sound$`Begin Time` <-

gun_new$`Selection Begin DateTime` <- gun_new$`File Start DateTime`+ gun_new$`File Offset (s)` # add File Offset (s)  to calculate selection date-time
gun_new$`Begin Date`<- as.Date(gun_new$'Selection Begin DateTime',"%Y%m%d",tz="Africa/Brazzaville") # different from File Name Date in that it is the date of the event
gun_new$`Begin Clock Time` <- format(gun_new$`Selection Begin DateTime`,"%H:%M:%S") # Time of event


# selection table name


# Split into tables of specific nrow
for (k in 1:length(merged_p4_tables)){
  split_file(merged_p4_tables[k],2000,prefix_out = paste(deployment_name,"_dep",deployment_num,"_HH6p2_p",sub("\\d.","",Filter_ScoreThreshold),"_rand_raw_XXX",sep=""),repeat_header = TRUE) #if this gives and error, check that the "_test.txt" file isn't " _test.txt" with a space. If so, delete the space
}

#### skip if the selection table is empty ####
if(nrow(elp_merged)>0){} #type the action between the brackets

#### split merged table into separate site-wise tables ####
for(k in unique(merge_filter$Site)) {
  nam <- paste(k, sep = ".")
  assign(nam, merge_filter[merge_filter$Site==k,],envir=.GlobalEnv)
  }
siteWise_list<- lapply(ls(pattern = "nn_*"), get,envir=.GlobalEnv) # list each site-specific dataframe
names(siteWise_list) <- ls(pattern = "nn_*",envir=.GlobalEnv)




#---- TESTING ZONE ----#
sound <- c("nn01a_","nn01a1_", "ch_nn01a_","ch_nn01a1_","nn01a_","nn09c2_")

strapplyc(sound,"nn\\d\\d\\D", simplify = TRUE) # based on site names 'nn01a', DOES NOT WORK FOR CLUSTER SITES (nn09c1)
site <- sub(".*_nn","",sound)
site <- sub("_.*","",sound) # excludes everything after the first underscore
str_match(sound,"nn\\d{2}(.*?)_")[,1]

begin_file<-str_match(`Begin Path`,"nn\\d{2}(.*?).wav")[,1] # get begin file from begin path
begin_file<-basename(`Begin Path`) # get begin file from begin path




