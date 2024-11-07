# MOVE BAD SOUND FILES TO ANOTHER FOLDER
# written by BJE
# bje37@cornell.edu
# Feb 2023


# This script will:
# - read the excel table produced by the sound_check script
# - move all sounds marked with a 'y' in the "Exclude (y)" column of the sound_check.xlsx file to the excluded sounds folder defined in the script
      # Sound files to exclude include files with less than 1 min of recording and bad sound files
      # For elephant rumbles, excluded sounds are those with less than 22.95 hours or more than 24 hours (remove for HoriHarm)
      # for each site, remove first and last sound file if they don't form a complete 24-hr day in the date
# - summarize the duration of the excluded files and save the info in the sound_check xlsx file
# - move the Swift config and debug .txt files to the swift_files folder
      # merge the Swift Config files into one .txt file and append that to the sound check sheet

#### TO DO ####
# update to accommodate new Swift Config files
# enable option for Swift files or not. if not, don't execute the swift file script.

sound.exclude <- function (x,y)
{
setwd(x)

  # x <- sound_path
  # y <- extra_sounds_folder
  standard_name_disk <- paste(deployment_name,"_dep",deployment_num,"_d",disk_ID, sep="") # output file names (deployment name, number, disk)
  z <- paste("~/R/Bobbi_Scripts/Packages/elpR/Files/sound_check/Sound_Check_Reports_",standard_name_disk,".xlsx",sep="")
  w <- "~/R/Bobbi_Scripts/Packages/elpR/Files/swift_files"# folder path where the renamed Swift files will be moved to


  #### SWIFT FILES ####
  #### rename Swift .txt files by appending folder name as a prefix and automatically move those files to new folder #### This doesn't work if there are no config files
if(have_SwiftFiles == "y"){
  swift_files <-list.files(path=x,all.files=TRUE,full.names = TRUE,include.dirs=TRUE,recursive = TRUE, pattern=".txt") #index file paths
  swift_files<-data.frame(swift_files)
  colnames(swift_files) <- "Current File Path"
  swift_files$`Current File Name` <- basename(swift_files$`Current File Path`)#list.files(x,pattern = sound_file_ext,all.files=FALSE,full.names=FALSE,recursive = TRUE,include.dirs=FALSE))
  swift_files$Site <- sub("\\_","",str_extract(swift_files$`Current File Path`,"[a-z]{2}\\d{2}[a-z]{1}."))
  #swift_files$`Site` <- str_match(swift_files$`Current File Path`,"[a-z]{2}\\d{2}[a-z]{1}_*")[,1]
  #swift_files$`Site` <- str_match(swift_files$`Current File Path`,"[a-z]{2}\\d{2}(.*?)_")[,1] # strapplyc(swift_files$`Current File Path`,"nn\\d\\d\\D", simplify = TRUE) #   CHECK THAT THIS WILL RENAME SITES WITH EXTRA CHARACTERS (NN019C)
  swift_files$`New File Name` <- paste(swift_files$`Site`,"_",standard_name_disk,"_",swift_files$`Current File Name`,sep="")
  for(l in 1:nrow(swift_files)){
    #swift_files$`New File Name`[l] <- gsub("\\s*\\([^\\)])",paste("",l,"",sep="_"),as.character(swift_files$`New File Name`[l]))
    # swift_files$`New File Path`[l] <- gsub(pattern = swift_files$`Current File Name`[l],
    #                                     replacement = swift_files$`New File Name`[l],
    #                                     x=swift_files$`Current File Path`[l])
    swift_files$`New File Path`[l] <- paste(dirname(swift_files$`Current File Path`[l]),swift_files$`New File Name`[l],sep="/")
    }
  for (i in 1:nrow(swift_files)){
    file.rename(swift_files$`Current File Path`[i],swift_files$`New File Path`[i])
  }
  swift_path <- swift_files[,"New File Path"] # get the path of each Swift file

  # move the swift files from the sound folder to the Swift files folder in the R package folder
  file.move(swift_path,w,overwrite =TRUE) # move Swift files to folder
  print("Swift files should have been moved. Check line above to confirm that all files were moved and none failed")

  ### merge the swift config files into new file ###
  if(merge_swift_files == "y"){
      swift_files_dir <- '~/R/Bobbi_Scripts/Packages/elpR/Files/swift_files/'# directory where swift files are (must have "SwiftConfig" as part of file name)
      setwd(swift_files_dir) # can this be removed?
      swift_files <-dir(path=swift_files_dir,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern="Config") # index swift files in SwiftFiles folder are (must have "SwiftConfig" as part of file name)
      l <- list()

      for (i in 1:length(swift_files)){
        swift_config <- read.table(swift_files[i],header=FALSE,sep="\t", check.names=FALSE,quote="\"") #read each file
        for (j in 1:nrow(swift_config)){
                # for n rows in swift file, sub("\\:.*",swift_config[1,]). Do that for each one and append together. NOT DONE YET
               swift_df <- data.frame(
                assign(paste(sub("\\:.*","",swift_config[1,1]),"",sep=""),sub('.*: ',"",swift_config[1,1])),
                assign(paste(sub("\\:.*","",swift_config[2,1]),"",sep=""),sub('.*: ',"",swift_config[2,1])),
                assign(paste(sub("\\:.*","",swift_config[3,1]),"",sep=""),sub('.*: ',"",swift_config[3,1])),
                assign(paste(sub("\\:.*","",swift_config[4,1]),"",sep=""),sub('.*: ',"",swift_config[4,1])),
                assign(paste(sub("\\:.*","",swift_config[5,1]),"",sep=""),sub('.*: ',"",swift_config[5,1])),
                assign(paste(sub("\\:.*","",swift_config[6,1]),"",sep=""),sub('.*: ',"",swift_config[6,1])),
                assign(paste(sub("\\:.*","",swift_config[7,1]),"",sep=""),sub('.*: ',"",swift_config[7,1])),
                assign(paste(sub("\\:.*","",swift_config[8,1]),"",sep=""),sub('.*: ',"",swift_config[8,1])),
                assign(paste(sub("\\:.*","",swift_config[9,1]),"",sep=""),sub('.*: ',"",swift_config[9,1])),
                assign(paste(sub("\\:.*","",swift_config[10,1]),"",sep=""),sub('.*: ',"",swift_config[10,1])),
                assign(paste(sub("\\:.*","",swift_config[11,1]),"",sep=""),sub('.*: ',"",swift_config[11,1])),
                assign(paste(sub("\\:.*","",swift_config[12,1]),"",sep=""),sub('.*: ',"",swift_config[12,1])),
                assign(paste(sub("\\:.*","",swift_config[13,1]),"",sep=""),sub('.*: ',"",swift_config[13,1])),
                assign(paste(sub("\\:.*","",swift_config[14,1]),"",sep=""),sub('.*: ',"",swift_config[14,1])),
                assign(paste(sub("\\:.*","",swift_config[15,1]),"",sep=""),sub('.*: ',"",swift_config[15,1]))
               )
              colnames(swift_df) <- c(
                paste(sub("\\:.*","",swift_config[1,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[2,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[3,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[4,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[5,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[6,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[7,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[8,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[9,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[10,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[11,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[12,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[13,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[14,1]),"",sep=""),
                paste(sub("\\:.*","",swift_config[15,1]),"",sep="")
              )
                  #"Site","Gain", "Sample Rate", "Max File Size", "Schedule","Start Date","Stop Date","Serial Number","Firmware Version")
        }
        l[[i]] <- swift_df
      }
      swift_merge <- do.call(rbind,l)
      #identical(names(l[[1]]), names(l[[13]]) )

      write.table(swift_merge,file=paste(swift_files_dir,paste(standard_name_disk,"Swift_config_Merge.txt",sep="_"),sep="/"),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)

  # append a new tab to the sound check file
  wb <- loadWorkbook(file = z)
  if("SwiftConfig" %in% names(wb)){
    removeWorksheet(wb, "SwiftConfig")
    saveWorkbook(wb, z,overwrite=TRUE)
  }
  addWorksheet(wb, sheetName = "SwiftConfig",tabColour='blue')
  writeData(wb, sheet = "SwiftConfig", x = swift_merge)
  saveWorkbook(wb,z,returnValue=FALSE,overwrite=TRUE)
  }
}

  #### Sound EXCLUDE ####
  #### move all sounds marked as 'y' in the sound_check file ####
  file_paths<-basename(list.files(path=x,all.files=TRUE,full.names = FALSE,include.dirs=TRUE,recursive = TRUE, pattern = sound_file_ext)) #index file paths
  sounds<-data.frame(file_paths)
  colnames(sounds) <- "Current File Name"
  #sounds$present<-"yes" #this is just to check that only sound files from this index will be in the dataframes below

  sound_check <- read.xlsx(z,sheet="Sounds",colNames=TRUE,check.names=FALSE,sep.names = " ")
  file_merge<-merge(sounds,sound_check,by="Current File Name",all.x=T) # merge the sound check info with the list of sound files in the folder (only includes sounds in folder)
  file_exclude<-file_merge[ which(file_merge$`Exclude (y/e)` == "y"),] # only select files marked with "y" in excluded column to be moved
  exclude_path<-file_exclude[,"File Path"]# list the paths of the files to be excluded

  # Total duration of problem files (amount of data that have issues)
  duration_issues_hrs<-paste("Total excluded sound (Hrs) = ",sum(file_exclude$`File Duration (s)`)/60/60) #Total hours of excluded sound
  #duration_issues_by_site<-file_exclude %>% group_by(Site) %>% summarise(duration = sum('File Duration (s)'))
  duration_issues_by_site <- aggregate(file_exclude$'File Duration (s)', by=list(Site=file_exclude$Site), FUN=sum)
  duration_issues_by_site$`Total Duration (hr)`<-round(duration_issues_by_site$x/60/60,digits=2) # total hours of excluded files
  #Sum_bad_files <- file_exclude %>% count("Site")
  Sum_bad_files <- plyr::count(file_exclude, "Site")
  duration_issues_by_site <- merge(duration_issues_by_site,Sum_bad_files,by="Site",all.x=T)
  colnames(duration_issues_by_site) <- c("Site","Total Duration (s)","Total Duration (hr)","Sum Bad Files")

  # save summary to tab of excel sheet
  wb <- loadWorkbook(file = z)
  if("Excluded_Files" %in% names(wb)){
    removeWorksheet(wb, "Excluded_Files")
    saveWorkbook(wb, z,overwrite=TRUE)
  }
  addWorksheet(wb, sheetName = "Excluded_Files",tabColour='salmon1')
  writeData(wb, sheet = "Excluded_Files", x = duration_issues_by_site)
  saveWorkbook(wb,z,returnValue=FALSE,overwrite=TRUE)

  summaries <- list(duration_issues_hrs,duration_issues_by_site)

  file.move(exclude_path,y,overwrite =FALSE) # move those files from current directory to new directory
  print("Extra sounds should have been moved. Check line above to confirm that all files were moved and none failed")

  return(summaries)


}

