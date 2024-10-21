# Read Swift Config Files
# bje37@cornell.edu
# March 2024

# read in all swift config files with for loop
swift_files_dir <- '~/R/Bobbi_Scripts/PNNNR/Files/swift_files/'# directory where swift files are (must have "SwiftConfig" as part of file name)
setwd(swift_files_dir)
deployment_name <- "an_202312_dec" # official name of the deployment
deployment_num <- "01" # update with current deployment number
disk_ID <- "406" # name of the disk where the sound files are stored (alphanumeric)
standard_name_disk <- paste(deployment_name,"_dep",deployment_num,"_d",disk_ID, sep="") # output file names (deployment name, number, disk)

swift_files <-dir(path=swift_files_dir,all.files=TRUE,include.dirs=TRUE,recursive = TRUE, pattern="Config") # index swift files in SwiftFiles folder are (must have "SwiftConfig" as part of file name)
l <- list()

for (i in 1:length(swift_files)){
  swift_config <- read.table(swift_files[i],header=FALSE,sep="\t", check.names=FALSE,quote="\"") #read each file
    for (j in 1:nrow(swift_config)){
      gain <- sub('.*: ',"",swift_config[1,1])
      sample_rate <- sub('.*: ',"",swift_config[2,1])
      swift_site <- sub('.*: ',"",swift_config[3,1])
      swift_max_size <- sub('.*: ',"",swift_config[4,1])
      swift_program <- sub('.*: ',"",swift_config[5,1])
      swift_date_start <- sub('.*: ',"",swift_config[6,1])
      swift_date_end <- sub('.*: ',"",swift_config[7,1])
      swift_SN <- sub('.*: ',"",swift_config[8,1])
      swift_firmWare <- sub('.*: ',"",swift_config[9,1])
      swift_df <- data.frame(swift_site, gain,  sample_rate, swift_max_size, swift_program, swift_date_start, swift_date_end, swift_SN, swift_firmWare)
      colnames(swift_df) <- c("Site","Gain", "Sample Rate", "Max File Size", "Schedule","Start Date","Stop Date","Serial Number","Firmware Version")
    }
  l[[i]] <- swift_df
}
swift_merge <- do.call(rbind,l)

write.table(swift_merge,file=paste(swift_files_dir,paste(standard_name_disk,"Swift_config_Merge.txt",sep="_"),sep="/"),sep="\t",na="",col.names=TRUE,row.names=FALSE,quote=FALSE)
