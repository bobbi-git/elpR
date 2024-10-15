# Merge sound files
# BJE
# Aug 2024

# This script uses functions from tuneR to merge sound files within SwiftOne day-folders together, into one sound file per day-folder.
# This script expects day-folders nested within site folders within the directory
# This will only work on .wav files right now
# The files will be merged by the order in which they are read, so be certain they are in the right order.
# cAUTION: If a file within the dayfolder is missing, the time stamps after that file will be incorrect in the newly created merged sound file
# This script should be used when all sounds are contiguous and there are not gaps in the recording time.
# The new sound file name will recieve the name of the first sound file in the day folder, and will have "merged" as a prefix

ls()
library(tuneR)


setwd("L:/ELP/Projects/Angola_NatGeo/an_sounds/an_202312_dec_dep01/8kHz_to_do")

# get path of site folder
path = "L:/ELP/Projects/Angola_NatGeo/an_sounds/an_202312_dec_dep01/8kHz_to_do" # where the sound files are (sites and day-folders)
output ="L:/ELP/Projects/Angola_NatGeo/an_sounds/an_202312_dec_dep01/8kHz_24hr_files" # where to save the new sounds

# prepare for file name


# list all site folders in directory
sites <-dir()

# create empty site-folders to receive the sound files
out_folders <- sites
for(z in 1:length(out_folders)){
  if (!dir.exists(paste(output,out_folders[z],sep="/"))) dir.create(paste(output,out_folders[z],sep="/"))
}

# merge all folders within day folder
for(h in 1:length(sites)){
  day <- dir(sites[h], full.names=TRUE)
  for(i in 1:length(day)){
    files <- list.files(day[i], pattern = ".wav")
    # sory by file name to ensure proper sort order (oldest to newest) before merging?
    list <- lapply(paste0(path, "/",day[i],"/", files),
                   function(file_name) {
                        ext <- tolower(tools::file_ext(file_name))
                        if (ext == "wave" | ext == "wav") {
                          s <- tuneR::readWave(file_name)
                  }
                })
    sound <- do.call(tuneR::bind, list)
    # check duration of sound file and flag it if < 24 hrs
    tuneR::writeWave(sound, paste0(output,"/",sites[h], "/merged_", files[1]))
    print(gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE))
    rm(list, sound)
    }
  }

# TO DO:
## flag any new sound files that are less than expected duration (24 hours)
## if day-folders are missing, ignore?

# NOTE:
## R script stopped when encountering files other than .wav files. not sure why. investigate...
