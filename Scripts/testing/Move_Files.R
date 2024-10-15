# Move Files
# Assumes all files and their paths are listed in a .txt file to load in

# Read in table with files listed
sounds <- read.table("~/R/Bobbi_Scripts/PNNNR/Files/temp/PNNN_dep11_moveFiles.txt", header=TRUE,sep="\t",row.names=NULL,check.names=FALSE)
old <- sounds$`File Path` # full path of the sound files, including the sound name and extension
new <- sounds$`New File Path`# full path of new sound file location, excluding the sound file name

# move files from old to new path
file.move(sounds$`File Path`,sounds$`New File Path`)
#file.move(old,new)
