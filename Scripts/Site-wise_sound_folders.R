# move sound files into site-specific folders


HH_folders <- read.table(paste("~/R/Bobbi_Scripts/PNNNR/Files/sites/",sites, sep=""),header=FALSE,row.names=NULL)
for(z in 1:nrow(HH_folders)){
 if (!dir.exists(paste(processed_HH,HH_folders[z,],sep=""))) dir.create(paste(processed_HH,HH_folders[z,],sep=""))
}


# make site-specific dataframes
for(k in unique(merge_df$Site)) {
  nam <- paste("merged", k, sep = ".")
  assign(nam, merge_df[merge_df$Site==k,])
}

# save each site-specific dataframe as a table
site_dataframes <- lapply(ls(pattern="merged.nn"), function(x) get(x))

for(m in 1:length(site_dataframes)){
  write.table(site_dataframes[m],file=paste(folder[j],"_dep",deployment_num,"_HHv6p2_p2.txt",sep=""),sep="\t",na="",col.names=TRUE,row.names=FALSE, quote=FALSE)
}


## temp scratch space
beginFile <- c("PNNN16_008K_nn09c2_20230217_000000Zp0100.wav",
               "PNNN16_008K_nn09c_20230217_000000Zp0100.wav",
               "PNNN_nn09c2_20230217_000000.wav",
               "Dzanga_dz09c_20230217_000000.wav")

sub("_.*","",beginFile) # no
substr(beginFile,1,5) # no
substr(str_extract(beginFile,"[a-z]{2}\\d{2}[a-z]{1}_*"),1,nchar(str_extract(beginFile,"[a-z]{2}\\d{2}[a-z]{1}_*"))-1)

