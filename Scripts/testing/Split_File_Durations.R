# split sound files using warbleR
# bje37@cornell.edu

library(warbleR)
setwd("L:/ELP/Projects/Nouabale/nn_analyses/Cluster_SECR/PNNN_Dep17/Single-Channnel_8kHz_01hr/nn09c00")
sounds <- "L:/ELP/Projects/Nouabale/nn_analyses/Cluster_SECR/PNNN_Dep17/Single-Channnel_8kHz_01hr/1hr"

# list_txt = list.files(path="L:/ELP/Projects/Nouabale/nn_analyses/Cluster_SECR/PNNN_Dep17/Single-Channnel_8kHz_01hr/", full.names = TRUE,pattern="*.txt",recursive = TRUE) #make a list of all the files within the subfolder j
# all_txt_df <- lapply(list_txt, function(x) {read.table(file = x, header = T, sep ="\t", check.names=FALSE)})  # Read the files in, assuming tab separator
# merge_df <- do.call("rbind", lapply(all_txt_df, as.data.frame)) # Combine them (the events cross over between sites and cuts off the last site - use the rbind.fill function)
# merge_df$sound.files <- merge_df$`Begin File`
# merge_df$selec <- as.character(merge_df$Selection)
# merge_df$start <- merge_df$`Begin Time (s)`
# merge_df$end <- merge_df$`End Time (s)`

split_sound_files(path = sounds,sgmt.dur = 3600,parallel = 6,pb = T)
