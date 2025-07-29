# helper.R
summarize_helper_function <- function(deployment_name, deployment_num, disk_ID,
                                      sites, sound_path, extra_sounds_folder,
                                      fileDurationMin, sample_rate, sound_file_ext) {
  data.frame(
    Setting = c("Deployment Name", "Deployment Number", "Disk ID", "Sites File",
                "Sound Path", "Extra Sounds Folder", "File Duration (min)",
                "Sample Rate (Hz)", "File Extension"),
    Value = c(deployment_name, deployment_num, disk_ID, sites, sound_path,
              extra_sounds_folder, fileDurationMin, sample_rate, sound_file_ext)
  )
}
