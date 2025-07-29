ui <- page_sidebar(
  title = "Sound Check",

  sidebar = sidebar(
    h4("Configuration"),
    textInput("deployment_name_in", "Deployment name:", value = "nn_202211_nov"),
    textInput("deployment_num_in", "Deployment number:", value = "01"),
    textInput("disk_ID_in", "Disk ID:", value = "00"),
    fileInput("sites_in", "Sites txt file:"),
    numericInput("fileDurationMin_in", "File duration (minutes):", value = 60),
    numericInput("sample_rate_in", "Sample rate (Hz):", value = 8000),
    selectInput("sound_file_ext_in", "File extension:",
                choices = c(".wav", ".flac", ".aiff")),

    # Replace shinyDirButton with directoryInput
    textInput("sound_path_in", "Sound files folder path:"),
    textInput("extra_sounds_in", "Output folder path:"),

    actionButton("check_info_in", "Verify Settings", class = "btn-primary"),
    actionButton("run_sound_check_in", "Run Sound Check", class = "btn-success")
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Settings Summary",
               tableOutput("check_info_output")),
      tabPanel("Results",
               verbatimTextOutput("run_sound_check_output"))
    )
  )
)
