server <- function(input, output, session) {
  # Reactive values to store all settings
  settings <- reactive({
    list(
      deployment_name = input$deployment_name_in,
      deployment_num = input$deployment_num_in,
      disk_ID = input$disk_ID_in,
      sites = input$sites_in$name,
      sound_path = input$sound_path_in,
      extra_sounds_folder = input$extra_sounds_in,
      fileDurationMin = input$fileDurationMin_in,
      sample_rate = input$sample_rate_in,
      sound_file_ext = input$sound_file_ext_in
    )
  })

  # Settings verification output
  output$check_info_output <- renderTable({
    req(input$check_info_in)

    # Validate inputs
    validate(
      need(input$sound_path_in != "", "Please enter a sound files folder path"),
      need(input$extra_sounds_in != "", "Please enter an output folder path"),
      need(input$sites_in, "Please upload a sites file")
    )

    # Create summary table
    data.frame(
      Setting = names(settings()),
      Value = unlist(settings())
    )
  })

  # Function to write settings to global environment
  write_to_global <- function(settings_list) {
    for(name in names(settings_list)) {
      assign(name, settings_list[[name]], envir = .GlobalEnv)
      message(paste("Assigned", name, "=", settings_list[[name]]))
    }
  }

  # Run sound check
  observeEvent(input$run_sound_check_in, {
    req(settings())

    # Validate paths
    validate(
      need(dir.exists(input$sound_path_in), "Sound files folder path is invalid"),
      need(dir.exists(input$extra_sounds_in), "Output folder path is invalid")
    )

    withProgress(message = 'Processing sound files...', {
      tryCatch({
        # Write settings to global environment before running sound.check
        write_to_global(settings())

        # Run the sound check function
        result <- sound.check(input$sound_path_in)
        output$run_sound_check_output <- renderPrint({
          result
        })
      }, error = function(e) {
        output$run_sound_check_output <- renderPrint({
          paste("Error:", e$message)
        })
      })
    })
  })
}
