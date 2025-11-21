library(shiny)
library(bslib) #shiny layout functions
library(shinyFiles) #shinyDirChoose(), parseDirPath()
library(gbRd) #Rd_fun()
library(tools) #Rd2HTML()
library(readr) #read_files()
library(openxlsx)

assign("shiny_dir", getwd(), envir = .GlobalEnv) #for rumble & gunshot restructure

####################################################################
# Define UI ----
ui <- page_fillable(

  tags$div(
    style = "
    position: absolute;
    top: 45px;
    left: 0px;
    width: 100%",
    hr()
  ),

  titlePanel(
    list(
      "ElpR",
      tags$div(
        style = "
        position: absolute;
        top: 17px;
        right: 20px",
        HTML("<img src = 'elp_logo.png'>")
      )
    )
  ),

  navlistPanel(
    well = TRUE,
    tabPanel(
      "1. Basic Info",
      layout_columns(
        card(
          card_header("Input Basic Information"),

          textInput("deployment_name_in", label = "Deployment name:", value = "kk_202405_may"),
          textInput("deployment_num_in", label = "Deployment number:", value = "02"),
          textInput("disk_ID_in", label = "Disk ID:", value = "00"),
          fileInput("sites_in", label = "Sites txt file:")
        ),

        "\n",
        "\n",
        "\n",

        col_widths = breakpoints(
          sm = c(12,12),
          md = c(8,12),
          lg = c(6,12)
        )
      )
    ),

    tabPanel(
      "2. Sound Check",
      layout_columns(
        card(
          card_header("Input Sound Check Information"),

          shinyDirButton(id = "sound_path_in",
                         label = "Choose folder containing sound files",
                         title = "Choose a folder"),
          textOutput("sound_path_recieved"),

          numericInput("fileDurationMin_in", label = "File duration (minutes):", value = 14000),
          numericInput("sample_rate_in", label = "Sample rate (Hz):", value = 800),
          selectInput("sound_file_ext_in", "Sound file extention:", choices = list(".wav", ".flac", ".aiff"))
        ),

        card(
          card_header("Run Sound Check"),

          actionButton("check_sound_check_info_in", "Check Your Info"),
          tableOutput("check_sound_check_info_output"),

          actionButton("run_sound_check_in", "Run Sound Check"),
          "Results preview:",
          verbatimTextOutput("run_sound_check_output")
        ),

        "\n",
        "\n",
        "\n",

        col_widths = breakpoints(
          sm = c(12,12,12),
          md = c(8,8,12),
          lg = c(6,6,12)
        )
      )
    ),

    tabPanel(
      "3. Exclude Files",
      layout_columns(
        card(
          card_header("Input Exclude Files Information"),

          shinyDirButton(id = "extra_sounds_in",
                         label = "Choose folder to output extra sounds",
                         title = "Choose a folder"),
          textOutput("extra_sounds_recieved"),

          textInput("have_swift_files_in", label = "Do you have Swift files in the sounds folder? (y/n):", value = "n"),
          textInput("merge_swift_files_in", label = "Do you want to merge the Swift files? (y/n):", value = "n")
        ),

        card(
          card_header("Run Exclude Files"),

          actionButton("check_exclude_files_info_in", "Check Your Info"),
          tableOutput("check_exclude_files_info_output"),

          actionButton("run_exclude_files_in", "Run Exclude Files"),
          "Results summary:",
          verbatimTextOutput("run_exclude_files_output")
        ),

        "\n",
        "\n",
        "\n",

        col_widths = breakpoints(
          sm = c(12,12,12),
          md = c(8,8,12),
          lg = c(6,6,12)
        )
      )
    ),

    tabPanel(
      "4. Restructure",
      layout_columns(
        "Choose type of data:",

        navset_pill(
          tabPanel(
            "Rumbles",
            layout_columns(
              "\n",

              card(
                card_header("Input Rumble Detector Information"),

                textInput("sample_rate_chr_in", label = "Sample Rate:", value = "8kHz"),
                textInput("three_rand_days_in", label = "Do you want 3 random days per week? (y/n):", value = "n"),
                textInput("min_23hours_in", label = "Does your project require a minimum of 23 hours per day? (y/n):", value = "y"),
                textInput("score_column_name_in", label = "Score column name:", value = "Score"),
                selectInput("detector_in", "Detector used:", choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector")),
                numericInput("detector_score_in", label = "Detector score:", value = 0.2),
                numericInput("filter_score_in", label = "Filter score:", value = 0.4)

              ),

              card(
                card_header("Run Rumble Restructure"),

                actionButton("check_restructure_info_in", "Check Your Info"),
                tableOutput("check_restructure_info_output"),

                actionButton("run_restructure_in", "Run Rumble Restructure"),
                "Results summary:",
                tableOutput("run_restructure_output")
              ),

              col_widths = breakpoints(
                sm = c(12,12,12),
                md = c(12,8,8),
                lg = c(12,6,6)
              )
            )
          ),

          tabPanel(
            "Gunshots",
            layout_columns(
              "\n",

              card(
                card_header("Input Gunshot Detector Information"),

                textInput("sample_rate_chr_gun_in", label = "Sample Rate:", value = "8kHz"),
                selectInput("detector_gun_in", "Detector used:", choices = list("DTDguns8")),
                numericInput("detector_score_gun_in", label = "Detector score:", value = 0.53),
                numericInput("filter_score_gun_in", label = "Filter score:", value = 0.53)

              ),

              card(
                card_header("Run Gunshot Restructure"),

                actionButton("check_gun_restructure_info_in", "Check Your Info"),
                tableOutput("check_gun_restructure_info_output"),

                actionButton("run_gun_restructure_in", "Run Gunshot Restructure"),
                "Results summary:",
                tableOutput("run_gun_restructure_output")
              ),

              col_widths = breakpoints(
                sm = c(12,12,12),
                md = c(12,8,8),
                lg = c(12,6,6)
              )
            )
          ),

          tabPanel(
            "General",
            layout_columns(
              "\n",

              card(
                card_header("Input General Information"),

                shinyDirButton(id = "general_merge_in",
                               label = "Choose folder containing selection tables to merge",
                               title = "Choose a folder"),
                textOutput("general_merge_recieved"),

                radioButtons("recursive_in", label = "How are the selection tables organized?",
                             choices = list("In one folder",
                                            "In multiple subfolders"))
              ),

              card(
                card_header("Run General Restructure"),

                actionButton("check_general_restructure_info_in", "Check Your Info"),
                tableOutput("check_general_restructure_info_output"),

                actionButton("run_general_restructure_in", "Run General Restructure"),
                "Results summary:",
                verbatimTextOutput("run_general_restructure_output")
              ),

              col_widths = breakpoints(
                sm = c(12,12,12),
                md = c(12,8,8),
                lg = c(12,6,6)
              )
            )
          )
        ),

        "\n",
        "\n",
        "\n",


        col_widths = breakpoints(
          sm = c(12),
          md = c(12),
          lg = c(12)
        )
      )
    ),

    tabPanel(
      "5. Data Summaries",
      layout_columns(
        card(
          card_header("Input Data Summaries Information"),

          textInput("project_name_in", label = "Project name:", value = "PNNN"),
          textInput("deployment_nums_in", label = "Deployment number(s):", value = "01-20"),
          selectInput("summary_detector_in", "Detector used:", choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector")),
          numericInput("summary_fileDurationMin_in", label = "File duration (minutes):", value = 1440),

          shinyDirButton(id = "summary_folder_in",
                         label = "Choose folder to output data summaries",
                         title = "Choose a folder"),
          textOutput("summary_folder_recieved"),

          shinyDirButton(id = "selection_tables_folder_in",
                         label = "Choose folder containing selection tables",
                         title = "Choose a folder"),
          textOutput("selection_tables_folder_recieved"),

          shinyDirButton(id = "zero_selection_tables_folder_in",
                         label = "Choose folder containing zero-day selection tables",
                         title = "Choose a folder"),
          textOutput("zero_selection_tables_folder_recieved")
        ),

        card(
          card_header("Input OPTIONAL Data Summaries Information"),

          radioButtons("sound_check_include_in", label = "Do you want to include a sound check file?",
                       choices = list("Yes", "No"),
                       selected = "No"),
          uiOutput("sound_check_include_output"),
          textOutput("sound_check_folder_recieved"),

          radioButtons("use_only_sites_provided_in", label = "Do you only want to include sites listed in a sites file?",
                       choices = list("Yes", "No"),
                       selected = "No"),
          uiOutput("use_only_sites_provided_output"),

          radioButtons("rand_dates_needed_in", label = "Do you need random dates?",
                       choices = list("Yes", "No"),
                       selected = "No"),
          radioButtons("ele_bad_sound_remove_in", label = "Do you want to exclude sounds <23 hours?",
                       choices = list("Yes", "No"),
                       selected = "No")
        ),

        card(
          card_header("Run Data Summaries"),

          actionButton("check_summary_info_in", "Check Your Info"),
          tableOutput("check_summary_info_output"),

          actionButton("run_summary_in", "Run Data Summaries"),
          "Results summary:",
          verbatimTextOutput("run_summary_output")
        ),

        "\n",
        "\n",
        "\n",

        col_widths = breakpoints(
          sm = c(12,12,12,12),
          md = c(8,8,12,12),
          lg = c(6,6,10,12)
        )
      )
    ),

    navbarMenu(
      "More Information",
      tabPanel(
        "HELP",
        layout_columns(
          accordion(
            open = FALSE,
            multiple = FALSE,

            accordion_panel(
              "What does \"Sound Check\" do?",
              htmlOutput("sound_check_documentation")
            ),
            accordion_panel(
              "What does \"Exclude Files\" do?",
              htmlOutput("exclude_files_documentation")
            ),
            accordion_panel(
              "What does \"Restructure\" for Rumbles do?",
              htmlOutput("restructure_documentation")
            ),
            accordion_panel(
              "What does \"Restructure\" for General do?",
              htmlOutput("general_restructure_documentation")
            )
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(
            sm = c(12,12),
            md = c(10,12),
            lg = c(10,12)
          )
        )
      ),

      tabPanel(
        "About",
        layout_columns(
          "Text here",

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(
            sm = c(12,12),
            md = c(10,12),
            lg = c(10,12)
          )
        )
      )
    )
  ),

  tags$div(
    style = "
    position: fixed;
    bottom: 0px;
    left: 0px;
    height: 50px;
    width: 100%;
    background-color: #f5f5f5",
  ),
  tags$div(
    style = "
    position: fixed;
    bottom: 17px;
    left: 0px;
    width: 100%",
    hr()
  ),

  tags$div(
    style = "position: fixed; bottom: -5px; left: 10px",
    HTML("<p>footer</p>")
  )

  #OLD LAYOUT WITH SIDEBAR

  # page_sidebar(
  #   title = "ElpR",
  #
  #   sidebar = sidebar(
  #     HTML("Instructions
  #            <br>
  #            <br>Here are instructions for using this app..."),
  #
  #     checkboxGroupInput(
  #       "checklist",
  #       label = "To Do list:",
  #       c("Input basic information",
  #         "Complete sound check",
  #         "Exclude desired files",
  #         "Restructure table",
  #         "Create data summaries",
  #         "Visualize data!")
  #     )
  #   ),
  #
  #   navset_tab(
  #     nav_panel(
  #       "Basic Info",
  #       layout_columns(
  #         "\n",
  #
  #         card(
  #           card_header("Input Basic Information"),
  #
  #           textInput("deployment_name_in", label = "Deployment name:", value = "kk_202405_may"),
  #           textInput("deployment_num_in", label = "Deployment number:", value = "02"),
  #           textInput("disk_ID_in", label = "Disk ID:", value = "00"),
  #           fileInput("sites_in", label = "Sites txt file:")
  #         ),
  #
  #         col_widths = breakpoints(
  #           sm = c(12,12),
  #           md = c(12,8),
  #           lg = c(12,6)
  #         )
  #       )
  #     )
  #   )
  # ),

  #hr(style = "margin-top: 0px; margin-bottom: 0px; padding-bottom: 5px"),
  #HTML("<p style = 'margin-bottom: 10px; margin-right: 10px; text-align: right'>footer</p>")

)


####################################################################
# Define server logic ----

server <- function(input, output) {

  #################################
  #DOCUMENTATION OUTPUTS

  #sound_check_documentation
  output$sound_check_documentation <- renderText({
    temp = Rd2HTML(Rd_fun("sound.check", keep_section = c("\\description", "\\value", "\\note", "\\author")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  #exclude_files_documentation
  output$exclude_files_documentation <- renderText({
    temp = Rd2HTML(Rd_fun("sound.exclude", keep_section = c("\\description", "\\value", "\\note", "\\author")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  #restructure_documentation
  output$restructure_documentation <- renderText({
    temp = Rd2HTML(Rd_fun("Rumble_Selection_Table_Restructure", keep_section = c("\\description", "\\value", "\\note", "\\author")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  #general_restructure_documentation
  output$general_restructure_documentation <- renderText({
    temp = Rd2HTML(Rd_fun("merge_selection_tables", keep_section = c("\\description", "\\value", "\\note", "\\author")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  #################################
  #FOLDER OUTPUTS
  volumes = getVolumes()()

  #sound_path
  shinyDirChoose(input, "sound_path_in", roots = volumes)
  output$sound_path_recieved <- renderText({
    paste("Sound path recieved: ", parseDirPath(volumes, input$sound_path_in))
  })

  #extra_sounds
  shinyDirChoose(input, "extra_sounds_in", roots = volumes)
  output$extra_sounds_recieved <- renderText({
    paste("Extra sounds path recieved: ", parseDirPath(volumes, input$extra_sounds_in))
  })

  #general_merge
  shinyDirChoose(input, "general_merge_in", roots = volumes)
  output$general_merge_recieved <- renderText({
    paste("Folder path recieved: ", parseDirPath(volumes, input$general_merge_in))
  })

  #summary_folder
  shinyDirChoose(input, "summary_folder_in", roots = volumes)
  output$summary_folder_recieved <- renderText({
    paste("Summary folder recieved: ", parseDirPath(volumes, input$summary_folder_in))
  })

  #selection_tables_folder
  shinyDirChoose(input, "selection_tables_folder_in", roots = volumes)
  output$selection_tables_folder_recieved <- renderText({
    paste("Selection tables folder recieved: ", parseDirPath(volumes, input$selection_tables_folder_in))
  })

  #zero_selection_tables_folder
  shinyDirChoose(input, "zero_selection_tables_folder_in", roots = volumes)
  output$zero_selection_tables_folder_recieved <- renderText({
    paste("Zero-day selection tables folder recieved: ", parseDirPath(volumes, input$zero_selection_tables_folder_in))
  })

  #################################
  #RADIO BUTTON OUTPUTS

  #sound_check_include
  output$sound_check_include_output <- renderUI({
    if(input$sound_check_include_in == "Yes"){
      shinyDirButton(id = "sound_check_folder_in",
                     label = "Choose folder containing sound check files",
                     title = "Choose a folder")
    }
    else if(input$sound_check_include_in == "No"){
      output$sound_check_folder_recieved <- renderText({
        NULL
      })
    }
  })
  observeEvent(input$sound_check_folder_in, {
    shinyDirChoose(input, "sound_check_folder_in", roots = volumes)
    output$sound_check_folder_recieved <- renderText({
      paste("Sound check folder recieved: ", parseDirPath(volumes, input$sound_check_folder_in))
    })
  })

  #use_only_sites_provided
  output$use_only_sites_provided_output <- renderUI({
    if(input$use_only_sites_provided_in == "Yes"){
      fileInput("sites_lat_long_in", label = "Sites txt file with latitude and longitude:")
    }
    # else if(input$use_only_sites_provided_in == "No"){
    #   NULL
    # }
  })

  #################################
  #INPUTS TO GLOBAL ENVIRONMENT

  #create list of user inputs
  inputs_sound_check <- reactive({
    list(
      deployment_name = input$deployment_name_in,
      deployment_num = input$deployment_num_in,
      disk_ID = input$disk_ID_in,
      sites = input$sites_in$datapath,
      sound_path = parseDirPath(volumes, input$sound_path_in),
      fileDurationMin = input$fileDurationMin_in,
      sample_rate = input$sample_rate_in,
      sound_file_ext = input$sound_file_ext_in
    )
  })

  inputs_exclude_files <- reactive({
    list(
      extra_sounds_folder = parseDirPath(volumes, input$extra_sounds_in),
      have_SwiftFiles = input$have_swift_files_in,
      merge_swift_files = input$merge_swift_files_in
    )
  })

  inputs_restructure <- reactive({
    if(input$detector_in == "HoriHarm"){
      detector_in_value = "HHv6"
    } else if(input$detector_in == "FruitPunchAI"){
      detector_in_value = "FPv1"
    } else if(input$detector_in == "Stanford Detector"){
      detector_in_value = "SDv1"
    }

    list(
      deployment_name = input$deployment_name_in,
      deployment_num = input$deployment_num_in,
      disk_ID = input$disk_ID_in,
      sites = input$sites_in$datapath,
      sample_rate = input$sample_rate_chr_in,
      three_rand_days = input$three_rand_days_in,
      min_23hrs = input$min_23hours_in,
      score_column_name = input$score_column_name_in,
      Detector = detector_in_value,
      Detector_ScoreThreshold = input$detector_score_in,
      Filter_ScoreThreshold = input$filter_score_in
    )
  })

  inputs_gun_restructure <- reactive({
    list(
      deployment_name = input$deployment_name_in,
      deployment_num = input$deployment_num_in,
      disk_ID = input$disk_ID_in,
      sites = input$sites_in$datapath,
      sample_rate = input$sample_rate_chr_gun_in,
      Detector = input$detector_gun_in,
      Detector_ScoreThreshold = input$detector_score_gun_in,
      Filter_ScoreThreshold = input$filter_score_gun_in
    )
  })

  inputs_general_restructure <- reactive({
    if(input$recursive_in == "In one folder"){
      recursive_in_value = FALSE
    } else if(input$recursive_in == "In multiple subfolders"){
      recursive_in_value = TRUE
    }

    list(
      path = parseDirPath(volumes, input$general_merge_in),
      recursive = recursive_in_value
    )
  })

  inputs_summary <- reactive({
    if(input$sound_check_include_in == "Yes"){
      sound_check_include_value = "y"
      sound_check_folder_value = parseDirPath(volumes, input$sound_check_folder_in)
    } else if(input$sound_check_include_in == "No"){
      sound_check_include_value = "n"
      sound_check_folder_value = "N/A"
    }
    if(input$use_only_sites_provided_in == "Yes"){
      use_only_sites_provided_value = "y"
      sites_lat_long_value = read.table(input$sites_lat_long_in$datapath)
    } else if(input$use_only_sites_provided_in == "No"){
      use_only_sites_provided_value = "n"
      sites_lat_long_value = "N/A"
    }
    if(input$rand_dates_needed_in == "Yes"){
      rand_dates_needed_value = "y"
    } else if(input$rand_dates_needed_in == "No"){
      rand_dates_needed_value = "n"
    }
    if(input$ele_bad_sound_remove_in == "Yes"){
      ele_bad_sound_remove_value = "y"
    } else if(input$ele_bad_sound_remove_in == "No"){
      ele_bad_sound_remove_value = "n"
    }

    list(
      project_name = input$project_name_in,
      deployment_num = input$deployment_nums_in,
      detector_name = input$summary_detector_in,
      fileDurationMin = input$summary_fileDurationMin_in,
      output = parseDirPath(volumes, input$summary_folder_in),
      ele_tables = parseDirPath(volumes, input$selection_tables_folder_in),
      zero_txt = parseDirPath(volumes, input$zero_selection_tables_folder_in),
      sound_check_include = sound_check_include_value,
      sound_checks = sound_check_folder_value,
      use_only_sites_provided = use_only_sites_provided_value,
      site_lat_long = sites_lat_long_value,
      rand_dates_needed = rand_dates_needed_value,
      ele_bad_sound_remove = ele_bad_sound_remove_value
    )
  })

  #define function to write inputs into global variables
  write_to_global <- function(inputs_list) {
    for(name in names(inputs_list)) {
      assign(name, inputs_list[[name]], envir = .GlobalEnv)
      message(paste("Assigned", name, "=", inputs_list[[name]]))
    }
  }

  #################################
  #ACTION BUTTON OUTPUTS

  #BUTTON: check sound check info
  output$check_sound_check_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$sites_in, "Please input a Sites txt file!"),
      need(input$sound_path_in, "Please input a folder with sound files!")
    )

    #create table
    x = unlist(inputs_sound_check())
    x = c(x[1:3], input$sites_in$name, x[5:8])
    data.frame(
      Fields = c("Deployment name",
                 "Deployment number",
                 "Disk ID",
                 "Sites file",
                 "Sound path",
                 "File duration",
                 "Sample rate",
                 "File extension"),
      Inputs = x
    )
  }, hover = TRUE) |>
    bindEvent(input$check_sound_check_info_in)

  #BUTTON: run sound check
  observeEvent(input$run_sound_check_in, {
    write_to_global(inputs_sound_check())

    result <- sound.check(parseDirPath(volumes, input$sound_path_in))
    output$run_sound_check_output <- renderPrint({
      result
    })
  })

  #BUTTON: check exclude files info
  output$check_exclude_files_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$sites_in, "Please input a Sites txt file!"),
      need(input$sound_path_in, "Please input a folder with sound files!"),
      need(input$extra_sounds_in, "Please input a folder to exclude files!")
    )

    #create table
    x = unlist(inputs_sound_check())
    y = unlist(inputs_exclude_files())
    z = c(x[1:3], input$sites_in$name, x[5:8], y[1:3])
    data.frame(
      Fields = c("Deployment name",
                 "Deployment number",
                 "Disk ID",
                 "Sites file",
                 "Sound path",
                 "File duration",
                 "Sample rate",
                 "File extension",
                 "Extra sounds path",
                 "Have Swift files",
                 "Merge Swift files"),
      Inputs = z
    )
  }, hover = TRUE) |>
    bindEvent(input$check_exclude_files_info_in)

  #BUTTON: run exclude files
  observeEvent(input$run_exclude_files_in, {
    write_to_global(inputs_sound_check())
    write_to_global(inputs_exclude_files())

    result <- sound.exclude(parseDirPath(volumes, input$sound_path_in), parseDirPath(volumes, input$extra_sounds_in))
    output$run_exclude_files_output <- renderPrint({
      result
    })
  })

  #BUTTON: check rumble restructure info
  output$check_restructure_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$sites_in, "Please input a Sites txt file!")
    )

    #create table
    x = unlist(inputs_restructure())
    x = c(x[1:3], input$sites_in$name, x[5:8], input$detector_in, x[10:11])
    data.frame(
      Fields = c("Deployment name",
                 "Deployment number",
                 "Disk ID",
                 "Sites file",
                 "Sample rate",
                 "Three random days per week",
                 "Minimum 23 hours per day",
                 "Score column name",
                 "Detector",
                 "Detector score",
                 "Filter score"),
      Inputs = x
    )
  }, hover = TRUE) |>
    bindEvent(input$check_restructure_info_in)

  #BUTTON: run rumble restructure
  observeEvent(input$run_restructure_in, {
    write_to_global(inputs_restructure())

    result <- Rumble_Selection_Table_Restructure()
    rumble_count <- c(sum(as.integer(result[,3]), na.rm = TRUE),
                      sum(as.integer(result[,4]), na.rm = TRUE),
                      sum(as.integer(result[,5]), na.rm = TRUE))
    rumble_type <- c("Rumbles > 0.2",
                     "Rumbles > 0.4",
                     "Random Rumbles > 0.4")
    output$run_restructure_output <- renderTable({
      data.frame(
        Detection = rumble_type,
        Count = rumble_count
      )
    }, hover = TRUE)
  })

  #BUTTON: check gun restructure info
  output$check_gun_restructure_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$sites_in, "Please input a Sites txt file!")
    )

    #create table
    x = unlist(inputs_gun_restructure())
    x = c(x[1:3], input$sites_in$name, x[5:8])
    data.frame(
      Fields = c("Deployment name",
                 "Deployment number",
                 "Disk ID",
                 "Sites file",
                 "Sample rate",
                 "Detector",
                 "Detector score",
                 "Filter score"),
      Inputs = x
    )
  }, hover = TRUE) |>
    bindEvent(input$check_gun_restructure_info_in)

  #BUTTON: run gun restructure
  observeEvent(input$run_gun_restructure_in, {
    write_to_global(inputs_gun_restructure())

    result <- gunshot_Selection_Table_Restructure()
    gunshot_count <- c(sum(as.integer(result[,3]), na.rm = TRUE))
    gunshot_type <- c("Gunshots")
    output$run_gun_restructure_output <- renderTable({
      data.frame(
        Detection = gunshot_type,
        Count = gunshot_count
      )
    }, hover = TRUE)
  })

  #BUTTON: check general restructure info
  output$check_general_restructure_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$general_merge_in, "Please input a path to selection tables!")
    )

    #create table
    x = unlist(inputs_general_restructure())
    x = c(x[1], input$recursive_in)
    data.frame(
      Fields = c("Folder path",
                 "Tables organization"),
      Inputs = x
    )
  }, hover = TRUE) |>
    bindEvent(input$check_general_restructure_info_in)

  #BUTTON: run general restructure
  observeEvent(input$run_general_restructure_in, {
    write_to_global(inputs_general_restructure())

    result <- merge_selection_tables(parseDirPath(volumes, input$general_merge_in), recursive)
    output$run_general_restructure_output <- renderPrint({
      result
    })
  })

  #BUTTON: check summary info
  output$check_summary_info_output <- renderTable({
    #validate if all fields have inputs
    validate(
      need(input$summary_folder_in, "Please input a folder to output data summaries!"),
      need(input$selection_tables_folder_in, "Please input a folder containing selection tables!"),
      need(input$zero_selection_tables_folder_in, "Please input a folder containing zero-day selection tables!"),
      if(input$sound_check_include_in == "Yes"){
        need(input$sound_check_folder_in, "Do you want to include a sound check file?")
      },
      if(input$use_only_sites_provided_in == "Yes"){
        need(input$sites_lat_long_in, "Do you want to include a sites file?")
      }
    )

    #create table
    x = unlist(inputs_summary())
    if(x[11] != "N/A"){
      file_name = input$sites_lat_long_in$name
    } else{
      file_name = "N/A"
    }
    x = c(x[1:7],
          input$sound_check_include_in,
          x[9],
          input$use_only_sites_provided_in,
          file_name,
          input$rand_dates_needed_in,
          input$ele_bad_sound_remove_in)
    data.frame(
      Fields = c("Project name",
                 "Deployment number(s)",
                 "Detector used",
                 "File duration",
                 "Folder to output summaries",
                 "Folder containing selection tables",
                 "Folder containing zero-day selection tables",
                 "Use sound check files",
                 "Folder containing sound check files",
                 "Use sites file",
                 "Sites file with latitude and longitude",
                 "Random dates needed",
                 "Exclude sounds <23 hours"),
      Inputs = x
    )
  }, hover = TRUE) |>
    bindEvent(input$check_summary_info_in)

  #BUTTON: run summary
  observeEvent(input$run_summary_in, {
    write_to_global(inputs_summary())

    result <- data_summaries()
    output$run_summary_output <- renderPrint({
      result
    })
  })

}


####################################################################
# Run the app ----
shinyApp(ui = ui, server = server)
