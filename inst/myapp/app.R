library(sortable)
library(tidyverse)
library(purrr)
library(ggplot2)
library(svglite)

devtools::load_all("CuRveR")

`%||%` <- function(a, b) if (is.null(a)) b else a

modalSavePlot <- function() {
  modalDialog(
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg" )),
    numericInput("dpi", "Resolution", 300,  min = 0, max = 1200, step = 10),
    numericInput("width", "width", 100),
    numericInput("height", "height", 100),
    selectInput("units", "Units", choices = c("in", "cm", "mm", "px")),
    downloadButton("downloadPlot", "Download"),
  )
}

modalSaveData <- function() {
  modalDialog(
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("csv")),
    downloadButton("downloadData", "Download"),
  )
}

color_tile_from_0 <- function(...) {
  formattable::formatter("span", style = \(x) {formattable::style(display = "block",
                                                                  padding = "0 4px",
                                                                  `border-radius` = "4px",
                                                                  `background-color` = formattable::csscolor(formattable::gradient(c(0,as.numeric(x)),...))[2:length(x)])})
}


############################################################################
############################################################################
###                                                                      ###
###                                  UI                                  ###
###                                                                      ###
############################################################################
############################################################################

ui <- navbarPage(title = div(img(src='Logo.svg',
                             height = "100%")),
             windowTitle = "CuRveR",
             id = "current_tab",

    ##################################################################
    ##                            Footer                            ##
    ##################################################################



    ##################################################################
    ##                         Data loading                         ##
    ##################################################################

    tabPanel("Load Data", value = 1,

      fluidRow(
        column(6,
          radioButtons("file_type", "Type of input file(s):",
                            c("XLSX" = "xlsx",
                              "CSV" = "csv"))
        ),
        column(6,
          conditionalPanel("input.file_type == 'xlsx'",
                          radioButtons("excel_type", "Type of input file(s):",
                                       c("Multiples files"  = "file",
                                         "Multiples sheets" = "sheet")))
        )
      ),
      fluidRow(
        column(6,
          conditionalPanel("input.file_type == 'xlsx' && input.excel_type == 'sheet'",
                           fileInput("excel_file", "Select your XLSX file"))
        )
      ),


      fluidRow(
        column(4,
          numericInput("n_signals", "Numbers of signal(s) to analyse:",
                       value = 1, min = 1, max = 10, step = 1),
        )
      ),
      fluidRow(
        column(12,
           uiOutput("signals")
        )
      ),
      fluidRow(
        column(2, offset = 5,
          actionButton("load_data", "Load data", style="display:center-align")
        )
      )




    ),

    #################################################################
    ##                        Data overview                        ##
    #################################################################

    tabPanel("Overview", value = 2,
      fluidRow(
        column(3,
          uiOutput("signal_selector_overview")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("overview", height = "80vh")
        )
      ),
      fluidRow(
        column(3,
          actionButton("dl_overview", "Save")
        ),
        column(4, offset = 1,
          actionButton("goto_condition", "Next step")
        )
      )
    ),

    #################################################################
    ##                     Conditions & Blanks                     ##
    #################################################################

    tabPanel("Conditions", value = 3,
      fluidRow(
        column(6,
          div(style = 'margin-top:20px',
            fillRow(flex = c(NA, NA, NA),
              selectizeInput("whichcondition", label = NULL, choices = c("Example_condition")),
              actionButton("add_condition", NULL, icon = icon("plus")),
              actionButton("del_condition", NULL, icon = icon("trash-alt")),
            )
          )
         ),
        column(6,
          spsComps::textButton(textId = "condition_name", label = "", btn_label = "Rename", placeholder = "New name")
        )
      ),
      fluidRow(
        column(4,
          div(style = 'width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;', uiOutput("well_list"))
        ),
        column(4,
          div(style = 'width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;', uiOutput("replicates"))
        ),
        column(4,
          div(style = 'width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;', uiOutput("blanks"))
        ),
      ),
      fluidRow(
        column(12,
          plotOutput("condition_recap", height = "25vh")
        )
      ),
      fluidRow(
        column(3,
          downloadButton("download_condition_template", "Save as template", style = 'margin-top:25px')
        ),
        column(3,
          fileInput("condition_file", "Import template")
        )
      )

    ),

    #################################################################
    ##                           Cutoffs                           ##
    #################################################################

    tabPanel("Cutoffs", value = 4,
      fluidRow(
        column(4,
          uiOutput("signal_selector_cutoff")
        ),
        column(4,
          uiOutput("well_selector_cutoff")
        ),
        column(4,
          checkboxInput("same_cutoff", "Same cutoff for all wells")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plot",
                     dblclick = "cutoff_click",
                     brush = brushOpts("cutoff_brush",
                                       direction = "x",
                                       resetOnNew = TRUE,
                                       fill = "#3DB2FF",
                                       opacity = .15))
        )
      )
    ),

    #################################################################
    ##                             Fit                             ##
    #################################################################

    tabPanel("Fit", value = 5,
      fluidRow(
        column(4,
          selectInput("optim_method", "Model method", choices = c("Richard"))
        ),
        column(4,
          selectInput("loss_fct", "Loss function", choices = c("LAD", "OLS"))
        ),
        column(4,
          selectInput("optim_method", "Optimization method", choices = c("GA"))
        )
      ),
      fluidRow(
        column(4, offset = 4,
          actionButton("fit", "Fit")
        )
      ),
      fluidRow(
        column(12,
          div(style = 'width:100%;overflow-x: scroll;height:15vh;overflow-y: scroll;',
            formattable::formattableOutput("fit_data")
          )
        )
      ),
      fluidRow(
        column(3,
          actionButton("dl_performance", "Save")
        )
      ),
      fluidRow(
        column(12,
          div(style = 'width:100%;overflow-x: scroll;height:30vh;overflow-y: scroll;',
            formattable::formattableOutput("data_by_condition")
          )
        )
      ),
      fluidRow(
        column(3,
          actionButton("dl_data", "Save")
        )
      ),
      fluidRow(
        column(12,
         div(style = 'width:100%;overflow-x: scroll;height:15vh;overflow-y: scroll;',
             formattable::formattableOutput("data_recap")
         )
        )
      ),

    ),

    #################################################################
    ##                            Plots                            ##
    #################################################################

    tabPanel("Plots", value = 6,
      fluidRow(
        column(3,
          radioButtons("plot_type", "Plot Type", choices = c("Metric comparison" = "metric_comparison",
                                                             "Metric Visualization" = "metric_visualization",
                                                             "Data Comparison" = "data_comparison"))
        ),
        column(3,
          conditionalPanel("input.plot_type == 'metric_comparison'",
            radioButtons("metric_to_compare", "Metric",
                         choiceValues = c("r_max", "p_max", "p_min", "s"),
                         choiceNames = c("r max",
                                         "p max",
                                         "p min",
                                         "s"))
          ),
          conditionalPanel("input.plot_type == 'metric_visualization'",
            checkboxGroupInput("metric_to_visualize", "Metric",
                          choiceValues = c("r_max", "p_max", "p_min", "s", "fit", "data_point"),
                          choiceNames = c("r max",
                                          "p max",
                                          "p min",
                                          "s",
                                          "Fit",
                                          "Data points"),
                          selected = c("r_max", "p_max", "p_min", "s", "fit", "data_point"))
          ),
          conditionalPanel("input.plot_type == 'data_comparison'",
            checkboxGroupInput("data_to_compare", "Plot features",
                               choices = c("Mean"        = "mean",
                                           "SD as bar"   = "sd_bar",
                                           "SD as halo"  = "sd_halo",
                                           "Fit"         = "fit",
                                           "Data points" = "data_points"),
                               selected = c("mean", "sd_halo","fit", "data_points"))
            )
          ),
        column(3,
          uiOutput("signal_selector_plot")
        ),
        column(3,
          uiOutput("condition_selector_plot")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("final_plot"),
        )
      ),
      fluidRow(
        column(3,
          actionButton("dl_final_plot", "Save")
        )
      )
    )
  )

############################################################################
############################################################################
###                                                                      ###
###                                SERVER                                ###
###                                                                      ###
############################################################################
############################################################################

server <- function(input, output, session){

  rvs <- reactiveValues(conditions = list(Example_condition = list(replicates = character(), blanks = character())))

  max_tab <- 8

  ##################################################################
  ##                            Footer                            ##
  ##################################################################

  ##------------------------------
  ##  Previous/Next page buttons
  ##------------------------------

  observeEvent(input$next_tab, {
    current_tab <- as.numeric(input$current_tab)
    if (current_tab < max_tab) {
      updateNavbarPage(session, "current_tab", selected = paste(current_tab + 1))
    }
  })

  ##################################################################
  ##                         Data loading                         ##
  ##################################################################

  ##-------------------------
  ##  Map Name - File/Sheet
  ##-------------------------

  observeEvent(input$n_signals, {

    output$signals <- renderUI({

      if (input$file_type == "xlsx" && input$excel_type == "sheet") {

        req(input$excel_file)

        purrr::map(
          1:input$n_signals,
          \(x) {
            fluidRow(
              column(4,
                textInput(inputId = paste0("signal_", x),
                          label = NULL,
                          value = input[[paste0("sheet_", x)]] %||% "")
              ),
              column(4,
                div(style = "display:inline-block; float:right",
                  selectInput(inputId = paste0("sheet_", x),
                              label = NULL,
                              choices = readxl::excel_sheets(input$excel_file[["datapath"]]),
                              selected = input[[paste0("sheet_", x)]] %||% NULL)
                )
              ),
              ignoreInit = TRUE,
            )
          })
        }

      else {

      purrr::map(
        1:input$n_signals,
        \(x) {
          fluidRow(
            column(4,
              textInput(inputId = paste0("signal_", x),
                        label = NULL,
                        value = input[[paste0("file_", x)]] %||% "")
            ),
            column(4,
              fileInput(inputId = paste0("file_", x),
                        label = NULL)
            )
          )
        })
      }
    })
  })

  ##-------------
  ##  Load Data
  ##-------------

  observeEvent(input$load_data, {

    if (input$file_type == "csv") {
      rvs$data <-
        purrr::map(
          1:input$n_signals,
          \(x) {read_csv(file = input[[paste0("file_", x)]], col_names = TRUE) |> mutate(signal = input[[paste0("signal_", x)]])
          }) |>
        dplyr::bind_rows()
    }

    else if (input$file_type == "xlsx" && input$excel_type == "file") {
      rvs$data <-
        purrr::map(
          1:input$n_signals,
          \(x) {readxl::read_xlsx(path = input$excel_file[["datapath"]]) |> mutate(signal = input[[paste0("signal_", x)]])
          }) |>
        dplyr::bind_rows()
    }

    else if (input$file_type == "xlsx" && input$excel_type == "sheet") {
      rvs$data <-
        purrr::map(
          1:input$n_signals,
          \(x) {readxl::read_xlsx(path = input$excel_file[["datapath"]],
                                  sheet = input[[paste0("sheet_", x)]]) |> mutate(signal = input[[paste0("signal_", x)]])
            }) |>
        dplyr::bind_rows()

    }

    # Tidy loaded data

    rvs$data |>
      select(-matches(regex("T°"))) |>
      clean_time(Time) |>
      format_wellplate_long() |>
      extract(well, into = c("row", "col"), regex = "(\\w)(\\d+)", remove = FALSE) |>
      mutate(col = as.numeric(col),
             left_cutoff = min(Time),
             right_cutoff = max(Time)) -> rvs$data

    rvs$all_wells <<- isolate(unique(rvs$data[["well"]]))
    rvs$all_signals <<- isolate(unique(rvs$data[["signal"]]))

    updateNavbarPage(session, "current_tab", selected = "2")
  })

  #################################################################
  ##                        Data overview                        ##
  #################################################################

  ##-------------------
  ##  Signal Selector
  ##-------------------

  output$signal_selector_overview <- renderUI({
    req(rvs$data)
    selectInput("selected_signal_overview", label = NULL, choices = unique(rvs$data[["signal"]]))
  })



  ##-----------------------
  ##  Plot Plate Overview
  ##-----------------------

  overview <- reactive({
    req(rvs$data, input$selected_signal_overview)

    rvs$data |>
      filter(signal == input$selected_signal_overview) |>
      ggplot(aes(x = Time, y = value)) +
      geom_line() +
      facet_grid(col ~ row) +
      theme_minimal()
  })

  output$overview <- renderPlot({
    overview()
  })

  observeEvent(input$dl_overview,{
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0(input$filename, ".", input$filetype) },
      content = function(file) { ggsave(file, plot = overview(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi) }
    )
    showModal(modalSavePlot())
  })

  observeEvent(input$goto_condition,{
    updateNavbarPage(session, "current_tab", selected = "3")
  })
  #################################################################
  ##                     Conditions & Blanks                     ##
  #################################################################

  wells <- reactive({unique(rvs$data[["well"]])})

  ##-----------------
  ##  Add condition
  ##-----------------

  observeEvent(input$add_condition, {

    new_condition <- ids::adjective_animal(max_len = 15)
    rvs$conditions[[new_condition]] <- list(replicates = character(), blanks = character())

    updateSelectizeInput(session, 'whichcondition',
                         label    = NULL,
                         choices  = names(rvs$conditions),
                         server   = TRUE,
                         selected = new_condition)
  })

  ##------------------
  ##  Save condition
  ##------------------

  output$download_condition_template <- downloadHandler(
      filename = function() {
        paste("Test.Rds", sep = "")
      },
      content = function(file) {
        saveRDS(rvs$conditions, file)
      })


  ##------------------
  ##  Load condition
  ##------------------

  observeEvent(input$condition_file, {


    req(input$condition_file)

    rvs$conditions <<-readRDS(input$condition_file[["datapath"]])

    output$replicates <- renderUI({replicates_UI()})
    output$blanks <- renderUI({blanks_UI()})

    updateSelectizeInput(session, 'whichcondition',
                         label    = NULL,
                         choices  = names(rvs$conditions),
                         server   = TRUE,
                         selected = names(rvs$conditions)[1])
  })

  ##----------------------------
  ##  Delete current condition
  ##----------------------------

  observeEvent(input$del_condition, {

    current_condition <- input$whichcondition

    rvs$conditions[[current_condition]] <- NULL

    selection <- names(rvs$conditions)[length(rvs$conditions)]

    updateSelectizeInput(session, 'whichcondition',
                         label    = NULL,
                         choices  = names(rvs$conditions),
                         server   = TRUE,
                         selected = selection)
  })

  ##----------------------------
  ##  Rename current condition
  ##----------------------------

  observeEvent(input$condition_name_btn, {

    current_condition <- input$whichcondition
    new_name <- input$condition_name

    rvs$conditions[[new_name]] <<- rvs$conditions[[current_condition]]
    rvs$conditions[[current_condition]] <- NULL


    updateSelectizeInput(session, 'whichcondition',
                         label    = NULL,
                         choices  = names(rvs$conditions),
                         server   = TRUE,
                         selected = new_name)

  })

  ##-------------
  ##  Well list
  ##-------------

  output$well_list <- renderUI({

    output$replicates <- renderUI({replicates_UI()})
    output$blanks <- renderUI({blanks_UI()})

    rank_list(text     = "Wells",
              labels   = wells(),
              input_id = "well_var",
              options  = sortable_options(group = list(name = "sort_conditions_group",
                                                       put   = TRUE,
                                                       pull  = "clone"),
                                          multiDrag = TRUE,
                                          onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                                          ))

  })

  ##-----------------------------------
  ##  Replicates of current condition
  ##-----------------------------------


    replicates_UI <- reactive({
      req(input$whichcondition)

      rank_list(text     = "Replicates",
                labels   = rvs$conditions[[input$whichcondition]][["replicates"]],
                input_id = input$whichcondition,
                options  = sortable_options(group = list(name = "sort_conditions_group",
                                                         put  = TRUE,
                                                         pull = TRUE),
                                            multiDrag = TRUE))
  })

  blanks_UI <- reactive({
    req(input$whichcondition)

    id <- paste(input$whichcondition, "blank", sep = "_")

    rank_list(text     = "Blanks",
              labels   = rvs$conditions[[input$whichcondition]][["blanks"]],
              input_id = id,
              options  = sortable_options(group = list(name = "sort_conditions_group",
                                                       put  = TRUE,
                                                       pull = TRUE),
                                          multiDrag = TRUE))
  })

  observeEvent({
    input$whichcondition
    input$load_template},{


    output$replicates <- renderUI({replicates_UI()})

  ##-------------------------------
  ##  Blanks of current condition
  ##-------------------------------

    output$blanks <- renderUI({blanks_UI()})
  })

  ##--------------------------------
  ##  Update conditions and blanks
  ##--------------------------------

  observe({

    names <- names(rvs$conditions)

    purrr::walk2(names, paste(names, "blank", sep = "_"),
                 \(x, y) {rvs$conditions[[x]]$replicates <<- unique(input[[x]]) %||% rvs$conditions[[x]]$replicates
                          rvs$conditions[[x]]$blanks     <<- unique(input[[y]]) %||% rvs$conditions[[x]]$blanks })

  }, priority = 2)


  condition_overview <- reactive({
    req(rvs$conditions)

    rvs$conditions |>
      rrapply::rrapply(how = "melt") |>
      unnest_longer(value) |>
      rename(condition =  L1, type = L2, well = value) |>
      extract(well, into = c("row", "col"), regex = "(\\w)(\\d+)", remove = FALSE) |>
      drop_na(well, row, col) |>
      mutate(col = as.numeric(col)) |>
      ggplot(aes(col, row)) +
      ggpattern::geom_tile_pattern(aes(pattern_density = type, fill = condition),
                                   pattern = "stripe",
                                   pattern_angle = 45,
                                   pattern_fill = "black") +
      ggpattern::scale_pattern_density_discrete(breaks = c(0.5, 0)) +
      scale_x_discrete(breaks=factor(1:12), limits = factor(1:12)) +
      scale_y_discrete(breaks=factor(LETTERS[8:1]), limits = factor(LETTERS[8:1])) +
      theme_minimal()
  })

    output$condition_recap <- renderPlot({
      condition_overview()
    })


  #################################################################
  ##                           Cutoffs                           ##
  #################################################################

  output$signal_selector_cutoff <- renderUI({
    req(rvs$all_signals)
    selectInput("selected_signal_cutoff", label = NULL, choices = rvs$all_signals)
  })

  output$well_selector_cutoff <- renderUI({
    req(rvs$all_wells)
    selectInput("selected_well_cutoff", label = NULL, choices = rvs$all_wells)
  })


  observeEvent({input$cutoff_brush
                input$same_cutoff},{

    req(rvs$data, input$selected_signal_cutoff, input$selected_well_cutoff)

    if (input$same_cutoff == TRUE) {

      isolate({
        rvs$data |> rowwise() |>
          mutate(left_cutoff  = input$cutoff_brush[["xmin"]] %||% rvs$last_left_cutoff  %||% left_cutoff,
                 right_cutoff = input$cutoff_brush[["xmax"]] %||% rvs$last_right_cutoff %||% right_cutoff) |>
          ungroup() -> rvs$data
      })
    }

    else {

      req(input$cutoff_brush)

      isolate({
        rvs$data |> rowwise() |>
          mutate(left_cutoff  = if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmin"]], left_cutoff ),
                 right_cutoff = if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmax"]], right_cutoff)) |>
        ungroup() -> rvs$data
      })
    }


    req(input$cutoff_brush)

    rvs$last_left_cutoff <- input$cutoff_brush[["xmin"]]
    rvs$last_right_cutoff <- input$cutoff_brush[["xmax"]]


  })

  output$plot <- renderPlot({

    req(rvs$data, input$selected_signal_cutoff, input$selected_well_cutoff)

    max_value <- max(rvs$data[["value"]])
    max_time  <- max(rvs$data[["Time"]])

    annot <- rvs$data |>
      filter(signal == input$selected_signal_cutoff,
             well   == input$selected_well_cutoff) |>
      summarise(left_cutoff = median(left_cutoff),
                right_cutoff = median(right_cutoff),
                value = max(value))

    rvs$data |>
      filter(signal == input$selected_signal_cutoff,
             well   == input$selected_well_cutoff) |>
      ggplot(aes(x = Time, y = value)) +
      annotate(geom = "rect",
               ymin = 0,
               ymax = annot$value + 0.2 * annot$value,
               xmin = annot$left_cutoff,
               xmax = annot$right_cutoff,
               alpha = 0.15,
               fill = "#3DB2FF") +
      geom_line(color = "#F9B208", size = 1) +
      geom_point(color = "white",fill = "#F98404",  size = 3.5, shape = 23) +
      geom_vline(aes(xintercept = median(left_cutoff)),
                 size = 1, linetype = "dashed") +
      geom_vline(aes(xintercept = median(right_cutoff)),
                 size = 1, linetype = "dashed") +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(breaks = seq(0, max_time, by = 2)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal() +
      theme(text = element_text(size=20))
  })

  #################################################################
  ##                             Fit                             ##
  #################################################################

  observeEvent(input$fit, {
    req(rvs$data, rvs$conditions)

    condition <- rvs$conditions |>
      rrapply::rrapply(how = "melt") |>
      unnest_longer(value) |>
      rename(condition =  L1, type = L2, well = value)

    rvs$data |>
      full_join(condition, by = c("well" = "well")) |>
      rowwise() |>
      mutate(in_cutoff = between(Time, left_cutoff, right_cutoff)) |>
      group_by(condition, signal) |>
      mutate(blank = mean(value[in_cutoff & type == "blanks"])) |>
      rowwise() |>
      mutate(value = if_else(is.na(value - blank), value, value - blank)) |>
      filter(type != "blanks") |>
      ungroup() -> data_to_fit

    data_to_fit |>
      rowwise() |>
      filter(between(Time, left_cutoff, right_cutoff)) |>
      fit_data(c(signal, well), value, Time, method = "LAD") |>
      select(signal, well, p_max, p_min, r_max, s) -> rvs$parameters_by_well

    data_to_fit |>
      rowwise() |>
      filter(between(Time, left_cutoff, right_cutoff)) |>
      fit_data(c(signal, condition), value, Time, method = "LAD") |>
      select(signal, condition, p_max, p_min, r_max, s) -> rvs$parameters_by_condition

    data_to_fit |>
        full_join(rvs$parameters_by_well, by = c("well" = "well", "signal" = "signal")) |>
        mutate(fit = richard(Time, p_max, p_min, r_max, s),
               doubling_time = log(2)/log(1+r_max/p_max)) -> rvs$data_by_well

    data_to_fit |>
      full_join(rvs$parameters_by_condition, by = c("condition" = "condition", "signal" = "signal")) |>
      mutate(fit = richard(Time, p_max, p_min, r_max, s),
             doubling_time = log(2)/log(1+r_max/p_max)) -> rvs$data_by_condition

    rvs$data_by_condition |>
      group_by(signal, condition) |>
      summarise(r_max = median(r_max), p_max = median(p_max), p_min = median(p_min), s = median(s)) -> rvs$recap



    rvs$data_by_condition |>
      rowwise() |>
      mutate(absolute_error = abs(value - fit),
             error2 = (value - fit)^2) |>
      group_by(signal, condition) |>
      summarise(MAE = mean(absolute_error),
                MSE = mean(error2),
                MSRE = MSE/(sd(value)^2),
                RMSE = sqrt(mean(absolute_error^2)),
                SRMSE = RMSE/sd(value),
                MAPE = mean(absolute_error/abs(value))*100,
                VEcv = (1 - (sum(error2) / sum((value-mean(fit))^2))) * 100,
                E1 = (1 - (sum(absolute_error) / sum(abs(value-mean(fit))))) * 100,
                d_r = if_else(E1 >= 0, E1, (((sum(abs(value-mean(fit)))) / sum(absolute_error)) - 1) * 100)) -> rvs$perfomances


    rvs$all_conditions <- unique(rvs$data_by_condition[["condition"]])

    output$fit_data <- formattable::renderFormattable({
      formattable::formattable(rvs$perfomances,
                               list(VEcv = color_tile_from_0("lightpink", "lightblue"),
                                    d_r = color_tile_from_0("lightpink", "lightblue")))

    })

    output$data_by_condition <- formattable::renderFormattable({
      formattable::formattable(rvs$data_by_condition)
    })

    output$data_recap <- formattable::renderFormattable({
      formattable::formattable(rvs$recap)
    })

  })


  observeEvent(input$dl_performance,{
    output$downloadData <- downloadHandler(
      filename = function() { paste0(input$filename, ".", input$filetype) },
      content = function(file) {
        if (input$filetype == "csv") {
          write_csv(rvs$perfomances, file = file, col_names = TRUE)
        }
      }
    )
    showModal(modalSaveData())
  })

  observeEvent(input$dl_data,{
    output$downloadData <- downloadHandler(
      filename = function() { paste0(input$filename, ".", input$filetype) },
      content = function(file) {
        if (input$filetype == "csv") {
          write_csv(rvs$data_by_condition, file = file, col_names = TRUE)
        }
      }
    )
    showModal(modalSaveData())
  })
  #################################################################
  ##                            Plots                            ##
  #################################################################

  output$signal_selector_plot <- renderUI({
    req(rvs$all_signals)
    radioButtons("selected_signal_plot", label = "Signal", choices = rvs$all_signals)
  })

  output$condition_selector_plot <- renderUI({
    req(rvs$all_signals)
    checkboxGroupInput("selected_condition_plot", label = "Conditions", choices = rvs$all_conditions, selected = rvs$all_conditions)
  })

  final_plot <- reactive({
    req(rvs$data_by_condition)

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      filter(condition %in% input$selected_condition_plot) |>
      drop_na(condition) -> plot_data

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      filter(condition %in% input$selected_condition_plot) |>
      drop_na(condition) |>
      group_by(condition, Time) |>
      summarise(sd = sd(value), mean = mean(value)) -> summarized_plot_data

    if (input$plot_type == "metric_comparison") {

      plot_data |>
        ggplot(aes(y = condition, x = !!as.symbol(input$metric_to_compare))) +
        geom_point(size = 5, color = "#e8871a") +
        theme_minimal()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p

    }

    else if (input$plot_type == "metric_visualization") {

      print("metric_visualization")

      plot_data |>
        mutate(intercept = richard(s, p_max, p_min, r_max, s) - r_max * s) |>
        ggplot(aes(x = Time, y = value, group = well)) -> p


      if ("data_point" %in% input$metric_to_visualize) {
        p <- p + geom_point(aes(x = Time, y = value, group = well),alpha = .4,  color = '#7570b3')
      }

      if ("p_max" %in% input$metric_to_visualize) {
        p <- p + geom_hline(aes(yintercept = p_max), linetype = "dashed", color = "#1B9E77")
      }

      if ("p_min" %in% input$metric_to_visualize) {
        p <- p + geom_hline(aes(yintercept = p_min), linetype = "dashed", color = "#1B9E77")
      }

      if ("s" %in% input$metric_to_visualize) {
        p <- p + geom_segment(aes(x = s, xend = s,
                                  y = 0, yend = richard(s, p_max, p_min, r_max, s)),
                                  linetype = "dashed",
                                  color = "#1B9E77")
      }

      if ("r_max" %in% input$metric_to_visualize) {
        p <- p + geom_line(aes(y = linear(Time, r_max, intercept)), linetype = "dashed", color = "#1B9E77")
      }


      if ("fit" %in% input$metric_to_visualize) {
        p <- p + geom_line(aes(y = fit), color = 'black')
      }

      p <- p + scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
        facet_wrap(~condition) +
        theme_minimal()

    }

    else if (input$plot_type == "data_comparison") {



      plot_data |>
      ggplot(aes(x = Time, color = condition, fill = condition)) -> p

      if ("data_points" %in% input$data_to_compare) {
        p <- p + geom_point(aes(y = value, group = well), size = 2, alpha = .4)
      }

      if ("sd_halo" %in% input$data_to_compare) {
        p <- p + geom_ribbon(data = summarized_plot_data,
                             aes(ymin = mean - sd,
                                 ymax = mean + sd),
                             alpha = .2)
      }

      if ("sd_bar" %in% input$data_to_compare) {
        p <- p + geom_errorbar(data = summarized_plot_data,
                             aes(ymin = mean - sd,
                                 ymax = mean + sd),
                             alpha = .4)
      }

      if ("mean" %in% input$data_to_compare) {
        p <- p + geom_line(data = summarized_plot_data,
                           aes(y = mean),
                           size = 1,
                           linetype = "dashed")
      }

      if ("fit" %in% input$data_to_compare) {
        p <- p + geom_line(aes(y = fit, group = well), size = 1)
      }


      p <- p +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
        theme_minimal()

    }

    p

  })

  output$final_plot <- renderPlot({
    final_plot()
  })

  observeEvent(input$dl_final_plot,{
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0(input$filename, ".", input$filetype) },
      content = function(file) { ggsave(file, plot = final_plot(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi) }
    )
    showModal(modalSavePlot())
  })

}


###########################################################################
###########################################################################
###                                                                     ###
###                                 APP                                 ###
###                                                                     ###
###########################################################################
###########################################################################

shinyApp(ui, server)


