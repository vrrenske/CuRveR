library(shiny)
library(sortable)
library(shinythemes)
library(tidyverse)
library(spsComps)
source("R/hello.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

############################################################################
############################################################################
###                                                                      ###
###                                  UI                                  ###
###                                                                      ###
############################################################################
############################################################################

ui <-
  navbarPage("cuRveR", id = "current_tab",

    ##################################################################
    ##                            Footer                            ##
    ##################################################################

    footer = fluidRow(
      column(2, offset = 1,  actionButton("previous_tab", "Previous step")),
      column(2, offset = 6,  actionButton("next_tab", "Next step"))
    ),

    ##################################################################
    ##                         Data loading                         ##
    ##################################################################

    tabPanel("Load Data", value = 1,

      radioButtons("file_type", "Type of input file(s):",
                  c("XLSX" = "xlsx",
                    "CSV" = "csv")),

      conditionalPanel("input.file_type == 'xlsx'",
        radioButtons("excel_type", "Type of input file(s):",
                      c("Multiples files"  = "file",
                        "Multiples sheets" = "sheet"))),

      conditionalPanel("input.file_type == 'xlsx' && input.excel_type == 'sheet'",
        fileInput("excel_file", "Select your XLSX file")),

      fluidRow(
        numericInput("n_signals", "Numbers of signal(s) to analyse:",
                     value = 1, min = 1, max = 10, step = 1),
        actionButton("change_signals", "OK")
      ),

      uiOutput("signals"),

      actionButton("load_data", "Load data")

    ),

    #################################################################
    ##                        Data overview                        ##
    #################################################################

    tabPanel("Overview", value = 2,
      uiOutput("signal_selector_overview"),
      plotOutput("overview")
    ),

    #################################################################
    ##                     Conditions & Blanks                     ##
    #################################################################

    tabPanel("Conditions", value = 3,
      fluidRow(
        column(4,
          h3("Wells"),
          div(style = 'width:100%;overflow-x: scroll;height:90vh;overflow-y: scroll;', uiOutput("well_list"))
       ),
        column(8,
          h3("Conditions"),
          fluidRow(
            column(3,
              selectizeInput("whichcondition", label = NULL, choices = NULL)
            ),
            column(5,
              textButton(textId = "condition_name", label = "", btn_label = "Rename", placeholder = "New name", class = "btn-primary")
            ),
            column(2,
              actionButton("add_condition", "+", class = "btn-primary")
            ),
            column(2,
              actionButton("del_condition", "Del")
            )
          ),
          fluidRow(
            column(2,
              actionButton("save_condition", "Save")
            ),
            column(2,
              fileInput("condition_file", "Import previous conditions")
            ),
            column(2,
              actionButton("load_condition", "Load")
            )
          ),
          fluidRow(
            column(6,
              uiOutput("replicates")
            ),
            column(6,
              uiOutput("blanks")
            ),
          ),
          fluidRow(
            plotOutput("condition_recap")
          )
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
          selectInput("loss_fct", "Loss function", choices = c("LAD", "OLS"))
        ),
        column(4,
          selectInput("optim_method", "Optimization method", choices = c("GA"))
        ),
        column(4,
          actionButton("fit", "Fit", style = 'margin-top:25px')
        )
      )
    ),

    #################################################################
    ##                            Plots                            ##
    #################################################################

    tabPanel("Plots", value = 6,
      fluidRow(
        uiOutput("signal_selector_plot")
      ),
      fluidRow(
        plotOutput("condition_params")
      ),
      fluidRow(
        plotOutput("condition_compare")
      )
    ),

    #################################################################
    ##                        Quality check                        ##
    #################################################################

    tabPanel("Quality check", value = 7,),

    tabPanel("Save", value = 8,),
)

############################################################################
############################################################################
###                                                                      ###
###                                SERVER                                ###
###                                                                      ###
############################################################################
############################################################################

server <- function(input, output, session){

  rvs <- reactiveValues(conditions = list())

  max_tab <- 8

  ##################################################################
  ##                            Footer                            ##
  ##################################################################

  ##------------------------------
  ##  Previous/Next page buttons
  ##------------------------------

  observeEvent(input$previous_tab, {
    current_tab <- as.numeric(input$current_tab)
    if (current_tab > 1) {
      updateNavbarPage(session, "current_tab", selected = paste(current_tab - 1))
    }
  })

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

  observeEvent(input$change_signals, {

    output$signals <- renderUI({

      if (input$file_type == "xlsx" && input$excel_type == "sheet") {

        req(input$excel_file)

        purrr::map(
          1:input$n_signals,
          \(x) {
            fluidRow(
              column(6, textInput(inputId = paste0("signal_", x), label = NULL,value = input[[paste0("signal_", x)]] %||% "", placeholder = paste0("signal ", x))),
              column(6, selectInput(inputId = paste0("sheet_", x), label = NULL, choices = readxl::excel_sheets(input$excel_file[["datapath"]]), selected = input[[paste0("sheet_", x)]] %||% NULL)))
          })
        }

      else {

      purrr::map(
        1:input$n_signals,
        \(x) {
          fluidRow(
            column(6, textInput(inputId = paste0("signal_", x), label = NULL, value = input[[paste0("signal_", x)]] %||% "", placeholder = paste0("signal ", x))),
            column(6, fileInput(inputId = paste0("file_", x),   label = NULL)))
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
        bind_rows()
    }

    else if (input$file_type == "xlsx" && input$excel_type == "file") {
      rvs$data <-
        purrr::map(
          1:input$n_signals,
          \(x) {readxl::read_xlsx(path = input$excel_file[["datapath"]]) |> mutate(signal = input[[paste0("signal_", x)]])
          }) |>
        bind_rows()
    }

    else if (input$file_type == "xlsx" && input$excel_type == "sheet") {
      rvs$data <-
        purrr::map(
          1:input$n_signals,
          \(x) {readxl::read_xlsx(path = input$excel_file[["datapath"]],
                                  sheet = input[[paste0("sheet_", x)]]) |> mutate(signal = input[[paste0("signal_", x)]])
            }) |>
        bind_rows()

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

  output$overview <- renderPlot({
    req(rvs$data, input$selected_signal_overview)

    rvs$data |>
      filter(signal == input$selected_signal_overview) |>
      ggplot(aes(x = Time, y = value)) +
      geom_line() +
      facet_grid(col ~ row) +
      theme_minimal()

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

  observeEvent(input$save_condition, {

    downloadHandler(
      filename = function() {
        paste("Test.Rds", sep = "")
      },
      content = function(file) {
        saveRDS(rvs$conditions, file)
      })

  })

  ##------------------
  ##  Load condition
  ##------------------

  observeEvent(input$load_condition, {

    req(input$condition_file)

    rvs$conditions <<-readRDS(input$condition_file[["datapath"]])

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

    rank_list(text     = NULL,
              labels   = wells(),
              input_id = "well_var",
              options  = sortable_options(group = list(name = "sort_conditions_group",
                                                       put   = FALSE,
                                                       pull  = "clone"),
                                          multiDrag = TRUE))
  })

  ##-----------------------------------
  ##  Replicates of current condition
  ##-----------------------------------

  output$replicates <- renderUI({

    req(input$whichcondition)

    rank_list(text     = "Replicates",
              labels   = rvs$conditions[[input$whichcondition]][["replicates"]],
              input_id = input$whichcondition,
              options  = sortable_options(group = list(name = "sort_conditions_group",
                                                       put  = TRUE,
                                                       pull = TRUE),
                                          multiDrag = TRUE
              ))

  })

  ##-------------------------------
  ##  Blanks of current condition
  ##-------------------------------

  output$blanks <- renderUI({

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

  ##--------------------------------
  ##  Update conditions and blanks
  ##--------------------------------

  observe({

    names <- names(rvs$conditions)

    purrr::walk2(names, paste(names, "blank", sep = "_"),
                 \(x, y) {rvs$conditions[[x]]$replicates <<- unique(input[[x]]) %||% rvs$conditions[[x]]$replicates
                          rvs$conditions[[x]]$blanks     <<- unique(input[[y]]) %||% rvs$conditions[[x]]$blanks })

  }, priority = 2)


  output$condition_recap <- renderPlot({

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
      scale_x_continuous(expand = c(0, 0),
                         breaks = seq(0, max_time, by = 2)) +
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
      group_by(condition, Time) |>
      mutate(blank = mean(value[type == "blanks"])) |>
      mutate(value = if_else(is.na(value - blank), value, value - blank)) |>
      filter(type != "blanks") |>
      rowwise() |>
      filter(between(Time, left_cutoff, right_cutoff)) |>
      ungroup() -> data_to_fit

    data_to_fit |>
      fit_data(c(signal, well), value, Time, method = "LAD") |>
      select(signal, well, p_max, p_min, r_max, s) -> rvs$parameters_by_well

    data_to_fit |>
      fit_data(c(signal, condition), value, Time, method = "LAD") |>
      select(signal, condition, p_max, p_min, r_max, s) -> rvs$parameters_by_condition

    data_to_fit |>
        full_join(rvs$parameters_by_well, by = c("well" = "well", "signal" = "signal")) |>
        mutate(fit = richard(Time, p_max, p_min, r_max, s),
               doubling_time = log(2)/log(1+r_max/p_max)) -> rvs$data_by_well

    print(rvs$data_by_well)

    data_to_fit |>
      full_join(rvs$parameters_by_condition, by = c("condition" = "condition", "signal" = "signal")) |>
      mutate(fit = richard(Time, p_max, p_min, r_max, s),
             doubling_time = log(2)/log(1+r_max/p_max)) -> rvs$data_by_condition

    print(rvs$data_by_condition |> rowwise() |>
            mutate(absolute_error = abs(value - fit),
                   error2 = (value - fit)^2) |>
            group_by(signal, condition) )

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

    print(rvs$perfomances)


  })

  #################################################################
  ##                            Plots                            ##
  #################################################################

  output$signal_selector_plot <- renderUI({
    req(rvs$all_signals)
    selectInput("selected_signal_plot", label = NULL, choices = rvs$all_signals)
  })

  output$condition_params <- renderPlot({
    req(rvs$data_by_condition)

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      drop_na(condition) |>
      mutate(intercept = richard(s, p_max, p_min, r_max, s) - r_max * s) |>
      ggplot(aes(x = Time, y = value, group = well)) +
      geom_point(alpha = .4,  color = '#7570b3') +
      geom_hline(aes(yintercept = p_max), linetype = "dashed", color = "#1B9E77")+
      geom_hline(aes(yintercept = p_min), linetype = "dashed", color = "#1B9E77")+
      geom_segment(aes(x = s, xend = s,
                       y = p_min, yend = richard(s, p_max, p_min, r_max, s)),
                   linetype = "dashed",
                   color = "#1B9E77") +
      geom_segment(aes(x = s, xend = max(Time),
                       y = richard(s, p_max, p_min, r_max, s), yend = richard(s, p_max, p_min, r_max, s)),
                   linetype = "dashed",
                   color = "#1B9E77") +
      geom_line(aes(y = linear(Time, r_max, intercept)), linetype = "dashed", color = "#1B9E77") +
      geom_line(aes(y = fit), color = 'black') +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
      facet_wrap(~condition) +
      theme_minimal()

  })



  output$condition_compare <- renderPlot({
    req(rvs$data_by_condition)

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      drop_na(condition) |>
      group_by(condition, Time) |>
      summarise(sd = sd(value), mean = mean(value)) -> summarized

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      group_by(condition) |>
      ggplot(aes(x = Time, color = condition, fill = condition)) +
        geom_point(aes(y = value, group = well), size = .8, alpha = .4, ) +
        geom_ribbon(data = summarized,
                    aes(ymin = mean - sd,
                        ymax = mean + sd),
                    alpha = .2) +
        geom_line(data = summarized,
                  aes(y = mean),
                  size = .7,
                  linetype = "dashed") +
        geom_line(aes(y = fit, group = well), size = .7) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
        theme_minimal()
  })

}


###########################################################################
###########################################################################
###                                                                     ###
###                                 APP                                 ###
###                                                                     ###
###########################################################################
###########################################################################

run_curver <- function() {shinyApp(ui = ui, server = server)}
