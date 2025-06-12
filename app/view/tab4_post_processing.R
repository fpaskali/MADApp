box::use(
  shiny,
  EBImage[display],
)

box::use(
  app/view/analyte_selector,
  app/logic/gridding[render_grid, get_one_cell],
  app/logic/const[LETTERS_EXT],
  app/logic/utils[quant_minmax_scaling]
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel(
    "Post Processing",
    value = "tab4",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        analyte_selector$ui(ns("emptyCells"), label = "Choose Empty IDs: ", multiple = TRUE),
        shiny$uiOutput(ns("advanced1")),
        analyte_selector$ui(ns("posControl"),
          label = "Choose Positive Control IDs: ",
          multiple = TRUE
        ),
        shiny$uiOutput(ns("advanced2")),
        shiny$actionButton(ns("plotIDs"), "Show IDs"),
        shiny$hr(),
        shiny$numericInput(
          ns("fp_thresh"),
          "False Positive Threshold",
          value = 0.2, min = 0, max = 1, step = 0.01
        ),
        shiny$actionButton(ns("plotFPs"), "Hide False Positives"),
        shiny$hr(),
        shiny$checkboxInput(ns("toScale"), "Scale data"),
        shiny$checkboxInput(ns("removeFalseP"), "Exclude false positives"),
        shiny$hr(),
        shiny$fluidRow(
          shiny$column(8, shiny$dateInput(ns("testdate"), label = "Date of Test:")),
          shiny$column(4, shiny$numericInput(ns("testId"), label = "ID", value = 1, 
                                             min = 1, max = 999))
        ),
        shiny$actionButton(ns("addData"), label = "Add To Data"),
        shiny$hr(),
        shiny$actionButton(ns("showIntensData"), "Show Intensity Data")
      ),
      shiny$mainPanel(
        width = 9,
        shiny$h3("Post-processing", align = "center"),
        shiny$plotOutput(
          ns("postProcessPlot"),
          click = ns("postProcessPlot_click"),
          dblclick = ns("postProcessPlot_dblclick"),
          brush = NULL,
          height = "75vh"
        ),
        shiny$h6("Click on the cell in the grid to add/remove false positive mark",
          align = "center"
        )
      )
    )
  )
}

#' @export
server <- function(id, parent_session, array_data, intensity_data, settings) {
  shiny$moduleServer(id, function(input, output, session) {
    false_positives <- shiny$reactiveVal()
    show_ids <- shiny$reactiveVal(FALSE)
    show_fp <- shiny$reactiveVal(TRUE)
    id_list <- shiny$reactiveValues(empty_cells = NULL, control_cells = NULL)

    shiny$observe({
      id_list$empty_cells <- analyte_selector$server("emptyCells", array_data$analytes, FALSE)
      id_list$control_cells <- analyte_selector$server("posControl", array_data$analytes, FALSE)
      false_positives((array_data$thresh_data$valid_pixels < input$fp_thresh) *
                        (array_data$thresh_data$valid_pixels > 0))
    })

    shiny$observe({
      if (settings$showAdvanced) {
        output$advanced1 <- shiny$renderUI({
          shiny$numericInput(
            inputId = session$ns("LOBquant"),
            label = "Empty Cells Quantile:",
            value = 0.9,
            min = 0.1,
            max = 1,
            step = 0.1,
            width = NULL
          )
        })

        output$advanced2 <- shiny$renderUI({
          shiny$numericInput(
            inputId = session$ns("MAXquant"),
            label = "Positive Control Quantile:",
            value = 0.1,
            min = 0.1,
            max = 1,
            step = 0.1,
            width = NULL
          )
        })
      } else {
        output$advanced1 <- NULL
        output$advanced2 <- NULL
      }
    })

    shiny$observeEvent(input$plotIDs, {
      if (show_ids() == FALSE) {
        show_ids(TRUE)
        shiny$updateActionButton(session, "plotIDs", "Hide IDs")
      } else {
        show_ids(FALSE)
        shiny$updateActionButton(session, "plotIDs", "Show IDs")
      }
    })

    shiny$observeEvent(input$plotFPs, {
      if (show_fp() == FALSE) {
        show_fp(TRUE)
        shiny$updateActionButton(session, "plotFPs", "Hide False Positives")
      } else {
        show_fp(FALSE)
        shiny$updateActionButton(session, "plotFPs", "Show False Positives")
      }
    })

    shiny$observe({
      output$postProcessPlot <- shiny$renderPlot({
        shiny$validate(shiny$need(array_data$roi$grid, "Not valid grid found!"))
        shiny$validate(shiny$need(array_data$thresh_data, "Please apply threshold!"))
        display(array_data$roi$image, method = "raster", margin = c(0, 30, 0, 0))
        render_grid(array_data$roi$image, array_data$roi$nrows, array_data$roi$ncols, NULL,
          array_data$roi$mode,
          analyte_grid = array_data$roi$grid,
          analyte_names = array_data$analytes, show_checkerboard = TRUE,
          show_fp = show_fp(), fp_grid = false_positives(), show_id = show_ids(),
          id_list = id_list
        )
      })
    })
    
    shiny$observeEvent(input$postProcessPlot_click, {
      shiny$req(is.null(input$postProcessPlot_brush))
      if (!is.null(input$postProcessPlot_click) && input$postProcessPlot_click$x >= 0 && input$postProcessPlot_click$y >= 0 &&
          input$postProcessPlot_click$x <= dim(array_data$roi$image)[1] && input$postProcessPlot_click$y <= dim(array_data$roi$image)[2]) {
        loc <- get_one_cell(input$postProcessPlot_click, array_data$roi)
        tmp_df <- false_positives()
        if (tmp_df[loc$y, loc$x] == 1) tmp_df[loc$y, loc$x] <- 0 else tmp_df[loc$y, loc$x] <- 1
        false_positives(tmp_df)
      }
    })

    shiny$observeEvent(input$testdate, {
      array_data$date <- input$testdate
    })

    shiny$observeEvent(input$testId, {
      array_data$id <- input$testId
    })

    shiny$observe({
      shiny$updateDateInput(session, "testdate", value = array_data$date)
      shiny$updateNumericInput(session, "testId", value = array_data$id)
    })

    shiny$observeEvent(input$addData, {
      if(is.null(array_data$thresh_data)) {
        shiny$showNotification("Intensities not found!", type = "error")
        return()
      }
      if (!is.null(intensity_data$df) && array_data$date %in% unique(intensity_data$df$Date) &&
          array_data$id %in% intensity_data$df$ID[intensity_data$df$Date == array_data$date]) {
          shiny$showModal(shiny$modalDialog(
            sprintf("There is intensity data with the same timestamp (%s (%s)). \
                  Please change the timestamp.", array_data$date, sprintf("%03d", array_data$id)),
            title = "Warning",
            footer = shiny$modalButton("Dismiss"),
            easyClose = FALSE,
            fade = TRUE))
      } else {
        mean_intensities <- array_data$thresh_data$mean_intensities
        median_intensities <- array_data$thresh_data$median_intensities
        thresh_data <- array_data$thresh_data$threshold
        
        if (input$toScale) {
          if (!is.null(id_list$empty_cells) && !is.null(id_list$control_cells)) {
            low_quant <- if (is.null(input$LOBquant)) 0.1 else input$LOBquant
            high_quant <- if (is.null(input$MAXquant)) 0.9 else input$MAXquant
            
            mean_intensities <- quant_minmax_scaling(mean_intensities, array_data$roi$grid,
                                                     array_data$analytes, id_list$empty_cells,
                                                     id_list$control_cells, high_quant, low_quant)
            median_intensities <- quant_minmax_scaling(median_intensities, array_data$roi$grid,
                                                     array_data$analytes, id_list$empty_cells,
                                                     id_list$control_cells, high_quant, low_quant)
          } else {
            shiny$showNotification("Empty and positive control cells are not specified!",
                                   type = "error")
          }
        }

        if (input$removeFalseP &&
            !is.null(false_positives()) &&
            all(dim(mean_intensities) == dim(false_positives()))) {
          valid_cells <- !false_positives()
          valid_cells[valid_cells == 0] <- NA
          mean_intensities <- mean_intensities * valid_cells
          median_intensities <- median_intensities * valid_cells
          thresh_data <- thresh_data * valid_cells
        }

        if (!input$toScale || (!is.null(mean_intensities) && !is.null(median_intensities) &&
                               !is.null(id_list$empty_cells) && !is.null(id_list$control_cells))) {
          df <- data.frame(
            Date = array_data$date,
            ID = array_data$id,
            File = array_data$imageName,
            Mode = array_data$convMode,
            Method = array_data$thresh_data$method,
            Probability = if (is.null(array_data$thresh_data$prob)) NA else array_data$thresh_data$prob,
            Cell = as.vector(sapply(LETTERS_EXT[seq_len(array_data$roi$ncols)],
                                    function(x) {
                                      paste0(x, seq_len(array_data$roi$nrows))
                                    })),
            Analyte = sapply(unlist(array_data$roi$grid),
                             function(x) {if (x != 0) array_data$analytes[x] else NA }),
            Mean = as.vector(mean_intensities),
            Median = as.vector(median_intensities),
            Threshold = as.vector(thresh_data))
          intensity_data$df <- rbind(intensity_data$df, df)
          shiny$showNotification("Intensity data added", type = "message")
        } else {
          shiny$showNotification("Intensities cannot be scaled!", type = "error")
        }
      }
    })

    shiny$observeEvent(input$showIntensData, {
      shiny$updateTabsetPanel(parent_session, "tabs", selected = "tab5")
    })
  })
}
