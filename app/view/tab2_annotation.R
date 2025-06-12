box::use(
  shiny,
  EBImage[display],
)

box::use(
  app/logic/gridding[render_grid, get_one_cell],
  app/logic/labels_io[read_labels, write_labels],
  app/logic/segmentation[segment],
  app/view/analyte_selector,
)

#' @export
ui <- function(id, array_data) {
  ns <- shiny$NS(id)
  shiny$tabPanel(
    "Annotation", 
    value = "tab2",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        shiny$fileInput(inputId = ns("labelFile"),
        label = "Load Label File",
        accept = c("text/csv")),
        shiny$fluidRow(
          shiny$column(
            shiny$actionButton(ns("removeLabels"), "Remove Labels", width = "100%"),
            width = 6
          ),
          shiny$column(
            shiny$downloadButton(ns("saveLabels"), "Save Labels", width = "100%"), 
            width = 6
          )
        ),
        shiny$hr(),
        shiny$textInput(ns("analyteName"), "Analyte name", value = ""),
        shiny$actionButton(ns("addAnalyte"), "Add Analyte"),
        shiny$br(), shiny$br(),
        analyte_selector$ui(ns("analyteSelector1")),
        shiny$actionButton(ns("plotANs"), "Hide Analyte Markers"),
        shiny$hr(),
        shiny$actionButton(ns("applySegmentation"), "Apply Segmentation")
      ),
      shiny$mainPanel(
        width = 9,
        shiny$h3("Label Configuration", align = "center"),
        shiny$plotOutput(
          ns("labelPlot"),
          click = ns("labelPlot_click"),
          dblclick = ns("labelPlot_dblclick"),
          brush = NULL,
          height = "75vh"),
        shiny$h6("Click on the cell in the grid to mark the selected analyte", align = "center")
      )
    )
  )
}

#' @export
server <- function(id, parent_session, array_data) {
  shiny$moduleServer(id, function(input, output, session) {
    show_an <- shiny$reactiveVal(TRUE)
    selected_analyte <- shiny$reactiveVal("")
    
    shiny$observeEvent(input$labelFile, {
      if (!is.null(input$labelFile$datapath)) {
        labels <- read_labels(input$labelFile$datapath, array_data$roi$ncols, array_data$roi$nrows)
        array_data$roi$grid <- labels$grid
        array_data$analytes <- labels$analyte_names
      }
    })

    shiny$observeEvent(input$plotANs, {
      if (show_an() == FALSE) {
        show_an(TRUE)
        shiny$updateActionButton(session, "plotANs", "Hide Analyte Markers")
      } else {
        show_an(FALSE)
        shiny$updateActionButton(session, "plotANs", "Show Analyte Markers")
      }
    })

    shiny$observe({
      selected_analyte(analyte_selector$server("analyteSelector1", array_data$analytes, TRUE))

      output$labelPlot <- shiny$renderPlot({
        shiny$validate(shiny$need(array_data$roi$grid, "Not valid grid found!"))
        display(array_data$roi$image, method = "raster", margin = c(0, 30, 0, 0))
        render_grid(array_data$roi$image, array_data$roi$nrows, array_data$roi$ncols, NULL,
                    array_data$roi$mode, analyte_grid = array_data$roi$grid, show_an = show_an(),
                    analyte_names = array_data$analytes, show_checkerboard = TRUE,
                    selected_analyte = selected_analyte())
      })
    })

    shiny$observeEvent(input$addAnalyte, {
      if (input$analyteName != "") {
        if (input$analyteName %in% array_data$analytes) {
          shiny$showNotification("Analyte already in the list!")
        } else {
          array_data$analytes <- c(array_data$analytes, input$analyteName)
          shiny$updateTextInput(session, "analyteName", value = "")
        }
      }
    })

    shiny$observeEvent(input$removeLabels, {
      array_data$analytes <- NULL
     array_data$roi$grid <- matrix(0, nrow = array_data$roi$nrows, ncol = array_data$roi$ncols)
    })

    output$saveLabels <- shiny$downloadHandler(
      filename = "Labels.csv",
      content = function(file) {
        write_labels(file, array_data$roi$grid, array_data$analytes)
      }
    )

    shiny$observeEvent(input$applySegmentation, {
      # Reset previous threshold data
      array_data$thresh_data <- NULL
      array_data$segment_list <- segment(array_data$roi$image, array_data$roi$nrows,
                                         array_data$roi$ncols, array_data$roi$mode)
      shiny$updateTabsetPanel(parent_session, "tabs", selected = "tab3")
    })
    
    shiny$observeEvent(input$labelPlot_click, {
      loc <- get_one_cell(input$labelPlot_click, array_data$roi)
      analyte_name <- match(selected_analyte(), array_data$analytes)
      if (!is.na(analyte_name)) array_data$roi$grid[loc$y, loc$x] <- analyte_name
    })
  })
}
