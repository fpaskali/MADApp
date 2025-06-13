box::use(
  shiny,
  EBImage[display],
  reactable[reactableOutput, renderReactable, reactable, colDef, colFormat],
  waiter[useWaiter, Waiter, spin_3],
)

box::use(
  app/logic/thresholding[otsu_threshold, quan_threshold],
  app/logic/gridding[render_grid],
  app/logic/const[LETTERS_EXT],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel(
    "Background Correction", value = "tab3",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        useWaiter(),
        shiny$radioButtons(ns("colorImage"),
                           label = "Color image?",
                           choices = c("No", "Yes"),
                           selected = "No"),

        shiny$conditionalPanel(
          ns = ns,
          condition = "input.colorImage == 'Yes'",
          shiny$radioButtons(
            ns("channel"),
            label = ("Conversion mode"),
            choices = c("luminance",
                        "gray",
                        "red",
                        "green",
                        "blue"),
            selected = "luminance")
        ),

        shiny$radioButtons(
            ns("spotType"),
            label = ("Spot Type"),
            choices = c("dark",
                        "light"),
            selected = "dark"),

        shiny$radioButtons(
          ns("thresh"),
          label = "Threshold Method",
          choices = c("Otsu", "Quantile"),
          selected = "Otsu"),

        shiny$conditionalPanel(
          ns = ns,
          condition = "input.thresh == 'Otsu'",
          shiny$radioButtons(
            ns("otsuMode"),
            "Choose Otsu Mode",
            choices = c("local", "global"),
            selected = "local")
        ),
        shiny$conditionalPanel(
          ns = ns,
          condition = "input.thresh == 'Quantile'",
          shiny$numericInput(
            ns("quantile1"),
            "Probability [%]:",
            value = 99,
            min = 0,
            max = 100,
            step = 0.1,
            width = NULL)
        ),
        shiny$hr(),
        shiny$actionButton(ns("threshold"), "Apply Background Correction"),
        shiny$hr(),
        shiny$actionButton(ns("go2postprocess"), "Proceed to Post-Processing"),
      ),
      shiny$mainPanel(
        width = 9,
        shiny$h3("Background Correction", align = "center"),
        shiny$splitLayout(shiny$uiOutput(ns("plot3Title")), shiny$uiOutput(ns("plot4Title"))),
        shiny$splitLayout(
          shiny$plotOutput(ns("plot3"), height = "53vh"),
          shiny$plotOutput(ns("plot4"), height = "53vh")
        ),
        shiny$div(
          class = "back-cor-table",
          reactableOutput(ns("threshData"))
        )
      )
    )
  )
}

#' @export
server <- function(id, parent_session, array_data) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observe({
      array_data$convMode <- "luminance"
      if (input$colorImage == "Yes") array_data$convMode <- input$channel
    })

    waiter <- Waiter$new(id = c(session$ns("plot3"),
                                session$ns("plot4")), html = spin_3(), color = "white")

    shiny$observeEvent(input$threshold, {
      array_data$thresh_data <- NULL # Remove old data, to clear the plots
      waiter$show()
      if (input$thresh == "Otsu") {
        array_data$thresh_data <- otsu_threshold(array_data$roi$image, array_data$segment_list,
                                                 array_data$convMode, input$spotType,
                                                 input$otsuMode)
      } else if (input$thresh == "Quantile") {
        array_data$thresh_data <- quan_threshold(array_data$segment_list, array_data$convMode,
                                                 input$spotType, input$quantile1)
      }
      # Save sidebar parameters
      array_data$thresh_data$params$colorImage <- input$colorImage
      array_data$thresh_data$params$channel <- input$channel
      array_data$thresh_data$params$spotType <- input$spotType
      array_data$thresh_data$params$thresh <- input$thresh
      array_data$thresh_data$params$otsuMode <- input$otsuMode
      array_data$thresh_data$params$quantile1 <- input$quantile1
      waiter$hide()
    })

    output$plot3Title <- shiny$renderUI({
      shiny$validate(shiny$need(array_data$segment_list, ""))
      shiny$validate(shiny$need(array_data$thresh_data, ""))
      shiny$h4("Signal Intensity Above Background", align = "center")
    })

    output$plot4Title <- shiny$renderUI({
      shiny$validate(shiny$need(array_data$segment_list, ""))
      shiny$validate(shiny$need(array_data$thresh_data, ""))
      shiny$h4("Signal After Background Correction", align = "center")
    })

    output$plot3 <- shiny$renderPlot({
      shiny$validate(shiny$need(array_data$segment_list, "Not valid segmentation list found!"))
      shiny$validate(shiny$need(array_data$thresh_data, "Background correction not applied"))
      display(array_data$thresh_data$image_above_bg, method = "raster", margin = c(0, 20, 0, 0))
      render_grid(array_data$thresh_data$image_above_bg, array_data$roi$nrows, array_data$roi$ncols,
                  NULL, array_data$roi$mode, analyte_grid = NULL, analyte_names = NULL,
                  show_checkerboard = TRUE)
    })

    output$plot4 <- shiny$renderPlot({
      shiny$validate(shiny$need(array_data$segment_list, ""))
      shiny$validate(shiny$need(array_data$thresh_data, ""))
      display(array_data$thresh_data$image_after_bc, method = "raster", margin = c(0, 20, 0, 0))
      render_grid(array_data$thresh_data$image_above_bg, array_data$roi$nrows, array_data$roi$ncols,
                  NULL, array_data$roi$mode, analyte_grid = NULL, analyte_names = NULL,
                  show_checkerboard = TRUE)
    })

    output$threshData <- renderReactable({
      shiny$validate(shiny$need(array_data$thresh_data, ""))
      df <- data.frame(Cell = as.vector(sapply(LETTERS_EXT[seq_len(array_data$roi$ncols)],
                                               function(x) {
                                                 paste0(x, seq_len(array_data$roi$nrows))
                                                 }
                                               )),
                       Threhold = as.vector(array_data$thresh_data$threshold),
                       Mean = as.vector(array_data$thresh_data$mean_intensities),
                       Median = as.vector(array_data$thresh_data$median_intensities))
      reactable(
        df,
        defaultColDef = colDef(
          format = colFormat(digits = 4),
          minWidth = 70,
          sortable = FALSE,
        ),
        bordered = TRUE,
        defaultPageSize = 5
      )
    })

    shiny$observeEvent(input$go2postprocess, {
      shiny$updateTabsetPanel(parent_session, "tabs", selected = "tab4")
    })
  })
}
