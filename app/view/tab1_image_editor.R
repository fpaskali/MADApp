box::use(
  shiny,
  EBImage[display, readImage],
)

box::use(
  app/logic/gridding[render_grid],
  app/logic/image_tools[crop_image, flip_hor, flip_ver, rotate_cw, rotate_ccw, rotate_fine],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel("Load Image",
    value = "tab1",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        shiny$fluidRow(
          shiny$column(8, shiny$dateInput(ns("testdate"), label = "Date of Test:")),
          shiny$column(4, shiny$numericInput(ns("testId"), label = "ID", value = 1, 
                                             min = 1, max = 999))
        ),
        shiny$hr(),
        shiny$radioButtons(ns("imageLoader"),
          label = "Load Image or use Sample",
          choices = list(
            "Load Image" = 1,
            "Sample" = 2
          ),
          selected = 2
        ),
        shiny$conditionalPanel(
          ns = ns,
          condition = "input.imageLoader == 1",
          shiny$fileInput(
            inputId = ns("arrayImageFile"),
            label = "Load Image",
            placeholder = "JPEG, PNG, and TIFF are supported",
            accept = c(
              "image/jpeg",
              "image/png",
              "image/tiff"
            )
          )
        ),
        shiny$hr(),
        shiny$sliderInput(ns("rotate"), "Rotate image",
          min = -45, max = 45, value = 0, step = 0.1
        ),
        shiny$actionButton(ns("rotateCCW"), "-90"),
        shiny$actionButton(ns("rotateCW"), "+90"),
        shiny$actionButton(ns("flipHor"), "FH"),
        shiny$actionButton(ns("flipVer"), "FV"),
        shiny$hr(),
        shiny$h5("Grid parameters", style = "font-weight:bold"),
        shiny$sliderInput(ns("array_nrows"), "Number of rows:", min = 1, max = 30, value = 3),
        shiny$sliderInput(ns("array_ncols"), "Number of columns:", min = 1, max = 30, value = 3),
        shiny$radioButtons(ns("roi_mode"),
          label = "Grid mode",
          choices = list(
            "Rectangular" = "rect",
            "Parallelogram" = "par"
          ),
          selected = "rect"
        ),
        shiny$hr(),
        shiny$fluidRow(
          shiny$column(shiny$actionButton(ns("reset"), "Reset", width = "100%"), width = 6),
          shiny$column(shiny$actionButton(ns("applyGrid"), "Apply Grid", width = "100%"), width = 6)
        )
      ),
      shiny$mainPanel(
        width = 9,
        shiny$h3("Cropping and Gridding", align = "center"),
        shiny$plotOutput(ns("mainPlot"),
          click = ns("mainPlot_click"),
          dblclick = ns("mainPlot_dblclick"),
          brush = ns("mainPlot_brush"),
          height = "75vh"
        ),
        shiny$h6("Click and drag to select a region of interest. Double click on \
                               selected region to zoom", align = "center")
      )
    )
  )
}

#' @export
server <- function(id, parent_session, array_data, settings) {
  shiny$moduleServer(id, function(input, output, session) {

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

    shiny$observe({
      # Loading Array from file.
      if (input$imageLoader == 1) {
        shiny$validate(
          shiny$need(
            input$arrayImageFile$type %in% c("image/jpeg", "image/png", "image/tiff"),
            "Must load a valid jpg, png, or tiff image"
          )
        )
        array_data$imageName <- input$arrayImageFile$name
        img <- readImage(input$arrayImageFile$datapath)
        array_data$imageOrigin <- img
        array_data$imageTransform <- img
        array_data$imageFinal <- img
      }
      # Using sample image.
      if (input$imageLoader == 2) {
        img <- readImage("app/static/sample_array.png")
        array_data$imageOrigin <- img
        array_data$imageTransform <- img
        array_data$imageFinal <- img
        array_data$imageName <- "SAMPLE"
      }
    })

    # Rotate functions -----------------------------------------------------------------------------

    shiny$observe({
      array_data$imageFinal <- rotate_fine(array_data$imageTransform, input$rotate)
    })

    shiny$observeEvent(input$rotateCCW, {
      transformed <- rotate_ccw(array_data$imageTransform)
      array_data$imageFinal <- transformed
      array_data$imageTransform <- transformed
    })

    shiny$observeEvent(input$rotateCW, {
      transformed <- rotate_cw(array_data$imageTransform)
      array_data$imageFinal <- transformed
      array_data$imageTransform <- transformed
    })

    shiny$observeEvent(input$flipHor, {
      transformed <- flip_hor(array_data$imageTransform)
      array_data$imageFinal <- transformed
      array_data$imageTransform <- transformed
    })

    shiny$observeEvent(input$flipVer, {
      transformed <- flip_ver(array_data$imageTransform)
      array_data$imageFinal <- transformed
      array_data$imageTransform <- transformed
    })

    # Zooming
    shiny$observeEvent(input$mainPlot_dblclick, {
      brush_roi <- input$mainPlot_brush
      if (brush_roi$xmin >= 0 && brush_roi$ymin >= 0 &&
            brush_roi$xmax <= dim(array_data$imageFinal)[1] &&
            brush_roi$ymax <= dim(array_data$imageFinal)[2]) {
        image <- crop_image(array_data$imageFinal, input$mainPlot_brush)
        session$resetBrush(input$mainPlot_brush$brushId)
        array_data$imageTransform <- image
        array_data$imageFinal <- image
        shiny$updateSliderInput(session, "rotate", value = 0)
      } else {
        shiny$showNotification("Grid out of boundaries.", type = "error")
      }
    })

    shiny$observe(
      if (settings$bigGrid) {
        shiny$updateSliderInput(session, "array_nrows", max = 50)
        shiny$updateSliderInput(session, "array_ncols", max = 50)
      } else {
        shiny$updateSliderInput(session, "array_nrows", max = 30)
        shiny$updateSliderInput(session, "array_ncols", max = 30)
      }
    )

    # Buttons --------------------------------------------------------------------------------------
    shiny$observeEvent(input$reset, {
      array_data$imageTransform <- array_data$imageOrigin
      array_data$imageFinal <- array_data$imageOrigin
      session$resetBrush(input$mainPlot_brush$brushId)
      shiny$updateSliderInput(session, "rotate", value = 0)
      shiny$updateSliderInput(session, "array_nrows", value = 3)
      shiny$updateSliderInput(session, "array_ncols", value = 3)
    })

    shiny$observeEvent(input$applyGrid, {
      brush_roi <- input$mainPlot_brush
      if (!is.null(brush_roi) && brush_roi$xmin >= 0 && brush_roi$ymin >= 0 &&
            brush_roi$xmax <= dim(array_data$imageFinal)[1] &&
            brush_roi$ymax <= dim(array_data$imageFinal)[2]) {
        array_data$roi$image <- crop_image(array_data$imageFinal, brush_roi)
        array_data$roi$ncols <- input$array_ncols
        array_data$roi$nrows <- input$array_nrows
        array_data$roi$cell_w <- (brush_roi$xmax - brush_roi$xmin) / input$array_ncols
        array_data$roi$cell_h <- (brush_roi$ymax - brush_roi$ymin) / input$array_nrows
        array_data$roi$mode <- input$roi_mode
        array_data$roi$grid <- matrix(0, nrow = input$array_nrows, ncol = input$array_ncols)

        # Reset old threshold data
        array_data$thresh_data <- NULL

        shiny$updateTabsetPanel(parent_session, "tabs", selected = "tab2")
      }
    })

    # Plotting -------------------------------------------------------------------------------------
    # If valid Array Image File loaded, plot image on the main plot
    shiny$observe({
      output$mainPlot <- shiny$renderPlot({
        shiny$validate(
          shiny$need(
            array_data$imageFinal & input$imageLoader == 1 & array_data$imageName != "SAMPLE" | 
              input$imageLoader == 2 & array_data$imageName == "SAMPLE",
            "Must load a valid jpg, png, or tiff image"
          )
        )
        display(array_data$imageFinal, method = "raster")
        if (!is.null(input$mainPlot_brush)) {
          render_grid(
            array_data$imageFinal, input$array_nrows, input$array_ncols,
            input$mainPlot_brush, input$roi_mode
          )
        }
      })
    })
  })
}
