box::use(
  shiny[fluidPage, moduleServer, NS, reactiveValues, tabsetPanel, titlePanel],
  shinythemes[shinytheme],
)

box::use(
  app/view/settings,
  app/view/tab1_image_editor,
  app/view/tab2_annotation,
  app/view/tab3_background_correction,
  app/view/tab4_post_processing,
  app/view/tab5_intensity_data,
  app/view/tab6_checkerboards,
  app/view/tab7_timeseries,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(theme = shinytheme("lumen"),
    titlePanel("Microarray Data Analysis App"),
    settings$ui(ns("settings")),
    tabsetPanel(id = ns("tabs"),
      tab1_image_editor$ui(ns("image-editor")),
      tab2_annotation$ui(ns("annotation")),
      tab3_background_correction$ui(ns("background-correction")),
      tab4_post_processing$ui(ns("post-processing")),
      tab5_intensity_data$ui(ns("intensity-data")),
      tab6_checkerboards$ui(ns("checkerboards")),
      tab7_timeseries$ui(ns("timeseries"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server Initialization ------------------------------------------------------------------------
    array_data <- reactiveValues(date = NULL, id = NULL, imageName = NULL, imageOrigin = NULL, 
                                 imageTransform = NULL, imageFinal = NULL, roi = NULL, 
                                 threshData = NULL, analytes = NULL, convMode = "luminance")

    intensity_data <- reactiveValues(df = NULL)

    settings <- settings$server("settings")

    tab1_image_editor$server("image-editor", session, array_data)
    tab2_annotation$server("annotation", session, array_data)
    tab3_background_correction$server("background-correction", session, array_data)
    tab4_post_processing$server("post-processing", session, array_data, intensity_data, settings)
    tab5_intensity_data$server("intensity-data", session, intensity_data)
    tab6_checkerboards$server("checkerboards", session, intensity_data)
    tab7_timeseries$server("timeseries", session, intensity_data)
  })
}
