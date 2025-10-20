box::use(
  rmarkdown[render],
  shiny,
  shinyWidgets[prettyRadioButtons],
  utils[sessionInfo],
  stats[setNames],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$actionButton(ns("showReportModal"), "Generate Report")
}

#' @export
server <- function(id, array_data) {
  shiny$moduleServer(id, function(input, output, session) {
    reportOpts <- shiny$reactiveVal(c("reportID", "orgImage", "iePar", "imwGrid", "analyteTable",
                           "bgPar", "bgPlots", "bgTable", "ppPlot", "ppPar",
                           "intensityTable", "sessionInfo"))
    
    shiny$observeEvent(input$showReportModal, {
      if (!is.null(array_data$roi)) {
        shiny$showModal(shiny$modalDialog(
        title = "Create Analysis Report",
        shiny$textInput(session$ns("reportName"), "Report Name", "Unnamed Report"),
        shiny$checkboxGroupInput(session$ns("reportOpts"), "Report",
          choices = list("Report ID" = "reportID", "Original Image" = "orgImage",
                         "Image Editor Parameters" = "iePar", "Image with Grid" = "imwGrid",
                         "Analytes Table" = "analyteTable",
                         "Background Correction Paramaters" = "bgPar",
                         "Background Correction Plots" = "bgPlots",
                         "Background Correction Table" = "bgTable",
                         "Post-processing Plot" = "ppPlot",
                         "Post-processing Parameters" = "ppPar",
                         "Session Information" = "sessionInfo"),
          selected = reportOpts()),
          size = "m",
          easyClose = FALSE,
          footer = shiny$tagList(
            shiny$modalButton("Cancel"),
            shiny$downloadButton(session$ns("saveReport"), "Save Report")
          )
        ))
      } else {
        shiny$showNotification("Report can't be generated (missing data)!", type = "error")
      }
    })
    
    shiny$observeEvent(input$reportOpts, {
      reportOpts(input$reportOpts)
    })
    
    output$saveReport <- shiny$downloadHandler(
      filename = function() {
        paste0(input$reportName, paste0(".html"))
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "temp_report.Rmd")
        file.copy("app/static/report_template.Rmd", tempReport, overwrite = TRUE)
        
        shiny$removeModal()
        render(
          input = tempReport,
          output_file = file,
          output_format = "html_document",
          params = c(list(title=input$reportName), setNames(as.list(rep(TRUE, length(reportOpts()))), reportOpts()))
        )
      }
    )
    
    shiny$reactive(reportOpts())
  })
}

generate_report_demo <- function() {
  demo_ui <- fluidPage(ui("demo"))
  demo_server <- function(input, output, session) {
    fluidPage(server("demo"))
    
  }
  shinyApp(demo_ui, demo_server)
}
