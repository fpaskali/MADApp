box::use(
  shiny,
  stats[aggregate],
  ggplot2[aes, geom_line, ggplot, theme, theme_light, element_text],
  shinyWidgets[airDatepickerInput, updateAirDateInput],
  plotly[ggplotly, plotlyOutput, renderPlotly],
)

box::use(
  app/view/analyte_selector,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel("Time Series", value = "tab7",
    shiny$sidebarLayout(
     shiny$sidebarPanel(
       airDatepickerInput(ns("plotRange"), "Choose Period:",
                          value=c(NULL, NULL),
                          autoClose=TRUE, range=TRUE),
       shiny$actionButton(ns("resetRange"), "Reset Period"),
       shiny$selectInput(ns("param"), "Select Parameter", choices = c("Mean", "Median")),
       analyte_selector$ui(ns("analyteSelector"))
     ),
     shiny$mainPanel(
       shiny$h3("Time Series Visualization", align = "center"),
       plotlyOutput(ns("mainPlot"), height = "80vh", inline=TRUE)
     )
    )
  )
}

#' @export
server <- function(id, parent_session, intensity_data) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observeEvent(input$resetRange, {
      updateAirDateInput(session, "plotRange", clear=TRUE)
    })
    
    shiny$observe({
    output$mainPlot <- renderPlotly({
      if (!is.null(intensity_data$df)) {
        tmp_df <- intensity_data$df
        tmp_df$Date <- as.Date(tmp_df$Date)
        tmp_df$Timeline <- paste0(tmp_df$Date, " (", tmp_df$ID, ")")

        if (is.null(input$plotRange)) {
          updateAirDateInput(session, "plotRange", value=range(tmp_df$Date))
        }

        tmp_df <- tmp_df[tmp_df$Date >= input$plotRange[1] & tmp_df$Date <= input$plotRange[2],]

        selected <- analyte_selector$server("analyteSelector", unique(tmp_df$Analyte), TRUE)
        tmp_df <- tmp_df[tmp_df$Analyte == selected,]

        if (nrow(tmp_df) != 0) {
          if (length(unique(tmp_df$Cell)) != 1) {
            averaged <- aggregate(tmp_df$Mean, list(tmp_df$Timeline), FUN=mean)
            names(averaged) <- c("Timeline", "avg")
            tmp_df <- merge(x = tmp_df, y = averaged, by = "Timeline", all = TRUE)
          }

          p <- ggplot(tmp_df, aes(x=Timeline, y=.data[[input$param]], group=Cell, color=Cell)) +
          geom_line() +
          theme_light() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
          if (!is.null(tmp_df$avg)) 
            p <- p + geom_line(aes(x=Timeline, y=avg), color="black", linetype="dashed")
        ggplotly(p)
        }
      }
    })
    })
  })
}
