downloadPlotUI <- function(id, label = "Download", tooltip=NULL, ...) {
  ns <- NS(id)
  btn <- actionButton(ns("download"), label=label, ...)
  if(!is.null(tooltip)){
    
    btn <- bslib::tooltip(
      trigger = btn,
      tooltip,
      options = list(trigger="hover")
    )
  }
  return(btn)
}

downloadPlotModalUI <- function(id, header) {
  ns <- NS(id)
  modalDialog(
    title = "Download plot",
    tags$div(
      fluidRow(
        column(2, h6("Name:")),
        column(4, textInput(inputId = ns("downloadPlotName"), label = NULL, value = header)),
        column(1, h6("Width:")),
        column(2, bayasNumericInput(inputId = ns("downloadPlotWidth"), label = NULL, 
                                    value = 8, min = 0, max = 100, step = 1,
                                    invalidMessage = T, invalidTooltip = T)),
        column(1, h6("Height:")),
        column(2, bayasNumericInput(inputId = ns("downloadPlotHeight"), label = NULL, 
                                    value = 4, min = 0, max = 100, step = 1,
                                    invalidMessage = T, invalidTooltip = T))
      ),
      fluidRow(
        column(2, h6("Image Format:")),
        column(4, selectInput(inputId = ns("plotExtension"), label = NULL, 
                              choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"), 
                              selected = "png")),
        column(1, h6("Units:")),
        column(2, selectInput(inputId = ns("downloadPlotUnit"), label = NULL, choices = c("inches","cm","mm"), selected = "inches")),
        column(1, h6("dpi: ")),
        column(2, bayasNumericInput(inputId = ns("downloadPlotDPI"), label = NULL, 
                                    value = 300, min = 1, max = 320, step = 10,
                                    invalidMessage = T, invalidTooltip = T))
      )
    ),
    footer = {
      tags$div(
        downloadButton(outputId = ns("downloadPlotButton"), label = "Download", 
                       clasS="btn-primary"),
        modalButton("Cancel")
      )
    },
    easyClose = F,
    size = "l"
  )

}

downloadPlotServer <- function(id, gInput, dataModel, plotType, plotName) {
  moduleServer(
    id,
    function(input, output, session) {

      output$downloadPlotButton <- downloadHandler(
        
        filename = function() paste0(input$downloadPlotName,".",input$plotExtension),
        
        content = function(file) {
          print(file)
          # fileName <- paste0(image_folder, "/tmp/",input$downloadPlotName,".", input$plotExtension)
          plot <- dataModel$getDataModelPlotModel()$get.plot_by_name(plotType, gInput[[plotName]])
          units <- ifelse(input$downloadPlotUnit == "inches", "in", input$downloadPlotUnit)
          ggsave(filename = file,  plot = plot, device = input$plotExtension, 
                 width = input$downloadPlotWidth, height = input$downloadPlotHeight, 
                 units = units, dpi = input$downloadPlotDPI)
          # ret <- file.copy(fileName, file, overwrite=T)
        },
        contentType = input$plotExtension
      )

    }
  )
}

