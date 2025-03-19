server <- function(input, output, session) {

  observeEvent(input$btn, {
    
    for(i in 1:1){
      appendTab(
        inputId="uploadTabPanelPlot",
        select=T,
        tab = tabPanel(
          title = paste0("Plot ", input$btn),
          tags$div(
            h6("nichts")
          )
          
        )
      )
    }
    
  })
  
  output$out <- renderText(input$uploadTabPanelPlot)
  
  
  if(localUse && useProfVis) callModule(profvis_server, "profiler")

  #Planning list
  mCDList <<- ModelCreatingDataList$new()
  
  
  # Create a data model that stores the whole information
  dataModel <- DataModel$new()
  perIterationDataModel <- PerIterationDataModel$new(cores=max(1,parallel::detectCores()-1))
  perIterationDataModel$setDataModelInputData(dataModel$getDataModelInputData())
  
  dataModel$set.cPerIterationDataModel(perIterationDataModel)
  #Increments the next id for PIDM with 1
  dataModel$inc.pIDM_next_id()
  
  # Create a new report progress model and add it to the dataModel
  rPM <- ReportProgressModel$new()
  dataModel$set.reportProgressModel(rPM)

  
  # Create global reactive values
  # global_reportProgressModel <- reactiveVal(rPM)
  global_reportProgressModel <- rPM
  

  recBtn <- NULL

  init_home_page(input, output, session, image_folder)

  server_BAYSIS(input, output, session, dataModel, global_reportProgressModel)
  server_planning(input, output, session, dataModel, mCDList, global_reportProgressModel)
  server_report(input, output, session, dataModel, mCDList, global_reportProgressModel)

  
  # print0("mem size: ",memory.size())
  
  
  ## Report progress panel
  # moduleServer 
  observeReportProgress(input = input, session = session, dataModel = dataModel,
                        global_reportProgressModel = global_reportProgressModel)
  
  
  #Footer
  observeEvent(input$testtest, {
    global_browser <<- !global_browser
    print(global_browser)
  })
  dsgvo_folder <- paste0(dirname(getwd()),"/Datenschutz/")
  observeEvent(input$dsgvo_button, {
    showModal(modalDialog(shiny::includeHTML(paste0(dsgvo_folder,"BAYAS_DS_TMP_3.htm")), size = "l", easyClose = T, title = "Datenschutz", footer = modalButton("Close")))
  })
  observeEvent(input$impressum_button, {
    showModal(modalDialog(HTML(impressum()), size = "l", easyClose = T, title = "Impressum", footer = modalButton("Close")))
  })
}
