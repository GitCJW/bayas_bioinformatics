server_planning <- function(input, output, session, dataModel, mCDList, global_reportProgressModel) {
  
  
  observeEvent(input$to_home_baysis_from_planning, {
    shinyjs::show(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
  })
  
  #Shortcut to evaluation and report
  observeEvent(input$sc_to_evaluation_from_planning,{
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "report_main_div")
    shinyjs::show(id = "baysis_main_div")
  })
  observeEvent(input$sc_to_report_from_planning,{
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::show(id = "report_main_div")
  })
  

  init_creatingModel_function(input, output, session, dataModel, mCDList, global_reportProgressModel)

  
  # Download of session
  output$saveBTNPlanning <- downloadHandler(
    filename = "Data_Analysis.bayas",
    content = function(file) {
      showModal(modalDialog(
        tags$div(
          style="",
          tags$div(
            class = "bayas_shinybusy_spinner",
            tags$div(
              style = "width: max-content; display: inline-block;",
              shinybusy::use_busy_spinner(
                spin = "folding-cube", spin_id="savingSpinner", 
                color="var(--bs-btn-bg)")
            )
          )
        ),
        
        title = "Download session",
        footer = modalButton("Cancel"),
        size = "s"
      ))
      
      shinybusy::show_spinner(spin_id="savingSpinner")
      on.exit(removeModal())
      saveSession(dataModel=dataModel, file=file, encrypt=T)
      output[["testParent"]] <- renderUI(tags$div())
    },
    contentType = "BAYAS/bayas"
  )
  
  
  # Upload of session
  observeEvent(input$loadBTNPlanning, {
    showModal(loadSessionModal("loadBTNPlanningFileInput"))
  })
  
  observe({
    file <- input[["loadBTNPlanningFileInput"]]
    if(is.null(file) || is.null(file$datapath) || file$datapath == "") return()
    isolate({
      shinybusy::show_spinner(spin_id="loadingSpinner")
      status <- loadSession(dataModel = dataModel, file=file$datapath, decrypt = T)
      shinybusy::hide_spinner(spin_id="loadingSpinner")
      if(status[[1]]){
        removeModal()
      }else{
        show("loadBTNPlanningFileInputTextOutputDiv")
        output[["loadBTNPlanningFileInputTextOutput"]] <- renderText(status[[2]])
      }
    })
  })

}
