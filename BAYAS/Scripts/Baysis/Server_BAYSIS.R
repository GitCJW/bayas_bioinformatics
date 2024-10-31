server_BAYSIS <- function(input, output, session, dataModel, global_reportProgressModel) {
  
  # prints the session token
  print(paste0("Session token: ", session$token))
  
  #Shortcut to planning and report
  observeEvent(input$sc_to_planning_from_evaluation,{
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::hide(id = "report_main_div")
    shinyjs::show(id = "planning_main_div")
  })
  observeEvent(input$sc_to_report_from_evaluation,{
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::show(id = "report_main_div")
  })
  

  # Different triggers to init some observer
  trigger_upload <- reactiveVal(F)
  trigger_refresh_Variables <- reactiveVal(F)
  trigger_load_from_local <- reactiveVal(F)
  trigger_run_fit <- reactiveVal(F)
  trigger_select_fit <- reactiveVal(F)
  
  # Other reactive values
  link_to_fitted_model<- reactiveVal("")
  
  
  
  # Download of session
  output$saveBTNEvaluation <- downloadHandler(
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
  observeEvent(input$loadBTNEvaluation, {
    showModal(loadSessionModal("loadBTNEvaluationFileInput"))
  })

  observe({
    file <- input[["loadBTNEvaluationFileInput"]]
    if(is.null(file) || is.null(file$datapath) || file$datapath == "") return()
    isolate({
      shinybusy::show_spinner(spin_id="loadingSpinner")
      status <- loadSession(dataModel = dataModel, file=file$datapath, decrypt = T)
      shinybusy::hide_spinner(spin_id="loadingSpinner")
      if(status[[1]]){
        removeModal()
      }else{
        show("loadBTNEvaluationFileInputTextOutputDiv")
        output[["loadBTNEvaluationFileInputTextOutput"]] <- renderText(status[[2]])
      }
    })
  })

   
  

  
  observeEvent(input$to_home_baysis, {
    shinyjs::show(id = "home_main_div")
    shinyjs::hide(id = "baysis_main_div")
  })
  

  ## Upload page
  init_upload_function(input = input, output = output, session = session, dataModel = dataModel)
  

  ## Visualization page
  init_data_visualization_function(input = input, output = output, session = session, dataModel = dataModel, 
                                   global_reportProgressModel=global_reportProgressModel)


  ## Model selection page
  init_model_selection_function(input = input, output = output, session = session, dataModel = dataModel)



  ## Run Model page
  init_run_model_function(input = input, output = output, session = session, 
                          dataModel = dataModel, global_reportProgressModel=global_reportProgressModel)



  ## Compare models page
  init_compare_models_function(input = input, output = output, session = session,
                               dataModel = dataModel, 
                               global_reportProgressModel=global_reportProgressModel)

  # 
  # ## Model prediction page
  init_model_prediction_function(input = input, output = output, session = session,
                                 dataModel = dataModel,
                                 global_reportProgressModel=global_reportProgressModel)

  
  ## Remove console file when user quit session
  session$onSessionEnded(function() {
    # Remove console file if exists
    console_path <- paste0(dirname(getwd()),"/Console_files/")
    file <- paste0(console_path,"/.console", session$token ,".txt")
    if(file.exists(file)){
      file.remove(file)
    } else{
      print("No file!")
    }
  })

}