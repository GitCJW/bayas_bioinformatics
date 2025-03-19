init_home_page <- function(input, output, session, image_folder){
  
  #Logo
  observeEvent(input$logo, {
    
    showModal(
      modalDialog(
        size="m",
        easyClose = T,
        footer=NULL,
        tags$div(
          style="display:flex; flex-direction:column; align-items: center;",
          tags$img(
            style="flex:1;",
            src=get_logo(),
            height = "600px", 
            width = "500px"
          ),
          tags$a(
            style="flex:1; margin-top:25px;",
            "BAYAS = Bayesian analysis simplified (Drawing of Baya weaver birds or Bayas)",
            href="https://en.wikipedia.org/wiki/Baya_weaver#Gallery",
            target="_blank"
          )
        )
      )
    )
    
  })
  
  
  #Planning Module
  observeEvent(input$image_module_planning_click,{
    
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::hide(id = "report_main_div")
    shinyjs::show(id = "planning_main_div")

  })

  
  #Evaluation Module
  observeEvent(input$image_module_evaluation_click,{

    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "report_main_div")
    shinyjs::show(id = "baysis_main_div")
    
  })

   
  #Report Module
  observeEvent(input$image_module_report_click,{
    
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::show(id = "report_main_div")
    
  })

  observeEvent(input$subscribeToBayas, {
    showModal(modalDialog(
      textInput("subscriptionMail", "Your e-mail address", width="100%", 
                placeholder="YourName@mail.com"),
      footer = tags$div(
        tags$button(type = "button", class = "btn btn-default", 
                    `data-dismiss` = "modal", `data-bs-dismiss` = "modal", 
                    style="float:left;", "Close"),
        actionButton("finalSubscribeToBayas", "Subscribe", class="btn-primary")),
      title="Subscribe",
      size="m",
      easyClose=T
    ))
  })
  
  observeEvent(input$finalSubscribeToBayas, {
    from <- "bayas@uni-due.de"
    to <- "bayas-request@lists.uni-due.de"
    subject <- "command_new_member"
    body <- paste0("subscribe address=",input$subscriptionMail)
    
    user <- readLines(file(paste0(pw_folder, "/email_auth.txt")))
    pwd <- readLines(file(paste0(pw_folder, "/email_auth_pw.txt")))
    
    mailControl <- list(host.name="mailout.uni-duisburg-essen.de",
                        port=587,
                        user.name=user, 
                        passwd=pwd,
                        ssl=T)
    send.mail(
      from=from, to=to, subject=subject, body=body,
      smtp=mailControl, authenticate =T
    )
    removeModal()
  })
  
  
  #Feedback
  observeEvent(input$checkFeedback, {
 
    # showModal(
    #   modalDialog(
    #     tags$div(
    #       style="",
    #       checkboxGroupInput("checkFeedback", label=NULL,
    #                          selected=input$checkFeedback,
    #                          choiceNames=c("Do not know where/how to start",
    #                                        "Need more explanations", "Workflow not clear", 
    #                                        "BAYAS crashed"),
    #                          choiceValues=c(1,2,3,4)),
    #       textAreaInput("feedbackText", label=NULL, placeholder="Personalized feedback")
    #     ),
    #     title="Feedback",
    #     easyClose=T,
    #     footer=tags$div(
    #       style="",
    #       tags$span(modalButton("Cancel"), style = "float: left;"),
    #       actionButton("feedbackModalConfirm", "Submit", class="btn-primary")
    #       )
    #   )
    # )
    
  })
  
  observeEvent(input$feedbackText , {
    shinyFeedback::feedback(inputId="feedbackText", show=F)
  })
  
  #Submit feedback
  observeEvent(input$submitFeedback, {
    if(!is.null(input$checkFeedback) || (!is.null(input$feedbackText) && input$feedbackText != "")){

      from <- "bayas@uni-due.de"
      to <- "bayas@uni-due.de"
      subject <- "BAYAS_feedback"

      user <- readLines(file(paste0(pw_folder, "/email_auth.txt")))
      pwd <- readLines(file(paste0(pw_folder, "/email_auth_pw.txt")))
      

      feedback <- c("Do not know where/how to start","Need more explanations", 
                    "Workflow not clear","BAYAS crashed")
      
      indText <- input$feedbackText
      indText <- gsub("<", "((", indText)
      indText <- gsub(">", "))", indText)
      
      body <- paste0(paste0(feedback[as.numeric(input$checkFeedback)], collapse=" - "),
                     " - Personalized feedback: ", indText)
      
      mailControl <- list(host.name="mailout.uni-duisburg-essen.de",
                          port=587,
                          user.name=user, 
                          passwd=pwd,
                          ssl=T)
      
      send.mail(
        from=from, to=to, subject=subject, body=body,
        smtp=mailControl, authenticate =T
      )
      
      shinyFeedback::feedback(inputId="feedbackText", show=T, text="Thank you!", 
                              color=BAYAS_COLORS$`--bs-btn-bg`)
    }
  })
  
  
  # Walkthrough video
  observeEvent(input$walkthroughVideoBtn, {
    showModal(
      modalDialog(
        walkthroughVideo_modal(),
        title = "Walkthrough videos",
        footer = modalButton("Cancel"),
        size = "xl",
        easyClose = T
      )
    )
  })

}