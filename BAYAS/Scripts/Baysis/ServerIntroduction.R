init_introduction_function <- function(input, output, session, dataModel){
  
  # Disable the guide option, because it is not implemented yet. 
  shinyjs::disable("introductionGuide")
  
  image_folder <- paste0(dirname(getwd()),"/Images/")
  
  output$imagePrework1 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Experiment.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)

  output$imagePrework2 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"data_1.png"), contentType = "image/png", width_scale = 0.4, height_scale = 0.4, alt = "")
  }, deleteFile = FALSE)

  
  output$imageTool1 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Tool/Upload.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)
  
  output$imageTool2 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Tool/Verify.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)
  
  output$imageTool3 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Tool/Selection.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)
  
  output$imageTool4 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Tool/Adjust.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)
  
  output$imageTool5 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"Tool/Result.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)

  
  output$imagePostwork1 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"norm_2_bad.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)

  output$imagePostwork2 <- renderImage({
    readPNG_as_list(file = paste0(image_folder,"norm_2_well.png"), contentType = "image/png", width_scale = 1, height_scale = 1, alt = "")
  }, deleteFile = FALSE)
  

  
  
  observeEvent(input$btnLetsstart, {
    updateNavbarPage(session, "navbar", selected = "Upload")
  })  
  
  observeEvent(input$btnWatchVideo, {
    # print(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)),"/Video/Video_deutsch.mp4"))
    showModal(modalDialog(title = "Walkthrough", size = "l", footer = tags$div(style = "text-align: center;", modalButton("Watched it")), easyClose = T,
                          tags$div(style = "text-align: center;", renderUI({
                            tags$video(src="Video/Video_deutsch.mp4", width="640", height="480", type='video/mp4"', controls="controls")
                          }))           
    ))
    
  })
  
  
  output$btnExampleData <- downloadHandler(
    filename = 'Example_data.csv',
    content = function(file) {
      file.copy(paste0(dirname(getwd()),"/Data/Example_data_glm_3.csv"), file)
    },
    contentType = "text/csv"
  )

  
}


