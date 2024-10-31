

init_data_visualization_function <- function(input, output, session, dataModel, 
                                             global_reportProgressModel){

 
  #Update user data
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("longFormat")
    
    isolate({
      #Update user data table
      sel <- dMID$getLongFormat()
      if(!is.empty(sel)){
        #Show long-format data
        output[["userInputDataTable"]] <- renderDT(
          datatable(
            data = sel,
            rownames = T,
            selection = "none",
            filter = "top",
            options = list(searching=F, paging=F, escape=F, info=F,
                           columnDefs = list(list(className = 'dt-right', targets = colnames(sel)))),
            fillContainer = T
          ) %>%
            formatStyle(0:(dim(sel)[2]), lineHeight="70%")
        )
      }else{
        output[["userInputDataTable"]] <- renderDT(NULL)
      }
      
      #Update plot inputs (such as variable names for axis and grouping)
      colnames <- dMID$getColnames()

      updateSelectInput(session, "selectResponseVariable",
                        choices = c("(Select a variable)"="",colnames),
                        selected = NULL)
      updateSelectizeInput(session, "uploadSelectX",
                           choices = c(X="",colnames),
                           selected = "")
      updateSelectizeInput(session, "uploadSelectY",
                           choices = c(Y="",colnames),
                           selected = "")
      updateSelectizeInput(session, "uploadSelectColorize",
                           choices = c('Group by'="",colnames),
                           selected = "")
      updateSelectizeInput(session, "uploadSelectFacetWrap",
                           choices = c('Facet wrap'="",colnames),
                           selected = "")
    })
  })
  
  ## Next Button that guides to next page
  observeEvent(input$btnVisualizePageNext, {
    updateNavbarPage(session, "navbar", selected = "Model selection")
  })  
  
  ## Back Button that guides to next page
  observeEvent(input$btnVisualizePageBack, {
    updateNavbarPage(session, "navbar", selected = "Data upload")
  }) 

 
  
  ########################## Plot functions ####################################
  
  
  ## When a valid combination of plot type and x,y axis and groupin is choosen
  ## highlight plot button
  observe({
    inputX <- input$uploadSelectX
    inputY <- input$uploadSelectY
    inputColorize <- input$uploadSelectColorize
    inputFacetWrap <- input$uploadFacetWrap
    inputType <- input$uploadPlotType

    #colorize btn when the combination is applicable
    isPos <- isPlotCombinationPossible(inputX, inputY, inputColorize, inputFacetWrap, inputType)
    if(isPos$check){
      shinyjs::addClass(id = "btnPlotInputData", "btn-primary")
    }else{
      shinyjs::removeClass(id = "btnPlotInputData", "btn-primary")
    }
  })
  

  ns_preplot <- NS("uploadTabPaneldownloadPreplot")
  
  ## Plot function for preview plots of user inputdata
  observeEvent(input$btnPlotInputData, {
    inputX <- input$uploadSelectX
    inputY <- input$uploadSelectY
    inputScaleX <- input$uploadScaleX
    inputScaleY <- input$uploadScaleY
    inputColorize <- input$uploadSelectColorize
    inputFacetWrap <- input$uploadFacetWrap
    inputType <- input$uploadPlotType
    
    
    isPos <- isPlotCombinationPossible(inputX, inputY, inputColorize, inputFacetWrap, inputType)
    if(!isPos$check){
      showNotification(isPos$description, type="warning")
      return()
    }
    
    y <- input$uploadSelectY
    x <- input$uploadSelectX
    inputColorize <- input$uploadSelectColorize
    inputFacetWrap <- input$uploadSelectFacetWrap
    selected <- input$uploadPlotType
 
    dMID <- dataModel$getDataModelInputData()
    varTypeX <- dMID$getInputProperty(x, type="type")
    varTypeY <- dMID$getInputProperty(y, type="type")
    
    #Some plots are not possible on some data. In this case, this flag will be T and the srcipt will return without creating a plot.
    notPossible <- F
    tabTitle <- ""
    plot <- ggplot()
    
    try({
      data <- NULL
      if(selected == "Histogram"){
        
        data <- dMID$getLongFormatVariable(varName=c(x,inputColorize,inputFacetWrap), completeCases=T)
        
        # For categorical data histograms are just possible with stat="count"
        data_x <- data[[x]]
        data_numeric <- suppressWarnings(sapply(data_x,as.numeric))
        categorical <- any(is.na(data_numeric))
        
        hist_list <- list(alpha=0.5, fill=BAYAS_COLORS$`--plotPreHist-color-1`, color=BAYAS_COLORS$`--plotPreHist-color-2`)
        aes_s <- aes(!!!list(x=sym(x)))
        plot <- ggplot(data, mapping=aes_s)
        if(!is.null(inputColorize) && inputColorize != "" && isPos$color){
          hist_list <- list(alpha=0.5, position="identity") 
          aes_s <- aes(!!!list(color=sym(inputColorize), fill=sym(inputColorize)))
        }
        if(categorical || varTypeX == characteristicEnum()$Categorical){
          hist_list$stat <- "count"
          plot <- plot + do.call(geom_histogram, hist_list) 
        }else{
          plot <- plot + aes_s + do.call(geom_histogram, hist_list) 
        }
        plot <- plot + aes_s + xlab(x)
        tabTitle <- paste0("Histogram of ","\'", x, "\'")
      
      }else if(selected == "Density"){
        
        data <- dMID$getLongFormatVariable(varName=c(x,inputColorize,inputFacetWrap), completeCases=T)
        
        dens_list <- list(alpha=0.5, fill=BAYAS_COLORS$`--plotPreHist-color-1`, color=BAYAS_COLORS$`--plotPreHist-color-2`)
        aes_s <- aes(!!!list(x=sym(x)))
        
        if(!is.null(inputColorize) && inputColorize != "" && isPos$color){
          dens_list <- list(alpha=0.5) 
          aes_s <- aes(!!!list(color=sym(inputColorize), fill=sym(inputColorize)))
        }
        
        plot <- ggplot(data) + 
          do.call(geom_density, dens_list) + aes_s +
          xlab(x) 
        tabTitle <- paste0("Density of ","\'", x, "\'")
        
      }else if(selected == "2dHistogram"){
        
        data <- dMID$getLongFormatVariable(varName=c(x,y,inputColorize,inputFacetWrap), completeCases=T)
        
        hist_list <- list(alpha=0.5, fill=BAYAS_COLORS$`--plotPreHist-color-1`, color=BAYAS_COLORS$`--plotPreHist-color-2`)
        aes_s <- aes(!!!list(x=sym(x)))
        
        if(!is.null(inputColorize) && inputColorize != "" && isPos$color){
          hist_list <- list(alpha=0.5, position="identity") 
          aes_s <- aes(!!!list(color=sym(inputColorize), fill=sym(inputColorize)))
        }
        
        aes_tmp <- aes(!!!list(x=sym(x), y=sym(y)))
        plot <- ggplot(data, aes_tmp) + 
          do.call(geom_bin2d, hist_list) + aes_s +
          ylab(y) + 
          xlab(x)

        tabTitle <- paste0("2dHistogram of ", "\'",x, "\'", " ~ ", "\'",y,"\'")
        
      }else{
        
        data <- dMID$getLongFormatVariable(varName=c(x,y,inputColorize,inputFacetWrap), completeCases=T)
        
        aes_s <- aes(!!!list(x=sym(x)))
        if(!is.null(inputColorize) && inputColorize != "" && isPos$color){
          aes_s <- aes(!!!list(color=sym(inputColorize), fill=sym(inputColorize)))
        }
        
        aes_tmp <- aes(!!!list(x=sym(x), y=sym(y)))
        plot <- ggplot(data, aes_tmp) + 
          geom_point() + aes_s +
          ylab(y) + 
          xlab(x)
        tabTitle <- paste0("Scatterplot of ", "\'",x, "\'", " ~ ", "\'",y,"\'")
      }
      
      if(selected == "Scatter+Histogram"){
        plot <- ggMarginal(plot, type="histogram", position="identity", groupColour=T, groupFill=T)
      }
    
      if(varTypeX==cNum$Continuous && !is.null(inputScaleX) && inputScaleX!="") 
        plot <- plot + scale_x_continuous(trans=inputScaleX)
      if(varTypeY==cNum$Continuous && !is.null(inputScaleY) && inputScaleY!="") 
        plot <- plot + scale_y_continuous(trans=inputScaleY)
      
      if((varTypeX!=cNum$Continuous && !is.null(inputScaleX) && inputScaleX!="") ||
         (varTypeY!=cNum$Continuous && !is.null(inputScaleY) && inputScaleY!="")) 
        showNotification(paste0("Scaling is only available for continuous data!"), duration = 10, type = "error")

      
      if(!is.null(inputFacetWrap) && all(inputFacetWrap != "") && isPos$facet) {
        if(selected == "Scatter+Histogram"){
          showNotification("Facet wrap does not work with 'Scatter+Histogram'", type="warning")
        }else{
          plot <- plot + facet_wrap(as.formula(paste0("~", paste0(inputFacetWrap, collapse="+"))))
        }
      }
        
    })
    
    plot_id_tmp <- dataModel$getDataModelPlotModel()$getNextId()
    plot_name <- paste0("Plot ", plot_id_tmp)

    # Save plot to data model
    dataModel$getDataModelPlotModel()$add.plot_history("raw", plotname=plot_name, id=plot_id_tmp, 
                                                       tabTitle=tabTitle, plot=plot, csvFile=dMID$getCurrentDataPath())
    
  })

  #Change button style depending on existing plots to report/download
  observeEvent(input$uploadTabPanelPlot, ignoreNULL = F, {
    if(!is.null(input$uploadTabPanelPlot)){
      shinyjs::addClass(id = "reportPreplot", "btn-primary")
      shinyjs::addClass(id = ns_preplot("download"), "btn-primary")
    }else{
      shinyjs::removeClass(id = "reportPreplot", "btn-primary")
      shinyjs::removeClass(id = ns_preplot("download"), "btn-primary")
    }
  })
  
  #Download preplot
  observeEvent(input[[ns_preplot("download")]], {
    if(is.null(input$uploadTabPanelPlot)){
      showNotification("Nothing to download. Create a plot before.", type="warning")
      return()
    }
    showModal(downloadPlotModalUI("uploadTabPaneldownloadPreplot", input$uploadTabPanelPlot))
  })
  downloadPlotServer(id="uploadTabPaneldownloadPreplot", 
                     gInput=input,
                     dataModel=dataModel,
                     plotType="raw",
                     plotName="uploadTabPanelPlot")
  

  #Report preplot
  observeEvent(input$reportPreplot, {
    
    if(is.null(input$uploadTabPanelPlot)){
      showNotification("Nothing to report. Create a plot before.", type="warning")
      return()
    }
    
    inputName <- input$uploadTabPanelPlot
    plot <- dataModel$getDataModelPlotModel()$get.plot_by_name("raw", inputName)
    
    reportDiv <- reportType(div=list(plot=plot))
    
    inputName <- str_replace_all(inputName, " ", "_")
    
    ggsave(paste0(report_folder, "/Thumbnails/preplot_",inputName,".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    tEnum <- reportTypeEnum()
    
    #Add element to report progress
    #pDIM_id=-1 -1 for reported items that are not related to a certain pDIM
    #There are no different dataModels, so that the csv name is used instead
    fileName <- dataModel$getDataModelPlotModel()$get.raw_plot_file_name(input$uploadTabPanelPlot)
    
    addItem(moduleType = "evaluation",
            dataModel_id=fileName, pDIM_id=-1,  pDIM_name = NULL,
            imgFile=paste0("Report/Thumbnails/preplot_",inputName,".jpg"), 
            type=tEnum$preplot, object=list(div=reportDiv, latex=plotToTex(paste0("preplot_",inputName),plot, caption=NULL)), 
            singleton=F, show=T, global_reportProgressModel=global_reportProgressModel)
  })
  

  #Remove preplot
  observeEvent(input$removePreplot, {
    remove <- input$uploadTabPanelPlot
    if(is.null(remove) || remove == ""){
      showNotification("There is nothing to close.", type="warning")
      return()
    }
    dataModel$getDataModelPlotModel()$remove.plot_history("raw", remove)
  })

  
  shownPlotsIds <- reactiveVal(c())
  #Show plots
  observe({
    
    dMPM <- dataModel$getDataModelPlotModel()
    
    dMPM$dependReactiveValue("plot_history")
    
    isolate({
      
      plots <- dMPM$get.plot_history("raw")
      
      shownPlotsIds_new <- c()
      add_ids <- c()
      
      for(pp_id in seq_along(plots)){
        pp <- plots[[pp_id]]
        #add
        if(!pp$id %in% shownPlotsIds()){
          
          # Create new tab for new plot in tab panel
          appendTab(
            inputId = "uploadTabPanelPlot", 
            select = T,
            tab = tabPanel(
              title = paste0("Plot ", pp$id),
              value = paste0("Plot ", pp$id),
              tags$div(
                pp$tabTitle,
                plotOutput(outputId = paste0("uploadTabPanelPlotPlot", pp$id))
              )
            )
          )
          add_ids <- c(add_ids, pp_id)
        }
        shownPlotsIds_new <- c(shownPlotsIds_new, pp$id)
      }
      
      sapply(add_ids, function(i){
        # Render selected type of Plot
        output[[paste0("uploadTabPanelPlotPlot", plots[[i]]$id)]] <- renderPlot(plots[[i]]$plot)
      })
      
      #remove
      ids <- shownPlotsIds()
      for(id in ids){
        if(!id %in% shownPlotsIds_new){
          
          selected <- dMPM$get.next_plot("raw", id=id)
          selected <- paste0("Plot ", selected)
          updateTabsetPanel(session, "uploadTabPanelPlot", selected = selected)
          removeTab(inputId = "uploadTabPanelPlot",paste0("Plot ", id))
          
        }
      }

      shownPlotsIds(shownPlotsIds_new)
    })
    
  })
  
}


## Returns a list with two elements
## First a boolean if the combination is valid
## Second a string of reasoning that this comb. is not valid
isPlotCombinationPossible <- function(x, y, group, facet, type){
  list <- list(check=T,color=T,facet=T, description="")
  if(is.null(x) || x == ""){
    list$check <- F
  }
  if(is.null(type)){
    list$check <- F
    list$description <- "No plot type!"
  }else{
    if(type=="Scatterplot"){
      if(is.null(y) || y == ""){
        list$check <- F
        list$description <- "Please provide a variable for the y-axis!"
      }
    }else if(type=="Scatter+Histogram"){
      if(is.null(y) || y == ""){
        list$check <- F
        list$description <- "Please provide a variable for the y-axis!"
      }
    }else if(type=="Histogram"){
      # list$color <- F
    }else if(type=="Density"){
      # list$color <- F
    }else if(type=="2dHistogram"){
      if(is.null(y) || y == ""){
        list$check <- F
        list$description <- "Please provide a variable for the y-axis!"
      }
    }else{
      list$check <- F
      list$description <- "Unknwon plot type!"
    }
  }

  return(list)
}
