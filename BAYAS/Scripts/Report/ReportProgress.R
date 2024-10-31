inputId <- "reportProgressPanel"
inputIdGoTo <- "reportProgressPanelToReport"

#server 
observeReportProgress <- function(input, session, dataModel, global_reportProgressModel){
  

  observeEvent(input[[inputId]], {
    rPM <- global_reportProgressModel
    rPM$setUpdateProgressPanel(F)
    removeItem(input[[inputId]])
  })
  
  observeEvent(input$reportProgressPanelToReport, {
    showHidePanel(F)
    shinyjs::hide(id = "home_main_div")
    shinyjs::hide(id = "planning_main_div")
    shinyjs::hide(id = "baysis_main_div")
    shinyjs::show(id = "report_main_div")
  })
  
  
  observe({
    
    rPM <- global_reportProgressModel
    rPM$dependReactiveValue("nextId")
    rPM$dependReactiveValue("items")
    rPM$dependReactiveValue("recommendedItems")
    rPM$dependReactiveValue("updateProgressPanel")
    
    dataModel$set.reportProgressModel(rPM)
    
    flag <- rPM$getUpdateProgressPanel()
    if(flag){
      items <- rPM$getItems()
      if(length(items) > 0){
        ids <- sapply(1:length(items), function(i){items[[i]]$getId()})
        images <- sapply(1:length(items), function(i){items[[i]]$getImgFile()})
        updateProgressPanel(session, inputId, ids, images)
      }
    }else{
      rPM$getUpdateProgressPanel()
    }
    # isolate(global_reportProgressModel(rPM))
  })

  
  # pDIM_id: id of related perDataIterionModel; -1 for none
  # imgFile: file name of shown image in report queue. If null, 
  # the default image is taken for the certain type.
  # type: Type of reported element: formula, ppc, preplot, etc.
  # object: Depends on the type. This object will be handled differently depending on the type.
  # Optional: model_latex; this is used for general latex printed once for a model fit for the corresponding subsection 
  # e.g.: list(div=DIV, latex=LATEX, model_latex=LATEX)
  # show: show progressbar, default TRUE
  # global_reportProgressModel: object of global_reportProgressModel
  addItem <<- function(moduleType=c("planning","evaluation","blank"),
                       dataModel_id=NULL, pDIM_id=NULL, pDIM_name=NULL, 
                       planningName=NULL,
                       imgFile=NULL, type, 
                       object, singleton, show=T, 
                       global_reportProgressModel, recommended=F){
  
    match.arg(moduleType)
    
    tEnum <- reportTypeEnum()
    
    if(!type %in% tEnum) stop(paste0("Unknown type '", type , "' for report progress panel!"))
    rPM <- global_reportProgressModel
    nextId <- rPM$getNextId()
    if(is.null(imgFile)) imgFile <- paste0("Images/Report/",type,".jpg")
    if(is.list(dataModel_id)){
      if("datapath" %in% names(dataModel_id)){
        base <- dirname(dataModel_id$datapath)
        dataModel_id <- paste0(base, "/", dataModel_id$name)
      }else{
        stop("Wrong dataModel_id format")
      }
    }

    isNew <- rPM$addItem(id=nextId, 
                         moduleType = moduleType,
                         dataModel_id=dataModel_id, pDIM_id=pDIM_id, 
                         planningName=planningName,
                         pDIM_name=pDIM_name,imgFile=imgFile, type=type, object=object,
                         singleton=singleton, recommended=recommended)
    rPM$incNextId()
    if(!recommended){
      if(isNew) addProgressPanel(inputId=inputId, id=nextId, image=imgFile)
      if(show) showProgressPanel(inputId=inputId)
    }
    return(nextId)
  }
  
  
  removeItem <<- function(ids){
    removeProgressPanel(inputId=inputId,ids=ids)
    rPM <- global_reportProgressModel
    rPM$removeItem(ids)
  }
  
  showHidePanel <<- function(flag=T){
    if(flag){
      showProgressPanel(inputId=inputId)
    }else{
      hideProgressPanel(inputId=inputId)
    }
  }
  
  get_nextId <<- function(){
    global_reportProgressModel$getNextId()
  }
  
  set_reportModel <<- function(reportModel){
    global_reportProgressModel <- reportModel
  }
  get_reportModel <<- function(){
    return(global_reportProgressModel)
  }

}


#ui
getReportProgress <- function(){
  footer <- tags$div(style="text-align:right;",
                     actionButton(inputId=inputIdGoTo, label="To Report"),
                     progressPanelButton()
  )
  pP <- progressPanel(
    inputId=inputId,
    title = "Report",
    visible=F,
    footer=footer
  )
  return(pP)
}

#For reporting something
getReportButton <- function(inputId, label=NULL, tooltip=NULL, class="btn-primary", ...){
  
  if(is.null(label)){
    label <- tags$div("Report")
  }
  btn <- actionButton(inputId=inputId, label=label,  class=class, ...) 

  if(!is.null(tooltip)){
    
    btn <- bslib::tooltip(
      trigger = btn,
      tooltip,
      options = list(trigger="hover")
    )

  }
  return(btn)
}