select_model_page <- function(){
  tabPanel(
     "Model selection",

     # first row for page elements. Second row holds next button
     verticalLayout(
       fluid = F,
       
       tags$div(
                 
         shinybayas::bayasSidePanel(
           inputId = "sidebarPanelModelSelection",
           
           mainPanelClass = "getActiveColor",
           sidePanelClass = "getActiveColor",
           
           sidePanel = tags$div(
             class="getActiveColor",
             
             
             div(

                selectizeInput(
                  inputId = "selectStanModelAvailable", 
                  label = h5(
                    style = "font-weight: bold;", 
                    "Select an appropriate statistical model"),
                  choices = c(), selected = ""),
                
               disabled(
                 ownTextAreaInput(
                   class= "infoBox",
                   style="height:120px;",
                   inputId = "selectStanModelNotAvailableInfoBox", 
                   label=NULL, 
                   placeholder = "Select a model for information",
                   resize="vertical"))

             )
             
           ),
           
           
           # Main Panel
           mainPanel = tags$div(
              wellPanel(
                id = "panelSelectModel", 
                class="getActiveColor",
                style= "margin-bottom: 1.5rem;", 
                h5("Revise your model", style="text-align:left;"),
                uiOutput("formulaOutput")
                ),
              
              wellPanel(
                id = "panelSelectModelPreview", 
                class="getActiveColor", 
                h5("Information", style="text-align:left;"),
                
                fluidRow(
                  column(6, uiOutput(outputId = "previewDataSelectModelLeft"), style="padding-right:5px;"),
                  column(6, uiOutput(outputId = "previewDataSelectModelRight")))
                )
           )
         )
       ),
       
       # Second Row of elements for e.g. Navigation (Next Button) 
       footerPanel(
         backButtonsId="btnModelSelectionBack",
         nextButtonsId="btnModelSelectionNext",
         backButtons="Data visualization",
         nextButtons="Model fitting"
       )
  
                     
     )
  )
}





#Create specific modal for parameter
createPriorModalView <- function(input, dataModel, parameter){
  
  predictor <- parameter$getParentPredictor()
  
  poss_dist <- parameter$getDistribution()
  id <- parameter$id
  
  #Set parameter tmp distribution to work on them
  parameter$distToTmpDist()
  
  if(!is.null(predictor) && predictor$same_prio_dist){
    title <- "Type of distribution for "
    name_of_pred <- predictor$name
    list_of_pred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$get_predictor_of_type(name_of_pred)
    index <- c()
    for(i in list_of_pred){
      index <- c(index,i$modelParameter$id)
    }
    min_index <- min(index)
    max_index <- max(index)
    min_index_str <- ""
    max_index_str <- ""
    for(i in list_of_pred){
      if(i$modelParameter$id == min_index) min_index_str <- i$modelParameter$getFullDisplayName()
      if(i$modelParameter$id == max_index) max_index_str <- i$modelParameter$getFullDisplayName()
    }
    if(min_index == max_index){
      title <- paste0("Type of distribution for ",min_index_str)
    }else if(length(index)==2){
      title <- paste0("Type of distribution for ",min_index_str,",",max_index_str)
    }else{
      title <- paste0("Type of distribution for ",min_index_str,",...,",max_index_str)
    }
  }else{
    title <- paste0("Type of distribution for ",parameter$getFullDisplayName())
  }
  
  
  #Get current values
  #if vector:
  #choicelist, selected val, recom.val, overall recom.val, prior, prior.aux
  #if not vector:
  #recom.val, prior, prior.aux
  if(!is.null(predictor)){
    selected_dist <- predictor$modelParameter$distribution
  }else{
    selected_dist <- parameter$distribution
  }
  selected_dist_name <- selected_dist$dist_name
  
  # recom <- ifelse(!is.null(input[[paste0("selectPriorAttributesDefault",id)]]),input[[paste0("selectPriorAttributesDefault",id)]],T)
  recom <- F
  
  if(is.null(parameter$modalID)) parameter$modalID <- modalIdObj$getNext()
  
  defaultPriorAttr <- tags$div(
    style = "",
    "No data specific priors available"
  )


  
  checkboxDataSpecificPriors <- tags$div(
    style="margin-top:5px;",
    
    checkboxInput(
      inputId="selectPriorAttributesDefault",
      "Use weakly informative (data-driven) priors.",
      value=recom)
  )
  
  priorAdjustment <- T
  if(!priorAdjustment) checkboxDataSpecificPriors <- hidden(disabled(checkboxDataSpecificPriors))
  
  # Checkbox of default/data specific parameters
  defaultPriorAttr <- bslib::tooltip(
    trigger = checkboxDataSpecificPriors,
    HTML(tooltip$recommendedValues),
    options = list(trigger="hover", delay=list(show= 1000, hide= 100))
  )
  
  #Info box for distributions
  infoBox <- wellPanel(
    class="infoBox",
    disabled(uiOutput(
      "previewPriorDistributionInfo"))
  )
  
  imgOutput <- tags$div(
    imageOutput(outputId = "previewPriorDistribution", height="400px")
  )
  
  # For parameters that are vectors
  if(!is.null(predictor) && predictor$get.is.vector()){
    
    sameDistDiv <- tags$div()
    if(predictor$same_prio_dist){
      sameDistDiv <- tags$div(
        style="padding-top:33px;",
        
        bslib::tooltip(
          trigger = tags$span(id="infoSamePriorDistribution",icon("exclamation-circle")),
          HTML(tooltip$samePriorDistribution),
          options = list(trigger="hover")
        )
      )
    }
  
    choiceList <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$getTermCombinationsOfPredictorList(predictor)
    return(
      tags$div(
        tags$div(
          style="display:flex;",
          
          tags$div(
            style= "flex:1;",
            
            tags$div(
              selectizeInput("selectParameterOfVector", "This parameter is a vector", 
                             width="100%",
                             choices=choiceList,  
                             options = list(render = I(
                               '{
                             item: function(item, escape) {
                             return "<div>" + item.label + "</div>"
                             },
                             option: function(item, escape) {
                             return "<div>" + item.label + "</div>"
                             }}'
                             ))),
              
              tags$div(
                style="display:flex; gap:5px;",
                tags$div(
                  style="flex:0.5;"
                ),
                tags$div(
                  style= "flex:5;",
                  tags$div(
                    style="",
                    selectInput("selectPriorDistribution", HTML(title), choices=poss_dist, 
                                selected=selected_dist_name, width="100%"),
                    uiOutput(outputId = "selectPriorAttributesDiv"),
                    defaultPriorAttr
                  )
                ),
                tags$div(
                  style= "flex:1;",
                  sameDistDiv
                  
                )
              )
            )
            
          ),
          tags$div(
            style= "flex:2;",
            
            imgOutput
          )
        ),
        infoBox
      )
    )
    # For parameters that uses the same kind of distribution (among other parameters)
  }else if(!is.null(predictor) && predictor$same_prio_dist){
    return(
      tags$div(
        tags$div(
          style="display:flex;",
          tags$div(
            style= "flex:1",
            tags$div(
              style="display:flex; gap:5px;",
              tags$div(
                style= "flex:5;",
                tags$div(
                  selectInput("selectPriorDistribution", HTML(title), width="100%", 
                              choices=poss_dist, selected=selected_dist_name),
                  uiOutput(outputId = "selectPriorAttributesDiv"),
                  defaultPriorAttr
                )
              ),
              tags$div(
                style= "flex:1",
                tags$div(
                  style="padding-top:33px;",
                  bslib::tooltip(
                    trigger = tags$span(id="infoSamePriorDistribution",icon("exclamation-circle")),
                    HTML(tooltip$samePriorDistribution),
                    options = list(trigger="hover")
                  )
                )
              )
            )
          ),
          tags$div(
            style= "flex:2;",
            imgOutput
          )
        ),
        infoBox
      )
    )
  }else{
    return(
      tags$div(
        tags$div(
          style="display:flex;",
          
          tags$div(
            style= "flex:1",
            
            tags$div(
              selectInput("selectPriorDistribution", HTML(title), width="100%"
                          , choices=poss_dist, selected=selected_dist_name),
              uiOutput(outputId = "selectPriorAttributesDiv"),
              defaultPriorAttr
            )
          ),
          tags$div(
            style= "flex:2;",
            imgOutput
          )
        ),
        infoBox
      )
    )
  }
}

