################################################################################
################################ Other Variable ################################
################################################################################

planning_PredictorModal <- function(stepDiv, helpDiv){
  tags$div(
    tags$div(
      class="borderColor-regular",
      style="border-bottom: 1px solid;",
      stepDiv
    ),
    
    tags$div(
      style="margin-top:20px;", 
      helpDiv
    )
  )
}

#primary is a vector of strings from ModelCreatingDataOtherVariable 
#type, range or dist
#dist: ModelCreatingDataOtherVariableDistributionAbstract
planning_creatingStepsPredictor <- function(ns, step, cMCD, pred,
                                            interceptInUse = F){
  
  l <- list()
  distEnum <- planningDistribtionsEnum("predictor")
  
  primary <- pred$getType()
  
  
  ###########################
  # Intercept vs Predictor  #
  ###########################
  if(step==1){
    
    interceptBtn <- imageButtonTitle(btnId=ns("typeOfPredictorIntercept"), 
                                     imageFile="Images/Planning/ModelPlanning/Predictor/Intercept.png", 
                                     title="Intercept", selected=F,
                                     btnStyle="width:auto; height:auto;", imgHeight="200px",
                                     primary=any("intercept" %in% primary))
    
    if(cMCD$predictorHasIntercept() && !("intercept" %in% primary))
      interceptBtn <- disabled(interceptBtn)
      
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How does your variable look like?</b>"))
      ),
      tags$div(
        style="display:flex; text-align:center;",
        tags$div(
          style="flex:1;",
          interceptBtn
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("typeOfPredictorPredictor"), 
                           imageFile="Images/Planning/ModelPlanning/Predictor/Predictor.png", 
                           title="Predictor", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="200px",
                           primary=any("predictor" %in% primary))
        )
      )
    )
  }
  
  ###########################
  ######## Predictor ########
  ###########################
  else if(step==2){
    
    ovIds <- cMCD$getOtherVariablesforPredictors()
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:20px;",
        HTML(paste0("<b>Add a new predictor depending on your defined variables</b>"))
      ),
      
      tags$div(
        
        fluidRow(
          column(6, 
                 selectizeInput(ns("planningPredictorModalVariables"),
                                label="Variable(s)", choices=ovIds, multiple=T,
                                width="100%"),
                 
                 tags$div(style="margin-bottom:20px;", 
                          disabled(textInput(ns("planningPredictorModalName"),
                                             label="Name", width="100%"))),
                 
                 disabled(textAreaInput(ns("planningPredictorModalInfo"),
                                        label="Information", width="100%",
                                        height="150px",
                                        resize="vertical"))),
          column(6, 
                 plotOutput(ns("planningPredictorModalPlot"), height="250px"),
                 uiOutput(ns("planningPredictorModalPlotInfo")))
        )
      )
      
    )
  }
  
  #Empty
  else{
    l$div <- tags$div(
      style="min-height:350px;"
    )
  }
  
  l <- tags$div(
    planning_backButton(ns, "planningPredictorModalBack", step, l),
    tags$div(
      style="margin-top:20px; margin-left:30px;",
      icon(class= "fontColor-primary", "info-circle"),
      tags$div(style="display: inline;font-weight: bold;font-size: 13px;", 
               "You can change everything later on")
    )
  )
  
  return(l)
}


###################################
########### Help pages ############ 
###################################

planning_creatingStepsPredictor_help <- function(ns, step){
  
  l <- tags$div()
  
  ###########################
  # Intercept vs Predictor  #
  ###########################
  if(step==1){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>What is an <b>intercept</b>?</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("The intercept represents the expected value of the dependent variable when all other influencing factors are zero. ",
                      "It is the starting point on the vertical axis (y-axis) of the model."))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>What is a <b>predictor</b>?</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("A predictor is an independent variable that is believed to influence the dependent variable.",
                      "It represents a characteristic, factor, or measurement that is thought to help explain variations or patterns in the dependent variable."))
        )
      )
    )
  }
  
  
  ###########################
  ######## Predictor ########
  ###########################
  else if(step==2){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Predictor</b></i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>The predictor is the variable that you believe has a relationship to the variable being explained (e.g. the dependent variable).</p>"))
        )
      )
    )
  }
  return(l)
}




#Back button
planning_backButton <- function(ns, id="planningModalBack", step, div){
  backButton <- actionButton(ns(id), label=NULL, icon=icon("angle-left"), class="backButton",
                             style=paste0("font-weight:bold; font-size:20px; margin: auto; padding: 5px;",
                                          "min-height:125px; max-height:125px;"))
  if(step==1) backButton <- hidden(backButton)
  
  tags$div(
    style="display:flex;",
    tags$div(
      style=paste0("height:inherit; margin-left:-10px; margin-right:20px;",
                   "display:flex; min-width:20px;"),
      backButton
    ),
    tags$div(
      style="flex:1;",
      div
    )
    
  )
}
