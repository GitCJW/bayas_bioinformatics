################################################################################
################################### Parameter ##################################
################################################################################

planning_ParameterModal <- function(stepDiv, helpDiv){
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

#primary is a vector of strings from ModelCreatingDataParameter 
#type, range or dist
#dist: ModelCreatingDataParameterDistributionAbstract
planning_creatingStepsParameter <- function(ns, parameter, sub){
  check <- checkboxInput(ns("parameterModalSameValuePrior"), 
                         "Use same value/prior for all subs", 
                         (length(parameter$getSubs())==1 || 
                            parameter$getUseSameDistForAllSubs()))
  
  subs <- parameter$getSubs()
  if(!is.null(sub) || length(subs)==1) check <- hidden(check)
  
  distGen <- sub$getValueDistribution()
  distPrior <- sub$getPrior()
  distNameGen <- ""
  distNamePrior <- ""
  if(!is.empty(distGen)) distNameGen <- distGen$getName()
  if(!is.empty(distPrior)) distNamePrior <- distPrior$getName()
  
  mcd <- parameter$getMcd()
  mcdResp <- mcd$getMcdResponse()
  respDist <- mcdResp$getDist()
  respLink <- mcdResp$getLink()
  
  distPriorChoices <- getParametersPossiblePriorDistributions(parameter$getType(), 
                                                              "inference", respDist, respLink)
  distGenChoices <- getParametersPossiblePriorDistributions(parameter$getType(), 
                                                            "generation", respDist, respLink)
  
  l <- tags$div(
    style="",
  
    fluidRow(
      column(4,
             wellPanel(
               class="getActiveColor",
                 style="min-height:358px; max-height: 398px; overflow:auto; padding:5px;",

               shinyTree(ns("parameterModalTree"),
                         checkbox=F, themeIcons=F, multiple=F, wholerow=T)
             )),
      column(8, 
             tags$div(
               id=ns("parameterModalGenPriorUIDiv"),
               check,
               fluidRow(
                 id=ns("parameterModalGenPriorUI"),
                 column(6, 
                        class= "plotColor-div-a",
                        style="border-radius: 3px;",
                        tags$label("Data generation"),
                        selectInput(ns("parameterModalGenDist"), label=NULL,
                                    choices=distGenChoices,
                                    selected=distNameGen),
                        tags$div(
                          style="margin-top:-10px; margin-bottom:10px;",
                          uiOutput(ns("parameterModalGenUi"))
                        )),
                 column(6, 
                        class= "plotColor-div-b",
                        style="border-radius: 3px;",
                        tags$label("Inference"),
                        selectInput(ns("parameterModalPriorDist"), label=NULL, 
                                    choices=distPriorChoices,
                                    selected=distNamePrior),
                        tags$div(
                          style="margin-top:-10px; margin-bottom:10px;",
                          uiOutput(ns("parameterModalPriorUi"))
                        ))
               ),
               plotOutput(ns("parameterModalPlot"), height="250px")
             ))
    )
    
  )

  return(l)
}


planning_creatingStepsParameter_parameter <- function(ns, name, dist){
  l <- tagList()
  paras <- dist$getParameter()
  seed <- dist$getSeed()
  
  for(di in seq_len(length(paras))){
    para <- paras[[di]]
    type <- para$getType()
    
    val <- tags$div(
      id=ns(paste0(name,"Div",di)),
      bayasNumericInput(ns(paste0(name,di)),
                        label=NULL, value=para$value, min=para$min_val, max=para$max_val, 
                        integer=para$discrete, invalidMessage=T, invalidTooltip = T,
                        class="randomizeFrequencyRandomSeedNumericInput"))
    
    paraDiv <- fluidRow(
      style="margin-left:-5px; margin-right:-5px;",
      column(6,
             HTML(para$display_name)),
      column(6,
             val)
    )
    l[[di]]<- paraDiv
  }
  seedDiv <- tags$div()
  if(dist$getName() != "FixedValue"){
    seedDiv <- tags$div(
      tags$div(
        style="margin-top:10px;",
        "Random seed"
      ),
      fluidRow(
        column(8,
               bayasNumericInput(ns(paste0(name,"RandomSeed")), label=NULL,
                                 value=seed, min=-.Machine$integer.max,
                                 max=.Machine$integer.max, step=1, integer=T,
                                 invalidMessage=T, invalidTooltip = T,
                                 class="randomizeFrequencyRandomSeedNumericInput")),
        column(2,
               style="margin-top:3px; padding-left:0px;",
               diceButton(inputId=ns(paste0(name,"RandomSeedDice")), 
                          tt=paste0("Generates a random number"))
        ),
        column(2,
               style="margin-top:8px;",
               labelWithInfo(id=ns(paste0(name,"RandomSeedHelp")), 
                             label="", ttHeader="Random seed",
                             ttContent=HTML(paste0("Setting seeds can be used to ensure reproducibility ",
                                                   "by initializing the random number generator with a fixed value, ",
                                                   "allowing the same random sampling process to be replicated.")))
        )
    )
    )
  }
  divPara <- tags$div(
    id=ns(name),
    style="",
    l
  )
  divAltPara <- NULL
  alt <- NULL
  if(dist$hasAlternative()){
    lAlt <- tagList()
    paras <- dist$getAlternativeParameter()
    for(di in seq_len(length(paras))){
      para <- paras[[di]]
      type <- para$getType()
      
      refVarId <- NULL
      
      val <- tags$div(
        id=ns(paste0(name,"AltDiv",di)),
        bayasNumericInput(ns(paste0(name,"Alt",di)),
                          label=NULL, value=para$value, min=para$min_val, max=para$max_val, 
                          integer=para$discrete, invalidMessage=T, invalidTooltip = T,
                          class="randomizeFrequencyRandomSeedNumericInput"))
      
      paraDiv <- fluidRow(
        style="margin-left:-5px; margin-right:-5px;",
        column(6,
               HTML(para$display_name)),
        column(6,
               val)
      )
      lAlt[[di]]<- paraDiv
    }
    
    alt <- tags$div(style="margin-top:20px;",
                    checkboxInput(
                      ns(paste0(name,"Checkbox")),
                      "Use alternative parameterization"))
    divAltPara <- hidden(tags$div(
      id=ns(paste0(name,"Alt")),
      style="",
      lAlt
    ))
  }
  ret <- tags$div(divPara,
                  divAltPara,
                  alt,
                  seedDiv)
  
  return(ret)
}


###################################
########### Help pages ############ 
###################################

planning_creatingStepsParameter_help <- function(ns){
  
  l <- tags$div(
    style="",
    
    wellPanel(
      class="infoBox",
      style="padding:10px;",
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<i>What is the <b>Data generation</b> distribution?</i>"))
      ),
      tags$div(
        style="",
        HTML(paste0("The <b>Data generation </b> distribution, also called likelihood distribution, describes the probability distribution of the data, ",
                    "assuming certain values for the model parameters.",
                    "It is the probability that the data have when the model parameters are known."))
        
      )
    ),
    wellPanel(
      class="infoBox",
      style="padding:10px;",
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<i>What is the <b>Inference</b> distribution?</i>"))
      ),
      tags$div(
        style="",
        HTML(paste0("The <b>Inference</b> distribution is a probability distribution set before analysis ",
                    "to represent initial assumptions or prior knowledge about the parameters of the model. ",
                    "It expresses how uncertain or certain you are about the values of the model parameters before you look at the data. ",
                    "In a Bayesian model, the prior distribution is updated using the generation distribution to obtain the posterior distribution."))
        
      )
    )
  )

  
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

