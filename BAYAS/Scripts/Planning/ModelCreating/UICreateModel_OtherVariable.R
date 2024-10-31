################################################################################
################################ Other Variable ################################
################################################################################

planning_OtherVariableModal <- function(stepDiv, helpDiv){
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
planning_creatingStepsOtherVariable <- function(ns, step, otherVariableName,  
                                                primary, dist, cate, cMCD, varId,
                                                tmp=NULL){
  
  l <- list()
  distEnum <- planningDistribtionsEnum("predictor")
  
  
  ###########################
  ##### Cont vs discrete ####
  ##### vs categorical   ####
  ###########################
  if(step==1){
    oV <- cMCD$getOtherVariable(varId)
    cons <- oV$getConstraints()
    
    contDiv <- tags$div(
      style="flex:1;",
      imageButtonTitle(btnId=ns("typeOfOtherVariableCont"), 
                       imageFile="Images/Planning/ModelPlanning/VariableTypes/TypeContinuous.png", 
                       title="Continuous", selected=F,
                       btnStyle="width:auto; height:auto;", imgHeight="200px",
                       primary=any("cont" %in% primary))
    )
    
    discDiv <- tags$div(
      style="flex:1; text-align:center;",
      imageButtonTitle(btnId=ns("typeOfOtherVariableDiscrete"), 
                       imageFile="Images/Planning/ModelPlanning/VariableTypes/TypeDiscrete.png", 
                       title="Discrete", selected=F,
                       btnStyle="width:auto; height:auto;", imgHeight="200px",
                       primary=any("discrete" %in% primary))
    )
    
    catDiv <- tags$div(
      style="flex:1; text-align:center;",
      imageButtonTitle(btnId=ns("typeOfOtherVariableCategorical"), 
                       imageFile="Images/Planning/ModelPlanning/VariableTypes/TypeCategorical.png", 
                       title=tags$div("Categorical",
                                      tags$div(style="font-size:12px;",
                                               HTML("&<br> (numerical replacement)"))), 
                       selected=F,
                       btnStyle="width:auto; height:auto;", imgHeight="200px",
                       primary=any("categorical" %in% primary))
    )
    
    if("integer" %in% cons){
      contDiv <- disabled(contDiv)
    }
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How does your variable look like?</b>"))
      ),
      tags$div(
        style="display:flex; text-align:center;",
        contDiv,
        discDiv,
        catDiv
      )
    )
  }
  
  ###########################
  ######## Cont range #######
  ###########################
  else if(step==2){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>What is the range of the variable?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCont1"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Positive_wo_null2.png", 
                           title="Positive >0", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("positiveGreater" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCont2"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Positive_w_null2.png", 
                           title=HTML("Positive &ge;0"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("positive" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCont3"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Percent2.png", 
                           title="%", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("percent" %in% primary))
        )
      ),
      tags$div(
        style="display:flex; text-align: center;",
        
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCont4"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Neg_pos2.png", 
                           title="Real (no limits)", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("unlimited" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          bslib::popover(
            trigger = imageButtonTitle(btnId=ns("rangeOfOtherVariableCont5"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Neg_w_null2.png", 
                                       title=HTML("Negative"), selected=F,
                                       btnStyle="width:auto; height:auto; cursor: not-allowed;", imgHeight="99px",
                                       imgClass = "borderColor-regular",
                                       imgStyle="border:1px solid; border-radius:2px;",
                                       primary=any("negGreater" %in% primary)),
            title="Negative values",
            tags$div("To create (only) negative values choose a positive range and check \"Negate values\" in the distribution overview."),
            placement="left",
            options=list(trigger="hover"))

        )
      )
      
    )
  }
  
  ###########################
  ###### Discrete range #####
  ###########################
  else if(step==3){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>What is the range of the  variable?</b>"))
      ),
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableDiscrete1"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos2.png", 
                           title=HTML("Positive"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("discretePos" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableDiscrete2"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos_N2.png", 
                           title="Limited by N", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("limitedN" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableDiscrete3"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos_0_1.png", 
                           title="Bernoulli", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("bernoulli" %in% primary))
        )
      )
      
      
    )
    
  }
  
  ###########################
  ######## Cont noise #######
  ######## positive   #######
  ###########################
  else if(step==4){ 
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable1"), imageFile="Images/Distributions/Continuous/Log_Normal.jpg", 
                           title=HTML("Log-Normal"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Log_Normal %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable2"), imageFile="Images/Distributions/Continuous/Gamma.jpg", 
                           title=HTML("Gamma"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Gamma %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable3"), imageFile="Images/Distributions/Continuous/Inverse_Gaussian.jpg", 
                           title=HTML("Inverse Gaussian"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      ),
      tags$div(
        style="display:flex; text-align: center; margin-top:10px;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable4"), imageFile="Images/Distributions/Continuous/F.jpg", 
                           title=HTML("F"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable5"), imageFile="Images/Distributions/Continuous/Chi_squared_DF_==_1.jpg", 
                           title=HTML("Chi-squared df = 1"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      )
    )
    
  }
  
  ###########################
  ###### Cont noise     #####
  ###### positive+zero  #####
  ###########################
  else if(step==5){ 
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable6"), imageFile="Images/Distributions/Continuous/Exponential.jpg", 
                           title=HTML("Exponential"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Exponential %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable7"), imageFile="Images/Distributions/Continuous/Chi_squared_DF_!=_1.jpg", 
                           title=HTML("Chi-squared df &ne; 1"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable8"), imageFile="Images/Distributions/Continuous/Weibull.jpg", 
                           title=HTML("Weibull"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      )
    )
    
  }
  
  ###########################
  ####### Cont noise  #######
  ####### %           #######
  ###########################
  else if(step==6){ 
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable9"), imageFile="Images/Distributions/Continuous/Beta.jpg", 
                           title=HTML("Beta"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Beta %in% primary))
        )
      )
    )
    
  }
  
  ###########################
  ####### Cont noise  #######
  ####### unlimited   #######
  ###########################
  else if(step==7){ 
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable10"), imageFile="Images/Distributions/Continuous/Normal.jpg", 
                           title=HTML("Normal"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable11"), imageFile="Images/Distributions/Continuous/Cauchy.jpg", 
                           title=HTML("Cauchy"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable12"), imageFile="Images/Distributions/Continuous/Logistic.jpg", 
                           title=HTML("Logistic"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      ),
      tags$div(
        style="display:flex; text-align: center; margin-top:10px;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable13"), imageFile="Images/Distributions/Continuous/Uniform.jpg", 
                           title=HTML("Uniform"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable14"), imageFile="Images/Distributions/Continuous/Student_t.jpg", 
                           title=HTML("Student t"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      )
    )
    
  }
  
  ###########################
  ##### Discrete noise ######
  ##### positive       ######
  ###########################
  else if(step==10){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable20"), imageFile="Images/Distributions/Discrete/Poisson.jpg", 
                           title=HTML("Poisson"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable21"), imageFile="Images/Distributions/Discrete/Negative_Binomial.jpg", 
                           title=HTML("Negative-Binomial"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      ),
      tags$div(
        style="display:flex; text-align: center; margin-top:10px;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable22"), imageFile="Images/Distributions/Discrete/Geometric.jpg", 
                           title=HTML("Geometric"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable23"), imageFile="Images/Distributions/Discrete/Hypergeometric.jpg", 
                           title=HTML("Hypergeometric"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Inverse_Gaussian %in% primary))
        )
      )
      
    )
    
  }
  
  ###########################
  ##### Discrete noise ######
  ##### limited by N   ######
  ###########################
  else if(step==11){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable24"), imageFile="Images/Distributions/Discrete/Binomial.jpg", 
                           title=HTML("Binomial"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Binomial %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable25"), imageFile="Images/Distributions/Discrete/Beta_Binomial.jpg", 
                           title=HTML("Beta-Binomial"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Beta_Binomial %in% primary))
        )
      )
    )
  }
  
  ###########################
  ##### Discrete noise ######
  ##### 0 or 1         ######
  ###########################
  else if(step==12){ 
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfOtherVariable26"), imageFile="Images/Distributions/Discrete/Bernoulli.jpg", 
                           title=HTML("Bernoulli"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Bernoulli %in% primary))
        )
      )
    )
    
  }
  
  ###########################
  ##### Categorical    ######
  ##### overview       ######
  ###########################
  else if(step==13){ 
    
    independentIsUse <- F

    
    for(ov in cMCD$getOtherVariables()){
      if(ov$getId() == varId) next
      type <- ov$getType()
      if(!is.null(ov$getSpecialRole())) next
      if(!is.null(type) && type=="categorical"){
        catModel <- ov$getCategoricalModel()
        catType <- catModel$getType()
        if(!is.null(catType) && catType == "independent"){
          independentIsUse <- T
          break
        }
      }
    }
    
    indNameDiv <- tags$div(
      "Independent"
    )

    if(independentIsUse){
      
      indNameDiv <- tags$div(
        "Independent",

          tags$div(
            class = "fontColor-warning",
            style="font-weight:normal; font-size:smaller;",
           "(Already used)",
 
           var <- bslib::popover(
             trigger = icon(id=ns("warningOfMultipleIndependentVars-tt"),
                       name="question-circle", 
                       class = "fontColor-warning",
                       style="margin-left:auto; margin-bottom: auto; margin-top:auto;"),
             title = "Mulitple independent variables",
             paste0("In most cases, multiple independent variables are not useful. ",
                    "Here \"independent\" only means that it is not a subgroup. ",
                    "Probably \"subgroup\" is the right choice."),
             placement = "right",
             options = list(trigger="hover")
           )

          )
        )

    }

    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How is the  variable distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCat1"), 
                           imageFile="Images/Planning/ModelPlanning/Range/Categorical/Cat_independent.png", 
                           title=indNameDiv, # HTML("Independent"), 
                           selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("independent" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCat2"), imageFile="Images/Planning/ModelPlanning/Range/Categorical/Cat_subgroup.png", 
                           title=HTML("Subgroup"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("subgroup" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfOtherVariableCat3"), imageFile="Images/Planning/ModelPlanning/Range/Categorical/Cat_replacement.png", 
                           title=HTML("Replacement"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("replacement" %in% primary))
        )
      )
      
    )
    
  }
  
  ###########################
  ##### Categorical    ######
  ##### Independent    ######
  ###########################
  else if(step==14){
    if(otherVariableName == "unnamed") otherVariableName <- ""
    
    values <- cate$getValues()
    dist <- cate$getValuesDistributed()
    if(is.null(dist) ||!dist %in% c("equally","explicit")) dist <- "equally"
    dist <- str_to_title(dist)
    seed <- cate$getSeed()
    randomize <- cate$getRandomize()
    randomParameter <- cate$getRandomParameter()
    valuesFreq <- cate$getValuesFrequency()
    capped <- cate$getCapped()
    
    #random options vis
    randomOptions <-  tags$div(
      id=ns("otherVariableCatRandomSeedDiv"),
      tags$div(
        style="",
        "Variation (in %)"
      ),
      fluidRow(
        column(8,
               bayasNumericInput(ns("otherVariableCatRandomParameter"), label=NULL,
                                 value=randomParameter, min=0,
                                 max=.Machine$integer.max, step=1, integer=F,
                                 invalidMessage=T, invalidTooltip = T,
                                 class="randomizeFrequencyRandomSeedNumericInput")),

        column(2, offset=2,
               style="margin-top:8px;",
               labelWithInfo(id=ns("otherVariableCatRandomizeVariationHelp"), 
                             label="", ttHeader="Variation",
                             ttContent=HTML(paste0("Determines the amount of variation within the frequencies. <br>",
                                                   "A value of 10 means that the number of e.g. \"a\" and \"b\" ",
                                                   "can deviate by 10%. <br>",
                                                   "Note that each element must occur at least once.")))
        )
      ),
      tags$div(
        style="margin-top:10px;",
        "Random seed"
      ),
      fluidRow(
        column(8,
               bayasNumericInput(ns("otherVariableCatRandomSeed"), label=NULL,
                                 value=seed, min=-.Machine$integer.max,
                                 max=.Machine$integer.max, step=1, integer=T,
                                 invalidMessage=T, invalidTooltip = T,
                                 class="randomizeFrequencyRandomSeedNumericInput")),
        column(2,
               style="margin-top:3px; padding-left:0px;",
               diceButton(inputId=ns("otherVariableCatRandomSeedDice"), 
                             tt=paste0("Generates a random number"))
        ),
        column(2,
               style="margin-top:8px;",
               labelWithInfo(id=ns("otherVariableCatRandomizeRandomSeedHelp"), 
                             label="", ttHeader="Random seed",
                             ttContent=HTML(paste0("Setting seeds can be used to ensure reproducibility ",
                             "by initializing the random number generator with a fixed value, ",
                             "allowing the same random sampling process to be replicated.")))
        )
      )
    )
    

    withr::with_seed(1337, {
      s <- sample(4,length(values),replace=T)
    })
    
    placeholder <- paste0(s, collapse=", ")
    
    explicitValues <- tags$div(
      id=ns("otherVariableCatIndependentValuesDistributedDiv"),
      class="textAreaOtherVariableFreq",
      textAreaInput(ns("otherVariableCatIndependentValuesDistributed"),
                    label="Frequency per unit", value=paste0(valuesFreq, collapse = ", "),
                    width="100%", resize="vertical",
                    placeholder=placeholder),
      # hidden(
      tags$div(
        id=ns("otherVariableCatIndependentValuesDistributedWarning"),
        class="fontColor-error",
        style=paste0("margin:-15px 0px 15px 5px;",
                     "font-size:12px; font-weight:bold;"),
        textOutput(ns("otherVariableCatIndependentValuesDistributedWarningText"))
      )
      # )
    )
    if(dist == "Equally") explicitValues <- hidden(explicitValues)
    
    
    ## Is randomization available/applicable?
    oV <- cMCD$getOtherVariable(varId)
    randAvail <- oV$isRandomizeAvailableForCatagorical()

    randomizeWarning <- tags$div(
      id=ns("otherVariableCatRandomizeWarning"),
      class="fontColor-warning",
      style="margin-top:-20px; padding-left:25px;",
      style="font-weight:bold; font-size:smaller;",
      "Not applicable"
    )
    styleClass <- ""
    styleClassHint <- ""
    
    violatedMessage <- paste0("Vary the number of frequencies by the given percentage. <br>",
                              "This is only applicable if this variable and all upper/sub ",
                              "categorical variables use \"Equally\" distributed frequencies. <br>",
                              "Note that each element must occur at least once.")
    
    if(!randAvail$valid){
      styleClass <- "randomizeFrequencyRadioButton"
      if(randomize){
        styleClassHint <- "randomizeFrequencyHint"
      }else{
        randomizeWarning <- hidden(randomizeWarning)
      }
      
      names <- c()
      for(id in randAvail$ids){
        names <- c(names, cMCD$getOtherVariable(id)$getName())
      }
      violatedMessage <- paste0(violatedMessage, "<br><br>",
                                "Not applicable due to: ", paste0(paste0("\"",names,"\""), collapse=", "))
    }else{
      randomizeWarning <- hidden(randomizeWarning)
    }
    
    randomizeDiv <- fluidRow(
      id=ns("otherVariableCatRandomizeDiv"),
      column(10,
             tags$div(
               id=ns("otherVariableCatRandomizeSubDiv"),
               class=styleClass,
               checkboxInput(ns("otherVariableCatRandomize"), 
                               label=tags$b("Randomize frequency"),
                               value=randomize)
             ),

             randomizeWarning
      ),
      column(2,
             style="margin-top:4px;",
             labelWithInfo(id=ns("otherVariableCatRandomizeHelp"), 
                           label="", ttHeader="Randomization",
                           ttContent=HTML(violatedMessage),
                           class=styleClassHint)
      )
    )
    
    if(dist == "Explicit"){
      randomizeDiv <- hidden(randomizeDiv)
      randomize <- F
    }
    if(!randomize || !randAvail$valid) randomOptions <- hidden(randomOptions)

    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      HTML(paste0("<b>Name your variable</b>"))
               ),
               column(8,
                      bayasTextInput(ns("variableIndependentModalName"), NULL, value=otherVariableName,
                                placeholder="unnamed", width="100%")
               )
      ),
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      tags$div(
                        tags$div(
                          class="textAreaOtherVariable",
                          textAreaInput(ns("otherVariableCatIndependentValues"),
                                        label="Values", value=paste0(values, collapse = ", "),
                                        width="100%", resize="vertical",
                                        placeholder="male, female\nsmall, medium, great\n1, 2, 3, 4 ...")),
                        
                        tags$div(
                          style="font-weight:bold;margin-bottom:5px;",
                          "Distributed"
                        ),
                        bayasGroupedButtons(ns("otherVariableCatIndependentDistributed"),
                                            btnNames=c("Equally","Explicit"), 
                                            btnValues=c("equally","explicit"),
                                            selected=tolower(dist), columns=2,
                                            outerStyle = "margin-bottom:15px;",
                                            btnStyle = "width:100%;"),
                        
                        
                        explicitValues,
                        
                        fluidRow(
                          column(10,
                                 checkboxInput(ns("otherVariableCatCapped"), 
                                                 label=tags$b("Capped"),
                                                 value=capped)
                          ),
                          column(2,
                                 style="margin-top:4px;",
                                 labelWithInfo(id=ns("otherVariableCatCappedHelp"), 
                                               label="", ttHeader="Capped",
                                               ttContent=paste0("If the dataset length is not a multiple of the number of elements, ",
                                                                "elements will be cut to the desired length."))
                          )
                        ),
                        
                        randomizeDiv,
                        randomOptions
                        
                      )
               ),
               column(8,
                      tags$div(
                        style="max-height:374px; overflow-y:auto;",
                        DTOutput(ns("otherVariableCatIndependentTable"), height="100%")
                      )
               )
      )
      
    )
  }
  
  ###########################
  ##### Categorical    ######
  ##### Subgroup       ######
  ###########################
  else if(step==15){
    if(otherVariableName == "unnamed") otherVariableName <- ""
    
    values <- cate$getLeafOfFirstElement(cate$getValues())
    dist <- cate$getLeafOfFirstElement(cate$getValuesDistributed())
    if(is.null(dist) ||!dist %in% c("equally","explicit")) dist <- "equally"
    dist <- str_to_title(dist)
    seed <- cate$getLeafOfFirstElement(cate$getSeed())
    randomize <- cate$getLeafOfFirstElement(cate$getRandomize())
    randomParameter <- cate$getLeafOfFirstElement(cate$getRandomParameter())
    valuesFreq <- cate$getLeafOfFirstElement(cate$getValuesFrequency())
    capped <- cate$getLeafOfFirstElement(cate$getCapped())
    
    
    #subgroup stuff
    subgroupOf <- cate$getSubgroupOf() #ids
    validUpperGroups <- cMCD$getOtherVariableIdsForSubgroupOV(id=varId) #Allow independent and subgroup
    names <- c()
    for(i in validUpperGroups){
      names <- c(names, cMCD$getOtherVariable(i)$getName())
    }
    names(validUpperGroups) <- names
    
    subgroupElements <- list()
    if(is.null(subgroupOf) ||
       !subgroupOf %in% validUpperGroups){ 
      subgroupOf <- ""
    }else{
      elements <- cMCD$getOtherVariable(subgroupOf)$getCategoricalModel()$getValues()
      el_for_list <- elements
      sel_for_list <- elements[1]
      if(is.list(elements)){ 
        el_for_list <- nested_list_for_cat_subgroups(elements)
        sel_for_list <- el_for_list[[1]][1]
      }
      
      subgroupElements <- el_for_list
    }
    subgroupElements[['Choose element']] <- ""
    hasSubgroup <- !is.null(subgroupOf) && subgroupOf != ""
    
    validUpperGroups <- as.list(validUpperGroups)
    validUpperGroups[['Subgroup of ...']] <- ""
    
    subgroupExplicit <- cate$getSubgroupExplicit()
    
    #random options vis
    randomOptions <-  tags$div(
      id=ns("otherVariableSubgroupCatRandomSeedDiv"),
      tags$div(
        style="",
        "Variation (in %)"
      ),
      fluidRow(
        column(8,
               bayasNumericInput(ns("otherVariableSubgroupCatRandomParameter"), label=NULL,
                                 value=randomParameter, min=0,
                                 max=.Machine$integer.max, step=1, integer=F,
                                 invalidMessage=T, invalidTooltip = T,
                                 class="randomizeFrequencyRandomSeedNumericInput")),
        
        column(2, offset=2,
               style="margin-top:8px;",
               labelWithInfo(id=ns("otherVariableSubgroupCatRandomizeVariationHelp"), 
                             label="", ttHeader="Variation",
                             ttContent=HTML(paste0("Determines the amount of variation within the frequencies. <br>",
                                                   "A value of 10 means that the number of e.g. \"a\" and \"b\" ",
                                                   "can deviate by a maximum of 10%.<br>",
                                                   "Note that each element must occur at least once.")))
        )
      ),
      tags$div(
        style="margin-top:10px;",
        "Random seed"
      ),
      fluidRow(
        column(8,
               bayasNumericInput(ns("otherVariableSubgroupCatRandomSeed"), label=NULL,
                                 value=seed, min=-.Machine$integer.max,
                                 max=.Machine$integer.max, step=1, integer=T,
                                 invalidMessage=T, invalidTooltip = T,
                                 class="randomizeFrequencyRandomSeedNumericInput")),
        column(2,
               style="margin-top:3px; padding-left:0px;",
               diceButton(inputId=ns("otherVariableSubgroupCatRandomSeedDice"), 
                          tt=paste0("Generates a random number"))
        ),
        column(2,
               style="margin-top:8px;",
               labelWithInfo(id=ns("otherVariableSubgroupCatRandomizeRandomSeedHelp"), 
                             label="", ttHeader="Random seed",
                             ttContent=HTML(paste0("Setting seeds can be used to ensure reproducibility ",
                                                   "by initializing the random number generator with a fixed value, ",
                                                   "allowing the same random sampling process to be replicated.")))
        )
      )
    )
    
    ## Is randomization available/applicable?
    oV <- cMCD$getOtherVariable(varId)
    randAvail <- oV$isRandomizeAvailableForCatagorical()
    
    randomizeWarning <- tags$div(
      id=ns("otherVariableSubgroupCatRandomizeWarning"),
      class="fontColor-warning",
      style="margin-top:-20px; padding-left:25px;",
      style="font-weight:bold; font-size:smaller;",
      "Not applicable"
    )
    styleClass <- ""
    styleClassHint <- ""
    
    violatedMessage <- paste0("Vary the number of frequencies by the given percentage. <br>",
                              "This is only applicable if this variable and all upper/sub ",
                              "categorical variables use \"Equally\" distributed frequencies. <br>",
                              "Note that each element must occur at least once.")
    
    if(!randAvail$valid){
      styleClass <- "randomizeFrequencyRadioButton"
      if(randomize){
        styleClassHint <- "randomizeFrequencyHint"
      }else{
        randomizeWarning <- hidden(randomizeWarning)
      }
      
      names <- c()
      for(id in randAvail$ids){
        names <- c(names, cMCD$getOtherVariable(id)$getName())
      }
      violatedMessage <- paste0(violatedMessage, "<br><br>",
                                "Not applicable due to: ", paste0(paste0("\"",names,"\""), collapse=", "))
    }else{
      randomizeWarning <- hidden(randomizeWarning)
    }
    
    randomizeDiv <- fluidRow(
      id=ns("otherVariableSubgroupCatRandomizeDiv"),
      column(10,
             tags$div(
               id=ns("otherVariableSubgroupCatRandomizeSubDiv"), # <<----
               class=styleClass,
               checkboxInput(ns("otherVariableSubgroupCatRandomize"), 
                               label=tags$b("Randomize frequency"),
                               value=randomize)
             ),
             
             randomizeWarning
      ),
      column(2,
             style="margin-top:4px;",
             labelWithInfo(id=ns("otherVariableSubgroupCatRandomizeHelp"), 
                           label="", ttHeader="Randomization",
                           ttContent=HTML(violatedMessage),
                           class=styleClassHint)
      )
    )
    
    if(dist == "Explicit"){
      randomizeDiv <- hidden(randomizeDiv)
      randomize <- F
    }
    if(!randomize || !randAvail$valid) randomOptions <- hidden(randomOptions)

    withr::with_seed(1337, {
      s <- sample(4,length(values),replace=T)
    })
    
    placeholder <- paste0(s, collapse=", ")
    
    explicitValues <- tags$div(
      id=ns("otherVariableSubgroupCatIndependentValuesDistributedDiv"),
      class="textAreaOtherVariableFreq",
      textAreaInput(ns("otherVariableSubgroupCatIndependentValuesDistributed"),
                    label="Frequency per unit", value=paste0(valuesFreq, collapse = ", "),
                    width="100%", resize="vertical",
                    placeholder=placeholder),
      hidden(
        tags$div(
          id=ns("otherVariableSubgroupCatIndependentValuesDistributedWarning"),
          class="fontColor-error",
          style=paste0("margin:-15px 0px 15px 5px;",
                       "font-size:12px; font-weight:bold;"),
          textOutput(ns("otherVariableSubgroupCatIndependentValuesDistributedWarningText"))
        )
      )
    )
    if(dist == "Equally") explicitValues <- hidden(explicitValues)
    
    leftContent <- NULL
    {
      leftContent <- tags$div(
        id=ns("otherVariableSubgroupCreateDiv"),
        class = if(subgroupExplicit) "otherVariableSubgroupExplicit" else "",
        
        tags$div(
          class="textAreaOtherVariable",
          textAreaInput(ns("otherVariableSubgroupCatIndependentValues"),
                        label="Values", value=paste0(values, collapse = ", "),
                        width="100%", resize="vertical",
                        placeholder="male, female\nsmall, medium, great\n1, 2, 3, 4 ...")),
        
        tags$div(
          style="font-weight:bold;margin-bottom:5px;",
          "Distributed"
        ),
        bayasGroupedButtons(ns("otherVariableSubgroupCatIndependentDistributed"),
                            btnNames=c("Equally","Explicit"), 
                            btnValues=c("equally","explicit"),
                            selected=tolower(dist), columns=2,
                            outerStyle = "margin-bottom:15px;",
                            btnStyle = "width:100%;"),
        
        
        explicitValues,
        
        fluidRow(
          column(10,
                 checkboxInput(ns("otherVariableSubgroupCatCapped"), 
                                 label=tags$b("Capped"),
                                 value=capped)
          ),
          column(2,
                 style="margin-top:4px;",
                 labelWithInfo(id=ns("otherVariableSubgroupCatCappedHelp"), 
                               label="", ttHeader="Capped",
                               ttContent=paste0("If the dataset length is not a multiple of the number of elements, ",
                                                "elements will be cut to the desired length."))
          )
        ),
        
        randomizeDiv,
        randomOptions
        
      )
    }
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      HTML(paste0("<b>Name your variable</b>"))
               ),
               column(8,
                      textInput(ns("variableSubgroupModalName"), NULL, value=otherVariableName,
                                placeholder="unnamed", width="100%")
               )
      ),
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      tags$div(
                        selectizeInput(ns("otherVariableSubgroupOf"),
                                       label=NULL, choices=validUpperGroups,
                                       selected=subgroupOf),
                        fluidRow(
                          column(10,
                                 checkboxInput(ns("otherVariableSubgroupExplicitPerSubgroup"),
                                                 label=tags$b("Explicitly for elements"),
                                                 value=subgroupExplicit)
                          ),
                          column(2,
                                 style="margin-top:4px;",
                                 labelWithInfo(id=ns("otherVariableSubgroupExplicitPerSubgroupHelp"), 
                                               label="", ttHeader="Explicitly for all elements",
                                               ttContent=paste0("If checked, you have to define for each element of the group ",
                                                                "the subgroup elements. This is useful if the subgroup is ",
                                                                "not equally distributed according to the upper group."))
                          )
                        ),
                        if(subgroupExplicit){
                          selectizeInput(ns("otherVariableSubgroupElements"),
                                         label=NULL, choices=subgroupElements)
                        }else{
                          hidden(
                            selectizeInput(ns("otherVariableSubgroupElements"),
                                           label=NULL, choices=subgroupElements)
                          )
                        }
                      ),
                      
                      if(hasSubgroup){
                        leftContent
                      }else{
                        disabled(leftContent)
                      }
                      
               ),
               column(8,
                      tags$div(
                        style="max-height:374px; overflow-y:auto;",
                        DTOutput(ns("otherVariableSubgroupCatTable"), height="100%")
                      )
               )
      )
      
    )
  }
  
  ###########################
  ##### Categorical    ######
  ##### Replacement    ######
  ###########################
  else if(step==16){
    if(otherVariableName == "unnamed") otherVariableName <- ""
    
    replacementVariables <- cMCD$getOtherVariableIdsForReplacement()
    if(!is.null(varId)) replacementVariables <- replacementVariables[replacementVariables != varId]
    choosenReplacementVar <- cate$getReplaceValuesOf() #ID
    
    replacementVariables <- c(replacementVariables, "Replace values of ..."="")
    if(is.null(choosenReplacementVar)) choosenReplacementVar <- ""
    
    varElements <- c()
    choosenElement <- "" 
    
    if(!is.null(choosenReplacementVar) && choosenReplacementVar != "" &&
       choosenReplacementVar %in% replacementVariables){
      
      oV <- cMCD$getOtherVariable(choosenReplacementVar)
      catModel <- oV$getCategoricalModel()
      catType <- catModel$getType()
      if(is.null(catType)) return()
      
      varElements <- oV$getCategoricalModel()$getValues()
      if(catType == "independent"){
        
      }else if(catType == "subgroup"){
        if(is.list(varElements)){ 
          varElements <- nested_list_for_cat_subgroups(varElements)
        }
      }else{
        warning("catType should be independent or subgroup!")
      }
      choosenElement <- varElements[[1]][1]
    }
    
    
    showElementList <- NULL
    if(choosenReplacementVar == ""){
      showElementList <- disabled(selectizeInput(ns("otherVariableReplacementElement"),
                                                 label=NULL, choices=c(), selected=""))
    }else{
      showElementList <- selectizeInput(ns("otherVariableReplacementElement"),
                                        label=NULL, choices=varElements,
                                        selected=choosenElement)
    }
    
    replaceElement <- NULL
    if(is.null(choosenElement) || choosenElement == ""){
      replaceElement <- disabled(tags$div(
        class="textAreaOtherVariable",
        textAreaInput(ns("otherVariableCatReplacementValues"),
                      label="Replacement value", value="",
                      width="100%", resize="vertical")))
    }else{
      val <- cate$getReplaceValues(choosenElement)[[2]]
      replaceElement <- tags$div(
        class="textAreaOtherVariable",
        textAreaInput(ns("otherVariableCatReplacementValues"),
                      label="Replacement value", value=val,
                      width="100%", resize="vertical"))
    }
    
    
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      HTML(paste0("<b>Name your variable</b>"))
               ),
               column(8,
                      textInput(ns("variableReplacementModalName"), NULL, value=otherVariableName,
                                placeholder="unnamed", width="100%")
               )
      ),
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      tags$div(
                        
                        selectizeInput(ns("otherVariableReplacementOf"),
                                       label=NULL, choices=replacementVariables,
                                       selected=choosenReplacementVar),
                        
                        showElementList,
                        replaceElement
                        
                      )
               ),
               column(8,
                      tags$div(
                        style="max-height:374px; overflow-y:auto;",
                        DTOutput(ns("otherVariableCatReplacementTable"), height="100%")
                      )
               )
      )
      
    )
  }
  
  ###########################
  ###### Distribution #######
  ###########################
  else if(step==40){
    if(otherVariableName == "unnamed") otherVariableName <- ""
    
    oV <- cMCD$getOtherVariable(varId)
    cons <- oV$getConstraints()
    
    negCheck <- checkboxInput(ns("otherVariableNegation"), 
                               label="Negate values", value=dist$getNegateValues())
    if("positive" %in% cons) negCheck <- disabled(negCheck)
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      HTML(paste0("<b>Name your variable</b>"))
               ),
               column(8,
                      textInput(ns("variableModalName"), NULL, value=otherVariableName,
                                placeholder="unnamed", width="100%")
               )
      ),
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      tags$div(
                        style="margin-bottom:20px;",
                        HTML(paste0("<b>Distribution parameter</b>"))),
                      planning_creatingStepsOtherVariable_parameter(ns, dist, 
                                                                    cMCD, varId),
                      
                      tags$div(
                        style="margin-top:25px;",
                        HTML(paste0("<b>Additional settings</b>")),
                        
                        tags$div(
                          style="margin-top:10px;",
                          negCheck
                        ),

                        fluidRow(
                          column(8,
                                 bayasNumericInput(ns("otherVariableRandomSeed"), label=NULL,
                                                   value=dist$getSeed(), min=-.Machine$integer.max,
                                                   max=.Machine$integer.max, step=1, integer=T,
                                                   invalidMessage=T, invalidTooltip = T,
                                                   class="randomizeFrequencyRandomSeedNumericInput")
                          ),
                          column(2,
                                 style="margin-top:3px; padding-left:0px;",
                                 diceButton(inputId=ns("otherVariableRandomSeedDice"), 
                                            tt=paste0("Generates a random number"))
                          ),
                          column(2,
                                 style="margin-top:8px;",
                                 labelWithInfo(id=ns("otherVariableRandomizeRandomSeedHelp"), 
                                               label="", ttHeader="Random seed",
                                               ttContent=HTML(paste0("Setting seeds can be used to ensure reproducibility ",
                                                                     "by initializing the random number generator with a fixed value, ",
                                                                     "allowing the same random sampling process to be replicated.")))
                          )
                        )
                        
                      )
                      
               ),
               column(8,
                      plotOutput(ns("otherVariableDistributionPlot"), height="300px")
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
    planning_backButton(ns, "planningOtherVariableModalBack", step, l),
    tags$div(
      style="margin-top:20px; margin-left:30px;",
      icon(class= "fontColor-primary", "info-circle"),
      tags$div(style="display: inline;font-weight: bold;font-size: 13px;", 
               "You can change everything later on")
    )
  )
  
  return(l)
}


planning_creatingStepsOtherVariable_parameter <- function(ns, dist, cMCD, varId){
  l <- tagList()
  paras <- dist$getParameter()
  for(di in seq_len(length(paras))){
    para <- paras[[di]]
    type <- para$getType()
    
    refVarId <- cMCD$getOtherVariableIdsForDependency()
    if(!is.null(varId)) refVarId <- refVarId[refVarId!=varId]
    selected <- if(!is.null(para$getOtherVariableId()) &&
                   para$getOtherVariableId() %in% refVarId) para$getOtherVariableId() else ""
    refVarId <- as.list(refVarId)
    refVarNames <- c()
    for(id in refVarId){
      refVarNames <- c(refVarNames,cMCD$getOtherVariable(id)$getName())
    }
    names(refVarId) <- refVarNames
    refVarId <- list.append(refVarId, "","Variable")
    
    val <- tags$div(
      id=ns(paste0("otherVariableDistributionParameterDiv",di)),
      bayasNumericInput(ns(paste0("otherVariableDistributionParameter",di)),
                        label=NULL, value=para$default_val, min=para$min_val, max=para$max_val, 
                        integer=para$discrete, invalidMessage=T, invalidTooltip = T))
    if(type!="variable"){
      val <- tags$div(
        id=ns(paste0("otherVariableDistributionParameterDiv",di)),
        bayasNumericInput(ns(paste0("otherVariableDistributionParameter",di)),
                          label=NULL, value=para$value, min=para$min_val, max=para$max_val, 
                          integer=para$discrete, invalidMessage=T, invalidTooltip = T))
    }
    var <- tags$div(
      id=ns(paste0("otherVariableDistributionParameterVarDiv",di)),
      class="selectInput-neg-margin-child",
      
      selectizeInput(ns(paste0("otherVariableDistributionParameterVar",di)),
                     label=NULL, choices=refVarId, selected=selected),
      
      var <- bslib::popover(
        id = ns(paste0("otherVariableDistributionParameterVarDivPopover",di)),
        trigger = hidden(tags$div(
          id=ns(paste0("otherVariableDistributionParameterVarDivPopoverDiv",di)),
          style = "text-align: right;",
          icon(
            name="question-circle", 
            class="fontColor-warning", 
            style="margin-left:auto; margin-bottom: auto; margin-top:auto;"))),
        title = "",
        "",
        placement = "right",
        options = list(trigger="hover")
      )

    )
    
    if(type == "value") var <- hidden(var)
    if(type == "variable") val <- hidden(val)

    
    paraDiv <- fluidRow(
      column(5,
             HTML(para$display_name)),
      column(5,
             val,
             var),
      column(2,
             style = "text-align: right;",
             tags$div(
               style="margin-top:5px;",
               bayasSelectButton(ns(paste0("otherVariableDistributionParameterSwitch",di)),
                                 type)))
    )
    l[[di]]<- paraDiv
  }
  divPara <- tags$div(
    id=ns("otherVariableDistributionParameter"),
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
      
      refVarId <- cMCD$getOtherVariableIdsForDependency()
      refVarId <- refVarId[refVarId!=varId]
      selected <- if(!is.null(para$getOtherVariableId()) &&
                     para$getOtherVariableId() %in% refVarId) para$getOtherVariableId() else ""
      refVarId <- as.list(refVarId)
      refVarNames <- c()
      for(id in refVarId){
        refVarNames <- c(refVarNames,cMCD$getOtherVariable(id)$getName())
      }
      names(refVarId) <- refVarNames
      refVarId <- list.append(refVarId, "","Variable")
      
      val <- tags$div(
        id=ns(paste0("otherVariableDistributionParameterAltDiv",di)),
        bayasNumericInput(ns(paste0("otherVariableDistributionParameterAlt",di)),
                          label=NULL, value=para$default_val, min=para$min_val, max=para$max_val, 
                          integer=para$discrete, invalidMessage=T, invalidTooltip = T))
      if(type!="variable"){
        val <- tags$div(
          id=ns(paste0("otherVariableDistributionParameterAltDiv",di)),
          bayasNumericInput(ns(paste0("otherVariableDistributionParameterAlt",di)),
                            label=NULL, value=para$value, min=para$min_val, max=para$max_val, 
                            integer=para$discrete, invalidMessage=T, invalidTooltip = T))
      }
      var <- tags$div(
        id=ns(paste0("otherVariableDistributionParameterAltVarDiv",di)),
        class="selectInput-neg-margin-child",
        
        selectizeInput(ns(paste0("otherVariableDistributionParameterAltVar",di)),
                       label=NULL, choices=refVarId, selected=selected),
        
        var <- bslib::popover(
          id = ns(paste0("otherVariableDistributionParameterAltVarDivPopover",di)),
          trigger = hidden(tags$div(
            id=ns(paste0("otherVariableDistributionParameterAltVarDivPopoverDiv",di)),
            style = "text-align: right;",
            icon(
              name="question-circle", 
              class="fontColor-warning", 
              style="margin-left:auto; margin-bottom: auto; margin-top:auto;"))),
          title = "",
          "",
          placement = "right",
          options = list(trigger="hover")
        )
        
      )

      
      if(type=="value") var <- hidden(var)
      if(type=="variable") val <- hidden(val)
      
      
      paraDiv <- fluidRow(
        column(5,
               HTML(para$display_name)),
        column(5,
               val,
               var),
        column(2,
               style = "text-align: right;",
               tags$div(
                 style="margin-top:5px;",
                 bayasSelectButton(ns(paste0("otherVariableDistributionParameterAltSwitch",di)),
                                   type)))
      )
      lAlt[[di]]<- paraDiv
    }
    
    alt <- tags$div(style="margin-top:10px;",
                    checkboxInput(
                      ns("otherVariableDistributionParameterCheckbox"),
                      "Use alternative parameterization"))
    divAltPara <- hidden(tags$div(
      id=ns("otherVariableDistributionParameterAlt"),
      style="",
      lAlt
    ))
  }
  ret <- tags$div(divPara,
                  divAltPara,
                  alt)
  
  return(ret)
}

###################################
########### Help pages ############ 
###################################

planning_creatingStepsOtherVariable_help <- function(ns, step){
  
  l <- tags$div()
  
  ###########################
  ##### Cont vs Discrete ####
  ####   vs Categorical  ####
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
          HTML(paste0("<i>What is a <b>variable</b>?</i>"))
        ),
        tags$div(
          style="",
          "Values that can be used as predictor to estimate the response variable."
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of variables</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<b>Continuous</b> measurements like the size, length or weight of an individual, ",
                      "or some concentration, signals, temperature etc., but also probabilities.",
                      "<br>",
                      "<b>Discrete</b> values like heart rate, number of cells or age in days/years.",
                      "<br>",
                      "<b>Categorical</b> variables like sex, strain or batch."))
        )
      )
    )
  }
  
  ###########################
  ######## Cont range #######
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
          HTML(paste0("<i><b>Positive</b> and <b>negative</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>It makes a difference for the statistical model if your ",
                      "variable is just positive, negative, unlimited or in another range. ",
                      "Additionally, exluding the zero makes also a difference.</p>",
                      "<p>Be aware of choosing the right type, but you can go back and change it at any time.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of different variable ranges</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Only <b>positive</b> variable without zero: Length or weight of an individual. ",
                      "A negative value or even 0 wouldn't make sense. </p>",
                      "<p>Only <b>positive</b> variable with zero: Covered distance from the center of a test field in an Open Field Test. </p>",
                      "<p><b>Percentage</b> variable: Proportion of disease incidence in a population. </p>",
                      "<p><b>Unlimited</b> variable: Difference in time, weight or any other size. </p>",
                      "<p>Only <b>negative</b> variable: Substraction where the minuend is always smaller. ",
                      "Negative values are not handeld directly. They are internally transformed to ", 
                      "positive values by multiplying them by -1.</p>"))
        )
      )
    )
  }
  
  ###########################
  ###### Discrete range #####
  ###########################
  else if(step==3){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Integer</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>It makes a difference for the statistical model if your ",
                      "variable is just positive, negative, or has a limit. </p>",
                      "<p>Be aware of choosing the right type, but you can go back and change it at any time.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of different variable ranges</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Unlimited <b>positive</b> variable: Number of cells, thunderbolts, etc. </p>",
                      "<p>Limited <b>positive</b> variable: Number of heads by 10 coin flips. </p>",
                      "<p><b>Bernoulli</b> variable: Success or failure. </p>",
                      "<p><b>Negative</b> variable: Difference between two integers. ",
                      "There is no distribution that could generate such data. ",
                      "Often you have a known maximum negative value that could be used to shift ", 
                      "the data to just postive values. Consider using this transformation to use e.g. ",
                      "unlimited positive variable. </p>"))
        )
      )
    )
  }
  
  ###########################
  ######## Cont noise #######
  ######## positive   #######
  ###########################
  else if(step==4){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Positive</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable allows only positive values <b>without</b> zero.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Log-normal</b>: Weight, size, etc. </p>",
                      "<p><b>Gamma</b>: Age distribution of cancer incidence, Peak calling in ChIP-seq. </p>",
                      "<p><b>Inverse Gaussian</b>: Reaction times or time-to-failure data. </p>",
                      "<p><b>F</b>: Comparing the expression levels of a particular gene between two or more groups of cells. </p>",
                      "<p><b>Chi-squared df = 1</b>: Testing for independence in contingency tables or modeling count data with extra zeros. </p>"))
        )
      )
    )
  }
  
  ###########################
  ###### Cont noise     #####
  ###### positive+zero  #####
  ###########################
  else if(step==5){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Positive</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable allows positive and zero values.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Exponential</b>: Time until a certain event. </p>",
                      "<p><b>Chi-squared df &ne; 1</b>: Measurements of gene expression levels or protein concentrations. </p>",
                      "<p><b>Weibull</b>: Similar to Exponential but with more flexibility in modeling the hazard rate over time. </p>"))
        )
      )
    )
  }
  
  ###########################
  ####### Cont noise  #######
  ####### %           #######
  ###########################
  else if(step==6){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Percentage</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable defines percentage values (0-1).</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Beta</b>: Percentage. </p>"))
        )
      )
    )
  }
  
  ###########################
  ####### Cont noise  #######
  ####### unlimited   #######
  ###########################
  else if(step==7){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Unlimited</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable defines values of real numbers.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Normal</b>: Differences, approximation for many use cases. </p>",
                      "<p><b>Cauchy</b>: Outliers or extreme observations. </p>",
                      "<p><b>Logistic</b>: Probability of having a genetic variant. </p>",
                      "<p><b>Uniform</b>: Growth rate of a population of bacteria over a given time period. </p>",
                      "<p><b>Student t</b>: Effect of a certain treatment on the body weight. </p>"))
        )
      )
    )
  }
  
  ###########################
  ##### Discrete noise ######
  ##### positive       ######
  ###########################
  else if(step==10){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Positive</b> integers</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable allows integer values without an upper limit.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Poisson</b>: The number of mutations on a strand of DNA per unit length; number of goals in sport. </p>",
                      "<p><b>Negative-Binomial</b>: Hospital length of stay. </p>",
                      "<p><b>Geometric</b>: Probability of an event occurring for the first time after a certain number of trials. </p>",
                      "<p><b>Hypergeometric</b>: Probability of selection of a certain number of individuals with a particular trait ", 
                      "in a sample drawn without replacement from a finite population of individuals with and without the trait of interest. </p>"))
        )
      )
    )
  }
  
  ###########################
  ##### Discrete noise ######
  ##### limited by N   ######
  ###########################
  else if(step==11){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Positive</b> integers</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable allows integer values with an upper limit.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Binomial</b>: Number of successes in N given trials. </p>",
                      "<p><b>Beta-Binomial</b>: Similar to Binomial, but with flexible dispersion. </p>"))
        )
      )
    )
  }
  
  ###########################
  ##### Discrete noise ######
  ##### 0 or 1         ######
  ###########################
  else if(step==12){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Positive</b> integers</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your variable allows just values of 0 and 1.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for the distributions</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Bernoulli</b>: Coin toss. </p>"))
        )
      )
    )
  }
  
  ###########################
  ##### Categorical    ######
  ##### overview       ######
  ###########################
  else if(step==13){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>What is a <b>categorical</b> variable?</i>"))
        ),
        tags$div(
          style="",
          "Categorical variables represent qualitative properties of the data which can't be measured numerically."
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of different categorical variables</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>Independent</b> categorical variable: Sex or strain of an individual. </p>",
                      "<p><b>Subgroup</b> of a categorical variable: Ways of treatment that might differ for each strain. </p>",
                      "<p><b>Replacement</b> of a categorical variable: Replace strain <q>A</q> with <q>1</q> and strain <q>B</q> with <q>2</q>. </p>"))
        )
      )
    )
  }
  
  ###########################
  ###### Distribution #######
  ###########################
  else if(step==40){
    
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

