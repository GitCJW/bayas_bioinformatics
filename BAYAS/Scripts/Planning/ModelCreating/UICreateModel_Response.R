
#primary is a vector of strings from ModelCreatingDataResponse 
#type, range or dist
planning_creatingStepsResponse <- function(ns, step, responseName,  primary, 
                                           links=c(unlist(planningLinkEnum()),NULL),
                                           positiveDistribution,
                                           selectedLink){
  
  l <- list()
  distEnum <- planningDistribtionsEnum("response")
  
  match.arg(links, several.ok = T)
  
  
  ###########################
  ##### Cont vs Discrete ####
  ###########################
  if(step==1){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      tags$div(
        style="margin-bottom: 10px;",
        HTML(paste0("<b>How does your response variable look like?</b>"))
      ),
      tags$div(
        style="display:flex; text-align:center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("typeOfResponseCont"), 
                           imageFile="Images/Planning/ModelPlanning/VariableTypes/TypeContinuous.png", 
                           title="Continuous", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="200px",
                           primary=any("cont" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("typeOfResponseDiscrete"), 
                           imageFile="Images/Planning/ModelPlanning/VariableTypes/TypeDiscrete.png", 
                           title="Discrete", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="200px",
                           primary=any("discrete" %in% primary))
        )
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
        HTML(paste0("<b>What is the range of the response?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfResponseCont1"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Positive_wo_null2.png", 
                           title="Positive >0", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("positiveGreater" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfResponseCont2"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Positive_w_null2.png", 
                           title=HTML("Positive &ge;0"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("positive" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfResponseCont3"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Percent2.png", 
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
          imageButtonTitle(btnId=ns("rangeOfResponseCont4"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Neg_pos2.png", 
                           title="Real (no limits)", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("unlimited" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfResponseCont5"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Neg_w_null2.png", 
                           title=HTML("Negative &le;0"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("negGreater" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfResponseCont6"), imageFile="Images/Planning/ModelPlanning/Range/Continuous/Neg_wo_null2.png", 
                           title="Negative <0", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("negative" %in% primary))
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
        HTML(paste0("<b>What is the range of the response?</b>"))
      ),
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfResponseDiscrete1"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos2.png", 
                           title=HTML("Positive"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("discretePos" %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("rangeOfResponseDiscrete2"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos_N2.png", 
                           title="Limited by N", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="99px",
                           imgClass = "borderColor-regular",
                           imgStyle="border:1px solid; border-radius:2px;",
                           primary=any("limitedN" %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("rangeOfResponseDiscrete3"), imageFile="Images/Planning/ModelPlanning/Range/Discrete/pos_0_1.png", 
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse1"), imageFile="Images/Distributions/Continuous/Log_Normal.jpg", 
                           title=HTML("Log-Normal"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Log_Normal %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfResponse2"), imageFile="Images/Distributions/Continuous/Gamma.jpg", 
                           title=HTML("Gamma"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Gamma %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse3"), imageFile="Images/Distributions/Continuous/Inverse_Gaussian.jpg", 
                           title=HTML("Inverse Gaussian"), selected=F,
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse4"), imageFile="Images/Distributions/Continuous/Exponential.jpg", 
                           title=HTML("Exponential"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Exponential %in% primary))
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse5"), imageFile="Images/Distributions/Continuous/Beta.jpg", 
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse6"), imageFile="Images/Distributions/Continuous/Normal.jpg", 
                           title=HTML("Normal"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Normal %in% primary))
        )#,
        # tags$div(
        #   style="flex:1;",
        #   imageButtonTitle(btnId=ns("distributionOfResponse7"), imageFile="Images/Distributions/Continuous/Logistic.jpg", 
        #                    title=HTML("Logistic"), selected=F,
        #                    btnStyle="width:auto; height:auto;", imgHeight="150px",
        #                    primary=any(distEnum$Logistic %in% primary))
        # )
      )
    )
    
  }
  
  ###########################
  ###### Cont noise     #####
  ###### negative+zero  #####
  ###########################
  else if(step==8){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>Are positive values <u>theoretically</u> possible?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          
          imageButtonTitle(btnId=ns("negativeValuesZeroPositiveValuesPossibleYes"), 
                           imageFile="Images/Planning/ModelPlanning/NegativeValues/NegativeValuesYes.jpg", 
                           title="Yes", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(c(T,"T") %in% primary))
          
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("negativeValuesZeroPositiveValuesPossibleNo"), 
                           imageFile="Images/Planning/ModelPlanning/NegativeValues/NegativeValuesNo.jpg", 
                           title="No", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(c(F,"F") %in% primary))
        )
      )
    )
  }
  
  ###########################
  ###### Cont noise     #####
  ###### negative <0    #####
  ###########################
  else if(step==9){
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<b>Are positive values <u>theoretically</u> possible?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          
          imageButtonTitle(btnId=ns("negativeValuesPositiveValuesPossibleYes"), 
                           imageFile="Images/Planning/ModelPlanning/NegativeValues/NegativeValuesYes.jpg", 
                           title="Yes", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(c(T,"T") %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("negativeValuesPositiveValuesPossibleNo"), 
                           imageFile="Images/Planning/ModelPlanning/NegativeValues/NegativeValuesNo.jpg", 
                           title="No", selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(c(F,"F") %in% primary))
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse8"), imageFile="Images/Distributions/Discrete/Poisson.jpg", 
                           title=HTML("Poisson"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Poisson %in% primary))
        ),
        tags$div(
          style="flex:1; text-align:center;",
          imageButtonTitle(btnId=ns("distributionOfResponse9"), imageFile="Images/Distributions/Discrete/Negative_Binomial.jpg", 
                           title=HTML("Negative-Binomial"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Negative_Binomial %in% primary))
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse10"), imageFile="Images/Distributions/Discrete/Binomial.jpg", 
                           title=HTML("Binomial"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Binomial %in% primary))
        ),
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse11"), imageFile="Images/Distributions/Discrete/Beta_Binomial.jpg", 
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
        HTML(paste0("<b>How is the response distributed?</b>"))
      ),
      
      tags$div(
        style="display:flex; text-align: center;",
        tags$div(
          style="flex:1;",
          imageButtonTitle(btnId=ns("distributionOfResponse12"), imageFile="Images/Distributions/Discrete/Bernoulli.jpg", 
                           title=HTML("Bernoulli"), selected=F,
                           btnStyle="width:auto; height:auto;", imgHeight="150px",
                           primary=any(distEnum$Bernoulli %in% primary))
        )
      )
    )
    
  }  
  
  
  ###########################
  ###### Distribution #######
  ###########################
  else if(step==40){ 
    
    
    l$div <- tags$div(
      id=ns(paste0("stepsContent",step)),
      style="",
      
      fluidRow(style="margin-bottom:5px;",
               column(4,
                      HTML(paste0("<b>Name your response</b>"))
               ),
               column(8,
                      textInput(ns("responseModalName"), NULL, value=responseName,
                                placeholder="Response", width="100%")
               )),
      
      HTML(paste0("<b>Link function</b>")),
      
      planning_creationSteps_link(ns, links, positiveDistribution=positiveDistribution, 
                                  selected=selectedLink)
      
    )
  }
  #Empty
  else{
    l$div <- tags$div(
      style="min-height:350px;"
    )
  }
  
  l <- tags$div(
    planning_backButton(ns, "planningModalBack",step, l),
    tags$div(
      style="margin-top:20px; margin-left:30px;",
      icon(class= "fontColor-primary", "info-circle"),
      tags$div(style="display: inline;font-weight: bold;font-size: 13px;", 
               "You can change everything later on")
    )
  )
  
  return(l)
}



planning_creationSteps_link <- function(ns, links=c("cauchit","cloglog","identity",
                                                    "inv_square","inverse","log",
                                                    "logit","probit", "sqrt"),
                                        selected=NULL, positiveDistribution,
                                        numberCol=5){
  match.arg(links, several.ok = T)
  
  # browser()
  
  l <- tagList()
  
  for(i in seq_len(length(links))){
    imgName <- links[i]
    if(positiveDistribution && links[i] %in% c("identity","inverse")){
      imgName <- HTML(paste0(imgName, " <br> (not recommended)"))
    }
    if(i%%numberCol==1) l_sub <- tagList()
    l_sub[[((i+(numberCol-1))%%numberCol)+1]] <- 
      tags$div(
        style="flex:1;",
        imageButtonTitle(btnId=ns(paste0("linkFunction_",links[i])), 
                         imageFile=paste0("Images/Planning/ModelPlanning/LinkFunction/", links[i], ".png"), 
                         title=imgName, selected=F,
                         btnStyle="width:auto; height:auto;", imgHeight=NULL, imgWidth="100%", 
                         imgClass = "borderColor-regular",
                         imgStyle=paste0("aspect-ratio: 1 / 1 !important; ",
                                         "max-width:150px;",
                                         "border:1px solid; border-radius:2px;"),
                         primary=(!is.null(selected)&&selected==links[i]))
      )
    
    if(i%%numberCol==0 || length(links)==i) {
      l[[floor(((i-1)/numberCol)+1)]] <- tags$div(
        style="display:flex; text-align: center;",
        l_sub
      )
    }
  }
  
  return(l)
}



###################################
########### Help pages ############ 
###################################

planning_creatingStepsResponse_help <- function(ns, step, 
                                                links=c(unlist(planningLinkEnum()),NULL),
                                                positiveDistribution){
  
  l <- tags$div()
  
  ###########################
  ##### Cont vs Discrete ####
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
          HTML(paste0("<i>What is a <b>response</b> variable?</i>"))
        ),
        tags$div(
          style="",
          "A reponse also known as the outcome or dependent variable, is the measured value that shows the effect."
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of response variables</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<b>Continuous</b> measurements like the size, length or weight of an individual, ",
                      "or some concentration, signals, temperature etc., but also probabilities.",
                      "<br>",
                      "<b>Discrete</b> values like heart rate, number of cells or age in days/years."))
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
                      "response variable is just positive, negative, unlimited or in another range. ",
                      "Additionally, exluding the zero makes also a difference.</p>",
                      "<p>Be aware of choosing the right type, but you can go back and change it at any time.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of different response ranges</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Only <b>positive</b> response without zero: Length or weight of an individual. ",
                      "A negative value or even 0 wouldn't make sense. </p>",
                      "<p>Only <b>positive</b> response with zero: Covered distance from the center of a test field in an Open Field Test. </p>",
                      "<p><b>Percentage</b> response: Proportion of disease incidence in a population. </p>",
                      "<p><b>Unlimited</b> response: Difference in time, weight or any other size., </p>",
                      "<p>Only <b>negative</b> response: Substraction where the minuend is always smaller. ",
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
                      "response variable is just positive, negative, or has a limit. </p>",
                      "<p>Be aware of choosing the right type, but you can go back and change it at any time.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Examples of different response ranges</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Unlimited <b>positive</b> response: Number of cells, thunderbolts, etc. </p>",
                      "<p>Limited <b>positive</b> response: Number of heads by 10 coin flips. </p>",
                      "<p><b>Bernoulli</b> response: Success or failure. </p>",
                      "<p><b>Negative</b> response: Difference between two integers. ",
                      "There is no distribution that could generate such data. ",
                      "Often you have a known maximum negative value that could be used to shift ", 
                      "the data to just postive values. Consider using this transformation to use e.g. ",
                      "unlimited positive response. </p>"))
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
          HTML(paste0("<p>Your response allows only positive values <b>without</b> zero.</p>"))
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
                      "<p><b>Inverse Gaussian</b>: Reaction times or time-to-failure data. </p>"))
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
          HTML(paste0("<p>Your response allows positive and zero values.</p>"))
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
          HTML(paste0("<p><b>Exponential</b>: Time until a certain event. </p>"))
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
          HTML(paste0("<p>Your response defines percentage values (0-1).</p>"))
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
          HTML(paste0("<p>Your response defines values of real numbers.</p>"))
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
          HTML(paste0("<p><b>Normal</b>: Differences, approximation for many use cases. </p>"))
        )
      )
    )
  }
  
  ###########################
  ####### Cont noise  #######
  ####### neg + zero  #######
  ###########################
  else if(step==8){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Negative</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Only negative values are uncommon and can mostly",
                      " simplified by transforming them to positive values.</p>",
                      "<p>If your data is just by chance pure negative, but can be in theory also positive ",
                      "choose a normal distribution. </p>",
                      "<p>If your negative sign is just artifical, choose a pure positive distribution.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for negative values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>By chance</b>: Difference between two measurements ",
                      "(e.g. scores, distances, chronometries, etc.), where the minuend was always smaller.</p>",
                      "<p><b>Artifical</b>: Ground level in ocean. The distance ",
                      "is measurement with a negative sign, but can be also interpreted in absolute values.</p>"))
        )
      )
    )
  }
  ###########################
  ####### Cont noise  #######
  ####### neg < 0     #######
  ###########################
  else if(step==9){
    l <- tags$div(
      id=ns(paste0("stepsContentExample",step)),
      style="",
      
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i><b>Negative</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Only negative values are uncommon and can mostly",
                      " simplified by transforming them to positive values.</p>",
                      "<p>If your data is just by chance pure negative, but can be in theory also positive ",
                      "choose a normal distribution. </p>",
                      "<p>If your negative sign is just artifical, choose a pure positive distribution.</p>"))
        )
      ),
      wellPanel(
        class="infoBox",
        style="padding:10px;",
        tags$div(
          style="margin-bottom:10px;",
          HTML(paste0("<i>Applications for negative values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p><b>By chance</b>: Difference between two measurements ",
                      "(e.g. scores, distances, chronometries, etc.), where the minuend was always smaller.</p>",
                      "<p><b>Artifical</b>: Ground level in ocean. The distance ",
                      "is measurement with a negative sign, but can be also interpreted in absolute values.</p>"))
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
          HTML(paste0("<p>Your response allows integer values without an upper limit.</p>"))
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
                      "<p><b>Negative-Binomial</b>: Hospital length of stay. </p>"))
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
          HTML(paste0("<p>Your response allows integer values with an upper limit.</p>"))
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
          HTML(paste0("<p>Your response allows just values of 0 and 1.</p>"))
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
  ####### Cont noise  #######
  ####### negative&0  #######
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
          HTML(paste0("<i><b>Negative</b> values</i>"))
        ),
        tags$div(
          style="",
          HTML(paste0("<p>Your response allows just values of 0 and 1.</p>"))
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
  ###### Distribution #######
  ###########################
  else if(step==40){
    l <- planning_creationSteps_link_help(ns, links, positiveDistribution)
  }
  return(l)
}



planning_creationSteps_link_help <- function(ns, 
                                             links=c(unlist(planningLinkEnum()),NULL),
                                             positiveDistribution){
  
  match.arg(links, several.ok = T)
  
  linkFunction <- planningLinkEnum()
  linkList <- list()
  if(linkFunction$cauchit %in% links){
    linkList[[match(linkFunction$cauchit, links)]] <- tags$p(tags$b("cauchit: "),"mapped to values between 0 and 1. Used for the probability of a binary event.")
  }
  if(linkFunction$cloglog %in% links){
    linkList[[match(linkFunction$cloglog, links)]] <- tags$p(tags$b("cloglog: "),"mapped to values between 0 and 1. Used for time to an event.")
  }
  if(linkFunction$identity %in% links){
    text <- "mapped to values the response can take. Used if response and linear predictor are on the same scale. Used for body size, weight, or biomass."
    if(positiveDistribution) text <- paste0(text, " Because the distribution used is positive, as is its expectation value, using a link function that maps a potentially negative linear predictor to negative values can result in significant sampling issues. Only use this link function if you have strong justification for doing so.")
    linkList[[match(linkFunction$identity, links)]] <- tags$p(tags$b("identity: "), text)
  }
  if(linkFunction$inv_square %in% links){
    linkList[[match(linkFunction$inv_square, links)]] <- tags$p(tags$b("inv_square: "),"Used for response times, waiting times, reaction times or enzyme activity, bacterial growth rate.")
  }
  if(linkFunction$inverse %in% links){
    text <- "Used if response and linear predictor are on the same scale. Used for reaction times, survival times or count data."
    if(positiveDistribution) text <- paste0(text, " Because the distribution used is positive, as is its expectation value, using a link function that maps a potentially negative linear predictor to negative values can result in significant sampling issues. Only use this link function if you have strong justification for doing so.")
    linkList[[match(linkFunction$inverse, links)]] <- tags$p(tags$b("inverse: "), text)
  }
  if(linkFunction$log %in% links){
    linkList[[match(linkFunction$log, links)]] <- tags$p(tags$b("log: "),"Used if scales of response and linear predictor differ, especially when the effect of the linear predictor is proportional to the expected value. Used for durations, sizes, or biomass.")
  }
  if(linkFunction$logit %in% links){
    linkList[[match(linkFunction$logit, links)]] <- tags$p(tags$b("logit: "),"mapped to values between 0 and 1. Used for the probability of success.")
  }
  if(linkFunction$probit %in% links){
    linkList[[match(linkFunction$probit, links)]] <- tags$p(tags$b("probit: "),"mapped to values between 0 and 1. Used for the probability of a binary event.")
  }
  if(linkFunction$sqrt %in% links){
    linkList[[match(linkFunction$sqrt, links)]] <- tags$p(tags$b("sqrt: "),"Used for the count of animals in a certain area as a function of habitat characteristics.")
  }
  
  l <- tags$div(
    style="",
    
    wellPanel(
      class="infoBox",
      style="padding:10px;",
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<i><b>Link function</b></i>"))
      ),
      tags$div(
        style="",
        HTML(paste0("<p>The link function transforms the expectation value of the ",
                    "response to the unlimited (-Inf, Inf) linear predictor. ",
                    "The recommended link function is always shown on the far left. ",
                    "E.g. a positive defined response uses often a log link function, ",
                    "because the log transformation maps the positive expecation values ",
                    "to an umlimited range (linear predictor).</p>"))
      )
    ),
    wellPanel(
      class="infoBox",
      style="padding:10px;",

      tags$div(
        style="",
        linkList
      )
    )
  )
  
  return(l)
}




###################################
############# Helper ##############
###################################

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
