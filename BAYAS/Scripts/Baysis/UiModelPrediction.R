model_prediction <- function(){
  ns <- NS("predictionTab")
  tabPanel(
    "Effects / Predictions",
           
     
     tags$div(
       style = "min-height:100vh; display:flex; flex-direction:column;",
       
       # first row for page elements. Second row holds next button
       tags$div(
         style = "flex:1;",
         
         shinybayas::bayasSidePanel(
           inputId = "sidebarPanelCompareModels",

           mainPanelClass = "getActiveColor",
           sidePanelClass = "getActiveColor",
           
           #Left panel
           sidePanel = tags$div(
             class="getActiveColor",
                        
              # Model formula
              # tags$div(style="padding:20px; border: solid 1px grey;",
              wellPanel(
                class="getActiveColor",
                
                selectInput(ns("selectModelFit"),label="Select fit", choices=c(), selected=""),
                
                bslib::accordion(
                  id = ns("collapseModelFormula"), 
                  open = NULL, 
                  bslib::accordion_panel(
                    title = "Show formula",
                    value = "open", 
                    # Formula
                    htmlOutput(outputId  = ns("selectedModelFormula"))
                  )
                ),
                
                #Model quantification or model prediction
                fluidRow(style="margin-top:40px;",
                         column(6, 
                                actionButton(ns("btnStableEffect"), label = "Effects ...", class = 'btn-primary', style="width:-moz-available; width: -webkit-fill-available;"),
                                tags$ul(style="margin-top:10px;",
                                  tags$li("Quantify effect sizes"),
                                  tags$li("Get credibility (significance)")
                                )),
                         column(6, 
                                actionButton(ns("verifyAndPredictSingleData"), "Predictions ...", class = 'btn-primary', style="width:-moz-available; width: -webkit-fill-available;"),
                                tags$ul(style="margin-top:10px;",
                                  tags$li("Credibility of predictions"),
                                  tags$li("Predictions for unknown datapoints")
                                ))
                )
              ),

              #Result list
              selectInput(ns("selectResults"), label=NULL, choices=c(), size=10,selectize=F)
           ),
           
           # Main Panel
           mainPanel = tags$div({
               tabsetPanel(id=ns("resultTabs")) 
             }
           )
         )
       ),
       
       # Row of elements for e.g. Navigation (Next Button) 
       tags$div(
         style = "flex:0;",
         footerPanel(
           backButtonsId=c(ns("buttonBackModelComparison"),ns("buttonBackModelFitting")),
           backButtons=c("Model comparison", "Model fitting")
         )
       )

     )
  )
}



model_prediction_effect_result_tab <- function(tabId, title, catChoices, numChoices, startValues){
  
  ns <- NS(tabId)

  tabPanel(value=ns("effectTab"), title = title,
    
    sidebarLayout(
      
      sidebarPanel = sidebarPanel(
        class="getActiveColor",
        
        tags$div(
          class = "borderColor-dark", 
          style ="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
          "Select single or multiple", 
          tags$b(" group "), 
          "or", 
          tags$b(" slope ") ,
          "effects"),
        
        labelWithInfo(ns("selectCatVar"),"Group effects","Group effects",tooltip$modelPredictionGroupEffects),
        
        selectInput(ns("selectCatVar"), label = NULL,
                    choices=catChoices, multiple = T, size=5, selectize = F),
        
        shinyjs::hidden(tags$div(id=ns("dependsOnString"), "depending on", #icon(name = "arrow-down", lib = "font-awesome"), 
                                 style="text-align:center; font-size:14px; font-weight:bold;")),
        
        build_numeric_inputs(ns, numChoices, startValues),

        
        tags$div(class="wordWrappedInHorizLine", " or "),
        
        labelWithInfo(ns("selectNumVar"),"Slope effects","Slope effects",tooltip$modelPredictionSlopeEffects),
        selectInput(ns("selectNumVar"), label=NULL, choices=numChoices$variable, multiple = T, size=5, selectize = F),
        
        
        tags$div(
          class = "borderColor-dark", 
          style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
          "Input options"),
        
        tags$div(style="margin-top:15px;", 
                 tags$label("Credible interval (CI)")),
        bayasGroupedButtons(ns("hdiType"), btnNames = c("HDI","ETI"), 
                       btnValues=c("hdi","eti"), columns=2,
                       selected="hdi", btnStyle="padding:3px; width:100%;"),
        
        tags$div(style="margin-top:15px;", 
                 tags$label("CI value")),
        
        sliderInput(ns("hdiRange"), label=NULL, 
                    value=0.9, min=0, max=1, step=0.01)
      ),
      
      mainPanel = mainPanel(style="margin-top:20px;",
                            
        #Verbal summary
        tags$div(
          class = "borderColor-dark", 
          style="margin-bottom:25px; border-bottom:1px solid;padding-bottom:15px;",
          tags$div(
            class="getActiveColor",
             uiOutput(ns("verbalSummary"))
             )
          ),
                   
        shinyjs::hidden(tags$div(id=ns("globalEffectDiv"),
                 
           #Result matrix
           tags$div(
             class = "borderColor-dark", 
             style="margin-bottom:25px; border-bottom:1px solid; padding-bottom:15px;",
             tags$label("Effect matrix", style=""),
             shinybayas::bayasGroupedButtons(inputId=ns("matrixValues"), 
                                              btnNames=c("CI_low","median","mean","CI_high","pi"), 
                                              btnValues=c("min","median","mean","max","pi"),
                                              selected="pi", columns=5,
                                              btnStyle="padding:1px; width:100%;",
                                              outerStyle="margin-bottom:10px;"),
              tags$div(class="getActiveColor",
                       style="padding:5px; margin-bottom:15px;",
                       uiOutput(ns("matrixPlaceholder")),
                       DTOutput(ns("matrix"))),
              
              getReportButton(ns("reportEffectMatrix"), tooltip="Report effect matrix", class="",
                              style="margin-left:auto; margin-right:0px; display:block;")
                    
           ),
           
           #Summary table and distribution plot
           #check tags$head in ui.R
           tags$div(id="tabsetPanelEffectsSummary",
                    uiOutput(ns("distPlotPlaceholder")),
                    withSpinner(plotOutput(ns("densityPlot"))),
                    
                    tags$div(class="getActiveColor",
                             style="padding:5px;",
                             uiOutput(ns("summaryPlaceholder")),
                             withSpinner(DTOutput(ns("summary")))),
                    
                    getReportButton(ns("reportEffectDistribution"), tooltip="Report effect distribution", class="",
                                    style="float:right; margin: 5px 5px 0px 0px;")

           )
         ))      
      )
    )
  )
}



model_prediction_predict_result_tab <- function(tabId, title, 
                                                response,
                                                catChoices, catValues,  
                                                numChoices, numStartValues,
                                                usedVarsAdd){
  ns <- NS(tabId)
  
  lower <- response$lower
  upper <- response$upper
  if(response$lower == "-INF"){
    lower <- .Machine$double.xmin
  }else if(response$lower == ">0"){
    lower <- 0
  }
  if(response$upper == "INF"){
    upper <- .Machine$double.xmax
  }else if(response$upper == "<1"){
    upper <- 1
  }

  
  tabPanel(
     value=ns("effectTab"), 
     title = title,
           
     sidebarLayout(
       
       sidebarPanel = sidebarPanel(
         class="getActiveColor",
                                   
         tags$div(
           class = "borderColor-dark", 
           style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", "Make predictions based on certain input values"),
         
         build_covariate_inputs(ns, 
                                catChoices, catValues,  
                                numChoices, numStartValues,
                                usedVarsAdd),
         
         fluidRow(style="margin-top:20px;", column(width=6,offset=6,
                         actionButton(ns("MakePrediction"), "Predict", class = 'btn-primary', width="100%"))),
  
         
         tags$div(
           class = "borderColor-dark", 
           style="text-align:left; border-bottom:1px solid; margin-bottom:20px; margin-top:20px;", 
           "Input options"),
         
         tags$div(style="margin-top:15px;", 
                  labelWithInfo("predictionInfoPlotType", "Plot type",
                                "Plot type", tooltip$modelPredictionPlotType)),
         bayasGroupedButtons(ns("plotType"), btnNames = c("Violin","Density"), 
                        btnValues=c("Violin","Density"), columns=2,
                        selected="Violin", btnStyle="padding:3px; width:100%;"),
         
         tags$div(style="margin-top:15px;",
                  labelWithInfo("predictionInfoFullPosterior", "Posterior (global variance)",
                                "Posterior (global variance)", tooltip$modelPredictionFullPosterior)),
         bayasGroupedButtons(ns("postType"), btnNames = c("Full","Mean"), 
                        btnValues=c("Full","Mean"), columns=2,
                        selected="Full", btnStyle="padding:3px; width:100%;"),
         
         tags$div(style="margin-top:15px;", 
                  labelWithInfo("predictionInfoCI", "Credible interval (CI)",
                                "Credible interval (CI)", tooltip$modelPredictionCI)),
         bayasGroupedButtons(ns("hdiType"), btnNames = c("HDI","ETI"), 
                        btnValues=c("hdi","eti"), columns=2,
                        selected="hdi", btnStyle="padding:3px; width:100%;"),
         
         tags$div(style="margin-top:15px;", 
                  labelWithInfo("predictionInfoCIValue", "CI value",
                                "CI value", tooltip$modelPredictionCIValue)),
         sliderInput(ns("hdiRange"), label=NULL, 
                           value=0.9, min=0, max=1, step=0.01),
         
         tags$div(style="margin-top:15px;", 
                  tags$label(HTML("Probability of area (P<sub>area</sub>)"))),
         tags$div(
           style= "display:flex; gap:5px;",
           tags$div(
             style = "flex:1;",
             bayasNumericInput(ns("probOfIntervalMin"), label="from", 
                               value = response$def_lower, min=lower, max=upper)
           ),
           tags$div(
             style = "flex:1;",
             bayasNumericInput(ns("probOfIntervalMax"), label="to", 
                               value = response$def_upper, min=lower, max=upper)
           )
         )

  
       ),
       
       mainPanel = mainPanel(style="margin-top:20px;",
                             
                             
         tags$div(
           class="getActiveColor borderColor-regular",
           style="overflow:auto; max-height:280px; padding:10px; margin-bottom:20px;",
           style="border: 1px solid; border-radius:5px;",
           DTOutput(ns("predictionsTable"))),
         
         
         #Summary table and distribution plot
         #check tags$head in Ui_BAYSIS.R
         tags$div(id="tabsetPanelPredictionSummary",
                  tabsetPanel(
                    tabPanel("Prediction distribution",
                             uiOutput(ns("distPredictionPlotPlaceholder")),
                             withSpinner(plotOutput(ns("densityPredictionPlot"))),
                             tags$div(
                               class="getActiveColor",
                               style="padding:5px;",
                               uiOutput(ns("summaryPredictionPlaceholder")),
                               withSpinner(DTOutput(ns("summaryPrediction")))),
                             getReportButton(inputId=ns("reportPrediction"), label="Report", 
                                             tooltip = "Reports the current plot and table", class="",
                                             style="margin-left:auto; margin-right:0px; margin-top:10px; display:block;")
                             ),
  
                    tabPanel("Prediction differences",
                             tags$div(class="getActiveColor",
                                      style="padding:5px;",
                                      uiOutput(ns("differencePredictionPlaceholder")),
                                      withSpinner(plotOutput(ns("differencePredictionPlot"))),
                                      withSpinner(DTOutput(ns("differencePrediction")))),
                             getReportButton(inputId=ns("reportPredictionDifference"), label="Report", 
                                             tooltip = "Reports the current plot and table", class="",
                                             style="margin-left:auto; margin-right:0px; margin-top:10px; display:block;"))
                    )
         )
       )
     )
  )
}


build_numeric_inputs <- function(ns, numChoices, startValues){
  list <- list()
  if(length(numChoices$variable) > 0){
    list <- lapply(1:length(numChoices$variable), function(i){
      model_prediction_num_inputs(ns=ns, numElement=numChoices[i,], numStartValue=startValues[i])
    })
  }
  return(list)
}

build_covariate_inputs <- function(ns, catChoices, catValues, numChoices,
                                   numStartValues, usedVarsAdd){
  
  ret <- list()
  if(length(catChoices) > 0){
    list <- lapply(1:length(catChoices), function(i){
      model_prediction_cat_inputs(ns=ns, catString=catChoices[i], catValues=catValues[[i]])
    })
    ret <- list
  }
  if(length(numChoices$variable) > 0){
    list <- lapply(1:length(numChoices$variable), function(i){
      model_prediction_num_inputs(ns=ns, numElement=numChoices[i,], 
                                  numStartValue=numStartValues[i], hidden=F,
                                  normalTitle=T)
    })
    ret <- list.append(ret, list)
  }
  return(ret)
}


model_prediction_num_inputs <- function(ns, numElement, numStartValue, hidden=T, 
                                        normalTitle=F){
  numString <- numElement$variable
  limits <- numElement[c("lower","upper")]
  minMax <- limitsToNumeric(limits)

  invalidMessage <- tags$div(style="float:left;",paste0("Invalid value"), icon(name="question-circle"))
  invalidTooltip <- paste0("Please provide a ", ifelse(cNum$Discrete==numElement$type,"discrete ",""),
                           " value between ", limits[1], " and ", limits[2],".")
  t <- tags$div(style="font-size:14px; font-weight:bold; margin-bottom:5px;", numString)
  if(!normalTitle) t <- divWithInfo(numString,numString,numString, tooltip$modelPredictionNumInputs)
  d <- tags$div(
      id=ns(paste0("div-",numString)),
      t,
           fluidRow(
             column(6, bayasNumericInput(ns(paste0("num-",numString)),
             label=NULL, min=minMax[1], max=minMax[2], value=as.numeric(str_trim(formatC(numStartValue,digits=4))), step=1,
             integer=cNum$Discrete==numElement$type, invalidMessage=T,
             invalidTooltip=T)),
             column(6, bayasGroupedButtons(ns(paste0("mMM-",numString)),
                                           btnNames=c("mean","min","median","max"), 
                                           selected="mean",
                                           columns=2, btnStyle="padding:0px; width:100%;"))
           ))
  if(hidden) return(hidden(d))
  return(d)
}

model_prediction_cat_inputs <- function(ns, catString, catValues){
    tags$div(
      id=ns(paste0("div-",catString)),
      tags$label(catString),
      selectInput(ns(paste0("cat-",catString)), label=NULL, choices = c("Choose an element"="",as.character(catValues)))
    )
}


explanation_of_effects <- function(){
  
  tags$div(
    tags$div(style="margin-bottom:20px;",
             tags$fieldset(tags$legend(style="font-size:16px; font-weight:bold;",
                                       "Short explanation"),
                           tags$p("We define effects as comparisons between groups, by using the marginal posteriors. ",
                                  "An effect is not limited by single marginal posteriors, but can be a sum or difference ",
                                  "of marignal posteriors.")),
    ),
    tags$div(style="",
             tabsetPanel(
               tabPanel("Illustration on an example",
                        tags$div(
                          
                          tags$div(style="display:flex;",
                                   tags$div(style="flex:1;",
                                            tags$p(paste0("We are using our BAYAS example dataset, where we try to predict the log of the concentration ",
                                                          "by sex and weight of each individual. Here, we have two different slopes for each sex (sex:weight).")),
                                            tags$p(style="text-align:center; margin-top:20px; margin-bottom:20px;",
                                                   "log_conc ~ sex + weight + sex:weight"),
                                            tags$p("We will simplify the model by ignoring the noise term and examine the marginal posteriors (coefficients) by just the median."),
                                            
                                            tags$div(style="display:flex;",
                                                     tags$div(style="flex:1;", "(Intercept): -1.12"),
                                                     tags$div(style="flex:1;", "sexmale: 1.98")
                                            ),
                                            tags$div(style="display:flex;",
                                                     tags$div(style="flex:1;", "weight: 0.04"),
                                                     tags$div(style="flex:1;", "sexmale:weight: 0.08")
                                            ),
                                            tags$p(style="text-align:center; margin-top:20px; margin-bottom:20px;",
                                                   "log_conc = -1.12 + 1.98*male + 0.04*weight + 0.08*weight*male"),
                                            tags$p("'male' is used as a dummy variable, which means that for groups of males 'male' is setted to 1, otherwise 0.")
                                   ),
                                   tags$div(style="flex:1;",
                                            tags$img(src = "Images/Effects/MP.png"))
                          ),
                          
                          tags$div(
                            tags$div(
                              id="modalEffectExplanation",
                              tabsetPanel(
                                tabPanel("Group effect",
                                         tags$div(style="padding:10px;",
                                                  tags$p(style="margin-top:20px;",
                                                         "Group effects are comparisons between categorical variables, defined in the dataset. ",
                                                         "Here we have one categorical variable 'sex' that can be either 'female' or 'male'.",
                                                         "The effect is the comparison between groups of 'females' and 'males' regarding of concentration."),
                                                  tags$p("In a simpler model without a sex dependent weight slope, the group effect of sex ",
                                                         "would just be the marginal posterior 'sexmale' of 1.98, which would be the shift between the two linear functions. ",
                                                         "In this model the group effect of sex also depends on weight. ",
                                                         "E.g. for groups of a (meaningless) weight of 0, the mean difference of both groups would be still 1.98 of log concentration. ",
                                                         "For a more meaningfull value like the mean of weight over all individuals (here ~19), the effect would be: "),
                                                  tags$p("(-1.12 + 1.98*0 + 0.04*19 + 0.08*19*0) - (-1.12 + 1.98*1 + 0.04*19 + 0.08*19*1)"),
                                                  tags$p("= 1.98*1 + 0.08*19*1 = 3.5"),
                                                  tags$p("If we increase the weight and compare both sex groups, the effect also increases. ")
                                         )),
                                tabPanel("Slope effect",
                                         tags$div(style="padding:10px;",
                                                  tags$p("Slope effects are either just the slope of groups (here female or male) ",
                                                         "or the difference of slopes of groups (here the difference between both slopes of females and males)."),
                                                  tags$p("The marginal posterior 'weight' is the slope for females."),
                                                  tags$p("The sum of 'weight' and 'sexmale:weight' is the slope for males."),
                                                  tags$p("'sexmale:weight' is the difference of slopes between both groups. ")))
                              )
                            )
                          )
                        )),
               tabPanel("Explanation in video",
                        renderUI({
                          tags$video(src="TEST_video.mp4", width="100%",type='video/mp4"', controls="controls")
                        })
               ))
    )
    
  )
  
}
