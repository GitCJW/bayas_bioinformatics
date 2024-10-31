planning_creatingModel_page <- function(){
  ns <- NS("creatingModel")
  
  devOptions <- ""
  if(localUse){
    devOptions <- 
      tags$div(
        style="margin-top:40px;",
        tags$label("Dev options"),
        tags$div(
          class="borderColor-dark",
          style="border:1px solid; padding:10px; border-radius:3px;",
          downloadButton(ns("saveExperiment"),"Save Experiment"),
          downloadButton(ns("saveExperimentAsState"),"Save Experiment as state"),
          fileInput(ns("loadExperiment"),"Load Experiment")
        )
      )
  }
  
  tabPanel("Creating model", 
           
     shinybayas::bayasSidePanel(
       inputId = "sidebarPanelPlanningCreateModel",
       
       mainPanelClass = "getActiveColor",
       sidePanelClass = "getActiveColor",
       
       sidePanelStyle = "",
       mainPanelStyle = "flex:5",
       
       sidePanel = tags$div(
         class="getActiveColor",

          actionButton(ns("newModel"), "New experiment", class="btn-primary",
                       width="100%"),
          wellPanel(
            style="padding:10px; margin-top:5px; text-align: justify;",
            class="getActiveColor",
            "Creates a new experiment design that contains an example dataset and a statistical model."
          ),
          
          tags$div(
            style="margin-top:30px;",
            tags$title("Created experiments"),
            tags$div(
              style="display:flex;",
              tags$div(
                style="flex:10",
                selectInput(ns("createdExperiments"), label="Experiments", 
                            choices=c())
              ),
              tags$div(
                style = "flex:1; padding:30px 5px 5px 10px;",
                title = "Copy the selected experiment",
                tags$button(
                  id=ns("cloneExperiment"),
                  icon("clone"), 
                  class="btn btn-trash action-button",
                  style="padding:0px; font-size:16px;")
              ),
              tags$div(
                style="flex:1; padding:30px 5px 5px 10px;",
                tags$button(
                  id=ns("removeExperiment"),
                  icon("trash"), 
                  class="btn btn-trash action-button",
                  style="padding:0px; font-size:16px;")
              )
            )
          ),
          
          getReportButton(ns("reportPlanningExperiment"), 
                          label = "Report experiment",
                          tooltip="Report whole planned experiment", class="",
                          style="width:100%;"),


          devOptions
  
       ),
       
       mainPanel = tags$div(
         class="getActiveColor",
                 tags$div(
                   fluidRow(
                     class="getActiveColor",
                     style="min-height:250px;",
                     style="margin-bottom:20px;",
                     
                     tags$div(
                       id=ns("dataForming"),
                       style="display:flex; gap: 20px;",
                     
                       tags$div(
                         style= "flex:1;",
                              style="",
                              tags$div(
                                id=ns("divOfSteps"),
                                
                                tabsetPanel(
                                  id=ns("uiOfSteps"),type="tabs", 
                                  header=tags$div(style="margin-bottom:20px;"),
                                  
                                  tabPanel(
                                    "1. Response",
                                    tags$div(
                                      id=ns("tabResponse"),
                                      style="margin:10px;",
                                      
                                      fluidRow(style="margin-bottom:5px;",
                                               column(4,
                                                      HTML(paste0("<b>Name your response</b>"))
                                                      ),
                                               column(8,
                                                      bayasTextInput(ns("responseName"), 
                                                                NULL, value="",
                                                                placeholder="Response", width="100%")
                                                      )),
                                                       
                                       fluidRow(style="margin-bottom:5px;",
                                                column(4,
                                                       HTML(paste0("<b>Noise term</b>"))
                                                ),
                                                column(8,
                                                       disabled(textInput(ns("noiseTermName"), 
                                                                          label=NULL, width="100%"))
                                                )),
                                       
                                       fluidRow(style="margin-bottom:20px;",
                                                column(4,
                                                       HTML(paste0("<b>Link function</b>"))
                                                ),
                                                column(8,
                                                       disabled(textInput(ns("selectLinkFunction"), 
                                                                          label=NULL, width="100%"))
                                                       # selectInput(ns("selectLinkFunction"),
                                                       #             label=NULL, choices=c(),
                                                       #             width="100%")
                                                       )),
                                       fluidRow(
                                         column(width=12, 
                                                 actionButton(ns("setNoiseTerm"), "Change response settings",
                                                              class="btn-primary",
                                                              width="100%"))
                                         )
                                      )
                                    ),
                            
                                  tabPanel("2. Other variables",
                                           tags$div(
                                             id=ns("tabVariables"),
                                           
                                             fluidRow(
                                               column(width=4,
      
                                                      actionButton(ns("addVariable"), "Add variable to data",
                                                                   class="btn-primary",
                                                                   width="100%",
                                                                   style="margin-bottom:10px;"),
                                                      tags$div(
                                                        class="colorizedList",
                                                        selectInput(ns("existingVariables"), NULL, size=12, choices=c(),
                                                                    selectize = F))
                                                      ),
                                               column(width=8,
                                                      tags$div(
                                                        style="display:flex;",
                                                        bayasTextInput(ns("variableName"), NULL, value="", 
                                                                  placeholder="unnamed", width="100%"),
                                                        tags$button(
                                                          id=ns("variableRemove"),
                                                          icon("trash"),
                                                          class="btn btn-trash action-button",
                                                          style=paste0("padding:0px; font-size:16px; ",
                                                                       "width:40px; height:20px; ",
                                                                       "padding: 9px 0px 0px 0px;")
                                                        )
                                                      ),
                                                      
                                                      
                                                      fluidRow(
                                                        column(6,
                                                               actionButton(ns("variableSetValues"), "Set values",
                                                                            style="padding: 5px;",
                                                                            width="100%", style="margin-bottom:10px;")),
                                                        column(6,
                                                               actionButton(ns("variableAddAsPredictor"),"As predictor",
                                                                            style="padding: 5px;",
                                                                            width="100%", style="margin-bottom:10px;"))
                                                      ),
                                                      hidden(uiOutput(ns("variableValues"))),
                                                      tags$div(
                                                        style="max-height:200px; overflow:auto;",
                                                        hidden(DTOutput(ns("variableValuesTable")))
                                                      ),
                                                      hidden(plotOutput(ns("variableValuesPlot"), height="250px"))
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("3. Model terms",
                                           tags$div(
                                             id=ns("tabPredictors"),
                                           
                                             fluidRow(
                                               column(width=4,
      
                                                      actionButton(ns("addPredictor"), "Add model term",
                                                                   class="btn-primary",
                                                                   width="100%",
                                                                   style="margin-bottom:10px;"),
      
                                                      tags$div(
                                                        class="colorizedList",
                                                        selectInput(ns("existingPredictors"), NULL, 
                                                                    size=12, choices=c(), selectize = F))
                                                      ),
                                               column(width=8,
                                                      tags$div(
                                                        style="display:flex;",
                                                        disabled(textInput(ns("predictorName"), label=NULL,
                                                                           width="100%")),
                                                        tags$button(
                                                          id=ns("predictorRemove"),
                                                          icon("trash"),
                                                          class="btn btn-trash action-button",
                                                          style=paste0("padding:0px; font-size:16px; ",
                                                                       "width:40px; height:20px; ",
                                                                       "padding: 9px 0px 0px 0px;")
                                                        )
                                                      ),
                                                      
                                                      tags$div(
                                                        style="font-weight:bold;",
                                                        "Involved variables"),
                                                      disabled(selectizeInput(ns("predictorInvolvedVariables"),
                                                                              label=NULL, choices=c(), multiple=T,
                                                                              width="100%")),
      
                                                      tags$div(
                                                        style="font-weight:bold;",
                                                        "Info"),
                                                      disabled(
                                                        tags$div(
                                                          class="infoBox",
                                                          style="padding:10px; margin-bottom:20px;",
                                                          uiOutput(ns("predictorInfo")))
                                                        ),

                                                      
                                                      actionButton(ns("predictorGoToParameter"), label="Show parameters",
                                                                   width="100%")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("4. Parameters",
                                           tags$div(
                                             id=ns("tabParameters"),
                                           
                                             fluidRow(
                                               column(width=4,
                                                      wellPanel(
                                                        class="getActiveColor",
                                                        style="min-height:297px; max-height: 338px; overflow:auto; padding:5px;",
                                                        
                                                        shinyTree(ns("parameterExistingPredictors"), 
                                                                  checkbox=F, themeIcons=F, multiple=F, wholerow=T)
                                                        
                                                      )
                                               ),
      
                                               column(width=8,
      
                                                      fluidRow(
                                                        style="margin-bottom:10px;",
                                                        column(12,  
                                                               actionButton(ns("parameterSetValue"), 
                                                                            "Set value/prior", width="100%", 
                                                                            style="padding: 5px;"))
                                                      ),
                                                      
                                                      # uiOutput(ns("parameterValueOverview")),
                                                      plotOutput(ns("parameterValueOverviewPlot"), height="220px"),
      
                                                      disabled(uiOutput(ns("parameterInfo")))
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("5. Generate data",
                                           tags$div(
                                             id=ns("tabGenerateData"),
                                             
                                             tags$div(
                                               style="margin:10px;",
                                               
                                               fluidRow(
                                                 style="margin-bottom:10px;",
                                                 column(4,
                                                        tags$div(
                                                          style="margin-top:8px;",
                                                          labelWithInfo(ns("sampleSizeUnitLabel"),
                                                                        "Sample Size unit",
                                                                        "Sample Size unit",
                                                                        HTML(tooltip$planningSampleSizeUnit))),
                                                 ),
                                                 column(4,
                                                        selectizeInput(ns("sampleSizeUnitVar"),
                                                                       label=NULL, choices=NULL)),

                                                 column(4,
                                                        bayasNumericInput(ns("sampleSizeUnitNumber"), label=NULL, 
                                                                                   value=1, min = 1, integer=T, 
                                                                                   invalidMessage = T, invalidTooltip = T,
                                                                          class="randomizeFrequencyRandomSeedNumericInput")
                                                        ),
                                               ),
                                               fluidRow(
                                                 column(4,
                                                        tags$div(
                                                          style="margin-top:8px;",
                                                          labelWithInfo(ns("sampleSizeDataPointsLabel"),
                                                                        "Total data points",
                                                                        "Total data points",
                                                                        HTML(tooltip$planningSampleSizeUnit))),
                                                 ),
                                                 column(4,
                                                          bayasNumericInput(ns("sampleSizeDataPoints"), 
                                                                            label=NULL, value=1, min = 1, max=1e6, integer=T, 
                                                                            invalidMessage = T, invalidTooltip = T,
                                                                            class="randomizeFrequencyRandomSeedNumericInput")
                                                 ),
                                                 column(4,
                                                        style="margin-top:7px;",
                                                        uiOutput(ns("sampleSizeTotalPointsValidMsg"))
                                                 ),
                                               ),
                                               
                                               fluidRow(
                                                 style="margin-top:30px;",
                                                 column(4,
                                                        labelWithInfo(id=ns("globalRandomSeedHelp"),
                                                                      label="Response seed", ttHeader="Random seed",
                                                                      ttContent=HTML(paste0("Setting seeds can be used to ensure reproducibility ",
                                                                                            "by initializing the random number generator with a fixed value, ",
                                                                                            "allowing the same random sampling process to be replicated.")))
                                                 ),
                                                 column(4,
                                                        bayasNumericInput(ns("globalRandomSeed"), label=NULL,
                                                                          value=123, min=-.Machine$integer.max,
                                                                          max=.Machine$integer.max, step=1, integer=T, 
                                                                          invalidMessage = T, invalidTooltip = T,
                                                                          class="randomizeFrequencyRandomSeedNumericInput")),
                                                 column(4,
                                                        style="margin-top:3px; padding-left:0px;",
                                                        diceButton(inputId=ns("globalRandomSeedDice")),
                                                                   tt=paste0("Generates a random number")
                                                 )
                                               ),
                                               tags$div(
                                                 class="borderColor-dark",
                                                 style="margin-top:30px; margin-bottom:20px; border-top:1px solid;"
                                               ),
                                               
                                               fluidRow(
                                                 style="",
                                                 column(width=4,
                                                        actionButton(ns("sampleSizeGenerateDataVariable"),
                                                                     "Randomize seed of 'other variables'",
                                                                     width="100%",
                                                                     style="white-space: normal;")
                                                 ),
                                                 column(4,
                                                        actionButton(ns("sampleSizeGenerateParameters"),
                                                                     "Randomize seed of 'parameters'", 
                                                                     width="100%",
                                                                     style="white-space: normal;")
                                                 ),
                                                 column(4,
                                                        actionButton(ns("sampleSizeUpdateData"),
                                                                     "Update data", class="btn-primary",
                                                                     width="100%",
                                                                     style="white-space: normal;"),

                                                        tags$div(
                                                          style="",
                                                          title="Update response data automatically when formula changes. Only available for datapoints up to 10,000.",
                                                          checkboxInput(ns("createDataSetAutomatically"), 
                                                                          label="Update data automatically",
                                                                          value=T),
                                                          hidden(tags$div(
                                                            id=ns("createDataSetAutomaticallyHint"),
                                                            class="fontColor-warning",
                                                            style="font-weight:bold; font-size:smaller;",
                                                            style="margin-top:-15px; margin-left:25px;",
                                                            "(Too many datapoints)"
                                                          ))
                                                        )
                                                 )
                                               )
                                               
                                             )

                                           )
                                  ),
                                  tabPanel("6. Estimate 'N'",
                                           tags$div(
                                             id=ns("tabSSD"),

                                             fluidRow(
                                               column(width=4,
                                                      
                                                      actionButton(ns("ssdAddGoal"), 
                                                                   "Add goal",
                                                                   class="btn-primary",
                                                                   width="100%",
                                                                   style="margin-bottom:10px;"),
                                                      
                                                      tags$div(
                                                        class="colorizedList",
                                                        selectInput(ns("ssdGoals"), NULL, 
                                                                    size=10, choices=c(), selectize = F))
                                               ),
                                               
                                               column(width=8,
                                                      
                                                      fluidRow(
                                                        style="margin-bottom:10px;",
                                                        column(12,
                                                               tags$div(
                                                                 style="display:flex;",
                                                                 bayasTextInput(ns("ssdGoalName"), 
                                                                           label=NULL,
                                                                           placeholder="unnamed", 
                                                                           width="100%"),
                                                                 tags$button(
                                                                   id=ns("ssdGoalRemove"),
                                                                   icon("trash"),
                                                                   class="btn btn-trash action-button",
                                                                   style=paste0("padding:0px; font-size:16px; ",
                                                                                "width:40px; height:20px; ",
                                                                                "padding: 9px 0px 0px 0px;")
                                                                 )
                                                               ),
                                                               fluidRow(
                                                                 column(6,
                                                                        checkboxInput(ns("ssdInUse"), "Use this goal", value=T)
                                                                        ),
                                                                 column(6,
                                                                        actionButton(ns("ssdSetGoal"),
                                                                                     "Set goal", width="100%",
                                                                                     style="padding: 5px;"))
                                                               )
                                                              )
                                                      ),
                                                      
                                                      hidden(plotOutput(ns("ssdPlot"), height="180px")),
                                                      hidden(disabled(uiOutput(ns("parameterForSSDInfo"))))
                                               )
                                             ),
                                             
                                             wellPanel(
                                               class="getActiveColor",
                                               style= "display:flex; flex-direction: row;",
                                               style="margin-bottom:0px; padding:15px;",
                                               
                                               tags$div(
                                                style="flex:1;",
                                                tags$div(
                                                  style="display:flex; margin-right:20px; margin-bottom:10px;",
                                                  tags$div(
                                                    style = "flex:1;align-content: center;",
                                                    labelWithInfo(ns("ssdDesiredPowerLabel"),label="Power", ttHeader = "The desired statistical power",
                                                                  ttContent = "The minimum probability to detect a true effect.")
                                                  ),
                                                  tags$div(
                                                    style = "flex:1;",
                                                    bayasNumericInput(ns("ssdDesiredPower"), label=NULL, value=0.8,  step=0.05,
                                                                      min=.Machine$double.xmin, max=1-.Machine$double.xmin, 
                                                                      invalidMessage = T, invalidTooltip = T,
                                                                      class="randomizeFrequencyRandomSeedNumericInput",
                                                                      style="margin-left:20px;", numericStyle="height:2em;")
                                                  )  
                                                  
                                                ),
                                                tags$div(
                                                  style="display:flex; margin-right:20px;",
                                                  tags$div(
                                                    style = "flex:1;align-content: center;",
                                                    labelWithInfo(ns("ssdMaxNLabel"),label="Max N",ttHeader = "Maximum acceptable sample size",
                                                                ttContent = "Specify the maximum acceptable sample size. Limiting the maximum sample size can help reduce computation time."),
                                                  ),
                                                  tags$div(
                                                    style = "flex:1;",
                                                    bayasNumericInput(ns("ssdMaxN"), label=NULL, value=20, min=2, max=1000, 
                                                                      invalidMessage = T, invalidTooltip = T,
                                                                    class="randomizeFrequencyRandomSeedNumericInput",
                                                                    style="margin-left:20px;", numericStyle="height:2em;")
                                                  )
                                                )
                                               ),
                                               
                                               tags$div(
                                                 style="flex:1;",

                                                 tags$div(
                                                   style="display:flex; margin-right:20px; margin-bottom:10px;",
                                                   tags$div(
                                                     style = "flex:1; align-content: center;",
                                                     labelWithInfo(ns("ssdAcceptedAccuracyLabel"), label="~Simulations", 
                                                                 ttHeader = "Approximate number of simulations",
                                                                 ttContent = paste0("The approximate number of simulations performed for each N. ",
                                                                                    "The higher the number, the more accurate the sample size determination, but at the cost of time.")),
                                                   ),
                                                   tags$div(
                                                     style = "flex:1;",
                                                     bayasNumericInput(ns("ssdAcceptedAccuracy"), label=NULL, value=200, min=1, max=1e6,
                                                                       invalidMessage = T, invalidTooltip = T,
                                                                       class="randomizeFrequencyRandomSeedNumericInput",
                                                                       style="margin-left:20px;", numericStyle="height:2em;")
                                                   )
                                                 ),
                                                 
                                                 tags$div(
                                                   style="display:flex; margin-right:20px;",
                                                   tags$div(
                                                     style = "flex:1;align-content: center;",
                                                     labelWithInfo(ns("ssdSeedLabel"),label="Seed", ttHeader = "Random seed",
                                                                 ttContent = paste0("Setting seeds can be used to ensure reproducibility ",
                                                                                    "by initializing the random number generator with a fixed value, ",
                                                                                    "allowing the same random sampling process to be replicated.")),
                                                   
                                                   ),
                                                   tags$div(
                                                     style = "flex:1;",
                                                     bayasNumericInput(ns("ssdSeed"), label=NULL, value=1, 
                                                                     min=-.Machine$integer.max,
                                                                     max=.Machine$integer.max, step=1, integer=T, 
                                                                     invalidMessage = T, invalidTooltip = T,
                                                                     class="randomizeFrequencyRandomSeedNumericInput",
                                                                     style="margin-left:20px;", numericStyle="height:2em;")
                                                   )
                                                 )
                                               ),
                                               
                                               tags$div(
                                                 style="flex:1;",
                                                 actionButton(ns("ssdStart"), "Start", 
                                                              style="margin-bottom:10px; padding:3px;",
                                                              width="100%", class="btn-primary"),
                                                 hidden(
                                                   actionButton(ns("ssdStop"), "Stop", width="100%", 
                                                                style="height:30px; padding:5px; margin-bottom:10px;",
                                                                onClick=paste0(
                                                                  "this.disabled=true; Shiny.setInputValue('",
                                                                  ns("ssdStop"),
                                                                  "', 2)")
                                                                # onClick=paste0(
                                                                #   "this.disabled=true; this.data('val',2);")
                                                   )
                                                   #creatingModel-ssdProgressBar-title
                                                 ),
                                                 
                                                 hidden(disabled(
                                                   actionButton(ns("ssdContinue"),
                                                                label="Continue",
                                                                style="padding:3px;",
                                                                width="100%")))
                                               )
                                             )

                                           )
                                  )
                                  
                                  )
                              )
                            ),
                       
                     tags$div(
                       style= "flex:1;",
                            style="padding-left:0px; padding-right:8px;",
                            
                            tags$div(
                              id=ns("tabsetShowStatModelAndSSDDiv"),
                              tabsetPanel(
                                id=ns("tabsetShowStatModelAndSSD"),
                                tabPanel("Statistical model",
                                         tags$div(
                                           id=ns("divStatModel"),

                                           class="getActiveColor",
                                           style="max-height:378px; height:378px;",
                                           style="display:flex; flex-direction:column;",
                                           style="margin:20px 10px 0px 10px;",

                                           tags$div(
                                             style="overflow-y:auto; overflow-x:hidden;",
                                             style="display:flex; flex-direction: column;",

                                             planningFormulaUi("formula")
                                           ),

                                           tags$div(
                                             style="margin-top:auto; margin-bottom:0px;",
                                             fluidRow(
                                               style="margin-top:auto;",

                                               column(6,
                                                      tags$label("Model"),
                                                      bayasGroupedButtons(ns("formulaSwitchPriorGenerative"),
                                                                          btnName=c("Data generation","Inference"),
                                                                          btnValues=c("Generative","Prior"),
                                                                          selected="Generative", columns=2,
                                                                          btnStyle="padding:3px; width:100%;")),

                                               column(6,
                                                      tags$label("Predictor line"),
                                                      bayasGroupedButtons(ns("formulaSwitchNextLine"),
                                                                          btnName=c("Multiple","Single"),
                                                                          selected="Multiple", columns=2,
                                                                          btnStyle="padding:3px; width:100%;")
                                               )
                                             )
                                          )

                                         )
                                ),
                                tabPanel("Estimate 'N'",
                                         tags$div(
                                           id=ns("divEstimateNInfoView"),

                                           class="getActiveColor",
                                           style="max-height:378px; height:378px;",
                                           style="display:flex; flex-direction:column;",
                                           style="margin:20px 10px 0px 10px;",
                                           style="padding:0px 10px 10px 10px;",
                                           style="overflow-y:auto;",

                                           tags$label("Status"),
                                           tags$div(
                                             style="margin-top:10px;",
                                             progressBar(ns("ssdProgressBar"),
                                                         value=0,
                                                         status="primary",
                                                         size="xxs",
                                                         title="Status sample size determination")
                                           ),
                                           
                                           tags$label("Summary"),
                                           tags$div(
                                             class="infoBox",
                                             style="padding:10px;",
                                             style="border: 1px solid",
                                             style="border-radius:4px;",
                                             style="margin-bottom:15px;",
                                             
                                             uiOutput(ns("ssdSummary"))
                                           ),

                                           tags$label("Plot description"),
                                           tags$div(
                                             class="infoBox",
                                             style="text-align: justify;",
                                             style="padding:10px;",
                                             style="border: 1px solid;",
                                             style="border-radius:4px;",
                                             paste0("The status of the sample size determination is also shown in the graph below (Estimate 'N' visualization). ",
                                                    "Simulated sample sizes (horizontal axis), and power (vertical axis) reached for these sample sizes. ",
                                                    "The circle marks the fraction of trials with the indicated sample size in which the goal was achieved. ",
                                                    "Vertical error bars are 90% credible intervals of power. \n",
                                                    "Arrows show the maximum accepted width of the power credible interval controlled by the '~Simulations' parameter. ",
                                                    "The blue dashed line shows the desired power and blue circle/bar shows the smallest of the tested sample sizes at which this power is probably surpassed. ",
                                                    "Black circle/bar indicates the last sample size taken.")
                                           ),
                                           
                                           
                                           if(localUse)
                                             tableOutput(ns("ssdSummaryTable"))
                                         )
                                )
                              )
                            )

                     )
                     
                   )
                     
                   )
                 ),
                tags$div(
                  style="display:flex; gap: 20px;",
                  
                        tags$div(
                          style= "flex:1;",

                           wellPanel(
                             class="getActiveColor",
                             style="overflow:auto; height:400px;",
                             tags$b("Data"),
                             hidden(
                               tags$div(
                                 id=ns("dataHint"),
                                 class="fontColor-warning",
                                 style="display:inline; margin-left:10px;",
                                 actionLink(ns("dataHintLink"), label=NULL, icon=icon("sync-alt"),
                                            class="fontColor-warning")
                               )
                             ),
                             downloadLink(ns("downloadWholeDataset"), 
                                          label=shiny::icon("download"),
                                          style="float:right;"),
                             
                             tags$div(
                               id=ns("divDataTable"),
                               hidden(
                                 tags$div(
                                   id=ns("datatableCuttedInfo"),
                                   class="fontColor-warning",
                                   style="font-weight:bold; font-size: smaller;",
                                   "(Only the first 10,000 datapoints are shown)"
                                 )
                               ),
                               tags$div(
                                 style="text-align:center;",
                                 withSpinner(DTOutput(ns("dataTable")), type=1)
                               )
                             )
                           )),
                        tags$div(
                           style= "flex:1;",
                           style="padding-left:0px; padding-right:8px;",

                             tags$div(
                               id=ns("tabsetDataVisualizationDiv"),
                               class="getActiveColor",
                               style=" overflow-y:auto; height:430px;",
                               
                               tabsetPanel(
                                 id=ns("tabsetDataVisualizationSSD"),
                                 tabPanel("Data visualization",
                                          
                                   tags$div(
                                     id=ns("divDataVisualization"),
                                     style="margin-top:10px; ",
                                     tags$div(
                                       style="display:flex;",
                                       tags$div(style="flex:1; margin:0px 2px 0px 2px;",
                                                selectizeInput(ns("dataVisualizationSelectY"), label=NULL,
                                                               choices=c("First"=""), width ="100%")),
                                       tags$div(style="flex:1; margin:0px 2px 0px 2px;",
                                                selectizeInput(ns("dataVisualizationSelectX"), label=NULL,
                                                      choices=c("Second"=""), width ="100%")),
                                       tags$div(style="flex:1; margin:0px 2px 0px 2px;",
                                                selectizeInput(ns("dataVisualizationSelectGroupBy"), label=NULL,
                                                               choices=c("Group by"=""), width ="100%", multiple=T)),
                                       tags$div(style="flex:1; margin:0px 2px 0px 2px;",
                                                selectizeInput(ns("dataVisualizationSelectPlotType"), label=NULL,
                                                               choices=c("Points", "Histogram", "Density"), 
                                                               selected="Points", width ="100%"))
                                     ),
                                     plotOutput(ns("dataVisualization"), height="300px")
                                   )
                                 ),
                                 
                                 tabPanel(
                                   title = "Estimate 'N' visualization",
                                   
                                   tags$div(
                                     
                                     class="getActiveColor",
                                     # style="max-height:378px; height:378px;",
                                     style="display:flex; flex-direction:column;",
                                     style="margin:20px 10px 0px 10px;",
                                     style="overflow-y:auto;",
                                     
                                     tags$div(
                                       style="height: 100%;",
                                       plotOutput(ns("SSDInfoPlot"),
                                                  height = "300px")
                                     )
                                     
                                     
                                   )
                                 )
                               )
                             )
                             
                           )
                  )

                )
     )
  )
}

planning_newExperiment <- function(ns){
  
  modalDialog(title = NULL, size="l", easyClose=T,
              
    tags$div(tags$h5("Choose an experiment"), style="text-align:center;"),
    tags$div(style="overflow-y:auto; overflow-x:hidden; max-height:450px;",
             fluidRow(style="padding-bottom:10px;",
                      column(4, style="min-width:min-content",
                             imageButtonTitle(ns("exampleExperiment_1"),
                                              imageFile=paste0("Images","/Planning/ModelPlanning/Examples/1_Empty.png"),
                                              title="Empty",
                                              selected=T)),
                      column(4, style="min-width:min-content",
                             imageButtonTitle(ns("exampleExperiment_2"),
                                              imageFile=paste0("Images","/Planning/ModelPlanning/Examples/2_T-test.png"),
                                              btnStyle="width:230px; height:auto;",
                                              title = tags$div(
                                                style =  "overflow-wrap: break-word; overflow: hidden; white-space: normal;",
                                                "Two (Gaussian) group comparison"))),
                      column(4, style="min-width:min-content",
                             imageButtonTitle(ns("exampleExperiment_3"),
                                              imageFile=paste0("Images","/Planning/ModelPlanning/Examples/3_simple_regression.png"),
                                              title="Simple regression"))
             )
    ),
    
    tags$div(
      class="borderColor-dark",
      style="margin: 20px 20px 0px 20px; padding:10px; border-top: 1px solid;",
      textOutput(ns("existingExperimentInfo"))
    ),
    
    footer = tags$div(
      style="display:flex; width: 100%;",
      tags$div(
        style = "flex:1",
        tags$div(modalButton("Cancel"), style="float:left;")),
      tags$div(
        style= "flex:4",
        tags$div(
          textInput(ns("modelName"), label=NULL, value="",
                    placeholder="Experiment name", width="100%"),
          uiOutput(ns("modelNameWarning"))
        )
      ),
      tags$div(
        style = "flex:1; text-align:right;",
        actionButton(ns("startPlanning"),"Start", class="btn-primary")
      )
    )
    
  )
}



planning_responseModal <- function(stepDiv, helpDiv){
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



planning_reportExperiment <- function(ns, mcd, reportItem=F, item=NULL){
  # "onlyUsedVars", "onlyValidParas", "includeGenData", "replaceMinData", "includeSSD", "includeSSDResult", "onlyUsedGoals", "onlySSD"
  preSel <- c(T,T,T,T,T,T,T,F)
  
  #defined?
  defined <- list(resp=F, oV=F, pred=F, para=F, genData=F, ssdGoals=F, ssd=F, ssdMcdOff=F)
  

  if(reportItem && !is.null(item)){

    indData <- item$getIndividualData()
    preSel <- indData$preSel
    defined <- indData$defined
    
  }else{
    preSel <- mcd$getReportExpInp()
    preSel[is.null(preSel)] <- c(T,T,T,T,T,T,T,F)[is.null(preSel)]

    mcdResp <- mcd$getMcdResponse()
    if(!is.null(mcdResp$getDist())) defined$resp <- T 
    
    oVs <- mcd$getOtherVariables()
    if(!is.empty(oVs)) defined$oV <- T 
    
    pred <- mcd$getPredictors()
    if(!is.empty(pred)) defined$pred <- T 
    
    para <- mcd$getParameters()
    if(!is.empty(para)) defined$para <- T 
    
    genData <- mcd$getData()
    if(defined$resp && !is.null(dim(genData)[1])) defined$genData <- T
    
    mcdSSD <- mcd$getMcdSSD()
    ssdObject <- mcdSSD$getSsdObject()
    
    if(!is.empty(mcdSSD$getGoals())) defined$ssdGoals <- T
    
    if(!is.null(ssdObject)){
      defined$ssd <- T
      defined$ssdMcdOff <- !mcdSSD$isMcdOffSame()
    }
  }
  

  divSSDResult <- tags$div()
  if(!reportItem){
    divSSDResult <- tagList(
      tags$div(
        style = "display:flex;",
        class = ifelse(!defined$ssdMcdOff || !(defined$ssd && preSel$includeSSD && preSel$includeSSDResult),"shinyjs-disabled", ""),
        tags$div(
          style = "flex:0; width: 25px; margin-bottom:-15px;",
          checkboxInput(ns("switchSSDOnlyMcdOff"), label=NULL, value=preSel$onlySSD)
        ),
        tags$div(
          style = "flex:1;",
          labelWithInfo(ns("switchSSDOnlyMcdOffLabel"),"Only estimate 'N' dependencies","Only Estimate \'N\' dependencies",
                        paste0("If your current model definition is different from the one used to compute the estimate of \'N\' ", 
                               "you may want to report only the  model used for the \'N\' estimation and not the current model definition. ",
                               "If that\'s the case, tick \'Only report model used for estimating \'N\'\'. <br>",
                               "Otherwise, the current model definition (response, other variables, etc.) will be reported as first report item, ",
                               "and then the model used for \'N\' estimation together with the result of that estimation will be reported as second report item. <br>",
                               "Differences in the model definition could be, for example, changes in parameters ",
                               "(distributions or their parameters) or even small changes in the names of \'other variables\'."))
        ) 
      ),
      tags$div(
        class = ifelse(defined$ssdMcdOff,"fontColor-error",""),
        style = "margin:0px 25px;",
        style = ifelse(defined$ssdMcdOff, "margin-top: -5px;", "display:none;"),
        HTML(paste0("There are some (minor) differences between the definition of the current model and the definition of the model that you have used to estimate 'N'. <br>",
                    "If this box is unchecked, we will report both model definitions."))
      )
    )
  }
  
  ret <- tags$div(
    style = "",
    
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, ""),
      column(1, ""), #tags$b("Defined")
      column(9, "") #tags$b("Options")
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Response"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$resp,"display:none;", ""))),
      column(8, "")
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Other variables"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$oV,"display:none;", ""))),
      column(8, tags$div(
        style = "display:flex;",
        style = ifelse(!defined$oV,"display:none;", ""),
        class = ifelse(!defined$resp,"shinyjs-disabled", ""),
        tags$div(
          style = "flex:0; width: 25px; margin-bottom:-15px;", 
          checkboxInput(ns("switchOVOnlyUsed"), label=NULL, value=defined$oV && preSel$onlyUsedVars)
        ),
        tags$div(
          style = "flex:1;",
          labelWithInfo(ns("switchOVOnlyUsedLabel"),"Only used variables","Only used variables",
                        HTML(paste0("If checked (recommended), only variables used directly or indirectly in the model are reported. <br>",
                                    "")))
        ))
      )
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Predictors"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$pred,"display:none;", ""))),
      column(9, "")
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Parameters"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$para,"display:none;", ""))),
      column(8, ""
        #      tags$div(
        # class = ifelse(!defined$para,"shinyjs-disabled", ""),
        # style = "display:flex;",
        # style = ifelse(!defined$para,"display:none;", ""),
        # tags$div(
        #   style = "flex:0; width: 25px; margin-bottom:-15px;", 
        #   checkboxInput(ns("switchParametersNonRedundant"), label=NULL, value= preSel$onlyValidParas)
        # ),
        # tags$div(
        #   style = "flex:1;",
        #   labelWithInfo(ns("switchParametersNonRedundantLabel"),"Only valid parameters","Only valid parameters",
        #                 HTML(paste0("If checked (recommended), only valid parameters will be reported. <br>",
        #                             "Invalid parameters will also be marked as invalid in the \'4. Parameters\' tab.")))
        # ))
      )
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Generated data"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$genData,"display:none;", ""))),
      column(8, tags$div(
        class = ifelse(!defined$genData,"shinyjs-disabled", ""),
        style = ifelse(!defined$genData,"display:none;", ""),
        tags$div(
          style = "display:flex;",
          tags$div(
            style = "flex:0; width: 25px; margin-bottom:-15px;",
            checkboxInput(ns("switchGeneratedData"), label=NULL, value=preSel$includeGenData)
          ),
          tags$div(
            style = "flex:1;",
            labelWithInfo(ns("switchGeneratedDataLabel"),"Include generated data","Include generated data",
                          HTML(paste0("If checked (recommended), either the generated data or a minimal example of data will be reported. <br>",
                                      "")))
          )
        ),
        tags$div(
          style = "display:flex;",
          tags$div(
            style = "flex:0; width: 25px; margin-bottom:-15px;",
            class = ifelse(!(defined$genData && preSel$includeGenData),"shinyjs-disabled", ""),
            checkboxInput(ns("switchGeneratedDataMinimal"), label=NULL, value=preSel$replaceMinData)
          ),
          tags$div(
            style = "flex:1;",
            labelWithInfo(ns("switchGeneratedDataMinimalLabel"),"Replace with minimal example","Minimal example",
                          HTML(paste0("If checked (recommended), a minimal example of data will be reported instead of the generated data for a better overview in the report. <br>",
                                      "")))
          )
        ))
      )
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Estimate 'N'"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$ssdGoals,"display:none;", ""))),
      column(8, tags$div(
        class = ifelse(!defined$ssdGoals,"shinyjs-disabled", ""),
        style = ifelse(!defined$ssdGoals,"display:none;", ""),
        tags$div(
          style = "display:flex;",
          tags$div(
            style = "flex:0; width: 25px; margin-bottom:-15px;",
            checkboxInput(ns("switchSSD"), label=NULL, value=preSel$includeSSD)
          ),
          tags$div(
            style = "flex:1;",
            labelWithInfo(ns("switchSSDLabel"),"Include Estimate 'N'","Include Estimate \'N\'",
                          HTML(paste0("If checked (recommended), the report will include the defined goals in \'Estimate \'N\'\' and the result (if any).<br>",
                                      "")))
          )
        ),
        tags$div(
          style = "display:flex;",
          class = ifelse(!(defined$ssdGoals && preSel$includeSSD), "shinyjs-disabled", ""),
          tags$div(
            style = "flex:0; width: 25px; margin-bottom:-15px;",
            checkboxInput(ns("switchSSDOnlyUsedGoals"), label=NULL, value=preSel$onlyUsedGoals)
          ),
          tags$div(
            style = "flex:1;",
            labelWithInfo(ns("switchSSDOnlyUsedGoalsLabel"),"Only used goals","Only used goals",
                          HTML(paste0("If checked (recommended), only validly defined goals marked as \'used\' will be reported.<br>",
                                      "")))
          )
        ))
      )
    ),
    fluidRow(
      class = "borderColor-regular",
      style = "padding-bottom:10px; margin-bottom:10px; border-bottom: 1px solid;",
      column(2, "Estimate 'N' result"),
      column(1, style = "text-align: center;", icon("check-circle",style = ifelse(!defined$ssd,"display:none;", ""))),
      column(8, tags$div(
        class = ifelse(!defined$ssd,"shinyjs-disabled", ""),
        style = ifelse(!defined$ssd,"display:none;", ""),
        tags$div(
          style = "display:flex;",
          class = ifelse(!(defined$ssd && preSel$includeSSD), "shinyjs-disabled", ""),
          tags$div(
            style = "flex:0; width: 25px; margin-bottom:-15px;",
            checkboxInput(ns("switchSSDResult"), label=NULL, value=preSel$includeSSDResult)
          ),
          tags$div(
            style = "flex:1;",
            labelWithInfo(ns("switchSSDResultLabel"),"Include Estimate 'N' result","Include Estimate \'N\' result",
                          HTML(paste0("If checked (recommended), the report will include the result of \'Estimate \'N\'\'.<br>",
                                      "")))
          )
        ),
        
        divSSDResult
        
        )
      )
    )
  )
  
  ret <- tags$div(
    style = "",
    tags$div(
      style = "",
      HTML(paste0(
        "Everything that is defined (",  
        icon("check-circle"),
        ") is by default listed in the report. <br>"
      ))
    ),
    ret
  )
  
}




