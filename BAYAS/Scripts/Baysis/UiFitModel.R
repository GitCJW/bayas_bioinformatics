runModel_page <- function(){
  tabPanel("Model fitting",
           
   # first row for page elements. Second row holds next button
   verticalLayout( 
     fluid = F,
                   
     tags$div(

       shinybayas::bayasSidePanel(
         inputId = "sidebarPanelModelFit",
         
         mainPanelClass = "getActiveColor",
         sidePanelClass = "getActiveColor",
         
         sidePanel = tags$div(
           
           wellPanel(
             class="getActiveColor",
             style= "margin-bottom: 1.5rem;", 
 
             # Textbox for model fit name
             tags$div(
               textInput(
                 inputId = "runModelFitName", 
                 label = "Enter a name of your fit",
                 value = "Model_fit"
               )
             ),
                     
             # Advanced settings, that include the sample parameters
             bslib::accordion(
               style="margin-bottom:1rem;",
               open=F,
               bslib::accordion_panel(
                title = "Sampling parameters", 

                value = "samplingPara",
                
                tags$div(

                  tags$div(
                    style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                    tags$div(
                      style = "flex:1;",
                      labelWithInfo(id="samplingParametersNumberIteration", "Number iterations","Iterations",tooltip$iteration)
                    ),
                    
                    tags$div(
                      style = "flex:1;",
                      bayasNumericInput(
                        inputId = "runModelItertations", label = NULL,
                        value = 2000, min = 100, max = 100000, step = 100,
                        integer=T, invalidMessage=T, invalidTooltip=T)
                    )
                  ),
                  
                  
                  tags$div(
                    style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                    tags$div(
                      style = "flex:1;",
                      labelWithInfo(id="samplingParametersNumberChains", "Number chains","Chains", tooltip$chain)
                    ),
                    
                    tags$div(
                      style = "flex:1;",
                      bayasNumericInput(
                        inputId = "runModelChains", label = NULL,
                        value = 4, min = 1, max = 8, step = 1,
                        integer=T, invalidMessage=T,invalidTooltip=T)
                    )
                  ),
                  
                  if(localUse){

                    tags$div(
                      style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                      tags$div(
                        style = "flex:1;",
                        labelWithInfo(id="samplingParametersNumberCores", "Number cores","Cores", tooltip$cores)
                      ),
                      
                      tags$div(
                        style = "flex:1;",
                        bayasNumericInput(
                          inputId = "runModelCores", label = NULL,
                          value = 1, min = 1, max = 8, step = 1,
                          integer=T, invalidMessage=T, invalidTooltip=T)
                      )
                    )
                  },
                  

                  tags$div(
                    style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                    tags$div(
                      style = "flex:1;",
                      labelWithInfo(id="samplingParametersAdaptDelta", "Adapt delta", "Adapt delta", tooltip$adaptDelta)
                    ),
                    
                    tags$div(
                      style = "flex:1;",
                      bayasNumericInput(
                        inputId = "runModelAdaptDelta", label = NULL,
                        value = 0.8, min = 0.1, max = 1, step = 0.05,
                        integer=F, invalidMessage=T, invalidTooltip=T)
                    )
                  ),
                  
                  tags$div(
                    style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                    tags$div(
                      style = "flex:1;",
                      labelWithInfo(id="samplingParametersMaxTreedepth", "Max treedepth", "Maximum treedepth", tooltip$maxTreepdeth)
                    ),
                    
                    tags$div(
                      style = "flex:1;",
                      bayasNumericInput(
                        inputId = "runModelMaxTreedepth", label = NULL,
                        value = 10, min = 1, max = 20, step = 1,
                        integer=T, invalidMessage=T, invalidTooltip=T)
                    )
                  ),

                  
                  tags$div(
                    style = "display:flex; gap: 15px; margin-bottom: 1rem; align-items: center;",
                    tags$div(
                      style = "display:flex; flex:1; gap: 10px;",
                      tags$div(
                        class= "form-group-mb-0",
                        style = "flex:0;",
                        checkboxInput(inputId="samplingParametersSeedCheck", label="", value=F)
                      ),
                      tags$div(
                        style = "flex:1;",
                        labelWithInfo(id="samplingParametersSeedInfo", "Seed", "Seed", tooltip$setSeed, style="width:100%;")
                      )
                    ),
                    
                    tags$div(
                      style = "flex:1;",
                      disabled(bayasNumericInput(
                        inputId = "runModelSeed", label = NULL,
                        value = 1234, min = -.Machine$integer.max, max = .Machine$integer.max, step = 1,
                        integer=T, invalidMessage=T, invalidTooltip=T))
                    )
                  )
                  
                  
                )

               )),

               
               # Run Button
               tags$div(style="text-align:right", 
                        # actionButton(class = 'btn-primary', "btnRunModelRun", "Start Sampling",  style="font-size: larger;")
                        input_task_button(class = 'btn-primary', id="btnRunModelRun", label="Start Sampling",  style="font-size: larger;")
                        ),
           ),
           
           wellPanel(
             class="getActiveColor",
             
             tags$div(
               style="display: flex;",
               selectInput("selectInputRunModelPreviousModels", 
                           label = "Fitted models", 
                           choices = c(""), 
                           selected = "", 
                           selectize = F,
                           width="100%"),
               
               bslib::tooltip(
                 trigger = tags$div(
                   id="removeCPIDMDiv",
                   tags$button(
                     id="removeCPIDM",
                     icon("trash"),
                     class="btn btn-trash action-button",
                     style=paste0("padding:0px; font-size:16px; ",
                                  "width:40px; height:20px; ",
                                  "padding: 33px 0px 0px 0px;")
                   )
                 ),
                 HTML(tooltip$removeCPIDM),
                 options = list(trigger="hover")
               )
             ),
             
             # Model formula
             wellPanel(id = "runModelInputOverview", style="padding: 5px;", class="getActiveColor",
                       # Formula
                       htmlOutput(outputId  ="runModelSelectedModelFormula")
             )),
         ),
         
         
         # Main Panel
         mainPanel = tags$div(
           style="padding:1px;",
           
           # Overview tabPanel
           tabsetPanel(id = "runModeltabsetPanel", selected = "Overview",
                       tabPanel( title = "Overview", 

                           tags$div(style="display:grid; grid-template-columns: 50% 50%;",
                                    
                             # Model validation
                             tags$div(
                               class = "borderColor-dark", 
                               style = "border-right: 3px solid; min-height:460px; padding:5px;",
                               tags$div(id = "center", actionLink(inputId = "runModelLinkUpperLeft", label = h5("Model validation"))),
                               withSpinner(htmlOutput(outputId  = "runModelVerbalResult")),
                               # htmlOutput(outputId  = "runModelVerbalResult"),
                               getReportButton("reportModelValidation", tooltip="Report diagnostic values", 
                                               class="",
                                               style="float:right; margin: 5px 5px 0px 0px;")
                             ),
                             # Posterior predictive check
                             tags$div(style="padding:5px;",
                                    tags$div(id = "center", actionLink(inputId = "runModelLinkUpperRight", label = h5("Posterior predictive check of response"))),
                                    withSpinner(plotOutput(outputId = "runModelPlotPPC"), color=BAYAS_COLORS$`--bs-btn-bg`),
                                      getReportButton("reportPreviewPPC", 
                                                      tooltip="Report the current PPC plot", class="",
                                                    style="float:right; margin: 5px 5px 0px 0px;")
                             ),
                             # Marginal posteriors
                             tags$div(
                               class = "borderColor-dark", 
                               style = "border-top: 3px solid;border-right: 3px solid; padding:5px;",
                               tags$div(id = "center", actionLink(inputId = "runModelLinkBottomLeft", label = h5("Marginal posteriors"))),
                               withSpinner(plotOutput(outputId = "runModelVariableSummaryPlot"))
                             ),
                             # Summary of marginal posteriors
                             tags$div(
                               class = "borderColor-dark", 
                               style = "border-top: 3px solid; padding:5px;",
                               tags$div(id = "center", h5("Summary of marginal posteriors")),
                               tags$div(id = "center",
                                        style="max-height:400px;",
                                        withSpinner(dataTableOutput(outputId = "runModelVariableSummaryTable")))#,
                             )
                             
                           )
                       ),
                       
                       
                       tabPanel(title = "Model validation",

                                tabsetPanel(
                                  id = "runModelModelValidationTabsetPanel", 
                                  selected = "Sampling quantities", 
                                  type="pills",
                                  
                                  tabPanel(
                                    title = "Sampling quantities", 
                                    style="margin-top:20px;",
                                    tags$div(
                                      style = "",
                                      DTOutput(outputId = "runModelModelValidationTabSQ"))),

                                  tabPanel(title = "Pairs", style="margin-top:20px;",
                                           sidebarLayout(
                                             sidebarPanel(width = 3,
                                                             
                                                          tags$p(style="font-weight:bold;margin-top:6px;","Formula elements"),
                                                          tags$div(style="margin-bottom:20px;", shinyTree("treePairsVars", checkbox=T, themeIcons=F), style="overflow: scroll; max-height:500px;"),
                                                          
                                                          tags$div(
                                                            
                                                            class="childs-low-tbmargin",
                                                            
                                                            actionButton(class = 'btn-primary', 
                                                                         "plotPairs", "Plot pairs"),
                                                            
                                                            getReportButton("reportPairs", tooltip="Report the current plot."),
                                                            
                                                            downloadPlotUI("runModelModelValidationTabSQPairsPlot", 
                                                                           label=icon("download"), 
                                                                           tooltip=HTML(tooltip$downloadMP)),
                                                            
                                                            bslib::tooltip(
                                                              trigger = actionButton("removePairs", "",icon("trash")),
                                                              HTML(tooltip$closeCurrentMP),
                                                              options = list(trigger="hover")
                                                            )
                                                          )
                                                          

                                                        ),
                                             
                                             mainPanel(width = 9,
                                                       tabsetPanel(id = "runModelModelValidationTabSQPairsPlot", type = "pills")
                                             )
                                           )),
                                  
                                  tabPanel(title = "Prior predictive check", style="margin-top:20px;",
                                           tags$div(plotOutput(outputId = "runModelModelValidationTabPriorPredictiveCheck"), style="overflow:auto;"),
                                           tags$div(style="text-align:right",
                                                    getReportButton("reportPriorPC", tooltip="Report the current plot.", class="")),
                                           
                                           ),
                                  tabPanel(title = "Prior vs posterior", style="margin-top:20px;",
                                           sidebarLayout(
                                             sidebarPanel(width = 3,
                                                          tags$p(style="font-weight:bold;margin-top:6px;","Formula elements"),
                                                          tags$div(style="margin-bottom:20px;",shinyTree("treePVPVars", checkbox=T, themeIcons=F), style="overflow: scroll; max-height:500px;"),
                                                          tags$div(style="margin-bottom:20px;",
                                                                   checkboxInput("groupByParameterPVP", "Group by parameters")),
                                                          
                                                          tags$div(
                                                            
                                                            class="childs-low-tbmargin",
                                                            
                                                            actionButton(class = 'btn-primary', "plotPVP", 
                                                                         "Plot PVP"),
                                                            
                                                            getReportButton("reportPVP", 
                                                                            tooltip="Report the current plot."),
                                                            
                                                            downloadPlotUI("runModelModelValidationTabSQPVPPlot", 
                                                                           label=icon("download"), 
                                                                           tooltip=tooltip$downloadMP),
                                                            
                                                            bslib::tooltip(
                                                              trigger = actionButton("removePVP", "",icon("trash")), 
                                                              HTML(tooltip$closeCurrentMP),
                                                              options = list(trigger="hover")
                                                            )
                                                          )
                                                        ),
                                                         
                                             mainPanel(width = 9,
                                                       tabsetPanel(id = "runModelModelValidationTabSQPVPPlot" ,type = "pills")
                                             )
                                           ))
                                            )
                       ),
                       tabPanel(title = "PPC", style = "padding:10px;",
                                sidebarLayout(
                                  sidebarPanel(width = 3, style="overflow:auto;",
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:6px;","Plot type")),
                                                        column(width=8,selectInput("selectionPPCType",width="auto", label=NULL, choices = c("Density overlay", "Interval", "Histogram", "Frequency polygon", "Violin grouped", "Bars"),selected="Density overlay"))),
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:-2px;","Grouped by")),
                                                        column(width=8,shinyjs::disabled(selectizeInput("selectionPPCGroup", label=NULL, choices = c(""), selected="", multiple=T)))),
                                               
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:6px;","Scale")),
                                                        column(width=8,selectizeInput("selectionPPCScale", label=NULL, 
                                                                                      choices = list("None"="", "Pseudo log"="pseudo_log","log"="log","log10"="log10", "log2"="log2",
                                                                                                     "reverse"="reverse", "sqrt"="sqrt"), selected="None"))),
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:6px;","Lims")),
                                                        column(width=4,style="padding-right:5px;",textInput("selectionPPCLowerLim", label=NULL, value="")),
                                                        column(width=4,style="padding-left:5px;",textInput("selectionPPCUpperLim", label=NULL, value=""))),
                                               
                                               fluidRow(column(width=4,tags$p(id="labelPPCDraws", style="font-weight:bold;margin-top:6px;","Draws")),
                                                        column(width=8,bayasNumericInput("numericPPCDraws", label=NULL, value = 50, min=1,max=200, step=10))),
                                               
                                               tags$div(
                                                 
                                                 class="childs-low-tbmargin",
                                                 actionButton(class = 'btn-primary', "plotPPC", 
                                                              "Plot PPC"),
                                                 
                                                 getReportButton("reportPPC", tooltip="Report the current plot."),
                                                 
                                                 downloadPlotUI("FitPagePPCPanelPlot", label=icon("download"), 
                                                                tooltip=tooltip$downloadMP),
                                                 
                                                 bslib::tooltip(
                                                   trigger = actionButton("removePPC", "",icon("trash")),
                                                   HTML(tooltip$closeCurrentMP),
                                                   options = list(trigger="hover")
                                                 )
                                               )
                                               
                                              ),
                                  
                                  mainPanel(width = 9,
                                            tabsetPanel(id = "FitPagePPCPanelPlot" ,type = "pills")
                                  )
                                )
                       ),
                       tabPanel(title = "Marginal posteriors", style = "padding:10px;",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:6px;","Plot type")),
                                                        column(width=8,selectInput("selectionMPType",width="auto", label=NULL, choices = c("Intervals","Areas","Density","Density overlay","Histogram","Violin"),selected="Intervals"))),
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:6px;","x-scale")),
                                                        column(width=8,selectInput("selectionMPTypeXScale", label=NULL, choices = c("None","Pseudo log"), selected="None"))),
                                               fluidRow(column(width=4,tags$p(style="font-weight:bold;margin-top:-2px;","Point estimate")),
                                                        column(width=8,selectInput("selectionMPPointEst", label=NULL, choices = c("Median","Mean", "None"), selected="Median"))),
                                               
                                               fluidRow(column(width=4,
                                                               
                                                               bslib::tooltip(
                                                                 trigger = tags$p(id="labelInnerHDI", style="font-weight:bold;margin-top:2px;","Inner HDI"),
                                                                 HTML(tooltip$labelInnerHDI),
                                                                 options = list(trigger="hover")
                                                               )),
                                                        column(width=8, 
                                                               bayasNumericInput("innerHDIValue", label=NULL, value = 0.5, min=0.001, max=1, step=0.05,
                                                                                 invalidMessage = "Must be in the range [0.001-1]."))),
                                               
                                               fluidRow(column(width=4,
                                                               bslib::tooltip(
                                                                 trigger = tags$p(id="labelOuterHDI", style="font-weight:bold;margin-top:2px;","Outer HDI"),
                                                                 HTML(tooltip$labelOuterHDI),
                                                                 options = list(trigger="hover")
                                                               )),
                                                               
                                                        column(width=8,
                                                               bayasNumericInput("outerHDIValue", label=NULL, value = 0.9, min=0.001, max=1, step=0.05,
                                                                                 invalidMessage = "Must be in the range [0.001-1]."))),
                                               
                                               
                                               
                                               tags$p(style="font-weight:bold;margin-top:6px;","Formula elements"),
                                               tags$div(style="margin-bottom:20px;",shinyTree("treeMPVars", checkbox=T, themeIcons=F), style="overflow: scroll; max-height:500px;"),
                                               
                                               tags$div(
                                                 
                                                 class="childs-low-tbmargin",
                                                 actionButton(class = 'btn-primary', "plotMP", 
                                                              "Plot MP"),
                                                 
                                                 getReportButton("reportMP", tooltip="Report the current plot."),
                                                 
                                                 downloadPlotUI("FitPageMPPanelPlot", label=icon("download"), 
                                                                tooltip=tooltip$downloadMP),
                                                 
                                                 bslib::tooltip(
                                                   trigger = actionButton("removeMP", "",icon("trash")),
                                                   tooltip$closeCurrentMP,
                                                   options = list(trigger="hover")
                                                 )
                                               )),
                                  
                                  
                                  mainPanel(width = 9,
                                            tabsetPanel(id = "FitPageMPPanelPlot" ,type = "pills")
                                            )
                                )
                       )
           )
         )
       )
     ),
     

     footerPanel(
       backButtonsId="btnRunModelBack",
       nextButtonsId=c("btnRunModelNext" , "btnRunModelNextPrediction"),
       backButtons="Model selection",
       nextButtons=c("Compare models", "Quantifications")
     )
                   
   )
  )
}