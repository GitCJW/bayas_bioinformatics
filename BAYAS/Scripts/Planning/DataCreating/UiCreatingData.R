planning_creatingData_page <- function(){
  
  tabPanel("Creating Data", 
           
    sidebarLayout(
      
      # brushing tool
      ## bruhsing dist
      ## brushing aux
      # bruhsing factor (* visible points)
      # X
      # Y
      # minX, maxX
      # minY, maxY
      sidebarPanel(width=3,style="padding:10px;", 
                   
         bsCollapse(id="planningCollapseProperties", open="planningCollapsePanelCreateData",
            bsCollapsePanel("Create data", value="planningCollapsePanelCreateData",
                          
                            actionButton("planningNewData", "New dataset", class="btn btn-default action-button btn-primary shiny-bound-input", width="100%"),
                            tags$div(class="wordWrappedInHorizLine", " or "),
                            tags$div(style="",
                              fileInput(inputId = "planningUploadDataset", NULL, multiple = FALSE, accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv",
                              ".txt"),
                              width = NULL)),
                            
                            #name of dataset
                            fluidRow(column(4,style="",tags$label(class="control-label", "Name", style="padding-top:10px;")),
                                     column(8, disabled(textInput("planningDatasetName", NULL, value="", width="100%")))),
                            
                            #List of datasets
                            fluidRow(column(4,style="",tags$label(class="control-label", "Created sets", style="padding-top:10px;")),
                                     column(6,selectInput("planningCreatedDatasets", NULL, choices=c())),
                                     column(2,actionButton("planningRemoveDataset", NULL, icon=icon("times"), style="height:34px;width:34px;padding:6px;"))),
                            
                            #Download dataset
                            fluidRow(column(4,style="",tags$label(class="control-label", "Download", style="padding-top:10px;")),
                                     column(4,downloadButton("planningDownloadDataset",label="Selected", style="width:100%;", class="btn-primary")),
                                     column(4,downloadButton("planningDownloadDatasets",label="All", style="width:100%;", class="btn-primary")))
                            
                            
                            ),
            bsCollapsePanel("Brushing data", value="planningCollapsePanelBrushingData", 
                            
                            # selectInput("planningBrushingDist", "Distribution", choices = c("Normal"), selected = "Normal"),
                            # numericInput("planningBrushingDistAux", "Sigma", value=1, min=0),
                            
                            fluidRow(column(4,style="padding-top:7px;",tags$label(class="control-label", "Brushing points", style="padding-top:0px;")),
                                     column(8,sliderInput("planningBrushingNumberPoints", NULL, value=10, min=1, max=20,step=1))),
                            
                            fluidRow(
                              column(4,style="padding-top:7px;",
                                     tags$label(class="control-label", "Size", style="padding-top:10px;"),
                                     actionLink("planningBrushingRadiusHint", label="", icon("question-circle"), 
                                                class = "fontColor-primary",
                                                style="font-size:15px;float:inline-end;padding-top:9px;")
                                     
                                     ),
                              column(8,
                                fluidRow(
                                 column(5,sliderInput("planningBrushingRadius", NULL, value=10, min=1, max=100,step=1), style="padding-right:0px;"),
                                 column(2, actionButton("planningBrushingRadiusEqual", label=NULL, icon=icon("equals"), style="padding:0px; text-align:center; width:20px; height:20px;"), style="padding:15px 0px 0px 0px; text-align:center;"),
                                 column(5,sliderInput("planningBrushingRadius2", NULL, value=10, min=1, max=100,step=1), style="padding-left:0px;"))
                                )
                              ),
                            
                            fluidRow(column(4,style="padding-top:7px;",tags$label(class="control-label", "Show fraction", style="padding-top:10px;")),
                                     column(8,sliderInput("planningBrushingFraction", NULL, value=1, min=0.01, max=1,step=0.01))),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;padding-top:7px;",tags$label(class="control-label", "Axis")),
                                     column(4,style="padding-left:5px;padding-right:5px;",selectInput("planningBrushingX", NULL, choices=NULL)),
                                     column(4,style="padding-left:5px;padding-right:15px;",selectInput("planningBrushingY", NULL, choices=NULL))),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;padding-top:7px;",tags$label(class="control-label", "Min limit")),
                                     column(4,style="padding-left:5px;padding-right:5px;",textInput("planningBrushingMinX", NULL, -100)),
                                     column(4,style="padding-left:5px;padding-right:15px;",textInput("planningBrushingMinY", NULL, -100))),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;padding-top:7px;",tags$label(class="control-label", "Max limit")),
                                     column(4,style="padding-left:5px;padding-right:5px;",textInput("planningBrushingMaxX", NULL, 100)),
                                     column(4,style="padding-left:5px;padding-right:15px;",textInput("planningBrushingMaxY", NULL, 100))),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;",tags$label(class="control-label", HTML(paste0(tags$u("A"),"dd")), style="height:20px;")),
                                     column(4,style="padding-left:5px;padding-right:5px;",tags$div(prettyCheckbox("planningRadioButtonAdd",NULL, value=T, shape="round",status="primary")), style="height:20px;"),
                                     column(4,style="padding-left:5px;padding-right:15px;")),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;",tags$label(class="control-label", HTML(paste0("Adju",tags$u("s"),"t to")),style="padding-top:7px;")),
                                     column(4,style="padding-left:5px;padding-right:5px;", tags$div(prettyCheckbox("planningRadioButtonAdjust",NULL, shape="round",status="primary"), style="height:39px; padding-top:7px;")),
                                     column(4,style="padding-left:5px;padding-right:15px;", tags$div(selectInput("planningBrushingAdjustTo", NULL, choices=c("")), style="max-height:0px;"))),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;",tags$label(class="control-label", HTML(paste0(tags$u("D"),"elete")))),
                                     column(4,style="padding-left:5px;padding-right:5px;",prettyCheckbox("planningRadioButtonDelete",NULL, shape="round",status="primary")),
                                     column(4,style="padding-left:5px;padding-right:15px;")),
                            
                            fluidRow(column(4,style="padding-left:15px;padding-right:5px;",tags$label(class="control-label", HTML(paste0("Only ", tags$u("N"),"A")))),
                                     column(4,style="padding-left:5px;padding-right:5px;", prettySwitch("planningSwitchOnlyNA",NULL, value=T, status="primary")),
                                     column(4,style="padding-left:5px;padding-right:15px;"))
                            ),
            
            #TODO replace
            # bsCollapsePanel(
            #   "Visualize data", 
            #   value="planningCollapsePanelVisualizeData", 
            #   tags$div()
            #   )
            )
      ),
      
      mainPanel(width=9,
        fluidRow(
          column(7, style = "border-right: 3px solid grey; min-height:700px; padding:0px; padding-right:15px;",
                 
                 tabsetPanel(id="planningCreatingDataTabsetPanel", 
                   tabPanel("Dataset",
                          hidden(tags$div(
                            id="planningPropertiesOverview", 
                            class = "borderColor-regular",
                            style="border-bottom:1px solid;padding:5px;border-radius:4px;",
                                  tags$div(style="overflow:auto;",
                                     fluidRow(id="planningPropertiesOverviewRow1", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow2", class = "borderColor-regular backgroundColor", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow3", class = "borderColor-regular", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow4", class = "borderColor-regular backgroundColor", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow5", class = "borderColor-regular", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow6", class = "borderColor-regular backgroundColor", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;"),
                                     fluidRow(id="planningPropertiesOverviewRow7", class = "borderColor-regular", style="margin:0px 15px 0px 0px;border-bottom:1px solid;width: max-content;border-bottom:1px solid"))),
                                 
                                   #General porperties
                                 tags$div(id="planningPropertiesOverviewGeneral", 
                                          class = "borderColor-regular",
                                          style="margin-top:25px;border-top:1px solid;padding:5px;border-radius:4px;",
                                   fluidRow(style="margin-left:0px;margin-right:0px;",
                                     tags$div(style="min-width:270px !important; max-width:270px !important; float:left; margin:5px;",
                                              tags$label("Remove empty rows", style="padding-top:6px;")),
                                     tags$div(style="min-width:130px !important; max-width:130px !important; float:left; margin:5px;",
                                              actionButton("removeEmptyRows", "Remove", width="100%"))
                                   ),
                                   fluidRow(style="margin-left:0px;margin-right:0px;",
                                            tags$div(style="min-width:270px !important; max-width:270px !important; float:left; margin:5px;",
                                                     tags$label("Remove rows with empty cells", style="padding-top:6px;")),
                                            tags$div(style="min-width:130px !important; max-width:130px !important; float:left; margin:5px;",
                                                     actionButton("removeRowsOfNA", "Remove", width="100%"))
                                   )
                                 )

                               ),
                   ),
                   tabPanel("Brushing data",
                            plotOutput(outputId  = "planningBruhsingPlot", click = "planningBruhsingPlotClick", 
                                       hover=hoverOpts("planningBruhsingPlotHover",delay=25)), 
                            fluidRow(
                              column(1,tags$label(class="control-label", "Y-axis", style="padding-top:8px;")),
                              column(4,uiOutput("planningPlotYAxisLimitsUI")),
                              column(1,offset=2,tags$label(class="control-label", "X-axis", style="padding-top:8px;")),
                              column(4,uiOutput("planningPlotXAxisLimitsUI"))
                            ),
                            tags$div(class="wordWrappedInHorizLine", " or "),
                            tags$div(style="margin:auto;display:table;", prettyCheckbox("planningPlotYAxisLimitsAuto", label ="Adjust limits automatically to data", value=T, status="primary",shape ="round"))
                   ),
                   tabPanel("Visualize data",
                            tags$div()
                   )
                 )
                 
                 ),
          column(5, style="margin-left:-3px; border-left:3px solid grey;",
                 h5("Summarize data", style="text-align:center;"),
                 tags$div(DTOutput(outputId = "planningSummarizePlot"), style="overflow:auto;")
                 )
        )
   )))
}