report_default_page <- function(){
  ns <- NS("reportTool")


  labels <- list()
  for(i in 1:12){
    labels[[i]] <- report_elements_sortable(paste0("test_",i), paste0("Images/Report/Formula_", GLOBAL_THEME,".png"))
  }
  names(labels) <- c(1:12)

  checklistContent <- get_checklist_content()
  
  tabPanel("Report", 
           
           sidebarLayout(
             
             sidebarPanel(
               class="getActiveColor",
               
               
               tags$div(
                 class="borderColor-dark",
                 style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
                 tags$label(
                   style = "margin-bottom:0px;",
                   "Filter items ...")
               ),
               
               # Filter by module type
               tags$div(style="margin-bottom:-10px;",
                        selectizeInput(ns("filterByBAYASModule"), label=NULL, 
                                       choices=list("Planning Evaluation"="", "Planning"="planning","Evaluation"="evaluation"), multiple=T)),
               
               # Filter by model data
               tags$div(style="margin-bottom:-10px;",
                        selectizeInput(ns("filterByData"), label=NULL, 
                                       choices=list("Evaluation: Data name"=""), multiple=T)),
               
               # Filter by pDIM (model fits)
               selectizeInput(ns("filterByFits"), label=NULL, 
                              choices=list("Evaluation: Fit"=""), multiple=T),
               
               
               # Show reported elements
               tags$div(
                 style="margin-bottom:20px; margin-top:30px;",
                 
                 tags$div(
                   class="borderColor-dark",
                   style="text-align:left; border-bottom:1px solid; margin-bottom:20px;",
                   tags$label(
                     style = "margin-bottom:0px;",
                     "Your reported items")),
                  
                  uiOutput(ns("reportElements")),
                  tags$div(style="display:flex; flex-wrap:wrap;",
                           actionButton(ns("selectAllReportedItems"), 
                                        "Select all", 
                                        style="flex: 1 1; margin:10px 10px 0px 0px;"),
                           actionButton(ns("deselectAllReportedItems"), 
                                        "Deselect all", 
                                        style="flex: 1 1; margin:10px 0px 0px 10px;"))
                        ),
               
               
               # Show recommended elements for a selected fit
               tags$div(
                 style="margin-bottom:20px; margin-top:30px;",
                 
                 tags$div(
                   class="borderColor-dark",
                   style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
                  tags$label(
                    style = "margin-bottom:0px;",
                    "Recommended items to report")),
                 
                 uiOutput(ns("reportRecommendedElements")),
                 tags$div(style="display:flex; flex-wrap:wrap;",
                          actionButton(ns("selectAllRecommendedItems"), 
                                       "Select all", 
                                       style="flex: 1 1; margin:10px 10px 0px 0px;"),
                          actionButton(ns("deselectAllRecommendedItems"), 
                                       "Deselect all", 
                                       style="flex: 1 1; margin:10px 0px 0px 10px;"))
               ),
               
               tags$div(
                 style="margin-bottom:20px; margin-top:30px;",
                 tags$div(
                   class="borderColor-dark",
                   style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
                   tags$label(
                     style = "margin-bottom:0px;",
                     "Custom text item")),
                 actionButton(ns("addBlankItem"), "Add blank custom item", width = "100%")
               )

             ),
             
             mainPanel(

               # sortable list of selected reported elements
               tags$div(
                 style="margin-bottom:20px;",
                 tags$p(style="font-weight:bold;",
                        "Final elements in report"),
                 
                 uiOutput(ns("sortableReportElements"))
               ),
               
               
               #Checklist progress
               getChecklistProgress("checklistReport", c(1,10), c("P1", paste0("E", 1:10))),
               
               #Tabs 'Create' and 'PDF Viewer'
               tags$div(id=ns("reportSelectedItemView"),
                 tabsetPanel(id=ns("reportSelectedItemViewTabset"),
                   tabPanel(title="Create", value="create",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                class="getActiveColor",
                                tags$div(
                                  tags$div(
                                    class="borderColor-dark",
                                    style="text-align:left; border-bottom:1px solid; margin-bottom:20px;", 
                                    tags$label("Output option")),

                                  
                                  tags$div(
                                    id=ns("outputOptionsSizeFlex"), 
                                    bayasNumericInput(
                                      ns("outputOptionsFontSize"),
                                      label="Font size", value=11,min=5,max=36,
                                      integer=T,
                                      invalidMessage=T, invalidTooltip = T,
                                      style="margin:0px 1px 15px 1px;")),
                                  
                                  tags$div(
                                    style = "",
                                    textInput(ns("outputOptionsOptionalTitle"), label=NULL, placeholder="Your title"),
                                    textInput(ns("outputOptionsOptionalName"), label=NULL, placeholder="Your name")
                                  ),


                                  tags$div(
                                    class="borderColor-dark",
                                    style="text-align:left; border-bottom:1px solid; margin:20px 0px 20px 0px;",
                                    labelWithInfo(ns("reportAllElements"),"All elements","All elements",tooltip$reportAllElements)
                                    ),
                                  tags$div(
                                    style="margin-left:5px;",
                                    checkboxInput(ns("reorderItemsRecommended"), label="Sort items to recommended order", value=F)
                                  ),
                                  tags$div(class=ns("outputPrintFlex"), 
                                           actionButton(ns("createPDFPreviewAll"), 
                                                        label="PDF Preview", width='100%', 
                                                        style="margin:0px 5px 5px 5px;"),
                                           tags$a(id = ns("downloadAll"), class = paste("btn btn-default shiny-download-link"),
                                                  target="_blank",
                                                  shiny::icon("download"), "Download",
                                                  style="margin:0px 5px 5px 5px; width:100%;")
   
                                  ),
                                  
                                 tags$div(
                                   class="borderColor-dark",
                                   style="text-align:left; border-bottom:1px solid; margin:20px 0px 20px 0px;",
                                   labelWithInfo(ns("reportSingleElements"),"Single elements","Single elements",tooltip$reportSingleElements)),
                                 
                                 tags$div(class=ns("outputPrintFlex"), 
                                          actionButton(ns("createPDFPreviewSingle"), 
                                                       label="PDF Preview", width='100%', 
                                                       style="margin:0px 5px 5px 5px;"),
                                          tags$a(id = ns("downloadSingle"), class = paste("btn btn-default shiny-download-link"),
                                                 target="_blank",
                                                 shiny::icon("download"), "Download",
                                                 style="margin:0px 5px 5px 5px; width:100%;")
                                 )

                                )
                              ),
                              
                              mainPanel(
                                wellPanel(class="getActiveColor",
                                          id = ns("singleElementDiv")
                                         )
                              )
                              
                            )
                            

                   ),
                   tabPanel(title="PDF Viewer", value="pdfViewer",
                            # uiOutput(ns("PDFPreview")))
                            htmlOutput(ns("PDFPreview"))),
                   
                   #Guidline
                   tabPanel(title="Reporting guideline", value="guidelines",
                            
                            tags$div(
                              style="margin:20px;",
                              HTML(paste0("<b>The guideline includes the most important elements for a transparent and reproducible report of the analysis. ",
                                          "It follows the ", tags$a(href="https://arriveguidelines.org/arrive-guidelines", "ARRIVE guideline", target="_blank"), 
                                          " (", tags$a(href="https://arriveguidelines.org/arrive-guidelines/statistical-methods", "7. Statistical methods", target="_blank"), ")",
                                          " and the ", tags$a(href="https://www.nature.com/articles/s41562-021-01177-7", "BARG", target="_blank"), 
                                          " list.</b>") 
                              )
                            ),
                            
                            #Legend of circles (default, primary-alt, primary; not reported, reported for subset, reported for all)
                            tags$div(
                              style="display:flex; align-items: center; justify-content: center;",
                              style="margin-bottom:20px;",
                              
                              tags$div(
                                style="display:flex;",
                                
                                actionButton(inputId="dummy", label="E1", 
                                             style="border-radius:50%; width:38px;height:38px; padding:0px;"),
                                
                                tags$div(
                                  style="margin:8px 20px 0px 5px;",
                                  "Not reported for any fit."
                                )
                                
                              ),

                              tags$div(
                                style="display:flex;",
                                
                                actionButton(inputId="dummy", label="E1", class="btn-outline-primary",
                                             style="border-radius:50%; width:38px;height:38px; padding:0px;"),
                                
                                tags$div(
                                  style="margin:8px 20px 0px 5px;",
                                  "Reported for a subset of fits."
                                )
                                
                              ),
                              
                              tags$div(
                                style="display:flex;",
                                
                                actionButton(inputId="dummy", label="E1", class="btn-primary",
                                             style="border-radius:50%; width:38px;height:38px; padding:0px;"),
                                
                                tags$div(
                                  style="margin:8px 20px 0px 5px;",
                                  "Reported for all fits."
                                )
                                
                              )
  
                            ),
                            

                            fluidRow(
                              column(6,
                                     bslib::accordion(
                                        id="reportChecklistOverview",
                                        open="reportChecklistOverviewItem1",
                                        
                                        bslib::accordion_panel(
                                          title=checklistContent$planning[[1]]$header,
                                          value="reportChecklistOverviewItemP1",
                                          checklistContent$planning[[1]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsP1"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[1]]$header,
                                          value="reportChecklistOverviewItemE1",
                                          checklistContent$evaluation[[1]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE1"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[2]]$header,
                                          value="reportChecklistOverviewItemE2",
                                          checklistContent$evaluation[[2]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE2"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[3]]$header,
                                          value="reportChecklistOverviewItemE3",
                                          checklistContent$evaluation[[3]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE3"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[4]]$header,
                                          value="reportChecklistOverviewItemE4",
                                          checklistContent$evaluation[[4]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE4"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[5]]$header,
                                          value="reportChecklistOverviewItemE5",
                                          checklistContent$evaluation[[5]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE5"))
                                        )
                                     )
                              ),
                              column(6,
                                     bslib::accordion(
                                        id="reportChecklistOverview2",

                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[6]]$header,
                                          value="reportChecklistOverviewItemE6",
                                          checklistContent$evaluation[[6]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE6"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[7]]$header,
                                          value="reportChecklistOverviewItemE7",
                                          checklistContent$evaluation[[7]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE7"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[8]]$header,
                                          value="reportChecklistOverviewItemE8",
                                          checklistContent$evaluation[[8]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE8"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[9]]$header,
                                          value="reportChecklistOverviewItemE9",
                                          checklistContent$evaluation[[9]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE9"))
                                        ),
                                        bslib::accordion_panel(
                                          title=checklistContent$evaluation[[10]]$header,
                                          value="reportChecklistOverviewItemE10",
                                          checklistContent$evaluation[[10]]$text,
                                          uiOutput(ns("reportChecklistMissingFitsE10"))
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


report_elements <- function(dataModel, rrModel, recommended=F,
                            filterByModule=NULL, filterByData=NULL, filterByFits=NULL){
  ns <- NS("reportToolElements")
  
  items <- rrModel$getItems(recommended=recommended)
  filter_items <- items
  
  if(!is.null(filterByModule) ||
     !is.null(filterByData) ||
     !is.null(filterByFits)){
    filter_items <- list()
    for(item in items){
      if(is.null(filterByModule) || item$getModuleType() %in% filterByModule){
        if(item$getModuleType() == "planning"){
          filter_items <- list.append(filter_items, item)
        }else{
          if((is.null(filterByData) || item$getDataModel_id() %in% filterByData) &&
             (is.null(filterByFits) || is.null(item$getpDIM_name()) || item$getpDIM_name() %in% filterByFits)){
            filter_items <- list.append(filter_items, item)
          }
        }
      }
    }
  }
  items <- filter_items
  
  imgs <- NULL
  if(length(items) > 0){
    imgs <- lapply(1:length(items), function(i){
      id <- items[[i]]$getId()
      tags$div(
        id=ns(paste0("progressElement_",id)),
        style="width:fit-content; float:left; margin:5px;",
        class=ifelse(items[[i]]$getClicked(),"progress-element-active","progress-element-inactive"),
        style="padding:5px; border-radius:5px;",
               tags$button(
                 id = ns(paste0("progressElementClick_",id)),
                 title=HTML(items[[i]]$getHoverInfo()),
                 class = "btn action-button",
                 tags$img(src=items[[i]]$getImgFile(),
                          width="100",
                          height="100",
                          style="border-radius:5px;"),
                 style="padding:0px; border-radius:5px;"
               ),
               if(!recommended) tags$div(style="text-align:center;",
                        actionButton(ns(paste0("progressElementRemove_",id)),icon("trash"),
                                     style="padding:2px; width:-moz-available; width: -webkit-fill-available;margin-top:5px;"))
      )
    })
  }
  imgs <- tagList(imgs)

  ret <- div(
    class="borderColor-regular",
    style="border:1px solid; border-radius:5px; padding:10px;",
    style="overflow-y:auto;",
    style= if(recommended) "max-height:265px; " else "max-height:325px;",
    imgs
  )

  return(ret)
}

report_elements_sortable <- function(buttonId, buttonImg){
  tags$div(
    tags$button(id=buttonId, type="button", class = "btn btn-default action-button",
                `data-val` = 0,
                tags$img(src=buttonImg,
                         width="100",
                         height="100",
                         style="border-radius:5px;"),
                style="padding:0px; border-radius:5px; border:none;"))
}
