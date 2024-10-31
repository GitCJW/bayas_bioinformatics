upload_page <- function(ns){
  tabPanel(
    "Data upload",
    
    tags$div(
      style="display: flex; margin-bottom:20px;",

      tags$div(
        id = "userUploadPanelLeft",
        style = "flex:1 0 0; min-width: 0px;",
        style = "margin-right:0px; padding:0px 20px 20px 20px;",
        class = "getActiveColor",

        h5(
          class= "borderColor-regular",
          style = "border-bottom: 1px solid; margin-bottom:20px;",
          "Input data"
        ),

        tags$div(

          fluidRow(
            column(
              width=8, 
              tags$div(
                style = "margin-bottom:0px;",
                ownFileInput(
                  inputId = ns("userInputData"), multiple = FALSE, 
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".txt", ".xlsx"),
                  progressBar=F)
              )),
            column(width=4,                               
                   tags$div(actionButton("exampleUserData","Example", style="width:-moz-available; width: -webkit-fill-available;")))
          )
        ),


        uiOutput(
          outputId = ns("uiUserRawData"),
          class= "borderColor-regular insetShadow",
          style="width:100%; min-height:300px;",
          style = "border:1px solid; border-radius: 4px;",
          style = "display:flex;",
        )
      ),


      tags$div(
        id = "dragHandleUploadPanelsDiv",
        style = "display:flex; flex-direction: column; justify-content:center; align-items:center; font-size:30px;",
        style = "margin:0px 15px;",
        
        tags$div(
          class = "dragHandleUploadPanels",
          
          style = "width:20px; height:100%; cursor: ew-resize;",
          style = "display: flex; justify-content: center; align-items: center;",
          tags$div(
            class = "backgroundColor-border-regular",
            style = "width:2px; height:100%;"
          )
        ),
        

        actionButton(ns("rawDataToTable"), label="", 
                     icon=tags$i(class="icon icon-angle-double-right-solid"), 
                     style = "margin:10px 0px; display: inline-flex; font-size: large; padding: 9px;"),
        # actionButton(ns("rawDataToTableAdd"), label="", 
        #              icon=tags$i(class="icon icon-angle-double-right-add-solid"), 
        #              style = "margin:10px 0px; display: inline-flex; font-size: large; padding: 9px;"),
        # actionButton(ns("rawDataToTableUndo"), label="", icon=icon("undo-alt"), style = "margin:10px 0px"),
        # actionButton(ns("rawDataToTableRedo"), label="", icon=icon("redo-alt"), style = "margin:10px 0px"),
        actionButton(ns("rawDataToTableRemove"), label="", icon=icon("trash"), style = "margin:10px 0px"),
        
        tags$div(
          class = "dragHandleUploadPanels",
          
          style = "width:20px; height:100%; cursor: ew-resize;",
          style = "display: flex; justify-content: center; align-items: center;",
          tags$div(
            class = "backgroundColor-border-regular",
            style = "width:2px; height:100%;"
          )
        )
      ),


      tags$div(
        id = "userUploadPanelRight",
        style = "flex:1 0 0; min-width: 0px;",
        style = "margin-left:0px;  padding:0px 20px 20px 20px;",
        class="getActiveColor",

        h5(
          class = "borderColor-regular",
          style = "border-bottom: 1px solid; margin-bottom:20px; display: flex; flex-direction: column;",
          "'Long format' data"
        ),

        #Table of values
        tags$div(
          class = "borderColor-regular insetShadow",
          style = "border:1px solid; border-radius: 4px;",
          style = "margin-bottom:20px;",

          DTOutput(ns("tableUserData"), height="310px"),

          
          tags$div(
            style = "padding: 20px;",

            hidden(
              tags$div(
                id = ns("tableUserDataInfo"),
                
                style = "text-align:center; padding-top: 20px; height: 100%;",
                style = "display:flex; flex-direction: column; height:310px; ",

                tags$div(
                  style = "font-size: larger; font-weight: bold;",
                  "Select an area or columns in the spreadsheet that contain all of your data and click on"
                ),

                tags$div(
                  style = "width:30px; height:30px; margin: 10px auto 0px auto;",

                  actionButton(ns("rawDataToTable2"), label="", 
                               icon=icon("angle-double-right"), 
                               class="btn-primary",
                               style = "margin:10px 0px"),
                  
                ),

                tags$div(
                  class = "borderColor-regular",
                  style = "border-top:1px solid;",
                  style = "margin-top: auto; text-align: left; padding: 20px;",

                  tags$div(
                    style = "",
                    paste0(
                      "Long format data, also known as 'stacked' or 'narrow' data, ",
                      "is structured in a way where each observation has its own row, ",
                      "and each variable has its own column, facilitating easy analysis."
                    ),

                    actionLink(ns("showExampleOfLongFormat"), "Show an example")

                  )
                )
              )
            )
          )
        ),


        h5(
          class = "borderColor-regular",
          style = "border-bottom: 1px solid; margin-bottom:20px;",
          "Input properties"
        ),

        #Table of input properties
        tags$div(
          class = "borderColor-regular insetShadow",
          style = "border:1px solid; border-radius: 4px;",
          style = "",
          DTOutput(ns("tableInputProperties")),
          
          hidden(
            tags$div(
              id = ns("tableInputPropertiesInfo"),
              style = "margin-left: 20px;",
              tags$h6("Hold 'Shift' to enable horizontal scrolling with the mouse wheel.")
            )
          )

        )

      )
    ),
    
    
    # Row of elements for e.g. Navigation (Next Button) 
    footerPanel(
      nextButtonsId = "btnUploadPageNext",
      nextButtons = "Data Visualization"
    )

  )
}



################################################################################
#################################### Import ####################################
################################################################################
upload_import <- function(ns, step, obj){
  
  l <- list()

  
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

      )
    )
  }
  
  ###########################
  ######## ... #######
  ###########################
  else if(step==2){
   
  }
  #Empty
  else{
    l$div <- tags$div(
      style="min-height:350px;"
    )
  }
  
  
  l <- tags$div(
    backButton(ns, "planningModalBack",step, l),
    tags$div(
      style="margin-top:20px; margin-left:30px;",
      icon(class= "fontColor-primary", "info-circle"),
      tags$div(style="display: inline;font-weight: bold;font-size: 13px;", 
               "You can change everything later on")
    )
  )

  
}




upload_pageDragInput <- function(ns){
  dragFileInput(
    inputId = ns("userInputData2"),  multiple = FALSE, 
    accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".txt", ".xlsx"))
}

upload_page_userData <- function(ns, disable=F, selDecSep = "point", selCellSep = "comma"){
  gBtnDecSep <- bayasGroupedButtons(inputId=ns("userInputDecimalSeparator"), 
                                 btnNames=c(".",","), btnValues=c("point","comma"),
                                 selected=selDecSep,columns=2,
                                 btnStyle="padding:2px; height:100%; text-algin:center; width:30px;")
  gBtnCellSep <- bayasGroupedButtons(inputId=ns("userInputCellSeparator"), 
                                     btnNames=c(",",";","TAB","' '"), btnValues=c("comma","semicolon","tab","empty"),
                                     selected=selCellSep,columns=4,
                                     btnStyle="padding:2px; height:100%; text-algin:center; width:40px;")
  if(disable){
    gBtnDecSep <- disabled(gBtnDecSep)
    gBtnCellSep <- disabled(gBtnCellSep)
  }
  
  tags$div(
    style = "width: 100%; overflow-x: auto;",
    
    #Options header
    tags$div(
      class = "borderColor-regular",
      style = "display:flex; border-bottom: 1px solid; margin-bottom:20px;",
      style = "align-items: center;",
      
      tags$div(
        style = "display:flex; align-items: center; margin:0px 0px 0px 20px;",
        style = "padding:0px 5px 0px 5px",
        "Decimal separator",
        tags$div(style="width:20px;"),
        gBtnDecSep
      ),
      
      tags$div(
        class = "borderColor-regular",
        style = "width:0px; margin:0px 20px 0px 20px; height:100%;",
        style = "border-left:1px solid;"
      ),
      
      tags$div(
        style = "display:flex; align-items: center;",
        style = "padding:0px 5px 0px 5px",
        "Cell separator",
        tags$div(style="width:20px;"),
        gBtnCellSep
      )

    ),
    
    tags$div(
      #Remove messy border of excel div
      class = "no_border_child",
      style = "width: 100%;",
      excelOutput(ns("rawInputExcel"), height="400px"),
      hidden(uiOutput(ns("rawInputExcelErrors"), style = "width: 100%; "))
    )
    
  )
}


upload_page_longFormat <- function(ns){
  
  tags$div(
    style = "display:flex;",
    
    tags$div(
      style = "flex:1;",
      
      excelOutput(ns("exampleExcelTable"))
      
    ),
    
    tags$div(
      style = "flex:1;",
      
      DTOutput(
        ns("exampleDataTable"),
        height = "100%"
      )
    )

  )
  
}

inputPropertiesTable <- function(dMID, onlyData=F){

  #Input properties table
  inputProp <- dMID$getInputProperties()
  if(is.null(inputProp)) return(NULL)

  for(i in seq_len(dim(inputProp)[1])){
    type <- inputProp$type[i]
    inputProp$type[i] <- as.character(
      tags$div(
        style = "margin-bottom:-20px; width:120px; max-width:none !important;",
        class = "table_select_intput",
        selectInput(
          inputId=paste0("row_select_a_", i), 
          label=NULL, 
          choices=c("Discrete","Continuous","Categorical"),
          selected=inputProp$type[i],
          width="100%"
        )
      )
    )
    inp <- selectInput(
      inputId=paste0("row_select_b_", i), 
      label=NULL, 
      choices=c("-INF","0",">0"),
      selected=inputProp$lower[i],
      width="100%"
    )
    if(type == "Categorical") inp <- disabled(inp)
    inputProp$lower[i] <- as.character(
      tags$div(
        style = "margin-bottom:-20px; width:120px; max-width:none !important;",
        class = "table_select_intput",
        inp
      )
    )
    inp <- selectInput(
      inputId=paste0("row_select_c_", i), 
      label=NULL, 
      choices=c("<1","1","INF"),
      selected=inputProp$upper[i],
      width="100%"
    )
    if(type == "Categorical") inp <- disabled(inp)
    inputProp$upper[i] <- as.character(
      tags$div(
        style = "margin-bottom:-20px; width:120px; max-width:none !important;",
        class = "table_select_intput",
        inp
      )
    )
    inputProp$response[i] <- as.character(
      tags$div(
        style = "margin:0px 0px -15px 0px; width:120px; text-align: center; max-width:none !important;",
        checkboxInput(
          inputId=paste0("row_select_d_", i), 
          label=NULL, 
          value=inputProp$response[i]=="TRUE"
        )
      )
    )
  }
  
  
  #Transpose
  inputProp_t <- t(inputProp)
  df_t <- as.data.frame(inputProp_t)
  df_t <- df_t[-1,,F]
  
  rn <- rownames(df_t)
  rn[1] <- as.character(labelWithInfo("row_select_a_info", "Type", "Type", tooltip$dataPropertiesCharacteristic))
  rn[2] <- as.character(labelWithInfo("row_select_b_info", "Lower limit", "Lower limit", tooltip$dataPropertiesLowerLimit))
  rn[3] <- as.character(labelWithInfo("row_select_c_info", "Upper limit", "Upper limit", tooltip$dataPropertiesUpperLimit))
  rn[4] <- as.character(labelWithInfo("row_select_d_info", "Response", "Response", tooltip$responseVariable))
  rownames(df_t) <- rn
  
  if(onlyData) return(df_t)
  
  dt <- datatable(
    data = df_t,
    rownames = T,
    escape = F, 
    selection = "none",
    extensions = c('FixedColumns'),
    options = list(searching=F, paging=F, info=F, sorting=F, processing = FALSE,
                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node());}'),
                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node());}'),
                   scrollX = TRUE,  fixedColumns = list(leftColumns=1), scrollY = 280
                   )
  )
  
  return(dt)
}



###################################
############# Helper ##############
###################################

#Back button
backButton <- function(ns, id="planningModalBack", step, div){
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
