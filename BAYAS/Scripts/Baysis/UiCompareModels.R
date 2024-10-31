compareModels_page <- function(){
  tabPanel(
    "Model comparison", 
           

           
           # first row for page elements. Second row holds next button
   tags$div( 

     tags$div(
               
       shinybayas::bayasSidePanel(
         inputId = "sidebarPanelCompareModels",
         
         mainPanelClass = "getActiveColor",
         sidePanelClass = "getActiveColor",
         
         sidePanel = tags$div(
           class="getActiveColor",
           
           # Compare two models
           tags$div(
             h5(
               style = "margin-bottom:0px;", 
               "Comparison of two models"),
             tags$div(
               class = "borderColor-regular",
               style = "border-top: 1px solid; padding:20px;",
               selectInput(inputId = "compareModelsSelectModelFirst", label = NULL, choices = c('Select first model' = "")),
               selectInput(inputId = "compareModelsSelectModelSecond", label = NULL, choices = c('Select second model' = "")),
               tags$div(style= "text-align:right;", 
                        actionButton(class = 'btn-primary', inputId = "compareModelsButtonCompareTwo", label = "Compare"))
             )

           ),
           
           
           tags$div(
             class = "wordWrappedInHorizLine",
             style = "",
             " or "
           ),
           
           # Compare models based on same repsonse variable
           tags$div(
             style = "margin-top:20px;",
             h5(
               style = "margin-bottom:0px;", 
               "Comparing models of same response"),
             tags$div(
               class = "borderColor-regular",
               style = "border-top: 1px solid; padding:20px;",
               selectInput(inputId = "compareModelsSelectResponse", label = NULL, choices = c('Select response' = "")),
               tags$div(
                 style= "text-align:right;",
                 actionButton(class = 'btn-primary', inputId = "compareModelsButtonCompareResponse", label = "Compare")),
               tags$div(
                 style = "",
                 uiOutput("compareModelsModelsOfSelectedResponse")
               )
             )
             
           )
           
         ),
         
         
         # Main Panel
         mainPanel = tags$div(
             h5( "Comparing results"),
             tags$br(),
             
             fluidRow(
               column(6, 
                      dataTableOutput(outputId = "compareModelsTableResult"),
                      tags$br(),
                      uiOutput(outputId = "compareModelsTableResultDescription")),
               column(6, 
                      plotOutput("compareModelsParetoDiagnostic")
                      )
             ),
             tags$div(
               class= "borderColor-dark",
               style="text-align:right;",
               style="margin-top:20px; padding-top:10px; border-top: 1px solid;",
               getReportButton(
                 inputId="reportModelComparison", 
                 label="Report", tooltip="Report the model comparison result.", 
                 class=""))
         )
       )
     ),
     
     
     footerPanel(
       backButtonsId="btnModelComparisonBack",
       nextButtonsId="btnModelComparisonNext",
       backButtons="Model fitting",
       nextButtons="Quantifications"
     )
            
   )
  )
}