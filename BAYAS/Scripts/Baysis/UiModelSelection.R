select_model_page <- function(){
  tabPanel(
     "Model selection",

     # first row for page elements. Second row holds next button
     verticalLayout(
       fluid = F,
       
       tags$div(
                 
         shinybayas::bayasSidePanel(
           inputId = "sidebarPanelModelSelection",
           
           mainPanelClass = "getActiveColor",
           sidePanelClass = "getActiveColor",
           
           sidePanel = tags$div(
             class="getActiveColor",
             
             
             div(

                selectizeInput(
                  inputId = "selectStanModelAvailable", 
                  label = h5(
                    style = "font-weight: bold;", 
                    "Select an appropriate statistical model"),
                  choices = c(), selected = ""),
                
               disabled(
                 ownTextAreaInput(
                   class= "infoBox",
                   style="height:120px;",
                   inputId = "selectStanModelNotAvailableInfoBox", 
                   label=NULL, 
                   placeholder = "Select a model for information",
                   resize="vertical"))

             )
             
           ),
           
           
           # Main Panel
           mainPanel = tags$div(
              wellPanel(
                id = "panelSelectModel", 
                class="getActiveColor",
                style= "margin-bottom: 1.5rem;", 
                h5("Revise your model", style="text-align:left;"),
                uiOutput("formulaOutput")
                ),
              
              wellPanel(
                id = "panelSelectModelPreview", 
                class="getActiveColor", 
                h5("Information", style="text-align:left;"),
                
                fluidRow(
                  column(6, uiOutput(outputId = "previewDataSelectModelLeft"), style="padding-right:5px;"),
                  column(6, uiOutput(outputId = "previewDataSelectModelRight")))
                )
           )
         )
       ),
       
       # Second Row of elements for e.g. Navigation (Next Button) 
       footerPanel(
         backButtonsId="btnModelSelectionBack",
         nextButtonsId="btnModelSelectionNext",
         backButtons="Data visualization",
         nextButtons="Model fitting"
       )
  
                     
     )
  )
}



