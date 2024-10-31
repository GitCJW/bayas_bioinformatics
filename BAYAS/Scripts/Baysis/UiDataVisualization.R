
data_visualization_page <- function(){
  tabPanel(
    "Data visualization",

    # first row for page elements. Second row holds next button
    tags$div(
                    
      tags$div(
                              
        shinybayas::bayasSidePanel(
          inputId = "sidebarPanelUpload",
          
          mainPanelClass = "getActiveColor",
          sidePanelClass = "getActiveColor",
          
          #Sidebar
          sidePanel = tags$div(
            # style = "width:0px;",
            DTOutput("userInputDataTable", height="637px")
          ),
          
          # Main
          mainPanel = tags$div(
            style = "",
            
            tags$div(
              class = "borderColor-regular",
              style="margin:0px; border-bottom: 1px solid; border-radius:4px;",
              h5("Plot your uploaded data as visual sanity check")
            ),
          
            tags$br(),
            fluidRow(
              column(3, 
                     tags$div(tags$fieldset(tags$legend("Axis", style="font-size:15px;")),
                              tags$div(selectizeInput(inputId = "uploadSelectX", label = NULL, choices = c(X=""))),
                              tags$div(selectizeInput(inputId = "uploadSelectY", label = NULL, choices = c(Y=""))))),
              column(3, 
                     tags$div(tags$fieldset(tags$legend("Scaling", style="font-size:15px;")),
                              tags$div(selectizeInput(inputId = "uploadScaleX", label = NULL, 
                                                      choices = c(pseudo_log="", "asn", "atanh", "boxcox", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt"))),
                              tags$div(selectizeInput(inputId = "uploadScaleY", label = NULL, 
                                                      choices = c(pseudo_log="", "asn", "atanh", "boxcox", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt"))))),
              column(3, #offset=1,
                     tags$div(tags$legend("Grouping", style="font-size:15px;"),
                              tags$div(selectInput(inputId = "uploadSelectColorize", label = NULL, choices = c('Colorize by'=""))),
                              tags$div(selectInput(inputId = "uploadSelectFacetWrap", label = NULL, choices = c('Facet wrap'=""), multiple=T)))),
              column(3, #offset=1,
                     tags$div(tags$legend("Plot type", style="font-size:15px;"),
                              
                              tags$div(
                                
                                bslib::tooltip(
                                  trigger = selectInput(
                                    inputId = "uploadPlotType", 
                                    label = NULL, 
                                    choices = c('Type plot' = "","Scatterplot","Scatter+Histogram","Histogram","Density", "2dHistogram"), 
                                    selected="Scatterplot"),
                                  tooltip$plotType,
                                  options = list(trigger="hover", delay=list(show= 1000, hide=100))
                                )),
                              
                              tags$div(
                                
                                actionButton(inputId = "btnPlotInputData", label = "Plot"),
                                getReportButton("reportPreplot", tooltip="Report the current plot.", class=""),
                                
                                downloadPlotUI("uploadTabPaneldownloadPreplot", label=icon("download"), tooltip=HTML(tooltip$downloadMP)),

                                bslib::tooltip(
                                  trigger = actionButton("removePreplot", "",icon("trash"), style= "float:right;"),
                                  tooltip$closeCurrentMP,
                                  options = list(trigger="hover")
                                )
                                
                              )))
            ),

            
            tags$br(),
            
            #Alternative, holds the plots in a tabpanel
            shiny::tabsetPanel(
              id = "uploadTabPanelPlot",
              type = "pills")
          )
        )
      ),
      
      # Second Row of elements for e.g. Navigation (Next Button) 
      footerPanel(
        backButtonsId="btnVisualizePageBack",
        nextButtonsId="btnVisualizePageNext",
        backButtons="Data upload",
        nextButtons="Model selection"
      )
      
    )
)}