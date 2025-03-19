introduction_page <- function(){
  
  tabPanel("Introduction", 
         
         verticalLayout( fluid = F, 
                         # First Row of elements. Containing the workflow of process. Experiment -> Bayesian Analysis -> Result
                         wellPanel(id = "pad_1_mar_min15", style= getBackgroundColor(),
                                   
                                   h1(id = "center", "Your Workflow"),    
                                   
                                   tags$div(align = "center", splitLayout(cellWidths = c(400,50,400,50,400),
                                               
                                               # Arrow
                                               # absolutePanel(id = "font_100", "-", top = 300, left = 30),
                                               
                                               # First Block (total of 3)
                                               wellPanel(id = "rounded_border", height = 200, style = getActiveColor(),
                                                          
                                                          tags$h2(id = "double_underline", "1. Prework"),
                                                          
                                                          wellPanel(id = "no_border", style = getActiveColor(),
                                                                    
                                                                    #todo, css auslagern in css skript
                                                                    tags$h4(id = "underline", "1. Your experiment"), 
                                                                    imageOutput(outputId = "imagePrework1", height = "100px"),
                                                                    
                                                                    tags$h4(id = "underline","2. Collect your data"), 
                                                                    imageOutput(outputId = "imagePrework2", height = "100px"),
                                                                    
                                                                    tags$h4(id = "underline","3. Your question"), 
                                                                    tags$h5("Does the drug have any effect?")
                                                          )
                                               ),
                                               
                                               # absolutePanel(id = "font_100", "-", top = 300, left = 462),
                                               absolutePanel(),
                                               
                                               # Second Block
                                               wellPanel(id = "highlight_border", height = 200, style = getActiveColor(),
                                                         
                                                         tags$h2(id = "double_underline", "2. BAYSIS"),
                                                         
                                                         wellPanel(id = "no_border", style = getActiveColor(),
                                                                   
                                                                   splitLayout(cellWidths = c(110,240),
                                                                               tags$div(id = "center", imageOutput(outputId = "imageTool1", height = "80px")),
                                                                               tags$h4("1. Upload your data")
                                                                   ),
                                                                   
                                                                   splitLayout(cellWidths = c(110,240),
                                                                               tags$div(id = "center", imageOutput(outputId = "imageTool2", height = "80px")),
                                                                               tags$h4("2. Verify your data")
                                                                   ),
                                                                   
                                                                   splitLayout(cellWidths = c(110,240),
                                                                               tags$div(id = "center", imageOutput(outputId = "imageTool3", height = "80px")),
                                                                               tags$h4("3. Select a feasible model")
                                                                   ),
                                                                   
                                                                   splitLayout(cellWidths = c(110,240),
                                                                               tags$div(id = "center", imageOutput(outputId = "imageTool4", height = "80px")),
                                                                               tags$h4("4. Adjust the model")
                                                                   ),
                                                                   
                                                                   splitLayout(cellWidths = c(110,240),
                                                                               tags$div(id = "center", imageOutput(outputId = "imageTool5", height = "80px")),
                                                                               tags$h4("5. Inspect your results")
                                                                   )
                                                                   
                                                         )
                                               ),
                                               
                                               # absolutePanel(id = "font_100", "-", top = 300, left = 895),
                                               absolutePanel(),
                                               
                                               # Third panel
                                               wellPanel(id = "rounded_border", height = 200, style = getActiveColor(),
                                                         
                                                         tags$h2(id = "double_underline", "3. Result"),
                                                         
                                                         wellPanel(id = "no_border", style = getActiveColor(),
                                                                   
                                                                   tags$h4(id = "underline","1. Review your question"), 
                                                                   tags$h5("Does the drug have any effect?"),
                                                                   tags$br(),
                                                                   tags$h4(id = "underline","2. Make your statement"), 
                                                                   tags$h5("Quantitative drug effect"),
                                                                   splitLayout(
                                                                     tags$div(tags$h5(id = "center_bold_red", "No"),
                                                                              tags$div(id = "center", imageOutput(outputId = "imagePostwork1", height = "100px"))),
                                                                     tags$div(tags$h4(id = "center_bold_green", "Yes"),
                                                                              tags$div(id = "center", imageOutput(outputId = "imagePostwork2", height = "100px")))
                                                                   )
                                                                   
                                                         )
                                               )
                                               
                                               # absolutePanel(id = "font_100", ">", top = 300, left = 1325)
                                               
                                   ))
                         ),
                         
                         # Second Row of elements for e.g. Navigation (Next Button) 
                         wellPanel(id = "next_footer", style = getActiveColor(), #style = "padding: 10px; margin: 0px",
                           
                              fluidRow(
                                column(2, offset = 2, wellPanel(style = getActiveColor(), style="text-align:justify;",
                                  h4(id = "center", "Some facts about BAYSIS"),
                                  tags$li("BAYSIS is a tool for easy access to Bayesian statistics. It is a graphical interface for ", 
                                          tags$a(href="https://mc-stan.org/", "stan", target="_blank")),
                                  tags$li("Stan models data as coming from distributions like normal, exponential, binomial, student t, etc.")
                                )),
                                column(2, wellPanel(style = getActiveColor(), style="text-align:justify;",
                                  h4(id = "center", tags$div(icon("exclamation-triangle"), style = "color: #1684c2; font-size: 22px;")),
                                  tags$li("This sign combines a warning with more information:"),
                                  tags$li("If you see this sign you have to do something, such as verify data, provide input, etc."),
                                  tags$li("If you click on the sign, a little text pops up with more information")
                                )),
                                column(2, wellPanel(style = getActiveColor(), style="text-align:justify;",
                                  h4(id = "center", "Need help?"),
                                  tags$li("Most elements have tooltips, hover over to show them."),
                                  # tags$li("First time users should enable the guide."),
                                  # checkboxInput(inputId = "introductionGuide", label= "Use the guide", value = F),
                                  tags$li("Or watch this ", actionLink(inputId = "btnWatchVideo", label = "short walkthrough video,")," to learn how to use this tool."),
                                  tags$li(downloadLink(outputId = "btnExampleData", label = "Example data"))
                                )),
                                column(2, align = "center", style = "margin-top: 50px;", 
                                       actionButton(class = 'btn-primary',"btnLetsstart", "Let's Start",  style=next_button(),style="font-size: 30px;")
                                )
                              )     
                                    
                           
                         )
         )
)
}