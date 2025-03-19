get_ui_home <- function(){

  hex_class <- "hexagon"
  
  ui <- tags$div(
    style = "padding: 0px; margin: 2px;", 


    ## Overview of the 3 main modules
    tags$div(
      
      style="display:flex; flex-direction:column; min-height:100vh;",

      #Logo
      tags$div(
        style="flex:1;",
        
        imageButtonTitle(
          btnId="logo", 
          imageFile=get_logo(), 
          title="", selected=F,
          btnStyle="width:auto; height:auto;", 
          imgHeight="80px")

        # tags$div(
        #   id = "logo",
        #   class = "btn action-button",
        #   style = "box-shadow: none;",
        #   tags$img(
        #     id = "logo_img", 
        #     src = get_logo(), 
        #     height = "80px", 
        #     width = "67px")
        # )
      ),
      

      #Hex representation
      tags$div(
        style = "flex: 1; justify-content: center; display: flex;",
        
        tags$div(
          style = "flex:1"
        ),

        tags$div(
          style = " width:405px; height:410px; margin:30px;",
          tags$div(
            id = "image_module_planning_click",
            class = "action-button",
            class=hex_class,
            style = "",
            
            tags$span(),
            
            tags$div(
              style = "z-index: 3; position: relative; display: flex; flex-direction: column; height: inherit;",
              style = "justify-content:center; align-items:center;",
              
              tags$div(
                style = "flex:10 0 0; min-height:0px; display: flex; align-items: center; justify-content: center;",
                tags$img(
                  style = "max-height:100%; cursor:pointer;user-select: none",
                  src = "Images/Home/Planning.png")
              ),
              tags$label(
                style = "flex:1 0 0; font-size: large; font-weight: bold; margin-top: 10px;cursor:pointer;user-select: none",
                "Planning")
              
            )
          ),
          
          tags$div(
            id = "image_module_evaluation_click",
            class = "action-button",
            class = hex_class,
            style = "position: relative; left: 205px; top: -173px;",
            
            tags$span(),
            
            tags$div(
              style = "z-index: 3; position: relative; display: flex; flex-direction: column; height: inherit;",
              style = "justify-content:center; align-items:center;",
              
              tags$div(
                style = "flex:10 0 0; min-height:0px; display: flex; align-items: center; justify-content: center;",
                tags$img(
                  style = "max-height:100%;cursor:pointer;user-select: none",
                  src = "Images/Home/Evaluation.png")
              ),
              tags$label(
                style = "flex:1 0 0; font-size: large; font-weight: bold; margin-top: 10px;cursor:pointer;user-select: none",
                "Evaluation")
              
            )
          ),
          
          tags$div(
            id = "image_module_report_click",
            class = "action-button",
            class=hex_class,
            style = "position: relative; left: 103px; top: -167px;",
            
            tags$span(),
            
            tags$div(
              style = "z-index: 3; position: relative; display: flex; flex-direction: column; height: inherit;",
              style = "justify-content:center; align-items:center;",
              
              tags$div(
                style = "flex:10 0 0; min-height:0px; display: flex; align-items: center; justify-content: center;",
                tags$img(
                  style = "max-height:100%;cursor:pointer;user-select: none",
                  src = "Images/Home/Report.png")
              ),
              tags$label(
                style = "flex:1 0 0; font-size: large; font-weight: bold; margin-top: 10px;cursor:pointer;user-select: none",
                "Report")
              
            )
          )
        ),
          
        tags$div(
          style = "flex: 1; display: flex;",
          
          tags$div(
            id = "id_module_planning_description", 
            class = "borderColor-dark",
            style = "text-align: left; padding-left: 20px; padding-top: 20px; display:none;", 
            style = "margin: 30px 0px 0px 30px; border-top: 1px solid", 
            
            tags$label("The 'Planning' tool"),
            
            tags$br(),
            
            tags$div(
              style = "margin-bottom:10px;",
              HTML("can be used to plan an experiment or survey <br> by building a statistical model to")
            ),
            
            tags$ul(
              style = "padding-left:15px;",
              tags$li("generate synthetic data", style = "margin: 10px;"),
              tags$li("determine sample size", style = "margin: 10px;")
            )
          ),

          tags$div(
            id = "id_module_evaluation_description", 
            class = "borderColor-dark",
            style = "text-align: left; padding-left: 20px; padding-top: 20px; display:none;", 
            style = "margin: 30px 0px 0px 30px; border-top: 1px solid;", 
            
            tags$label("The 'Evaluation' tool"),
            
            tags$br(),
            
            tags$div(
              style = "margin-bottom:10px;",
              HTML("can be used to analyze data using Bayesian inference <br>by performing the following steps:")
            ),
            
            tags$ul(
              style = "padding-left:15px;",
              tags$li("Choose a reasonable model", style = "margin: 10px;"),
              tags$li("Customize the model to fit your data", style = "margin: 10px;"),
              tags$li("Quantify effects", style = "margin: 10px;"),
              tags$li("Create meaningful graphs", style = "margin: 10px;")
            )
          ),

          tags$div(
            id = "id_module_report_description", 
            class = "borderColor-dark",
            style = "text-align: left; padding-left: 20px; padding-top: 20px; display:none;", 
            style = "margin: 30px 0px 0px 30px; border-top: 1px solid;", 
            
            tags$label("The 'Report' tool"),
            
            tags$br(),
            
            tags$div(
              style = "margin-bottom:10px;",
              HTML("can be used to summarize the work in 'Planning' and 'Evaluation'.")
            ),

            tags$ul(
              style = "padding-left:15px;",
              tags$li("Create a PDF file for scientific writing (ARRIVE compliant)", style = "margin: 10px;"),
              tags$li("Predefined explanations", style = "margin: 10px;")
            )
          )
                   
        )

      ),
      
     
      tags$div(
        style="flex:3; display: flex;",
        style="text-align:center;",
        
        tags$div(
          style="flex:1; flex-grow:3;"
        ),
        
        tags$div(
          style="border-top: 1px solid #5555; ",
          style="min-width:250px;",
          style="margin: 0px 30px 0px 30px;",
          style="flex:1;",
          tags$h5(
            style="margin-top:10px;",
            "What is BAYAS?"),
          tags$ul(
            style="text-align:left; margin-bottom:20px;",
            tags$li("BAYAS simplifies Bayesian analysis"),
            tags$li("Watch walkthrough videos to learn how to use it")
          ),
          
          imageButtonTitle(
            btnId="walkthroughVideoBtn",
            imageFile="Images/Home/Video.png",
            title="Walkthrough videos", selected=F,
            btnStyle="width:auto; height:auto;", imgHeight="100px")
          
        ),
        
        tags$div(
          class = "borderColor-dark",
          style="border-top: 1px solid; ",
          style="min-width:250px;",
          style="margin: 0px 30px 0px 30px;",
          style="flex:1;",
          tags$h5(
            style="margin-top:10px;",
            "What's new?"),
          tags$ul(
            style="text-align:left; margin-bottom:20px;",
            tags$li("Planning of statistical analyses"),
            tags$li("Sample size determination"),
            tags$li("Walkthrough videos for the 'Planning' and 'Evaluation' tools")
          ),
          actionButton(
            "subscribeToBayas", "Subscribe to newsletter", 
            class="btn-primary", style="margin-bottom:20px;"
          )
        ),
        
        tags$div(
          class = "borderColor-dark",
          style="border-top: 1px solid; ",
          style="min-width:250px;",
          style="margin: 0px 30px 0px 30px;",
          style="flex:1;",
          tags$h5(
            style="margin-top:10px;",
            "Your feedback"),
          tags$div(
            style="text-align:left; margin-top:20px;",
            checkboxGroupInput("checkFeedback", label=NULL,
                               choiceNames=c("Do not know where/how to start",
                                         "Need more explanations", "Workflow not clear", 
                                         "BAYAS crashed"),
                               choiceValues=c(1,2,3,4)),
            
            textAreaInput("feedbackText", label=NULL, placeholder="Personalized feedback")
          ),
          actionButton("submitFeedback", "Submit", class="btn-primary"),
          
          tags$div(style="margin-top:15px;")
        ),
        


        
        tags$div(
          style="flex:1; flex-grow:3;"
        )
      )
  
    )           
  )
  
  return(ui)
}

#Returns walkthrough video modal
walkthroughVideo_modal <- function(){
  ret <- tags$div(
    
    tags$div(
      style = "display:flex; gap:50px;",
      tags$div(
        style ="flex:1; display:flex; justify-content: center; align-items: center; flex-direction: column;",
        tags$label("Planning"),

        getVideo(src="https://www.youtube.com/embed/rvO-A3N8E54?si=NbM3i26m1fAtPrUA",
                 title="BAYAS walkthrough planning", width="100%", height="300px")
        
      ),
      
      tags$div(
        style ="flex:1; display:flex; justify-content: center; align-items: center; flex-direction: column;",
        tags$label("Evaluation"),
        getVideo(src="https://www.youtube.com/embed/aiY_5tiMl-Y?si=oI_S_poFzNqo-mDe",
                 title="BAYAS walkthrough planning", width="100%", height="300px")
        
      )
      
    ),
  
    tags$div(
      style = "margin-top:20px;",
      tags$p("We recommend using the Picture-in-Picture ",
             tags$i(style="font-size:1.5rem; maring:0px 5px;",class="icon icon-PiP"), 
             HTML(" mode to watch the walkthrough video and work in BAYAS at the same time. "), 
             "(You can activate it by right-clicking on the video and selecting 'Picture-in-Picture'.)")
    )
    
  )
  return(ret)
}

#Returns vimeo video
getVideo <- function(src, title, width, height){

  tags$div(
    style=paste0("width:", width, "; height:", height,";"),
    tags$iframe(
      src=src,
      style="width:100%; height:100%;",
      frameborder="0",
      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
      referrerpolicy="strict-origin-when-cross-origin",
      allowfullscreen=TRUE,
      title=title
    )
  )

}

#Returns seasonal logo
get_logo <- function(){
  
  cDate <- Sys.Date()
  cYear <- as.numeric(str_split(cDate, "-")[[1]][1])
  
  #Interval defintion
  #Easter
  easterStart <- as.Date(paste0(cYear, "-03-22"))
  easterEnd <- as.Date(paste0(cYear, "-04-25"))
  
  #Halloween
  halloweenStart <- as.Date(paste0(cYear, "-10-17"))
  halloweenEnd <- as.Date(paste0(cYear, "-11-14"))
  
  #Spring
  
  #Sommer
  
  #Autumn

  #Winter
  
  file <- "Images/Logo/"
  
  if(cDate >= easterStart && cDate <= easterEnd){
    file <- paste0(file,"logo_easter.png")
  }
  else if(cDate >= halloweenStart && cDate <= halloweenEnd){
    file <- paste0(file,"logo_halloween.png")
  }
  else{
    file <- paste0(file,"logo_farbe.png")
  }
  
  #Overwrite
  file <- "Images/Logo/logo_flying_birds_font.png"
  
  return(file)
}

