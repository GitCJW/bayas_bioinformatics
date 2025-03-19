
ui_planning <- function(){
  
  page_fluid(
    id = "ui_baysis_page",
    style = "padding: 0px; margin: 2px;", 

    # Navigation bar
    bslib::page_navbar(
      id = "navbar_planning", 
      navbar_options = navbar_options(position = "static-top", 
                                      bg = BAYAS_COLORS$`--navbar-bg`),
      
      window_title="BAYAS", 
      
       title = tags$div(
         style="display:flex;",

         tags$button(
           id = "to_home_baysis_from_planning", 
           class = "icon-button action-button",
           style = "font-size: 20px; padding:0px 3px; margin:0px;",
           icon("home")
         ),
         
         tags$div(
           class = "navbarSeparator",
           style="display:flex; gap:3px;",
           style="padding-left:7px; padding-right:7px; margin-left:15px;",
           style="border-right:1px solid; border-left:1px solid;",
           
           tags$div(
             id="sc_to_planning_from_planning", 
             class = "navbar-hex-button action-button navbar-hex-button-active",
             tags$img(src = "Images/Home/P_hex.png",
                      height = "25px", width = "25px")
           ),
           tags$div(
             id="sc_to_evaluation_from_planning", 
             class = "navbar-hex-button action-button",
             tags$img(src = "Images/Home/E_hex.png", 
                      height = "25px", width = "25px")),
           tags$div(
             id="sc_to_report_from_planning", 
             class = "navbar-hex-button action-button",
             tags$img(src = "Images/Home/R_hex.png", 
                      height = "25px", width = "25px"))

          ),
         
         #Upload/Download BAYAS file
         tags$div(
           style = "display:flex; align-items: center; margin-right: 20px;",
           tags$div(
             class = "navbarSeparator",
             style = "border-right:1px solid; padding-right:15px; padding-left:15px;",

             tags$button(
               id = "loadBTNPlanning", 
               class = "icon-button action-button",
               style = "font-size: initial; padding:4px 6px; margin:0px;",
               icon("folder-open", style="font-weight: bold;")
             ),
             
             tags$a(
               id = "saveBTNPlanning",
               style = "font-size: initial; padding:4px 6px; margin:0px;",
               class = "icon-button shiny-download-link saveLoadBtn", href = "",
               target = "_blank", download = NA,
               icon("save", style="font-weight: bold;"))
             
             )
           )
        ),
       
       # planning_creatingData_page()#,
       planning_creatingModel_page()
    )

  )
  
}