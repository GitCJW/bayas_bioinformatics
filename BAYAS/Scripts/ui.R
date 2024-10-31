library(rstan)
library(rstanarm)
library(brms)
library(loo)

library(R6)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyTree)
library(shinycssloaders)
library(shinyalert)
library(shinyFeedback) 
library(shinybusy)
library(bslib)

library(DT)
library(futile.logger)
library(future)
library(promises)
library(stringr)
library(dplyr)
library(tidyr)
library(bayesplot)
library(ggplot2)

library(LaplacesDemon) 
library(png)
library(ggExtra)
library(data.table)
library(gtools)
library(RColorBrewer)

library(bayestestR)
library(spatstat)
library(statmod)
library(sortable)
library(knitr)
library(pdftools) 
library(filesstrings) 
library(mailR) 
library(pracma)

library(excelR)
library(readxl)
library(sodium)
library(ggpubr)
library(withr)

library(shinybayas)
library(ssdbayas)


BAYAS_COLORS <<- NULL


# Source files
s <- list.files(getwd(), pattern = ".R$", recursive = T)
s <- s[!s %in% c("ui.R","server.R")]
s <- paste0(getwd(),"/", s)
flag <- T
while(flag){
  print(flag)
  flag <- F
  for(si in s){
    t <- tryCatch({
        source(si)
        T
      },
      error=function(cond){
        print(cond)
        F
      })
    if(!t) flag <- T
  }
}

localUse <<- F
askForLeaving <- !localUse

# set.seed(1)
set.seed(drawRandomSeed())


# Enable multiprocessing
ifelse(localUse,plan(sequential),plan(multisession, workers=2))


# Load profiler
useProfVis <<- F
if(localUse && useProfVis) library(profvis)

# Enable generic error messages
if(!localUse){ 
  options(shiny.sanitize.errors = TRUE)
}else{
  options(shiny.sanitize.errors = F)
  options(shiny.fullstacktrace = TRUE)
} 

#Logger
flog.threshold(INFO) #DEBUG ,INFO
flog.layout(layout.format('[~l] ~m'))

# Change maximum upload size to 1024mb
options(shiny.maxRequestSize=1024*1024^2)

# Set up the number of workers within cluster
numberOfIterationsPerStep <<- 20
if(localUse) numberOfIterationsPerStep <<- 20

#Log file for planning ssd workers
planningClusterPath <<- paste0(dirname(getwd()), "/out.txt")
if(file.exists(planningClusterPath)) file.remove(planningClusterPath)


#Add a resource folder
image_folder <<- paste0(dirname(getwd()),"/Images")
icon_folder <<- paste0(dirname(getwd()),"/Icons")
video_folder <<- paste0(dirname(getwd()),"/Video")
css_folder <<- paste0(dirname(getwd()),"/CSS")
js_folder <<- paste0(dirname(getwd()),"/Javascript")
report_folder <<- paste0(dirname(getwd()),"/Report")
planning_model_folder <<- paste0(dirname(getwd()),"/Planning_models")
stanModels_folder <<- paste0(dirname(getwd()),"/StanModels")
pw_folder <<- paste0(dirname(getwd()),"/PW")
data_folder <<- paste0(dirname(getwd()),"/Data")
data_user_folder <<- paste0(dirname(getwd()),"/Data_user")

shiny::addResourcePath("Images",image_folder)
shiny::addResourcePath("Icons",icon_folder)
shiny::addResourcePath("Video",video_folder)
shiny::addResourcePath("CSS",css_folder)
shiny::addResourcePath("JS",js_folder)
shiny::addResourcePath("Report",report_folder)
shiny::addResourcePath("Planning_models",planning_model_folder)
shiny::addResourcePath("StanModels",stanModels_folder)
shiny::addResourcePath("PW",pw_folder)
shiny::addResourcePath("Data",data_folder)
shiny::addResourcePath("Data_user",data_user_folder)

#Remove old data files
sapply(list.files(path = data_user_folder, full.names = TRUE), file.remove)


#Cleanup at start
if(localUse){
  if(file.exists(paste0(report_folder,"/Images"))){
    for(file in list.files(paste0(report_folder,"/Images"))){
      file.remove(paste0(report_folder,"/Images/",file))
    }
  }
  if(file.exists(paste0(report_folder,"/Thumbnails"))){
    for(file in list.files(paste0(report_folder,"/Thumbnails"))){
      file.remove(paste0(report_folder,"/Thumbnails/",file))
    }
  }
  if(file.exists(paste0(report_folder,"/Tex"))){
    for(file in list.files(paste0(report_folder,"/Tex"))){
      file.remove(paste0(report_folder,"/Tex/",file))
    }
  }
  if(file.exists(paste0(report_folder,"/PDF"))){
    for(file in list.files(paste0(report_folder,"/PDF"))){
      if(file != "Empty.pdf")
        file.remove(paste0(report_folder,"/PDF/",file))
    }
  }
}

#Create subfolders
if(!dir.exists(paste0(report_folder,"/Images"))) dir.create(paste0(report_folder,"/Images"))
if(!dir.exists(paste0(report_folder,"/Thumbnails"))) dir.create(paste0(report_folder,"/Thumbnails"))
if(!dir.exists(paste0(report_folder,"/Tex"))) dir.create(paste0(report_folder,"/Tex"))
if(!dir.exists(paste0(report_folder,"/PDF"))) dir.create(paste0(report_folder,"/PDF"))



GLOBAL_THEME <<- "zephyr"
# GLOBAL_THEME <<- "cerulean"
BAYAS_COLORS <<- bayasColors(GLOBAL_THEME)

#set bayesplot theme
setBayasBayesplotColors(GLOBAL_THEME)

theme_set(theme_bw())


ui <- bslib::page(
  theme = bslib::bs_theme(version = 5, bootswatch=GLOBAL_THEME),
  # theme = bslib::bs_theme(version = 3, bootswatch=theme),
  style = "height:auto; padding:0px; gap:0px;",
  
  tagList(

    if(localUse && useProfVis)  profvis_ui("profiler"),

    includeScript(paste0(js_folder,"/TwoDivResizable.js")),
    includeScript(paste0(js_folder,"/CustomMessageHandler.js")),
    includeScript(paste0(js_folder,"/Hover3Module.js")),
    includeScript(paste0(js_folder,"/KeyInputs.js")),
    includeCSS(paste0(icon_folder,"/css/icons.css")),
    lapply(list.files(css_folder, pattern = "\\.css$", full.names = TRUE, recursive = F), includeCSS),
    lapply(list.files(paste0(css_folder,"/",GLOBAL_THEME), pattern = "\\.css$", full.names = TRUE, recursive = F), includeCSS),
    
    # use_cicerone(),

    tags$head(
      tags$style(
        HTML(tabsetEqualHeader("tabsetPanelEffectsSummary",2)),
        HTML(tabsetEqualHeader("tabsetPanelPredictionSummary",2)),
        HTML(tabsetEqualHeader("reportTool-reportSelectedItemView",3)),
        HTML(tabsetEqualHeader("modalEffectExplanation",2)),
        HTML(tabsetEqualHeader("modalEffectExplanationVideo",2)),
        HTML(tabsetEqualHeader("creatingModel-tabsetShowStatModelAndSSDDiv",2)),
        HTML(tabsetEqualHeader("creatingModel-tabsetDataVisualizationDiv",2))
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "Icons/css/icons.css") #load bayas icons
    ),


    tags$div(
      titel = "BAYAS",
      class = "getBackgroundColor",
      style = "padding: 0px; margin: 2px;",
      style="height:100%; min-height:100vh;",


      # Confirm leaving the site
      if(askForLeaving)
        tags$head(tags$script('window.onbeforeunload = function() {return "Your changes will be lost!";};')),

      #Init the "home" view, choosing one of the modules
      tags$div(
        id = "home_main_div",
        get_ui_home()
      ),


      shinyjs::useShinyjs(),
      useShinyFeedback(),
      
      shinybusy::add_busy_spinner(timeout=500),
      
      # shiny::busyIndicatorOptions(),
      # shiny::useBusyIndicators(),
      
      
      hidden(tags$div(id = "planning_main_div", ui_planning())),
      hidden(tags$div(id = "baysis_main_div", ui_baysis())),
      hidden(tags$div(id = "report_main_div", ui_report())),

      #Add a new report progress panel (top)
      getReportProgress()
    ),


    #Footer
    tags$div(
      style = "margin-top: auto; text-align:center;",
       if(localUse) actionButton("testtest","browser", style="display:block;"),
       actionLink(inputId = "dsgvo_button", label = "Datenschutz",
                  class = "fontColor-regular-bg",
                  style = "text-decoration: underline; margin-left:3px;margin-right:3px;"),
       actionLink(inputId = "impressum_button", label = "Impressum",
                  class = "fontColor-regular-bg",
                  style = "text-decoration: underline; margin-left:3px;margin-right:3px;"),
       tags$div("The development of BAYAS was funded by grant 60-0102-01.P584 of the German Federal Institute for Risk Assessment (BfR).")
    )
  )
)
