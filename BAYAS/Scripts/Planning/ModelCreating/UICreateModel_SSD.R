################################################################################
########################### Sample Size Determination ##########################
################################################################################

planning_SSDModal <- function(stepDiv, helpDiv){
  tags$div(
    tags$div(
      imgClass = "borderColor-regular",
      style="border-bottom: 1px solid;",
      stepDiv
    ),
    
    tags$div(
      style="margin-top:20px;", 
      helpDiv
    )
  )
}

#primary is a vector of strings from ModelCreatingDataParameter 
#type, range or dist
#dist: ModelCreatingDataParameterDistributionAbstract
planning_creatingStepsSSD <- function(ns, goal){
  type = goal$getType()
  hdi = goal$getHDI()
  
  precWidth = goal$getPrecWidth()
  
  ropeExclude = str_to_title(goal$getRopeExcludeInclude())
  ropeLower =  goal$getRopeLower()
  ropeUpper = goal$getRopeUpper()
  
  name <- goal$getName()
  
  #Name
  if(!goal$getNameSetted()){
    name <- ""
  }

  #Rope / precision
  ropeDiv <- tags$div(
    id=ns("ropeDiv"),
    tags$div(
      imgClass = "borderColor-regular",
      class="borderColor-dark",
      style="border-top: 1px solid; border-radius:4px; padding:10px;",
      bayasGroupedButtons(ns("ssdRadioExlude"), btnNames=c("Exclude","Include"),
                          btnValues=c("exclude","include"), 
                          selected=tolower(ropeExclude),
                          columns=2,
                          btnStyle="height:25px; padding: 0px; width:100%;"),
      tags$div(
        style="display:flex; margin-top:10px;",
        tags$div(
          style="flex:1; min-width:50px;",
          bayasNumericInput(ns("ssdRopeLower"), label="Lower", value=ropeLower,
                            numericStyle="height:25px;",
                            invalidMessage=T, invalidTooltip = T)
        ),
        tags$div(
          style="flex:1; min-width:50px;",
          bayasNumericInput(ns("ssdRopeUpper"), label="Upper", value=ropeUpper,
                            numericStyle="height:25px;",
                            invalidMessage=T, invalidTooltip = T)
        )
      )
    )
  )
  
  precisionDiv <- tags$div(
    id=ns("precisionDiv"),
    tags$div(
      imgClass = "borderColor-regular",
      class="borderColor-dark",
      style="border-top: 1px solid; border-radius:4px; padding:10px;",
      bayasNumericInput(ns("ssdPrecWidth"), label="Width", value=precWidth,
                        min=.Machine$double.xmin, 
                        invalidMessage=T, invalidTooltip = T,
                        numericStyle="height:25px;")
    )
  )
  
  typeLabel <- "ROPE"
  if(type=="rope"){
    precisionDiv <- hidden(precisionDiv)
  }else{
    typeLabel <- "Precision"
    ropeDiv <- hidden(ropeDiv)
  }
  ropePrecision <- tags$div(
    style="margin-top:15px;",
    ropeDiv,
    precisionDiv
  )
  

  
  l <- tags$div(
    style="",
    
    fluidRow(
      # style="display:flex;",
      
      column(
        4,
        
        labelWithInfo(id=ns("ssdModalTreeAInfo"), label="First group",
                      ttHeader="Parameter group", ttContent="Select all parameters for the first comparison group."),
        wellPanel(
          class="getActiveColor",
          style="min-height:217px; max-height: 217px; overflow:auto; padding:5px;",
          shinyTree(
            ns("ssdModalTreeA"),
            checkbox=T, themeIcons=F, multiple=T, wholerow=T
          )
        )
      ),
      column(
        4,
        labelWithInfo(id=ns("ssdModalTreeBInfo"), label="Second group",
                      ttHeader="Parameter group", ttContent="Select all parameters or none for the second comparison group."),
        wellPanel(
          class="getActiveColor",
          style="min-height:217px; max-height: 217px; overflow:auto; padding:5px;",
          shinyTree(
            ns("ssdModalTreeB"),
            checkbox=T, themeIcons=F, multiple=T, wholerow=T
          )
        )
      ),
      column(
        4,
        tags$div(
          textInput(ns("ssdModalGoalName"), label=NULL, value = name, 
                    placeholder="unnamed", width="100%"),
          tags$div(
            style="display:flex;",
            tags$div(
              style="flex:1;",
              tags$label(style="margin-bottom:.5rem;","Goal type"),
              bayasGroupedButtons(
                ns("ssdSwitchRopePrecision"), 
                btnNames=c("ROPE","Precision"), 
                btnValues=c("rope","precision"),
                selected=tolower(typeLabel), columns=1,
                btnStyle="height:25px; padding: 0px; width:100%;")
              ),


              bayasNumericInput(
                ns("ssdHDI"), label="Probability mass", value=hdi, 
                min=.Machine$double.xmin, max=1, step=0.01, 
                invalidMessage=T, invalidTooltip = T,
                numericStyle = "height:25px;",
                style="flex:1; min-width: 50px; margin-bottom:15px;")
           
            ),
            ropePrecision               
          )
        )
    ),
    
    tags$div(
      style="",
      plotOutput(ns("ssdModalPlot"), height="250px")
    )
  )
  
  return(l)
}


###################################
########### Help pages ############ 
###################################

planning_creatingStepsSSD_help <- function(ns){
  
  l <- tags$div(
    style="",
    
    wellPanel(
      class="infoBox",
      style="padding:10px;",
      tags$div(
        style="margin-bottom:10px;",
        HTML(paste0("<i>A single <b>'Goal'</b> of the sample size determination</i>"))
      ),
      tags$div(
        style="",
        HTML(
          paste0(
            "A <b>'Goal'</b> describes a condition that the statistical model should satisfy. ",
            "In general, this condition involves an effect between (sums of) parameters of the model. ",
            "This effect is defined by the sum of the <b>'First group'</b> of parameters minus the sum of the <b>'Second group'</b>. <br>",
            "Remember that parameters, and thus effects, are not single values, but distributions. ",
            "Considering e.g. 95% of the <b>'probability mass'</b> of these distributions can lead to a more robust approach. "
          )
        )
      )
    ),
    fluidRow(
      column(6,
             wellPanel(
               class="infoBox",
               style="padding:10px;",
               tags$div(
                 style="margin-bottom:10px;",
                 HTML(paste0("<i>Goal type: <b>ROPE</b></i>"))
               ),
               tags$div(
                 style="",
                 HTML(
                   paste0(
                     "<b>ROPE</b> (Region Of Practical Equivalence) refers to a range of values that are interpreted as equal. ",
                     "For example, if you want to show a unit effect of 3, you can declare a ROPE of 2.8-3.2. ",
                     "Any value within this range will be treated as 'equal'. <br>",
                     "To <b>'Exclude'</b> means that none of the considered probability mass of the effect distribution is allowed to be in the defined ROPE. ",
                     "Note: if the probability density mass is truncated by the ROPE, only the remaining majority on one side is taken into account. <br>",
                     "To <b>'Include'</b> means that all of the considered probability mass of the effect distribution have to be in the defined ROPE. </b> ",
                     "<b>'Lower'</b> and <b>'Upper'</b> define the ROPE limits."
                   ))
                 
               )
             )
             ),
      column(6,
              wellPanel(
                class="infoBox",
                style="padding:10px;",
                tags$div(
                  style="margin-bottom:10px;",
                  HTML(paste0("<i>Goal type: <b>Precision</b></i>"))
                ),
                tags$div(
                  style="",
                  HTML(
                    paste0(
                      "Precision refers to an accepted width of an effect given by its distribution. <br>",
                      "<b>Width</b> determines the maximally accepted width of a credible interval of the ",
                      "probability distribution of the considered effect."
                    ))
                  
                )
              )
              )
    )

  )

  return(l)
}




#Back button
planning_backButton <- function(ns, id="planningModalBack", step, div){
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

