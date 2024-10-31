ownSidebarPanel = function(sidebar, mainPanel, buttonInputName, sidebar_name){
  fluidRow(style="padding:0px;margin:0px;",
           tags$div( id =paste(sidebar_name,"_content"),class="ownOpenPanel", style="float:left;margin-right:10px;",
                     tags$div(style="padding:0px;margin:0px;float:right; width:20px;",
                              actionButton(buttonInputName, label="", icon = icon("caret-left"), style = "padding:0px; margin:0px; width:20px;height:20px;")
                     ),
                     tags$div(id=sidebar_name, style="padding:0px;", #overflow:hidden;
                                sidebar
                              )
                     
           ),
           tags$div(style="overflow:hidden;",
                      mainPanel
           )
  )
}
ownSidebarServer = function(session, input, inputButton, sidebar_name){
  observeEvent(input[[inputButton]], {
    shinyjs::toggle(id = sidebar_name)
    shinyjs::toggleClass(id=paste(sidebar_name,"_content"),"full_hide")
    shinyjs::toggleClass(id=paste(sidebar_name,"_content"),"ownOpenPanel")
    if(input[[inputButton]] %% 2 == 1){
      updateActionButton(session, inputButton, icon = icon("caret-right"))
    }else{
      updateActionButton(session, inputButton, icon = icon("caret-left"))
    }
  })
}

uiChecklist <- function(type=c("check","warning","error"), idCheck, description, 
                        message, qMarkTitle, qMarkContent, first){
  iconName <- ""
  iconStyle <- ""
  iconColor <- ""
  divColor <- ""
  if(type=="check"){
    iconName <- "check"
    iconStyle <- "font-size:20px;"
    iconColor <- "uiChecklist-font-check"
    divColor <- "uiChecklist-div-check"
  }else if(type=="warning"){
    iconName <- "exclamation"
    iconStyle <- "font-size:20px;"
    iconColor <- "uiChecklist-font-warning"
    divColor <- "uiChecklist-div-warning"
  }else if(type=="error"){
    iconName <- "times"
    iconStyle <- "font-size:24px;"
    iconColor <- "uiChecklist-font-error"
    divColor <- "uiChecklist-div-error"
  }else{
    stop("Wrong type!")
  }
  showIcon <- tags$div(id=idCheck, icon(iconName), style=iconStyle)
  if(first) showIcon <- hidden(showIcon)
    

  return(
    tagList(
      tags$div(
        class = paste("uiChecklist-row", divColor, iconColor),
        stlye="display:flex;",
        
        tags$div(
          style="flex:1; display: flex; align-items: center; justify-content: center; ", showIcon),
        tags$div(
          style="flex:3; display: flex; align-items: center;", description),
        tags$div(
          style="flex:7; display: flex; align-items: center;", message),
        tags$div(
          style="flex:1; display: flex; align-items: center;", 
               
          bslib::popover(
            trigger = actionLink(paste0(idCheck,"Hint"), 
                                label="", 
                                icon("question-circle")),
            title = qMarkTitle,
            HTML(qMarkContent),
            placement = "right",
            options = list(trigger="focus")
          )
        )
        
      )
    )
  )
}


imageButtonTitle <- function(btnId, imageFile, title, selected=F,
                             btnStyle="width:230px; height:200px;",
                             imgHeight="150px", imgClass="", imgStyle="", imgWidth = NULL,
                             primary=F){
  classPrim <- ifelse(primary, "planningExampleBtnPrimary","planningExampleBtn")
  if(selected) classPrim <- paste0(classPrim, " planningSelectedExampleBtn ")
  
  btn <- tags$button(
    id=btnId, 
    class=paste0("btn action-button  ", classPrim), 
    style = "box-shadow: none;",
    style=btnStyle,
    tags$img(src=imageFile, height=imgHeight, width=imgWidth, class=imgClass, style=imgStyle),
    tags$div(
      tags$label(class="control-label", title, style="padding-top:10px;")
    )
  )

  return(btn)
}

setPrimaryImageButtonTitle <- function(btnId, primary){
 if(primary){
   addCssClass(btnId, "planningExampleBtnPrimary")
   removeCssClass(btnId, "planningExampleBtn")
 }else{
   removeCssClass(btnId, "planningExampleBtnPrimary")
   addCssClass(btnId, "planningExampleBtn")
 }
}

shinyInputActionButton <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

textInputHeight <- function(id, value, height="15px",margin=c(0,0,0,0)){
  tags$div(#class="form-group shiny-input-container",
           tags$label(class="control-label shiny-label-null"),
           tags$input(id=id, type="text", class="form-control", value=value,
                      style=paste0("height:",height,";","margin: ",paste0(" ",margin,"px", collapse=""),";"),)
           )
}

textAreaInputWithoutContainer <- function(inputId,label,value = "",cols = NULL,rows = NULL, placeholder = NULL, style="", styleDiv=""){
  ret <- tags$div(style=styleDiv,
                  if(!is.null(label)) tags$label(clasS="control-label shiny-label", style=paste0("for=",inputId), label),
                  tags$textarea(id=inputId, class="form-control",style=style, value)
  )
  return(ret)
}

#backButtonsId: inputIds 
#nextButtonsId: inputIds
#backButtons: Names 
#nextButtons: Names
footerPanel <- function(backButtonsId = NULL, nextButtonsId = NULL,
                        backButtons = NULL, nextButtons = NULL){
  backs <- NULL
  nexts <- NULL

  if(!is.null(backButtonsId)){
    if(is.null(backButtons) || length(backButtonsId) != length(backButtons)) stop("Unequal length")
    backs <- lapply(seq_along(backButtons), function(i){
      tags$div(
        style = "",
        actionButton(
          inputId = backButtonsId[i],
          style = "min-width:200px;", 
          label = tags$span(
            style = "display:flex; justify-content:center; align-items:center;",
            # style = "padding: 3px 0px 5px 0px;",
  
            icon(
              style = "flex:0 0 0%; text-align: left;", 
              "chevron-left"),
            tags$label(
              style = "font-weight:normal; cursor: pointer; margin-bottom: 0px;",
              style = "flex:1 0 0%;",
              HTML(backButtons[i])
            )
            
          )
        )
      )
    })
  }

  if(!is.null(nextButtonsId)){
    if(is.null(nextButtons) || length(nextButtonsId) != length(nextButtons)) stop("Unequal length")
    nexts <- lapply(seq_along(nextButtons), function(i){
      tags$div(
        style = "",
        actionButton(
          inputId = nextButtonsId[i],
          class = "btn-primary",
          style = "min-width:200px; font-size:larger;",
          label = tags$span(
            style = "display:flex; justify-content:center; align-items:center;",
            # style = "padding: 3px 0px 5px 0px;",
            
            tags$label(
              style = "font-weight:normal; cursor: pointer; margin-bottom: 0px;",
              style = "flex:1 0 0%;",
              HTML(nextButtons[i])
            ),
            icon(
              style = "flex:0 0 0%; text-align: right;", 
              "chevron-right")))
      )
    })
  }
  
  backDiv <- tags$div(
    style = "flex:1; display:flex; flex-direction:column; align-items: start; gap: 10px;",
    backs
  )
  nextDiv <- tags$div(
    style = "flex:1; display:flex; flex-direction:column; align-items: end; gap: 10px;",
    nexts
  )

  tags$div(
    class = "borderColor-regular",
    style = "border-top:2px solid; padding:20px; display: flex;",
    backDiv,
    nextDiv
  )
}

# checkboxInput2 <- function (inputId, label, value = FALSE, style = "", labelStyle=""){
#   value <- restoreInput(id = inputId, default = value)
#   inputTag <- tags$input(id = inputId, type = "checkbox")
#   if (!is.null(value) && value) 
#     inputTag$attribs$checked <- "checked"
#   div(class = "form-group shiny-input-container", style = style, 
#       div(class = "checkbox", tags$label(inputTag, label, style=labelStyle)))
# }



#Model explanation list with its hover and click events
#listing and description are list, indented a column with length of listing and description
#shinys output
modelExplanationUI <- function(id, checklist, importance, output){
  
  wellStyle <- "border:none; border-top:1px solid; box-shadow:none;"
  
  ret <- "<ul style=list-style:none;>"
  count <- 1
  neg <- 0
  if(length(checklist)==0){
    return(tags$div())
  }
  for(i in 1:length(checklist)){

    if(importance){
      class <- "modelExplanationHoverCheckImportant"
    }else{
      class <- "modelExplanationHoverCheck"
    }

    ret <- paste0(ret, tags$li(id=paste0(id,i),clasS=class,value=i-neg, style="margin-bottom:5px;", HTML(checklist[[i]]$short)))
    count <- 1
    
  }
  ret <- paste0(HTML(ret), "</ul>")
  info <- paste0(h5("Description"))
  sapply(length(checklist):1, function(i){
    shinyjs::onclick(paste0(id,i), {output$previewDataSelectModelRight <- renderUI(
      wellPanel(
        class="getActiveColor borderColor-regular", 
        style="text-align:justify;",
        style=wellStyle, 
        HTML(info,checklist[[i]]$long)))})
  })
  if(importance){
    ret <- tags$div(h5(HTML("Things you <b>must</b> do:"), style="margin-left:14px;"), HTML(ret))
  }else{
    ret <- tags$div(h5(HTML("Things you <b>can</b> do:"), style="margin-left:14px;"), HTML(ret))
  }
  ret <- wellPanel(ret, 
                   class="getActiveColor borderColor-regular", 
                   style="padding:10px 5px 5px 5px; margin-bottom:5px;", 
                   style=wellStyle)
  
  return(ret)
}

#Model explanation list with its hover and click events
#listing and description are lists and shinys output
modelLegendUI <- function(id, leg, output){
  wellStyle <- "border:none; border-top:1px solid; box-shadow:none;"
  
  listItems <- tagList()
  
  for(i in 1:length(leg)){
    listItems[[i]] <- tags$li(id=paste0(id,i), class="modelExplanationHover", 
                              fluidRow(column(4,leg[[i]]$name, style="padding-right:0px; overflow-x:clip;"),
                                       column(8,leg[[i]]$short)))
  }

  ret <- tags$ul(style="list-style:none;",listItems)
  info <- paste0(h5("Description"))
  sapply(1:length(leg), function(i){
    shinyjs::onclick(paste0(id,i), {output$previewDataSelectModelRight <- renderUI(
      wellPanel(
        class="getActiveColor borderColor-regular",
        style="text-align:justify;",
        style=wellStyle, 
        HTML(info,leg[[i]]$long)))})
  })
  
  ret <- tags$div(h5("Legend", style="margin-left:14px;"), HTML(as.character(ret)))
  ret <- wellPanel(ret, 
                   class="getActiveColor borderColor-regular", 
                   style="padding:10px 5px 5px 5px;", 
                   style=wellStyle)
  return(ret)
}

#Text area input with style argument
ownTextAreaInput <- function (inputId, label, value = "", 
          cols = NULL, rows = NULL, placeholder = NULL, resize = NULL, style = NULL, class = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", 
                                  "horizontal"))
    style <- paste0(style, " resize: ", resize, ";" )
  }
  div(class = "form-group shiny-input-container", shinyInputLabel(inputId, label), 
      tags$textarea(id = inputId, class = "form-control", 
                    placeholder = placeholder, class = class, style = style, rows = rows, 
                    cols = cols, value))
}

#Acutally the same as shiny::inputFile() but with a primary 'Browse' button
ownFileInput <- function(inputId, multiple = FALSE, accept = NULL,
                          buttonLabel = "Browse...", placeholder = "No file selected",
                         progressBar = T){
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(
    id = inputId, 
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;", 
    name = "ttts", type = "file", 
    `data-restore` = restoredValue
  )
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  progress <- ""
  if(progressBar){
    progress <-  tags$div(
      id = paste(inputId, "_progress", sep = ""), 
      class = "progress active shiny-file-input-progress", 
      style = ifelse(progressBar, "","display:none;"),
      tags$div(class = "progress-bar")
    )
  }
  div(
    class = "form-group shiny-input-container",
    style = "width:100%",

    div(
      class = "input-group", 
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(class = "btn btn-default btn-file btn-primary", 
             buttonLabel, inputTag)), 
      tags$input(
        id = paste0(inputId, "-text"),
        type = "text", 
        class = "form-control", 
        placeholder = placeholder, readonly = "readonly"
      )
    ), 
    progress
  )
}

dragFileInput  <- function(inputId,  multiple = FALSE, accept = NULL,
                           buttonLabel = "Browse...", placeholder = "No file selected"){
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(
    id = inputId,
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    name = "ttts", type = "file",
    `data-restore` = restoredValue
  )
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(
    class = "form-group shiny-input-container",
    style = "flex:1; margin-bottom:0px;",
    div(
      class = "input-group", 
      style = "width:100%; height:100%; display:flex; justify-content:center; align-items:center;",
      
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(class = "btn btn-default btn-file btn-primary", 
             style = "display:none; width:0px;",
             buttonLabel, inputTag)), 
      
      tags$div(
        type = "text", 
        class = "form-control", 
        style = "border:dashed; border-radius:5px; width:50%; max-width:50%; height:50%;",
        style = "display:flex; justify-content:center; align-items:center; flex-direction: column;",
        tags$div(
          style = "font-size: x-large; margin-bottom: 10px;",
          "Drop file here"
        ),
        tags$div(
          icon(name="file-download", style="font-size:30px;"), #icon("arrow-alt-circle-down")
        ),
        readonly = "readonly"
      )
    )
  )
}


shinyInputLabel <- function(inputId, label = NULL, width=NULL) {
  if(!is.null(width)){
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      # `id` attribute is required for `aria-labelledby` used by screen readers:
      id = paste0(inputId, "-label"),
      `for` = inputId,
      style=paste0("width:",width,";")
    )
  }else{
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      # `id` attribute is required for `aria-labelledby` used by screen readers:
      id = paste0(inputId, "-label"),
      `for` = inputId
    )
  }

}


# Returns a label with a question mark that contains a tooltip (hover)
# id: ID of icon 
# label: label name
# ttHeader: tooltip header
# ttConten: tooltip content
labelWithInfo <- function(id, label, ttHeader, ttContent, placement="right", 
                          type=c("info", "warning", "error"), ...){
  
  type <- match.arg(type)
  
  cssClass <- "fontColor-primary"
  cssClassLabel <- "fontColor-regular-bg"
  if(type=="warning") cssClass <- cssClassLabel <- "fontColor-warning"
  if(type=="error") cssClass <- cssClassLabel <- "fontColor-error"
  
  ttId <- paste0(id,"-tt")
  parId <- paste0(id,"-par")
  ret <- tags$label(
    class=cssClassLabel,
    style="width:100%; display:flex;",
    id=parId,
    label,
    
    bslib::popover(
      trigger = icon(id=ttId,name="question-circle", 
                     class=cssClass, 
                     style="margin-left:auto; margin-bottom: auto; margin-top:auto;"),
      title = ttHeader,
      HTML(ttContent),
      placement = placement,
      options = list(trigger="hover")
    ),
    ...
  )
  return(ret)
}

# Returns a legend with a question mark that contains a tooltip (hover)
# id: ID of icon 
# label: label name
# ttHeader: tooltip header
# ttConten: tooltip content
legendWithInfo <- function(id, label, ttHeader, ttContent, placement="right"){
  
  ttId <- paste0(id,"-tt")
  ret <- tags$legend(
    style="font-size:14px; font-weight:bold; margin-bottom:10px;",
    label,
    bslib::popover(
      trigger = icon(
        id=ttId,
        name="question-circle", 
        class="fontColor-primary",
        style="float:right;"),
      title = ttHeader,
      HTML(ttContent),
      placement = placement,
      options = list(trigger="hover")
    )
  )
  return(ret)
}


# Returns a div with a question mark that contains a tooltip (hover)
# id: ID of icon 
# label: label name
# ttHeader: tooltip header
# ttConten: tooltip content
divWithInfo <- function(id, label, ttHeader, ttContent, placement="right"){
  ttId <- paste0(id,"-tt")
  ret <- tags$div(
    style="font-size:14px; font-weight:bold; margin-bottom:5px;",
    
    label,
    bslib::popover(
      trigger = icon(
        id=ttId,
        name="question-circle", 
        class="fontColor-primary",
        style="float:right;"),
      title = ttHeader,
      HTML(ttContent),
      placement = placement,
      options = list(trigger="hover")
    )
  )
  return(ret)
}



diceButton = function(inputId, tt=NULL, linkStyle="font-size:18px; padding:0px;"){
  tags$div(
    title=tt,
    
    tags$button(
      id=inputId,
      icon("dice-d6"),
      class="btn randomDizeButton action-button",
      style=linkStyle)

  )
}


################################################################################
############################## bayasSelectButton ###############################
################################################################################

bayasSelectButton <- function(inputId, selected=c("value","variable")) {
  
  match.arg(selected)
  
  valueOn <- paste0("document.getElementById('",
                    paste0(inputId,"-value-icon"),
                    "').style.display = \"inline-block\";")
  valueOff <- paste0("document.getElementById('",
                     paste0(inputId,"-value-icon"),
                     "').style.display = \"none\";")
  varOn <- paste0("document.getElementById('",
                    paste0(inputId,"-variable-icon"),
                    "').style.display = \"inline-block\";")
  varOff <- paste0("document.getElementById('",
                    paste0(inputId,"-variable-icon"),
                    "').style.display = \"none\";")
  
  hideDropdownMenu <- paste0("document.getElementById('",
                             paste0(inputId,"_state"),
                             "').classList.remove(\"open\")")
  
  ret <- shinyWidgets::dropdownButton(
    inputId=inputId,
    size="xs",
    icon=icon("dot-circle", class="bayasSelectButton-icon"),
     tags$div(
       style=paste0(
         "margin:-5px -10px;"
       ),
       actionButton(inputId=paste0(inputId,"-value"), 
                    label=tags$div(
                      tags$div(
                        style="display:inline-block; width:20px;",
                        icon(
                          id=paste0(inputId,"-value-icon"), 
                          style=paste0("margin-right:5px; display:",
                                       ifelse(selected=="value","inline-block","none;")),
                          name="check")),
                      "Value"), 
                    class="bayasSelectButton",
                    onclick=paste0(valueOn,varOff,hideDropdownMenu)),
       
       actionButton(inputId=paste0(inputId,"-variable"), 
                    label=tags$div(
                      tags$div(
                        style="display:inline-block; width:20px;",
                        icon(
                          id=paste0(inputId,"-variable-icon"), 
                          style=paste0("margin-right:5px; display:",
                                       ifelse(selected=="variable","inline-block","none;")),
                          name="check",
                          display=ifelse(selected=="variable","inline-block","none;"))),
                      "Variable"), 
                    class="bayasSelectButton",
                    onclick=paste0(valueOff,varOn,hideDropdownMenu))
     )
    )
  ret
}





################################################################################
############################## Save/Load Session ###############################
################################################################################
loadSessionModal <- function(id){
  modalDialog(
    tags$div(
      style = "",
      fileInput(id, label=NULL, accept =".bayas"),
      tags$div(
        style="",
        tags$div(
          class = "bayas_shinybusy_spinner",
          tags$div(
            style = "width: max-content; display: inline-block;",
            shinybusy::use_busy_spinner(
              spin = "folding-cube", spin_id="loadingSpinner", 
              color="var(--bs-btn-bg)"), # #0275D8
            hidden(tags$div(
              id = paste0(id,"TextOutputDiv"),
              class="fontColor-error",
              style = "font-weight:bold;",
              textOutput(paste0(id,"TextOutput"))
            ))
          )
        )
      )
    ),
    title = "Upload session",
    footer = modalButton("Cancel"),
    easyClose = F,
    size = "s"
  )
}




# copied from shiny since it's not exported
`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}
