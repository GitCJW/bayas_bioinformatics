#' @import htmltools
#'
#' @title
#' BAYAS Numeric Input
#'
#'@description
#' Creates a numeric input similar to shinys numericInput but with a few additional options.
#' If a value exceeds the given min and max or if the value is not numeric
#' (or not an integer) a message is shown and the numeric input is bordered red.
#' Also the return value is slightly different, see the section below 'Server return'.
#'
#'@usage
#'\code{bayasNumericInput(
#'   inputId,
#'   label,
#'   value,
#'   min = NA,
#'   max = NA,
#'   step = NA,
#'   integer = F,
#'   placeholder = 100,
#'   emptyWarning = T,
#'   invalidMessage = NULL,
#'   invalidTooltip = NULL,
#'   showMessages = !is.null(invalidMessage),
#'   numericStyle = NULL
#')}
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param step Interval to use when stepping between min and max.
#' @param integer TRUE if only integer values are allowed.
#' @param placeholder A character string or number giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.
#' @param emptyWarning If true (default) empty inputs are invalid.
#' @param invalidMessage Showed message for invalid values.
#' @param invalidTooltip Tooltip when hover over the message.
#' @param numericStyle Style arguments for the input itself
#' @param ... Further arguments passed to outer div.
#' @param showMessages If invalidMessage should be shown.
#'
#' @section Server return:
#' Using the common shiny access via 'input'.
#' Returns a vector of two values. 0/1 for an (in)valid value and
#' the value itself.
#' @export
#'
bayasNumericInput <- function(inputId, label, value, min = -Inf, max = Inf, step = NA,
                              integer=F, invalidMessage = NULL,
                              placeholder=NULL, emptyWarning=T,
                              invalidTooltip = NULL, numericStyle = NULL,
                              showMessages = !is.null(invalidMessage), ...) {
  head <- getDependencies()

  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "text", class = "bayasNum form-control", #type = "number"
                         value = formatNoSci(value),
                         placeholder=placeholder,
                         style = paste0("margin-bottom:0px; border:1px solid #cccccc;", numericStyle))


  valid <- T

  if(!is.numeric(value) || (!is.na(min) && value < min) ||
     (!is.na(max)&&value > max) || (integer && value!=round(value))) valid <-F

  if(!emptyWarning && (is.null(value) || value == "")){
    valid <- T
  }


  if(!valid) inputTag$attribs$class <- c(inputTag$attribs$class, "bayas-invalid")

  if(!is.na(min)){
    if(min==-Inf) min <- -.Machine$double.xmax
    inputTag$attribs$min = min
  }
  if(!is.na(max)){
    if(max==Inf) max <- .Machine$double.xmax
    inputTag$attribs$max = max
  }
  if(is.na(step)) step = 1
  if(integer && step != round(step)){
    step = 1
    warning("'Step' value is set to 1 because you are using integer values. In this case, 'step' must also be an integer.")
  }

  if(is.logical(invalidMessage) && invalidMessage){
    invalidMessage <- paste0("Invalid input (?)")
  }
  if(is.logical(invalidTooltip) && invalidTooltip){
    invalidTooltip <- paste0("Have to be ")
    if(integer) invalidTooltip <- paste0(invalidTooltip, "an integer ")
    invalidTooltip <- paste0(invalidTooltip, "between ", min, "-", max, ".")
  }

  inputTag$attribs$step = step
  inputTag$attribs$integer = ifelse(integer,1,0)
  inputTag$attribs$valid = ifelse(valid,1,0)
  inputTag$attribs$showMsg = showMessages
  inputTag$attribs$emptyWarning = emptyWarning

  invMess <- tags$span(style="color: #c80000; font-weight:bold;",
                       style=" font-size:12px; margin-left:5px;",
                       title=invalidTooltip, invalidMessage)
  if(valid){
    invMess$attribs$class <- "bayas-invis"
  }

  inputTag <- tags$div(
    class="bayasNumEditWrapper",
    inputTag,
    tags$div(
      class = "bayasNumEditParent",
      tags$div(
        class = "bayasNumEdit",
        tags$button(class = "bayasNumInc", type = "button", icon("caret-up"), style="color:gray;"),
        tags$button(class = "bayasNumDec", type = "button", icon("caret-down"), style="color:gray;"))
      )
  )

  ret <-  tags$div(...,
    div(id=paste0(inputId,"SurroundedDiv"), class = "form-group shiny-input-container bayasNum-parent",
        style="margin-bottom:0px;", shinyInputLabel(inputId, label), inputTag),
    tags$div(id=paste0(inputId,"InvalidMessage"), invMess))


  htmltools::tagList(
    singleton(htmltools::tags$head(head)),
    ret
  )
}

#' @title
#' Update BAYAS Numeric Inputs
#'
#'@description
#' Updates an existing bayas numeric input created with \code{bayasNumericInput()}
#'
#' @param session The session object passed to function given to shinyServer.
#' Default is getDefaultReactiveDomain().
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param step Interval to use when stepping between min and max.
#' @param integer TRUE if only integer values are allowed.
#' @param placeholder A character string or number giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.
#' @param emptyWarning If true (default) empty inputs are invalid.
#' @param invalidMessage Showed message for invalid values.
#' @param invalidTooltip Tooltip when hover over the message.
#' @param showMessages If invalidMessage should be shown.
#'
#' @export
updateBayasNumericInput <- function(session = getDefaultReactiveDomain(), inputId,
                                    label=NULL, value=NULL, min=NULL, max=NULL,
                                    step=NULL, integer=NULL,
                                    placeholder=NULL, emptyWarning=NULL,
                                    invalidMessage=NULL, invalidTooltip=NULL,
                                    showMessages = NULL){
  if(is.logical(invalidMessage) && invalidMessage){
    invalidMessage <- paste0("Invalid input (?)")
  }
  if(is.logical(invalidTooltip) && invalidTooltip){
    invalidTooltip <- paste0("Have to be ")
    if(integer) invalidTooltip <- paste0(invalidTooltip, "an integer ")
    invalidTooltip <- paste0(invalidTooltip, "between ", min, "-", max, ".")
  }

  if(is.null(showMessages) && !is.null(invalidMessage))
    showMessages <- T

  l <- list(element=inputId,
            label=label,
            value=value,
            min=min,
            max=max,
            step=step,
            integer=integer,
            placeholder=placeholder,
            emptyWarning=emptyWarning,
            invalidMessage=invalidMessage,
            invalidTooltip=invalidTooltip,
            showMessages=showMessages)
  session$sendCustomMessage(type="updateBayasNumericInput", message=l)
}



# CTRL+ALT+SHIFT+R: Create @param etc
# CTRL+SHIFT+D call devtools::document()
# usethis::use_version(which="patch")
# devtools::build()
# devtools::install()
