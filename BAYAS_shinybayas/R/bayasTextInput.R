#' @title
#' Create a text input control (similar to shinys textInput)
#'
#'@description
#' Create an input control for entry of unstructured text values, that is triggered
#' when the focus is lost or the 'Enter' key is pressed.
#'
#'@usage
#'\code{bayasTextInput(
#'   inputId="textInputName",
#'   label="Name",
#'   value="personal name",
#'   width="200px",
#'   placeholder="name"
#')}
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label
#' @param value Initial value.
#' @param width The width of the input, e.g. '400px', or '100%'
#' @param placeholder A character string giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.
#'
#' @export
#'
bayasTextInput <- function(inputId, label, value = "",
                              width = NULL, placeholder = NULL) {
  head <- getDependencies()
  value <- restoreInput(id = inputId, default = value)

  ret <- div(class = "form-group shiny-input-container",
             style = css(width = validateCssUnit(width)),
             shinyInputLabel(inputId, label),
             tags$input(id = inputId,
                        type = "text", class = "shiny-input-text bayasTextInput form-control",
                        value = value, placeholder = placeholder))

  htmltools::tagList(
    singleton(htmltools::tags$head(head)),
    ret
  )
}

#' @title
#' Change the value of a text input on the client
#'
#'@description
#' Change the value of a text input on the client
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label
#' @param value Initial value.
#' @param width The width of the input, e.g. '400px', or '100%'
#' @param placeholder A character string giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.
#'
#' @export
#'
#'@usage
#' \code{
#' updateTextInput(
#' session = getDefaultReactiveDomain(),
#' inputId,
#' label = NULL,
#' value = NULL,
#' placeholder = NULL
#' )}
updateBayasTextInput <- function(session = getDefaultReactiveDomain(), inputId,
                                 label=NULL, value=NULL, placeholder=NULL, trigger=T){
  session$sendCustomMessage(type="updateBayasTextInput",
                            message=list(element=inputId, placeholder=placeholder,
                                         value=value, label=label, trigger=trigger))
}


# CTRL+ALT+SHIFT+R: Create @param etc
# CTRL+SHIFT+D call devtools::document()
# usethis::use_version(which="patch")
# devtools::build()
# devtools::install()
