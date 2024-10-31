#' @import htmltools
#'
#' @title
#' Grouped Action Buttons Inputs
#'
#'@description
#' Create a group of actions buttons with an optional default selected button.
#' Works similar to grouped radio buttons.
#'
#'@usage
#'\code{bayasGroupedButtons(
#'   inputId,
#'   btnNames,
#'   btnValues,
#'   selected=NULL,
#'   columns=3,
#'   btnStyle="",
#'   outerStyle=""
#')}
#'
#' @param inputId The input slot that will be used to access the value.
#' @param btnNames A string vector for button labels. Have to be unique names.
#' @param btnValues A vector of same length as btnNames containing the values
#' that should returned when clicking on the buttons.
#' @param selected The selected selected button. NULL for no selected selection.
#' @param columns The number of elements per row.
#' @param btnStyle Optional styling for the buttons.
#' @param outerStyle Optional styling for the outer div.
#'
#' @section Server return:
#' Using the common shiny access via 'input'.
#' Returns the corresponding value given by btnValues.
#' @export
#'
#' @examples \code{
#' bayasGroupedButtons("groupedBtn", c("small","medium","big"), selected="small",
#'  columns = 2)
#' }
bayasGroupedButtons <- function(inputId, btnNames, btnValues=NULL, selected=NULL,
                           columns=3, btnStyle="", outerStyle="") {
  if(length(btnNames) != length(unique(btnNames))) stop("btnNames have to contain unique values.")
  if(is.null(btnValues)) btnValues <- btnNames
  head <- getDependencies()


  row <- ceiling(length(btnValues) / columns)
  listBtn <- lapply(1:row, function(j){

    i_start <- ((j-1)*columns)+1
    i_end <- min(j*columns,length(btnValues))

    tags$div(
      style = "display:flex; flex-direction: row;",
      htmltools::tags$div(
        style="padding-bottom:1px;margin-left:0px;margin-right:0px;",
        style="flex:1; display:flex;",
        tagList(
          lapply(i_start:i_end, function(i){
            htmltools::tags$div(
              style="margin:0px;padding-right:1px;padding-left:1px; flex:1;",
              htmltools::tags$button(
                id = paste0(inputId,"-",btnValues[i]),
                class = ifelse(!is.null(selected) && selected==btnValues[i],"groupedButtons btn btn-default btn-primary","groupedButtons btn btn-default"),
                type = "button",
                as.character(btnNames[i]),
                'data-name' = btnNames[i],
                'data-value' = btnValues[i],
                style=btnStyle))
          })
        )
      )
    )

  })

  htmltools::tagList(
    singleton(htmltools::tags$head(head)),
    htmltools::tags$div(
      id = inputId,
      class="groupedButtons-parent",
      'data-name' = inputId,
      'data-clicked' = selected,
      'data-value' = selected,
      htmltools::tagList(listBtn),
      style = outerStyle
    )
  )
}

#' @title
#' Update Grouped Action Buttons Inputs
#'
#'@description
#' Updates an existing grouped button created with \code{groupedButtons()}
#'
#' @param session 	The session object passed to function given to shinyServer.
#' Default is getDefaultReactiveDomain().
#' @param inputId The id of the input object.
#' @param selected The button that should be selected.
#'
#' @export
#'
#' @examples \code{
#' updateBayasGroupedButtons(inputId="groupedBtn", selected="big")
#' }
updateBayasGroupedButtons <- function(session = getDefaultReactiveDomain(), inputId,
                                 selected, value=NULL){
  if(is.null(value)) value <- selected
    session$sendCustomMessage(type="updateGroupedActionButton",
                              message=list(element=inputId, selected=selected,
                                           value=value))
}



# CTRL+ALT+SHIFT+R: Create @param etc
# CTRL+SHIFT+D call document()
# devtools::install()
