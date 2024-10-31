#' @import shiny
#'
#' @title
#' Layout a sidebar and main area
#'
#' @description
#' A sidebar layout similar to shinys 'sidebarLayout' but with a collapsable sidebar panel
#'
#' @param inputId The id of the input object. Used to open/close from server side.
#' @param sidePanel A div containing the left sidebar panel.
#' @param mainPanel A div containing the main panel.
#' @param sidePanelClass Additional styling of the sidebar panel
#' @param mainPanelClass Additional styling of the main panel
#' @param sidePanelStyle Additional styling of the sidebar panel
#' @param mainPanelStyle Additional styling of the main panel
#'
#' @export
#'
#' @note see: \link[shinybayas]{updateBayasSidePanel}
bayasSidePanel <- function (inputId, sidePanel, mainPanel,
                            sidePanelClass=NULL,
                            mainPanelClass=NULL,
                            sidePanelStyle=NULL,
                            mainPanelStyle=NULL){
  tags$div(
    style="display:flex;",

    #Side panel
    shiny::wellPanel(
      id = inputId,
      class=paste0("bayas-sidePanel-open ", sidePanelClass),
      style="margin-right:20px; min-width:0px;",
      style=sidePanelStyle,

      tags$div(
        style="flex:1; width:0px;",
        sidePanel
      ),

      tags$div(
        style="flex:0; display:flex; margin:-1rem -1rem -1rem 1rem;",
        style="border-left: 1px solid var(--bs-border-color-translucent);",
        style="border-radius: 0px 5px 5px 0px;",
        style="box-shadow: inset 0 0px 1px rgba(0,0,0,0.05);",

        tags$a(
          class = "action-button bayasBtnCollapse",
          style = "width:20px; height:20px; margin:10px 10px 0px 10px; padding:0px;",
          style = "text-align: center; font-size: larger; cursor:pointer;",
          shiny::icon("caret-left"))
      )
    ),

    #main panel
    shiny::wellPanel(
      class=mainPanelClass,
      style="flex:2; flex-shrink:2; overflow:hidden;",
      style=mainPanelStyle,

      mainPanel
    )
  )
}

#' @import shiny
#'
#' @title
#' Updates a 'bayasSidePanel'
#'
#' @description
#' Updates the status (open, close) of a 'bayasSidePanel'.
#'
#' @param session The session object passed to function given to shinyServer.
#' @param status c("open","close")
#' @param inputId The id of the input object.
#'
#' @examples \code{
#' updateBayasSidePanel(session = getDefaultReactiveDomain(), inputId = "bayasSidebarPanel",
#' status = "close")
#' }
#'
#'@export
#'
#' @note see: \link[shinybayas]{bayasSidePanel}
updateBayasSidePanel <- function(session, inputId, status=c("open","close")){
  session$sendCustomMessage(type="updateBayasSidePanel",
                            message=list(element=inputId,status=status))
}

# CTRL+ALT+SHIFT+R: Create @param etc
# CTRL+SHIFT+D call document()
# devtools::install()
# usethis::use_version(which="patch")
# devtools::build(path=dirname(getwd()))
