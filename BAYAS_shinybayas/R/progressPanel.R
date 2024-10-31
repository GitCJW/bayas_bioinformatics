#' @import htmltools
#'
#' @title
#' Progress Panel
#'
#'@description
#' Creates a progress panel that behaves like a modal.
#' Add removable elements that shows the current progress.
#'
#'@usage
#'\code{progressPanel(
#'   inputId,
#'   initIds = NULL,
#'   initImages = NULL,
#'   visible = F,
#'   title = NULL,
#'   footer = progressPanelButton()
#')}
#'
#' @param inputId The input slot that will be used to access the value.
#' @param initIds Ids for the optional initial elements of the overall panel.
#' @param initImages Paths of images to the optional initial elements of the progress panel.
#' @param visible Should be the panel visible at start?
#' @param title  The title, if NULL for no title.
#' @param footer The footer, if NULL for no footer.
#'
#' @section Server return:
#' Using the common shiny access via 'input'.
#' Returns the user given id of the last removed element.
#' @export
#'
#' @examples \code{
#' progressPanel("progressPanel", initIds=c(1,2), initImages=c("folder/img1.jpg","folder/img2.jpg",
#' visible = F, title = "Added elements"),
#' }
progressPanel <- function (inputId, initIds = NULL, initImages=NULL, visible=F,
                            title = NULL, footer=progressPanelButton()){

  if(length(initIds) != length(unique(initIds))) stop("initIds have to contain unique values.")
  if(length(initImages) != length(initIds)) stop("initIds and initImages have to be of equal size.")

  head <- getDependencies()


  imgs <- list()
  if(length(initIds) > 0){
    imgs <- lapply(1:length(initIds), function(i){
      tags$div('help-data'='removable',
               'stored-data'=initIds[i],
        tags$div(
                 style="margin:0px 5px; float:left; border: 1px solid #e3e3e3;",
                 style="border-radius:5px;",
                 tags$img(src=initImages[i],
                          width="100",
                          height="100",
                          style="border-radius:5px;")),
        tags$div(style="text-align:center;",
                 tags$button(type = "button",
                             class = "close-progress btn btn-default action-button",
                             style="margin:5px; width:-moz-available; width: -webkit-fill-available; height:25px; padding:2px;",
                             icon("trash"),
                             style="border-radius:5px"))
      )
    })
  }

  imgs <- tagList(imgs)

  footer <- div(footer)

  ret <- div(
    style="position:fixed; z-index:2000; overflow-x:hidden; overflow-y:auto;",
    style="opacity:0; background-color: #c1c1c182; transition:opacity 0.3s;",
    class= if(visible) "slider-top-vis",
    div(class = "slider",
        class= if(visible) "slider-vis",
        'help-data'="progressPanel",
        style="padding:0px; z-index:2000; width:-moz-available; width: -webkit-fill-available;",
        div(style="margin:0px 10px; width: -moz-available; width: -webkit-fill-available;",
            div(
              div(style="height:30px; border-bottom:1px solid #555;",
                  tags$h4(title)),
              div(id=inputId,
                  'data-removed'= NULL,
                  class = "progress-panel modal-body",
                  style="overflow-x:auto; padding:5px; border-bottom:1px solid #555; display:flex; height:160px;",
                  imgs),
              footer
              ))
      )
  )

  htmltools::tagList(
    singleton(htmltools::tags$head(head)),
    ret
  )
}

#' @title
#' Adds elements to a Progress Panel
#'
#'@description
#' Adds elements to a Progress Panel by given id and path to image
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#' @param id The ids of the added elements.
#' @param image Paths of the added images.
#'
#' @export
#'
#' @examples \code{
#' addProgressPanel(inputId="progressPanel", id=10, image="images/img10.jpg")
#' }
#'
addProgressPanel <- function(session = getDefaultReactiveDomain(), inputId,
                             id, image){

  if(length(id)!=1) stop("id has to be of length 1!")
  if(length(image)!=1) stop("image must contain a single id!")

  addDiv <- tags$div('help-data'='removable',
           'stored-data'=id,
           tags$div(
             style="margin:0px 5px; float:left; border: 1px solid #e3e3e3;",
             style="border-radius:5px;",
                    tags$img(src=image,
                             width="100",
                             height="100",
                             style="border-radius:5px;")),
           tags$div(style="text-align:center;",
                    tags$button(type = "button",
                                class = "close-progress btn btn-default action-button",
                                style="margin:5px; width:-moz-available; width: -webkit-fill-available; height:25px; padding:2px;",
                                icon("trash"),
                                style="border-radius:5px"))
  )

  session$sendCustomMessage(type="addProgressPanel",
                            message=list(element=inputId,html=paste0(addDiv)))
}


#' @title
#' Removes elements of a Progress Panel
#'
#'@description
#' Removes elements of a Progress Panel by given id
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#' @param id The ids of the elements that should be removed.
#'
#' @export
#'
#' @examples \code{
#' removeProgressPanel(inputId="progressPanel", id=10)
#' }
removeProgressPanel <- function(session = getDefaultReactiveDomain(), inputId,
                             ids){
  for(i in ids){
    session$sendCustomMessage(type="removeProgressPanel",
                              message=list(element=inputId,id=i))
  }
}

#' @title
#' Updates Progress Panel
#'
#'@description
#' Updates an existing Progress Panel
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#' @param id The ids of the changed elements.
#' @param image Paths of the changed images.
#'
#' @export
#'
#' @examples \code{
#' updateProgressPanel(inputId="groupedBtn", id=10, image="images/img10.jpg")
#' }
#'
updateProgressPanel <- function(session = getDefaultReactiveDomain(), inputId,
                                ids, images){

  if(length(ids) != length(unique(ids))) stop("ids have to contain unique values.")
  if(length(images) != length(ids)) stop("ids and images have to be of equal size.")

  #Remove all existing entries
  session$sendCustomMessage(type="removeAllProgressPanel",
                            message=list(element=inputId))

  for(i in 1:length(ids)){
    addProgressPanel(session, inputId, ids[i], images[i])
  }
}

#' @title
#' Shows an existing Progress Panel
#'
#'@description
#' Shows an existing Progress Panel
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#'
#' @export
#'
#' @examples \code{
#' showProgressPanel(inputId="groupedBtn")
#' }
#'
showProgressPanel <- function(session = getDefaultReactiveDomain(), inputId){
  session$sendCustomMessage(type="showProgressPanel",
                            message=list(element=inputId))
}

#' @title
#' Hides an existing Progress Panel
#'
#'@description
#' Hides an existing Progress Panel
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#'
#' @export
#'
#' @examples \code{
#' hideProgressPanel(inputId="groupedBtn")
#' }
#'
hideProgressPanel <- function(session = getDefaultReactiveDomain(), inputId){
  session$sendCustomMessage(type="hideProgressPanel",
                            message=list(element=inputId))
}


#' @title
#' Creates a close button for a Progress Panel
#'
#'@description
#' Creates a close button for a Progress Panel
#'
#' @export
#'
#' @examples \code{
#' progressPanelButton()
#' }
#'
progressPanelButton <- function(){
  return(tags$button(
    style="margin:10px;",
    type="button",
    class="progressPanel-close btn btn-default","Ok"))
}

# CTRL+ALT+SHIFT+R: Create @param etc
# CTRL+SHIFT+D call document()
# devtools::install()
# usethis::use_version(which="patch")
# devtools::build()
