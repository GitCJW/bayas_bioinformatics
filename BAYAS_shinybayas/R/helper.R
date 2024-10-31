getDependencies <- function(){
  head <- htmltools::htmlDependency(name = "shinybayas-js", version=packageVersion("shinybayas"),
                                    package = "shinybayas",
                                    src = "www",
                                    script = c("ProgressPanel.js", "GroupedButtons.js",
                                               "BayasNumericInput.js", "bayasSidePanel.js",
                                               "BayasTextInput.js"),
                                    stylesheet = c("ProgressPanel.css","BayasNumericInput.css",
                                                   "bayasSidePanel.css")
  )
}
formatNoSci <- function(x) {
  if (is.null(x)) return(NULL)
  format(x, scientific = FALSE, digits = 15)
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
