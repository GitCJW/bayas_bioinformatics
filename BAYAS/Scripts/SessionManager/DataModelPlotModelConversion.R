DataModelPlotModelConvertFrom0.1To0.2 <- function(state){
  state$id <- length(state$plot_history)+1
  return(state)
}