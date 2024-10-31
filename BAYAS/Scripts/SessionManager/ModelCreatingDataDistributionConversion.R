OVDLogNormalConvertFrom0.1To0.2 <- function(state){
  state$parameter$sigma$display_name <- "Standard deviation"
  state$parameter$sigma$description <- "The standard deviation"
  return(state)
}
OVDNormalConvertFrom0.1To0.2 <- function(state){
  state$parameter$sigma$display_name <- "Standard deviation"
  state$parameter$sigma$description <- "The standard deviation"
  return(state)
}