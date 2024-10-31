## Build the formula.
buildStaticFormula <- function(pIDM, session){
  

  stanModel <- pIDM$get.selected_BAYSIS_stan_model()
  formula_elements <- stanModel$new_formula(pIDM$getDataModelInputData()$getResponseVariable(onlyName=T))
  
  rows <- list()
  tagCount <- 1
  
  #Iterate through "FormulaElements" objects
  for(element in formula_elements){
    
    math <- ""
    if(element$predictorLine){
      math_divs <- c()
      index <- 1
      for(pred in stanModel$get_predictor_line()){
        para <- pred$modelParameter
        #e.g. intercept that is displayed by just the parameter (b0)
        if(is.null(pred$display_name)){
          if(index > 1){
            mathString <- tags$div(" + ", tags$span(para$getFullDisplayName(),class="formulaParameter"), 
                                   style="display:inline;")
          }else{
            mathString <- tags$div(tags$span(para$getFullDisplayName(),class="formulaParameter"), 
                                   style="display:inline;")
          }
        }else{
          if(index > 1){
            #right side of = ~ ... with more than one term
            mathString <- tags$div(" + ", HTML(paste0(tags$span(para$getFullDisplayName(),class="formulaParameter"), "&sdot;", pred$display_name)),
                                   style="display:inline;")
          }else{
            #right side of = ~ ... with just one term
            mathString <- tags$div(HTML(paste0(tags$span(para$getFullDisplayName(),class="formulaParameter"), "&sdot;", pred$display_name)), 
                                   style="display:inline;")
          }
        }
        math_divs[[index]] <- tags$div(mathString, style="display:inline;") 
        index <- index+1
      }
      math <- tags$div(element$rightSide, math_divs)
    }else{
      math <- tags$div(element$rightSide, style="display:inline;")
    }
    
    #Row to be added
    rows[[tagCount]] <- tags$div(fluidRow(column(3, tags$span(element$leftSide, style="text-align:right;"),
                                                 tags$span(element$center, style="text-align:center;padding:2px;margin:0px;"),
                                                 style="text-align:right;padding:2px;margin:0px;"),
                                          column(9, tags$div(math, style = "margin:0px;"), style="padding:2px; text-align:left;")))
    
    
    tagCount <- tagCount+1
  }
  
  #Changing formula ui
  return(tagList(rows))
}