get_checklist_content <- function(){
  
  ret <- list(planning=list(),
              evaluation=list())
  
  #First planning point
  tmp <- list()
  tmp$header <- "P1. Planning experiments"
  tmp$text <- paste0("There is just a single report item for the 'Planning experiments'. ",
                     "It contains e.g. the model definition with its (generative) priors and also the 'Estimate N' goals used and the result, if any. ",
                     "In the case of a sample size determination ('Estimate N'): ",
                     "According to the ARRIVE guidelines, the sample size should be reported per study group. ",
                     "In this report, the sample size is reported as a total number. ",
                     "Please use 'Custom text item' to add the number of sample sizes per study group.")
  ret$planning[[1]] <- tmp
  
  
  #First evaluation point
  tmp <- list()
  tmp$header <- "E1. Outcome measurements"
  tmp$text <- paste0("It is good practice to show the outcome measurements. ",
                     "If possible also in dependency of the predictors. ",
                     "Use the plot function (so the grouping/facet option) in ",
                     "the 'Data visualization' section of the evaluation tool.")
  ret$evaluation[[1]] <- tmp
  
  
  #Second evaluation point
  tmp <- list()
  tmp$header <- "E2. Statistical model"
  tmp$text <- paste0("The statistical model is the keypoint of the analysis. ",
                     "It describes the behaviour of the outcome variable ",
                     "and also the relationship between outcome variable and predictors. ",
                     "This report item is part of the 'recommended items'.")
  ret$evaluation[[2]] <- tmp
  
  
  #Third evaluation point
  tmp <- list()
  tmp$header <- "E3. Model validation"
  tmp$text <- paste0("The model validation includes information about the sampling. ",
                     "This is essential, since it tells if a sampling went well ",
                     "and is reliable for e.g. quantifications and interpretations. ",
                     "This report item is part of the 'recommended items'.")
  ret$evaluation[[3]] <- tmp
  
  
  #Forth evaluation point
  tmp <- list()
  tmp$header <- "E4. Posterior predictive check"
  tmp$text <- paste0("The PPC is used to show whether your data is predictable ",
                     "by the model. Systematically differences are signs of ",
                     "misspecification of the model. ",
                     "PPCs are information-rich but easy understandable. ",
                     "This report item is part of the 'recommended items'.")
  ret$evaluation[[4]] <- tmp
  
  
  #Fifth evaluation point
  tmp <- list()
  tmp$header <- "E5. Prior predictive check"
  tmp$text <- paste0("Prior predictive checks are used to show predictions of the ",
                     "model just based on the piors exluding the data. ",
                     "For non or weakly informative priors it can be used to ",
                     "show whether the model predictions captures the real data. ",
                     "Informative priors can be verified by prior predictive checks. ",
                     "This report item is part of the 'recommended items'.")
  ret$evaluation[[5]] <- tmp
  
  
  #Sixth evaluation point
  tmp <- list()
  tmp$header <- "E6. Prior vs Posterior"
  tmp$text <- paste0("Prior vs posterior plots shows whether non or weakly informative ",
                     "priors makes sense. ",
                     "Prior vs posterior plots can be created under the Evaluation tool ", 
                     "-> tab 'Model fitting' -> tab 'Model validation' -> 'Prior vs posterior'.")
  ret$evaluation[[6]] <- tmp
  
  
  #Seventh evaluation point
  tmp <- list()
  tmp$header <- "E7. Marginal posteriors"
  tmp$text <- paste0("Marignal posteriors are quantifications of the model that ",
                     "are can be used to show effects. ")
  ret$evaluation[[7]] <- tmp
  
  
  #Eighth evaluation point
  tmp <- list()
  tmp$header <- "E8. Effects"
  tmp$text <- paste0("Effects summarizes the marignal posteriors for a better ",
                     "understanding. They can be simple differences between ",
                     "groups (or the differences/size of a slope) or also e.g. ",
                     "ratios, log odds, etc. ",
                     "Effects are done for individual model under the ",
                     "'Effects / Predictions' tab in the Evaluation tool.")
  ret$evaluation[[8]] <- tmp
  
  
  #Ninth evaluation point
  tmp <- list()
  tmp$header <- "E9. Predictions"
  tmp$text <- paste0("In addition to effects, predictions show possible outcome ",
                     "(distribution) or differences for a certain combination of predictors. ",
                     "Predictions are done for individual model under the ",
                     "'Effects / Predictions' tab in the Evaluation tool.")
  ret$evaluation[[9]] <- tmp
  
  
  #Tenth evaluation point
  tmp <- list()
  tmp$header <- "E10. Model comparison"
  tmp$text <- paste0("Model comparison is used to compare models with the same outcome variable. ",
                     "Comparison is done by applying Pareto Smoothed Importance Sampling ",
                     "leave-one-out (PSIS-loo) to each model. The result of comparison shows ",
                     "differences in the ability of predicting the real data over the models.",
                     "Model comparison can be done under the Evaluation tool -> 'Model comparison'.")
  ret$evaluation[[10]] <- tmp
  
  return(ret)
}


get_checklist_data_vs_fit <- function(){
  return(list(
    planning = c(1),
    data = c(2,11),
    fit = c(3:10)
  ))
}
