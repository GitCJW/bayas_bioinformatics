#source("BAYSIS_Stan_models.R", chdir = TRUE)

## Filters the Stan models by characteristic, upper and lower limit of the Predictor variable
## and all others.
## Returns two vectors, each containing the index of the stan model from ‘stan_models’
## First vector for appliable models, and second for not appliable
## Filter_values:
# Matrix of:
# Columns: characteristic, lower limit, upper limit, description
# Rows: Predictor, and each other
filter_stan_models <- function(stan_models = NULL, dataModel){
  
  filtered_stan_models <- c()
  out <- c()
  
  # take all stan models, if no list is delivered
  if(is.null(stan_models)) stan_models <- get_all_stan_models()
  
  # The used variables
  response_var <- dataModel$get.cPerIterationDataModel()$get.users_variables(onlyResponse = T)
  co_var <- dataModel$get.cPerIterationDataModel()$get.users_variables(response = F)
  number_co_var <- length(co_var$characteristics)

  # print(response_var)
  
  ## check each stan model
  for(i in 1:length(stan_models)){
    tmp_model <- stan_models[[i]]
    # print("0")
    ## Check there is a response var
    if(length(response_var$var_name) == 0){
      out <- cbind(out, tmp_model$id)
      next
    } 
    
    
    ## Check number of co-variablen 
    #  #user-var <= model_co-var+model_opt_co_var 
    if(!(tmp_model$number_optional_co_var == 'VAR' ||
       number_co_var <= (as.numeric(tmp_model$number_optional_co_var) + tmp_model$number_co_var)))
    {
      out <- cbind(out, tmp_model$id)
      next
    } 
    
    ## Check if the response var fits
    if((tmp_model$parameters$characteristics[1] != 'VAR' && tmp_model$parameters$characteristics[1] != response_var$characteristics) |
       (tmp_model$parameters$upper_limit[1] != 'VAR' && tmp_model$parameters$upper_limit[1] != response_var$upper_limit) |
       (tmp_model$parameters$lower_limit[1] != 'VAR' && tmp_model$parameters$lower_limit[1] != response_var$lower_limit)) 
    {
      out <- cbind(out, tmp_model$id)
      next
    } 
    
    
    ## Check that each model_co_var can applied by at least one user_var, when it isn't variable
    model_co_var_applied <- T
    if(tmp_model$number_co_var > 0){
      for(i in 1:tmp_model$number_co_var){
        if(length(compare_variable(variable = tmp_model$parameters[i+1,], matrix = co_var)) == 0) 
        {
          out <- cbind(out, tmp_model$id)
          model_co_var_applied <- F
          break #TODO
        } 
      }
    }
    if(!model_co_var_applied) next
    # print("3")
    
    
    ## Check that each user_var can be used by the model_co_var + model_opt_co_var
    FIT <- F
    for(k in 1:length(co_var$characteristics)){
      
      FIT <- T
      
      ## Check if the user_co_var fits to one out of model_co_var, if there are at least one co_var
      if(tmp_model$number_co_var > 0 && length(compare_variable(co_var[k,], tmp_model$parameters[-1,])) == 0) FIT <- F
      
      ## Check if the user_co_var fits otherwise to an optional model_co_var
      if(!FIT){
        # Check each list element of optional co-var
        for(tmp_model_co_var in tmp_model$optional_values){
          # users co_var fits to an optional variable, now check if the possible other optional co variable are fitted by minimum another users co-var
          if(compare_variable(co_var[k,], tmp_model_co_var) > 0 ){
            for(j in 1:length(tmp_model_co_var$characteristics)){
              # A users co-var fits to a possible other optional co variable
              if(compare_variable(tmp_model_co_var[j,], co_var) > 0) FIT <- T 
            }
          }
        }
      }
      
      if(!FIT){
        out <- cbind(out, tmp_model$id)
        next
      }
      
    }
    
    
    ## Check if each condition is accepted
    if(FIT){
      filtered_stan_models <- cbind(filtered_stan_models, tmp_model$id)
    }else{
      out <- cbind(out, tmp_model$id)
    }
    
  }
  
  
  out_sort <- NULL
  in_sort <- NULL
  if(!is.null(out)) out_sort <- sort(out)
  if(!is.null(filtered_stan_models)) in_sort <- sort(filtered_stan_models)
  
  return(list(fit_models = in_sort, no_fit_models = out_sort))
}




#Returns a list of 3 elements:
#fit_models
#not_recommended
#no_fit_models = out_sort
filter_stan_models_2 <- function(stan_models = NULL, dataModel){
  
  filtered_stan_models <- c()
  not_recommended <- c()
  out <- c()
  
  # take all stan models, if no list is delivered
  if(is.null(stan_models)) stan_models <- get_all_stan_models()
  
  # The used variables
  dMID <- dataModel$getDataModelInputData()
  response_var <- dMID$getResponseVariable()
  co_var <- dMID$getOtherVariables()
  
  if(length(response_var[,1])==0){
    return(list(fit_models = NULL, not_recommended = NULL, 
                no_fit_models = 1:length(stan_models)))
  } 
  
  ## check each stan model
  for(i in seq_along(stan_models)){
    tmp_model <- stan_models[[i]]
    
    proplist <- tmp_model$modelProperties
    must <- proplist$must
    opt <- proplist$opt
    
    if(length(proplist)==0){
      warning(paste0("proplist of ", tmp_model$display_name, " is empty!"))
      out <- c(out, tmp_model$id)
      next
    }
    
    applicable <- T
    
    #Verify that 'must' parameters can be offered
    if(!variable_fit(response_var, must$response)){
      out <- c(out, tmp_model$id)
      next
    }
    t_m <- must
    t_m$response <- NULL
    if(length(t_m) > 0 && !match_modelProp(co_var, t_m)){
      out <- c(out, tmp_model$id)
      next
    }
    filtered_stan_models <- c(filtered_stan_models, tmp_model$id)
  }
  
  #extra rules = non recommended distributions, 
  #but possible (e.g. normal for positive data): 
  #Gamma id:6   Expo id:10   normal id:1
  # if(6 %in% filtered_stan_models) not_recommended <- 10
  if(any(c(6,10) %in% filtered_stan_models)) not_recommended <- c(1)
  out <- setdiff(out, not_recommended)
  
  out_sort <- NULL
  in_sort <- NULL
  if(!is.null(out)) out_sort <- sort(out)
  if(!is.null(filtered_stan_models)) in_sort <- sort(filtered_stan_models)
  
  return(list(fit_models = in_sort, not_recommended = not_recommended, 
              no_fit_models = out_sort))
}


## Function that determines if a variable with its properties does fit against a matrix
# Both must have 3 columns: characteristic, lower_limit, upper_limit
# Return vector of indices of matrix that fits
compare_variable <- function(variable, matrix){

  indices <- c()
  
  if(is.na(matrix$characteristics[1])) return(indices)
  
  for(i in 1:length(matrix$characteristics)){

    if((matrix$characteristics[i] == 'VAR' || variable$characteristics == 'VAR' || matrix$characteristics[i] == variable$characteristics) &&
       (matrix$upper_limit[i] == 'VAR' || variable$upper_limit == 'VAR' || matrix$upper_limit[i] == variable$upper_limit) &&
       (matrix$lower_limit[i] == 'VAR' || variable$lower_limit == 'VAR' || matrix$lower_limit[i] == variable$lower_limit)) 
      indices <- cbind(indices, i)
  }
  
  return(indices)
}






match_variable <- function(variables, modelProps){
  for(v in variables){
    ok_v <- F
    for(m in modelProps){
      if(variable_fit(v,m)) ok_v <- T
    }
    if(!ok_v) return(F)
  }
  return(T)
}

match_modelProp <- function(variables, modelProps){
  for(m in modelProps){
    ok_m <- F
    for(v in variables){
      if(variable_fit(v,m)) ok_m <- T
    }
    if(!ok_m) return(F)
  }
  return(T)
}

variable_fit <- function(variable, modelProp){
  if(!is.na(modelProp$char) && variable$type != modelProp$char) return(F)
  if(!is.na(modelProp$lower) && variable$lower != modelProp$lower) return(F)
  if(!is.na(modelProp$upper) && variable$upper != modelProp$upper) return(F)
  return(T)
}

