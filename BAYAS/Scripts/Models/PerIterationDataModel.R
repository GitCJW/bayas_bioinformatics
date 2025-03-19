## This is a R6 Class for holding necessary data for one Tool iteration.

PerIterationDataModel <- R6Class(
  classname = "PerIterationDataModel", 
  inherit = ReactiveSerializationInterface,
                              
   private = list(
     stateVersion = "0.1",
     
     #unique id
     id = 0,

     
     #DataModelInputData containing all information about the raw input and converted data
     dataModelInputData = NULL,
     
     
     #(Unique) data path (generated tmp path when uploading)
     #Used for reporting
     dataPath = NULL,
     
     # File name, which is no part of dataPath. 
     # fileName = NULL,
     
     # The name will be shown in the SelectInput of the previous models. 
     # As long as this model is not fitted (Button Run Fit activated), the name will be "(Not_fitted)".
     name = "(Not_fitted)",
     
     # The data that are used for a fit. This is the same data as in the data Model with same changes to e.g. categorical variables.
     # In use.
     # Have to be, since the data in the modelData can change during the use. But e.g. the ppc needs the data of the fit,
     # not the data of the dataModel, which maybe changed in the meantime
     data = NULL,
     
     # Timepoint when button "Run fit" is clicked
     time_of_run_fit = NULL,
     
     # Contains the characteristics and limits, response and name
     # Data frame of five columns
     # var_name (char)
     # response (bool)
     # characteristics (discrete, continuous, categorical)
     # lower_limit (char)
     # upper_limit (char)
     users_variables = NULL,
     
     # Column names
     # var_names = NULL,
     
     # Name of users selected response variable
     # response_variable = NULL,
     
     # Dataframe containing the name and status of checkboxes, that determine if a variable is used.
     # Vector of same length and in order of checkbox ui elements.
     # checkboxes_variable = NULL,
     
     # Stores the used user variables in a vector
     # @Deprecated
     used_variables = NULL,
     
     # Selected BAYSIS stan model
     selected_BAYSIS_stan_model = NULL,
     
     # Fitted Stan model (stanfit, stanreg)
     calculated_stan_object = NULL,
     
     # Verbal result of a fit. 
     verbal_result = NULL,
     
     # PPC plot for preview ppc, necessary for reporting the ppc
     ppc_plot = NULL,
     ppc_plot_warnings = F, #Whether there have been any warnings due to discrepancies.
     
     # Used fixed terms, the type of term is fixed
     # Data frame of two columns
     # index (of selectInput)
     # var_name (certain column name of user_cleaned_input_data)
     # used_terms = data.frame(stringsAsFactors = F, index = NULL, var_name = NULL),
     
     # Optional terms that are added. The type is flexible and must be saved as well.
     # index: of selectInput(s) (in case of two terms)
     # typeTerm the type of added term (oneTerm or twoTerm)
     # var_name: vector of selected var_name, length depends on typeTerm
     used_optional_terms = list(index = NULL, typeTerm = NULL, var_name = NULL),
     
     
     # Prior predictive check
     priorPredictiveCheckPlot = NULL,
     
     # Settings for model fit
  
     number_iterations = 2000,
     
     number_chains = 4,
     
     number_cores = 1,
     
     adapt_delta = 0.8,
     
     max_treedepth = 10,
     
     seed = 0,
     seedByUser = F
   ),
   
   
   
   public = list(
     
     initialize = function(cores=1,
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       private$number_cores <- cores
       private$dataModelInputData <- DataModelInputData$new(perIterationDataModel=self)
     },
     
     set.id = function(id, silent=F){
       private$id <- id
       if(!silent) self$triggerReactiveValue("id")
     },
     get.id = function(){
       return(private$id)
     },
     
     setDataModelInputData = function(dataModelInputData, silent=F){
       private$dataModelInputData <- dataModelInputData
       if(!silent) self$triggerReactiveValue("dataModelInputData")
     },
     getDataModelInputData = function(){
       return(private$dataModelInputData)
     },
     
     set.dataPath = function(x, silent=F){
       private$dataPath <- x
       if(!silent) self$triggerReactiveValue("dataPath")
     },
     get.dataPath = function(){
       return(private$dataPath)
     },
     

     set.name = function(x, silent=F){
       private$name <- x
       if(!silent) self$triggerReactiveValue("name")
     },
     get.name = function(){
       return(private$name)
     },
     

     set.time_of_run_fit = function(x, silent=F){
       private$time_of_run_fit <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("time_of_run_fit")
     },
     get.time_of_run_fit = function(){
       return(private$time_of_run_fit)
     },
     
     
     # set.response_variable = function(x, silent=F){
     #   private$response_variable <- x
     #   self$set.name("(Not_fitted)")
     #   if(!silent) self$triggerReactiveValue("response_variable")
     # },
     get.response_variable = function(){
       return(private$users_variables[private$users_variables$response==TRUE,]$var_name)
     },
     
     
     set.users_variables = function(x, silent=F){
       private$users_variables <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("users_variables")
     },
     
     
     # If onlyResponse is T, response and selected will be ignored 
     get.users_variables = function(response = T, onlyUsedVars = F, onlyResponse = F, 
                                    var = "All", characteristic = "All"){
  
       tmp <- private$users_variables
       if(!response){
         tmp <- tmp[!tmp$response,]
       }
       if(onlyResponse){
         tmp <- private$users_variables[private$users_variables$response,]
       }
       if(onlyUsedVars){
         tmp <- tmp[tmp$var_name %in% private$used_variables,]
       }
       if(var != "All"){
         tmp <- tmp[tmp[,1] == var,]
       }
       if(!"All" %in% characteristic){
         tmp <- tmp[tmp[,3] %in% characteristic,]
       }
       return(tmp)
     },
     
     
     get.users_variable_position = function(var_name){
       return(match(var_name, private$users_variables$var_name))
     },
     
     
     get.users_variable_characteristic = function(var_names){
       return(private$users_variables[private$users_variables$var_name %in% var_names,]$characteristics)
     },
     
     am.I.categorical = function(var_names){
       #Probably not useful
       char <- self$get.users_variable_characteristic(var_names)
       return(characteristicEnum()$Categorical %in% char)
     },
     
    
     set.used_variables = function(x, silent=F){
       private$used_variables <- x
       if(!silent) self$triggerReactiveValue("used_variables")
     },
     get.used_variables = function(){
       return(private$used_variables)
     },
  
     
     set.selected_BAYSIS_stan_model = function(x, silent=F){
       private$selected_BAYSIS_stan_model <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("selected_BAYSIS_stan_model")
     },
     get.selected_BAYSIS_stan_model = function(){
       return(private$selected_BAYSIS_stan_model)
     },
  
  
     set.calculated_stan_object = function(x, silent=F){
       attr(attr(x[["model"]], "terms"), ".Environment") <- NULL
       attr(x[["terms"]], ".Environment") <- NULL
       private$calculated_stan_object <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("calculated_stan_object")
     },
     get.calculated_stan_object = function(){
       return(private$calculated_stan_object)
     },
     
     
     set.verbal_result = function(x, silent=F){
       private$verbal_result <- x
       if(!silent) self$triggerReactiveValue("verbal_result")
     },
     get.verbal_result = function(){
       return(private$verbal_result)
     },
     
     
     set.ppc_plot = function(x, silent=F){
       private$ppc_plot <- x
       if(!silent) self$triggerReactiveValue("ppc_plot")
     },
     get.ppc_plot = function(){
       return(private$ppc_plot)
     },
     
     set.ppc_plot_warnings = function(flag){
       private$ppc_plot_warnings <- flag
     },
     get.ppc_plot_warnings = function() return(private$ppc_plot_warnings),
     
     
     set.used_optional_terms = function(x, silent=F){
       private$used_optional_terms <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("used_optional_terms")
     },
     add.used_optional_terms = function(index, typeTerm, var_name, silent=F){
       if(is.null(private$used_optional_terms$index)){
         l <- 1
       }else{
         l <- length(private$used_optional_terms$index) + 1
       }
       private$used_optional_terms$index[l] <- index
       private$used_optional_terms$typeTerm[l] <- typeTerm
       private$used_optional_terms$var_name[l] <- var_name
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("used_optional_terms")
     },
     set.used_optional_terms_var_name = function(index, index_2=1, var_name, silent=F){
       l <- match(index, private$used_optional_terms$index)
       private$used_optional_terms$var_name[[l]][[index_2]] <- var_name
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("used_optional_terms")
     },
     remove.used_optional_terms = function(index, silent=F){
       if(length(private$used_optional_terms$index) == 1){
         private$used_optional_terms = list(index = NULL, typeTerm = NULL, var_name = NULL)
         if(!silent) self$triggerReactiveValue("used_optional_terms")
         return()
       }
       l <- match(index, private$used_optional_terms$index)
       private$used_optional_terms$index <- private$used_optional_terms$index[-l]
       private$used_optional_terms$typeTerm <- private$used_optional_terms$typeTerm[-l]
       private$used_optional_terms$var_name <- private$used_optional_terms$var_name[-l]
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("used_optional_terms")
     },
     get.used_optional_terms = function(){
       return(private$used_optional_terms)
     },
     get.used_optional_terms_highest_index = function(){
       if(is.null(private$used_optional_terms$index)){
         return(0)
       }else{
         return(max(private$used_optional_terms$index))
       }
     },
     
     set.priorPredictiveCheckPlot = function(x, silent=F){
       private$priorPredictiveCheckPlot <- x
       if(!silent) self$triggerReactiveValue("id")
     },
     get.priorPredictiveCheckPlot = function(){
       return(private$priorPredictiveCheckPlot)
     },
     
     set.number_iterations = function(x, silent=F){
       private$number_iterations <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("number_iterations")
     },
     get.number_iterations = function(){
       return(private$number_iterations)
     },
    
     
     set.number_chains = function(x, silent=F){
       private$number_chains <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("number_chains")
     },
     get.number_chains = function(){
       return(private$number_chains)
     },
   
     
     set.number_cores = function(x, silent=F){
       private$number_cores <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("number_cores")
     },
     get.number_cores = function(){
       return(private$number_cores)
     },
  
     
     set.adapt_delta = function(x, silent=F){
       private$adapt_delta <- x
       self$set.name("(Not_fitted)")
       if(!silent) self$triggerReactiveValue("adapt_delta")
     },
     get.adapt_delta = function(){
       return(private$adapt_delta)
     },
     
     set.max_treedepth = function(x, silent=F){
       private$max_treedepth <- x
       if(!silent) self$triggerReactiveValue("max_treedepth")
     },
     get.max_treedepth = function(){
       return(private$max_treedepth)
     },
     
     set.seed = function(x, silent=F){
       private$seed <- x
       if(!silent) self$triggerReactiveValue("seed")
     },
     get.seed = function(){
       return(private$seed)
     },
     
     set.seedByUser = function(x, silent=F){
       private$seedByUser <- x
       if(!silent) self$triggerReactiveValue("seedByUser")
     },
     get.seedByUser = function(){
       return(private$seedByUser)
     },
     
     parameters.setted = function(){
       return(!(is.null(private$number_iterations) || is.null(private$number_chains) ||
                  is.null(private$adapt_delta) || is.null(private$max_treedepth) ||
                  is.null(private$seed)))
     },
     

     getInstance = function(id){
       newInstance <- PerIterationDataModel$new()
       
       #Non R6 objects, that are safe to call by reference
       if(is.null(id)){
         newInstance$set.id(private$id, silent=T)
       }else{
         newInstance$set.id(id, silent=T)
       }
       
       newInstance$set.dataPath(private$dataPath, silent=T)
       newInstance$set.time_of_run_fit(private$time_of_run_fit, silent=T)
       newInstance$set.number_iterations(private$number_iterations, silent=T)
       newInstance$set.number_chains(private$number_chains, silent=T)
       newInstance$set.number_cores(private$number_cores, silent=T)
       newInstance$set.adapt_delta(private$adapt_delta, silent=T)
       newInstance$set.max_treedepth(private$max_treedepth, silent=T)
       # newInstance$set.data(private$data, silent=T)
       
       newInstance$set.seed(private$seed, silent=T)
       newInstance$set.seedByUser(private$seedByUser, silent=T)
       
       newInstance$set.verbal_result(private$verbal_result, silent=T)
       newInstance$set.used_optional_terms(private$used_optional_terms, silent=T)
       newInstance$set.calculated_stan_object(private$calculated_stan_object, silent=T)
       newInstance$set.users_variables(private$users_variables, silent=T)
       newInstance$set.used_variables(private$used_variables, silent=T)
       
       #R6 Objects
       if(!is.null(private$dataModelInputData))
         newInstance$setDataModelInputData(private$dataModelInputData$getInstance(perIterationDataModel=newInstance), silent=T)
       if(!is.null(private$selected_BAYSIS_stan_model)) 
         newInstance$set.selected_BAYSIS_stan_model(private$selected_BAYSIS_stan_model$getInstance(newInstance), silent=T)

       
       newInstance$set.name(private$name, silent=T)
       return(newInstance)
     },
     
     setInstance = function(instance, dataModel, removeSelcetedBAYSISModel = F){

       private$id <- instance$get.id()
       private$dataPath <- instance$get.dataPath()
       private$name <- instance$get.name()
       # private$data <- instance$get.data()
       private$time_of_run_fit <- instance$get.time_of_run_fit()
       private$users_variables <- instance$get.users_variables()
       private$calculated_stan_object <- instance$get.calculated_stan_object()
       private$verbal_result <- instance$get.verbal_result()
       private$ppc_plot <- instance$get.ppc_plot()
       private$priorPredictiveCheckPlot <- instance$get.priorPredictiveCheckPlot()
       private$number_iterations <- instance$get.number_iterations()
       private$number_chains <- instance$get.number_chains()
       private$number_cores <- instance$get.number_cores()
       private$adapt_delta <- instance$get.adapt_delta()
       private$max_treedepth <- instance$get.max_treedepth()
       private$seed <- instance$get.seed()
       private$seedByUser <- instance$get.seedByUser()
       private$used_optional_terms <- instance$get.used_optional_terms()
       private$used_variables <- instance$get.used_variables()
       
       #R6
       if(!is.null(instance$getDataModelInputData())) 
         private$dataModelInputData$setInstance(instance$getDataModelInputData()$getInstance()) 
       if(!is.null(instance$get.selected_BAYSIS_stan_model())){
         if(!removeSelcetedBAYSISModel){
           if(is.null(private$selected_BAYSIS_stan_model)){
             private$selected_BAYSIS_stan_model <- instance$get.selected_BAYSIS_stan_model()$getInstance(dataModel=dataModel)
           }else{
             private$selected_BAYSIS_stan_model$setInstance(instance$get.selected_BAYSIS_stan_model()$getInstance(dataModel=dataModel))
           }
         }else{
           private$selected_BAYSIS_stan_model <- NULL
         }
       }
       
       self$triggerReactiveValues()
     },
     

     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)

         dataModelInputDataState <- NULL
         if(!is.null(private$dataModelInputData)){
           dataModelInputDataState <- private$dataModelInputData$getState(nextUUID)
           nextUUID <- dataModelInputDataState$nextUUID
         }
         selected_BAYSIS_stan_modelState <- NULL
         if(!is.null(private$selected_BAYSIS_stan_model)){
           selected_BAYSIS_stan_modelState <- private$selected_BAYSIS_stan_model$getState(nextUUID)
           nextUUID <- selected_BAYSIS_stan_modelState$nextUUID
         }
         

         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           id = private$id,
           dataPath = private$dataPath,
           name = private$name,
           data = private$data,
           time_of_run_fit = private$time_of_run_fit,
           users_variables = private$users_variables,
           calculated_stan_object = private$calculated_stan_object,
           verbal_result = private$verbal_result,
           ppc_plot = private$ppc_plot,
           priorPredictiveCheckPlot = private$priorPredictiveCheckPlot,
           number_iterations = private$number_iterations,
           number_chains = private$number_chains,
           number_cores = private$number_cores,
           adapt_delta = private$adapt_delta,
           max_treedepth = private$max_treedepth,
           seed = private$seed,
           seedByUser = private$seedByUser,
           used_optional_terms = private$used_optional_terms,
           used_variables = private$used_variables,
           
           #R6
           # dataModel = dataModelState,
           dataModelInputData = dataModelInputDataState,
           selected_BAYSIS_stan_model = selected_BAYSIS_stan_modelState
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PerIterationDataModel' = ret
       )
       return(ret)
     },
     setState = function(state){
       check <- super$checkVersion(private$stateVersion, state$stateVersion, stop=T)
       if(check$needsConversion){
         if(check$conversionPossible){
           for(func in check$conversionList){
             state <- do.call(get(func), list(state=state))
           }
         }
       }
       
       private$id <- state$id
       private$dataPath <- state$dataPath
       private$name <- state$name
       private$data <- state$data
       private$time_of_run_fit <- state$time_of_run_fit
       private$users_variables <- state$users_variables
       private$calculated_stan_object <- state$calculated_stan_object
       private$verbal_result <- state$verbal_result
       private$ppc_plot <- state$ppc_plot
       private$priorPredictiveCheckPlot <- state$priorPredictiveCheckPlot
       private$number_iterations <- state$number_iterations
       private$number_chains <- state$number_chains
       private$number_cores <- state$number_cores
       private$adapt_delta <- state$adapt_delta
       private$max_treedepth <- state$max_treedepth
       private$seed <- state$seed
       private$seedByUser <- state$seedByUser
       private$used_optional_terms <- state$used_optional_terms
       private$used_variables <- state$used_variables
      
       #R6
       # private$dataModel <- state$dataModelState
       private$dataModelInputData <- state$dataModelInputData
       private$selected_BAYSIS_stan_model <- state$selected_BAYSIS_stan_model
     },
     resetState = function(){
       if(!super$resetState()) return()
       # if(!is.null(private$dataModel)) private$dataModel$resetState()
       if(!is.null(private$dataModelInputData)) private$dataModelInputData$resetState()
       if(!is.null(private$selected_BAYSIS_stan_model)) private$selected_BAYSIS_stan_model$resetState()
     }
     
   )                 
)