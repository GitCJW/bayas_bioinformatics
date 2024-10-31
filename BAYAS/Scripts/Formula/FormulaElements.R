#' Display elements of Formula
#'
#' This is a R6 Object. To initialize a new object call 'FormulaElements$new()'.
#' Parameter within the 'new()' call:
#' @param type (Predictor, distribution)
#' @param addPriorButton
#' @param predictorLine
#' @param leftSide
#' @param center
#' @param rightSide
#' @param latex = list(left="",center="",right="")
#' @param description
#' @param isRemovable
#' @param myRemovableParameter
FormulaElements <- R6Class(
   classname = "FormulaElements", 
   inherit = SerializationInterface,
   
   private = list(
     stateVersion = "0.1"
   ),
   
   public = list(
     
     type = NULL, #Role of this part, e.g. error distribution of response, or predictor
     addPriorButton = F, #
     predictorLine = F, #True, if the right side contains var terms
     leftSide = NULL, #Left side of statement y ~ N(mu,sd) --> "y"
     center = NULL, #center of statement y ~ N(mu,sd) --> "~"
     rightSide = NULL, #Whole right side as HTML "N(mu,sd)", also colorized
     latex = NULL, #Line as latex, list of 3 arguments: left, center, right
     description = NULL, #Description of this part e.g. tooltip for GLM term, predictor line etc.
     
     isRemovable = F,
     
     #R6 classes
     myRemovableParameter = NULL, #ModelParameter
     
     initialize = function(type, addPriorButton, predictorLine, 
                           leftSide, center, rightSide, latex=list(left="",center="",right=""),
                           description,
                           isRemovable=F, myRemovableParameter = NULL, 
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       
       if(type %in% c("NoiseTerm","PredictorLine","GLMPredictor","AuxParameter")){
         self$type <- type
       }else{
         stop(paste0("Wrong type: ", type))
       }
       self$addPriorButton <- addPriorButton
       self$predictorLine <- predictorLine
       self$leftSide <- leftSide
       self$center <- center
       self$rightSide <- rightSide
       self$latex <- latex
       self$description <- description
       self$isRemovable <- isRemovable
       self$myRemovableParameter <- myRemovableParameter
     },
     
     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)
         
         myRemovableParameterState <- self$myRemovableParameter$getState(nextUUID)
         nextUUID <- myRemovableParameterState$nextUUID
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           type = self$type,
           addPriorButton = self$addPriorButton,
           predictorLine = self$predictorLine,
           leftSide = self$leftSide,
           center = self$center,
           rightSide = self$rightSide,
           latex = self$latex,
           description = self$description,
           isRemovable = self$isRemovable,
           myRemovableParameter = myRemovableParameterState
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'FormulaElements' = ret
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
       
       self$myRemovableParameter <- state$myRemovableParameter
       self$type = state$type
       self$addPriorButton = state$addPriorButton
       self$predictorLine = state$predictorLine
       self$leftSide = state$leftSide
       self$center = state$center
       self$rightSide = state$rightSide
       self$latex = state$latex
       self$description = state$description
       self$isRemovable = state$isRemovable
     }
     
   )
)