ModelCreatingDataResponse <- R6Class(
  classname = "ModelCreatingDataResponse", 
  inherit = SerializationInterface,
  
   private = list(
     stateVersion = "0.1",
     mcd = NULL,
     name = NULL,
     type = NULL, #cont, discrete
     range = NULL, #"positiveGreater", "positive", "percent", etc.
     dist = NULL, #planningDistribtionsEnum()
     link = NULL,
     useOnlyNegVal = NULL,
     step = c(1)
   ),
   
   public = list(
     
     initialize = function(mcd,
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       
       private$mcd <- mcd
     },
     
     setMcd = function(mcd){
       private$mcd = mcd
     },
     
     setName = function(name){ 
       private$name = name
       private$mcd$setHeader("Response", name)
       private$mcd$doTriggerResponse("name")
     },
     getName = function(){
       return(private$name)
     },
     
     setType = function(type=c("cont","discrete")){
       match.arg(type)
       if(!is.null(private$type) && private$type != type){
         private$range = NULL
         private$dist = NULL
       }
       private$type = type
       private$mcd$doTriggerResponse("step")
     },
     getType = function() return(private$type),
     
     setRange = function(range=c("positiveGreater", "positive", "percent",
                                 "unlimited","negGreater","negative",
                                 "discretePos","limitedN","bernoulli")){ 
       match.arg(range)
       if(!is.null(private$range) && private$range != range){
         private$dist = NULL
       }
       private$range = range
     },
     getRange = function() return(private$range),
     
     setDist = function(dist=unlist(planningDistribtionsEnum("response"))){
       match.arg(dist)
       if(is.null(private$dist) || private$dist != dist) {
         private$link = planningLinkFunctionMapping(dist)[1]
       }
       private$dist = dist
       private$mcd$doTriggerResponse("dist")
     },
     getDist = function(){
       return(private$dist)
     },
     
     setLink = function(link=c(unlist(planningLinkEnum()), 
                               NULL)){ 
       match.arg(link)
       private$link = link
       private$mcd$doTriggerResponse("link")
     },
     getLink = function() return(private$link),
     
     setUseOnlyNegVal = function(useOnlyNegVal=c(TRUE,FALSE,NULL)){ 
       private$useOnlyNegVal = useOnlyNegVal
     },
     getUseOnlyNegVal = function() return(private$useOnlyNegVal),
     
     
     clear = function(){
       private$type = NULL
       private$range = NULL
       private$dist = NULL
       private$Link = NULL
     },
     
     addStep = function(step){ 
       private$step = c(private$step, step)
       private$mcd$doTriggerResponse("step")
     },
     setStep = function(step){ 
       private$step = step
       private$mcd$doTriggerResponse("step")
     },
     setStepBack = function(){
       if(last(private$step) > 1){
         private$step <- head(private$step, -1)
         private$mcd$doTriggerResponse("step")
       }
     },
     getLastStep = function() return(last(private$step)),
     getSteps = function() return(private$step),
     
     getInstance = function(){
       new <- ModelCreatingDataResponse$new(private$mcd)
       
       new$setName(self$getName())
       new$setType(self$getType())
       
       new$setRange(self$getRange())
       new$setDist(self$getDist())
       
       new$setLink(self$getLink())
       new$setUseOnlyNegVal(self$getUseOnlyNegVal())
       new$setStep(self$getSteps())
  
       return(new)
     },
     
     compareTo = function(cT){
       
       if(private$name != cT$getName()) return(F)
       if(private$type != cT$getType()) return(F)
       if(private$range != cT$getRange()) return(F)
       if(private$dist != cT$getDist()) return(F)
       if(private$link != cT$getLink()) return(F)
       if(!equal0(private$useOnlyNegVal,cT$getUseOnlyNegVal())) return(F)
       if(!vectorEqual(private$step, cT$getSteps())) return(F)
       
       return(T)
     },
     
     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)
         
         mcdState <- NULL
         if(!is.null(private$mcd)){
           mcdState <- private$mcd$getState(nextUUID)
           nextUUID <- mcdState$nextUUID
         }
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           name = private$name,
           type = private$type,
           range = private$range,
           dist = private$dist,
           link = private$link,
           useOnlyNegVal = private$useOnlyNegVal,
           step = private$step,
           
           #R6
           mcd = mcdState
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'ModelCreatingDataResponse' = ret
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
       
       private$name <- state$name
       private$type <- state$type
       private$range <- state$range
       private$dist <- state$dist
       private$link <- state$link
       private$useOnlyNegVal <- state$useOnlyNegVal
       private$step <- state$step
       
       #R6
       private$mcd <- state$mcd
     },
     resetState = function(){
       if(!super$resetState()) return()
       if(!is.null(private$mdc)) private$mdc$resetState()
     }
     
     
   )
                                     
)