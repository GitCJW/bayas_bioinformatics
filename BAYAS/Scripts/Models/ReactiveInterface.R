ReactiveInterface <- R6Class(
  classname = "ReactiveInterface", 
  
  private = list(
    reactiveList = list()
  ),
  
  public = list(
    
    initialize = function(privateList = private){
      for(p_name in names(privateList)){
        if(p_name != "reactiveList")
          private$reactiveList <- list.append(private$reactiveList, reactiveTrigger(), paste0(p_name, "_reactive"))
      }
    },
    
    triggerReactiveValue = function(privateName){
      private$reactiveList[[paste0(privateName,"_reactive")]]$trigger()
    },
    
    triggerReactiveValues = function(){
      for(tt in private$reactiveList){
        tt$trigger()
      }
    },
    
    dependReactiveValue = function(privateName){
      return(private$reactiveList[[paste0(privateName,"_reactive")]]$depend())
    }
    
  )
)

