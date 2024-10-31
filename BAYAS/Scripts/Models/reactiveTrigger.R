reactiveTrigger <- function(initNULL=T) {
  if(initNULL){
    trigger_counter <- reactiveVal(NULL)
  }else{
    trigger_counter <- reactiveVal(0)
  }
  list(
    depend = function() {
      trigger_counter()
      invisible()
    },
    trigger = function() {
      if(is.null(isolate(trigger_counter()))){
        trigger_counter(0)
      }else{
        trigger_counter(isolate(trigger_counter())+1)
      }
    }
  )
}
