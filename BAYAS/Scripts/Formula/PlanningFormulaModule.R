planningFormulaUi <- function(id){
  ns <- NS(id)
  uiOutput(ns("formulaOutput"))
}

planningFormulaServer <- function(id, planningFormula){
  moduleServer(
    id,
    function(input, output, session) {
      pF <- planningFormula
      
      observe({
        pF$getReactive("add")
        pF$getReactive("remove")
        
        isolate({
          output$formulaOutput <- renderUI({tags$div(pF$display())})
        })
      })
      
      observe({
        pF$getReactive("change")
        
        isolate({
          queue <- pF$getChangingQueue()
          
          for(el in queue){
            #remove complete div if content is null
            if(is.null(el$content)){
              removeUI(paste0("#",el$id), immediate=T)
            }else{
              shinyjs::runjs(paste0("$('#",el$id,"').fadeOut(500, function(){$(this).html('",el$content,"').fadeIn();})"))
            }
          }
          
          pF$clearChangingQueue()
        })
      })
    }
  )
}
