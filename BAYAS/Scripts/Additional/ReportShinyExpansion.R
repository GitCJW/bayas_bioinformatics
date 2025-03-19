getChecklistProgress <- function(checklistId, numberElements, label){
  if(sum(numberElements) != length(label)){
    label <- seq_len(numberElements)
    warning("numberElements and label have to be of equal length.")
  }
  
  ns <- NS(checklistId)
  widget <- tags$div(
    class="getActiveColor borderColor-regular",
    
    style="display:flex;",
    style="padding:2px;",
    style="margin-bottom:20px;",
    style="border: 1px solid; border-radius: 15px;",

    lapply(1:length(numberElements), function(j){
      
      ret <- lapply(1:numberElements[j], function(i){
        count <- sum(numberElements[seq_len(j-1)]) +i
        tags$div(
          style="flex: 1 1 0px; text-align:center;",
          
          actionButton(ns(count), 
                       label=label[[count]],
                       style="border-radius:50%; width:38px; height:38px;",
                       style="padding:0px;")
        )
      })
      
      if(j < length(numberElements)){
        ret <- list.append(ret, tags$div(class="borderColor-regular", style="border-left:1px solid;"))
      }
      ret
    })
    
  )
  
  return(widget)
}

#active: list of same length as elements in the checklist
# 0: inactive; 1: some model fits (btn-outline-primary); 2:active
updateChecklistProgress <- function(session, checklistId, active, label){
  
  ns <- NS(checklistId)
  for(i in 1:length(active)){
    if(active[i]==0){
      removeClass(ns(i), class="btn-primary")
      removeClass(ns(i), class="btn-outline-primary")
      updateActionButton(session, ns(i), label=label[[i]], icon=character(0))
    }else if(active[i]==1){
      removeClass(ns(i), class="btn-primary")
      addClass(ns(i), class="btn-outline-primary")
      updateActionButton(session, ns(i), label=label[[i]], icon=character(0))
    }else{
      removeClass(ns(i), class="btn-outline-primary")
      addClass(ns(i), class="btn-primary")
      # updateActionButton(session, ns(i), label="", icon=icon("check"))
      updateActionButton(session, ns(i), label=label[[i]], icon=character(0))
    }
  }

}
