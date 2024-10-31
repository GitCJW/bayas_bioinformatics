planningFormulaEnum <- function(){
  l <- list(glmResponse="glmResponse", glmLinPred = "glmLinPred",
            parameterAdd="parameterAdd", auxAdd="auxAdd")
  return(l)
}
planningFormulaAttributeEnum <- function(){
  l <- list(response="response", noiseTerm="noiseTerm",
            linPredName="linPredName", linPredNewLine="linPredNewLine",
            predNew="predNew", predName="predName", paraName="paraName", 
            paraVector="paraVector", paraDist="paraDist", paraCenter="paraCenter",
            removePred="removePred", removePara="removePara", removeAux="removeAux",
            colorPred="colorPred", colorPara="colorPara", colorAux="colorAux",
            fontSize="fontSize", linFontSize="linFontSize")
  return(l)
}
pFAE <- planningFormulaAttributeEnum()


planningFormulaEnumClassMapping <- function(){
  l <- list(glmResponse="PFE_noiseTerm", 
            glmLinPred = "PFE_linPred",
            parameterAdd="PFE_parameterDef", 
            auxAdd="PFE_auxDef")
  return(l)
}


#' Displays formula, upper class of PlanningFormulaElements.
#' Not used by Evaluation tool, but for planning
#'
#' This is a R6 Object. To initialize a new object call 'PlanningFormula$new()'.
#' Parameter within the 'new()' call:
#' @param 
PlanningFormula <- R6Class(
   classname = "PlanningFormula", 
   inherit = ReactiveSerializationInterface,
   
   private = list(
     stateVersion = "0.1",
     ns = NULL,
     ids = 1,
     elements = list(),
     rxTrigger = NULL,
     rxTriggerChange = NULL,
     rxTriggerRemove = NULL,
     changingQueue = list()
   ),
   
   public = list(
     
     initialize = function(ns,
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       
       private$ns <- ns
     },
     
     createElement = function(planningFormulaElementType = planningFormulaEnum()){
       match.arg(planningFormulaElementType)
       element <- NULL
       if(planningFormulaElementType == planningFormulaEnum()$glmResponse){
         element <- PFE_noiseTerm$new(private$ids)
       }else if(planningFormulaElementType == planningFormulaEnum()$glmLinPred){
         element <- PFE_linPred$new(private$ns, private$ids)
       }else if(planningFormulaElementType == planningFormulaEnum()$parameterAdd){
         element <- PFE_parameterDef$new(private$ids)
       }else if(planningFormulaElementType == planningFormulaEnum()$auxAdd){
         element <- PFE_auxDef$new(private$ids)
       }
       
       private$ids <- private$ids+1
       return(element)
     },
     
     #where=c("start","end") or numeric
     addElement = function(planningFormulaElement, where="end"){
       if(!is.null(where) && !is.numeric(where) && !(where %in% c("start","end"))) where <- "end"
       if(is.numeric(where)){
         if(where > length(private$elements)) where <- length(private$elements)+1
       }else{
         if(where=="start") where <- 1
         if(where=="end") where <- length(private$elements)+1
       }
       private$elements <- list.insert(private$elements, planningFormulaElement, where)
       
       self$doTrigger("add")
     },


     changeAttribute = function(id=NULL, attrName=pFAE, attr){
       match.arg(attrName)
       
       if(attrName %in% c(pFAE$removePara, pFAE$removeAux)){
         class <- "PFE_parameterDef"
         if(attrName == pFAE$removeAux) class <- "PFE_auxDef"
         self$removeElementParameter(id=id, parameterName=attr$parameterName,
                                     class)
       }
       for(el in private$elements){
         if(el$canHandleAttribute(attrName)){
           if(is.null(id) || el$getId() == id){
             #returns list(list(side=c("left", "right"), content))
             ret <- el$changeAttribute(attrName, attr) 
             for(retElement in ret){
               l <- list(id=retElement$side, content=retElement$content)
               if(retElement$side=="left"){
                 l$id <- private$ns(paste0("elementLeft",el$getId()))
               }else if(retElement$side=="right"){
                 l$id <- private$ns(paste0("elementRight",el$getId()))
               }else if(retElement$side=="center"){
                 l$id <- private$ns(paste0("elementCenter",el$getId()))
               }
               private$changingQueue <- list.append(private$changingQueue, l)
             }
           }
         }
       }
       self$doTrigger("change")
     },
     
     #id or parameterName
     #wrapper for "PFE_auxDef" and "PFE_parameterDef" elements
     removeElementParameter = function(id = NULL, parameterName = NULL,
                                       class=c(pFAE$removePara, pFAE$removeAux)){
       if(is.null(id) && is.null(parameterName)) return()
       if(is.null(id)){
         for(el in private$elements){
           if(any(class %in% class(el))){
             if(el$getParameterName() == parameterName){
               id <- el$getId()
               break;
             }
           }
         }
       }
       self$removeElement(id)
     },
     
     #id: vector of ids
     removeElement = function(id){
       if(is.null(id)) return()
       newElements <- list()
       for(el in private$elements){
         if(!el$getId() %in% id){
           newElements <- list.append(newElements, el)
         }
       }
       private$elements <- newElements
       self$doTrigger("remove")
     },
     
     getElement = function(id){
       if(is.null(id)) return()
       for(el in private$elements){
         if(el$getId() == id) return(el)
       }
     },
     
     getPosOfPlanningFormulaElement = function(element = planningFormulaEnum()){
       match.arg(element)
       map <- planningFormulaEnumClassMapping()
       pos <- c()
       for(el in private$elements){
         if(map[[element]] %in% class(el)){
           pos <- c(pos, el$getId())
         }
       }
       return(pos)
     },
     
     getPosOfPlanningFormulaElementById = function(elementId){
       for(el_id in seq_len(length(private$elements))){
         if(private$elements[[el_id]]$getId() ==elementId) return(el_id) 
       }
     },
       
     getChangingQueue = function(){
       return(private$changingQueue)
     },
     
     clearChangingQueue = function(){
       private$changingQueue <- list()
     },
     
     display = function(){
       tagList <- list()
       for(el in private$elements){
          fR <- fluidRow(
            style="min-height:20px;", 
            title=el$getDescription(),
            column(4,style="padding-right:3px; text-align:right;",
                   tags$span(style="text-align:right; display: inline;",
                             tags$div(
                               style="display:inline;",
                               id=private$ns(paste0("elementLeft",el$getId())),
                               HTML(el$display("left"))),
                             tags$div(
                               style="display:inline;",
                               id=private$ns(paste0("elementCenter",el$getId())),
                               HTML(el$display("center")))
                             )
                   ),
            column(8,style="padding-left:0px;",
                   tags$div(
                     tags$span(
                       style="text-align:left; display: inline;",
                       id=private$ns(paste0("elementRight",el$getId())),
                       HTML(el$display("right"))
                       )
                     ))
            )
          
          tagList <- list.append(tagList, fR)
       }
       return(tagList)
     },
     
     asLatex = function(){
       latex <- "\\begin{equation}\n\t\\begin{split}\n"
       
       tagList <- list()
       for(el in private$elements){
         
         math <- toLatex(el$latex$left, paste0(" &",el$latex$center), el$latex$right)
         math <- paste0(math, collapse=" ")

         latex <- paste0(latex, "\t\t", math, "\\\\ \n")
       }
       
       latex <- paste0(latex, "\t\\end{split}\n\\end{equation}\n")
       return(latex)
     },
     
     doTrigger = function(type=c("add","change", "remove")){
       match.arg(type)
       if(type=="add"){
         self$triggerReactiveValue("rxTrigger")
       }else if(type=="change"){
         self$triggerReactiveValue("rxTriggerChange")
       }else if(type=="remove"){
         self$triggerReactiveValue("rxTriggerRemove")
       }
     },
     
     getReactive = function(type=c("add","change","remove")){
       match.arg(type)
       if(type=="add"){
         self$dependReactiveValue("rxTrigger")
       }else if(type=="change"){
         self$dependReactiveValue("rxTriggerChange")
       }else if(type=="remove"){
         self$dependReactiveValue("rxTriggerRemove")
       }
     },
     
     
     getPrivates = function(){
       return(list(ns=private$ns,
                   ids=private$ids,
                   elements=private$elements))
     },
     
     #list from getPrivates
     setPrivates = function(list){
       private$ns <- list$ns
       private$ids <- list$ids
     },
     
     #Should only called from getInstance and setInstace
     setIds = function(newIds){
       private$ids <- newIds
     },
     
     getInstance = function(){
       pF <- PlanningFormula$new(private$ns)
       pF$setIds(private$ids)
       for(el in private$elements){
         pF$addElement(el$getInstance())
       }
       return(pF)
     },
     
     #pF: object of this class
     setInstance = function(pF){
       if("PlanningFormula" %in% class(pF)){
         privates <- pF$getPrivates()
         self$setPrivates(privates)
         private$elements <- list()
         for(el in privates$elements){
           self$addElement(el$getInstance())
         }
         self$doTrigger("add")
       }else{
         stop("pF is not an object of class 'PlanningFormula'!")
       }
     },
     
     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)

         
         elementsState <- list()
         for(aa in private$elements){
           cState <- aa$getState(nextUUID)
           elementsState <- list.append(elementsState, cState)
           nextUUID <- cState$nextUUID
         }
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           ns = sub_str(private$ns(""),-1),
           ids = private$ids,
           changingQueue = private$changingQueue,
           
           #R6
           elements = elementsState
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PlanningFormula' = ret
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
       
       private$ns <- NS(state$ns)
       private$ids <- state$ids
       private$changingQueue <- state$changingQueue
       
       #R6
       private$elements <- state$elements
     },
     resetState = function(){
       if(!super$resetState()) return()
       for(aa in private$elements){
         aa$resetState()
       }
     }
     
   )
)




#' Display elements of Formula
#'
#' This is a abstract R6 Object. To initialize a new object call 'PlanningFormulaElements$new()'.
#' Parameter within the 'new()' call:
#' @param leftSide
#' @param center
#' @param rightSide
#' @param latex = list(left="",center="",right="")
#' @param description
PlanningFormulaElements <- R6Class(
  classname = "PlanningFormulaElements", 
  inherit = SerializationInterface,
  
    private = list(
      stateVersion = "0.1"
    ),
  
   public = list(
     id = NULL,
     
     handleAttributes = c(),
     leftSide = NULL, #Left side of statement y ~ N(mu,sd) --> "y"
     center = NULL, #center of statement y ~ N(mu,sd) --> "~"
     rightSide = NULL, #Whole right side as HTML "N(mu,sd)", also colorized
     latex = NULL, #Line as latex, list of 3 arguments: left, center, right
     description = NULL, #Description of this part e.g. tooltip for GLM term, predictor line etc.
     
     initialize = function(id, 
                           leftSide="", center="", rightSide="", 
                           latex=list(left="",center="",right=""),
                           description="",
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       
       self$id <- id
       self$leftSide <- leftSide
       self$center <- center
       self$rightSide <- rightSide
       self$latex <- latex
       self$description <- description
     },
     
     canHandleAttribute = function(attrName=pFAE){
       match.arg(attrName)
       return(attrName %in% self$handleAttributes)
     },
     
     display = function(side=c("left", "center", "right")){
       match.arg(side)
       
       if(side=="left"){
         return(self$leftSide)
       }else if(side=="center"){
         return(self$center)
       }else if(side=="right"){
         return(self$rightSide)
       }
     },
     
     setLine = function(type=c("left","center","right"), value){
       if(type=="left"){
         self$leftSide <- value
       }else if(type=="center"){
         self$center <- value
       }else if(type=="right"){
         self$rightSide <- value
       }
     },
     
     setLatex = function(type=c("left","center","right", "asIt"), value){
       if(type=="left"){
         self$latex$left <- value
       }else if(type=="center"){
         self$latex$center <- value
       }else if(type=="right"){
         self$latex$right <- value
       }else if(type=="asIt"){
         self$latex <- value
       }
     },
     
     setDescription = function(value){
       self$description <- value
     },
     
     getDescription = function(){
       return(self$description)
     },
     
     getId = function(){
       return(self$id)
     },
     
     #Defined by concrete classes
     changeAttribute = function(attrName, attr){},
     
     #Defined by concrete classes
     termsToVerbal = function(){},
     
     termsToVerbalCheck = function(left, center, right){
       flag <- c(!equal0(self$leftSide, left),
         !equal0(self$center, center),
         !equal0(self$rightSide, right))

       self$leftSide <- left
       self$center <- center
       self$rightSide <- right
       return(flag)
     },

     getInstance = function(){
       
     },
     
     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           id = self$id,
           handleAttributes = self$handleAttributes,
           leftSide = self$leftSide, 
           center = self$center, 
           rightSide = self$rightSide, 
           latex = self$latex, 
           description = self$description
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PlanningFormulaElements' = ret
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
       
       self$id <- state$id
       self$handleAttributes <- state$handleAttributes
       self$leftSide <- state$leftSide
       self$center <- state$center
       self$rightSide <- state$rightSide
       self$latex <- state$latex
       self$description <- state$description
     }
   )
)



#' Display elements of Formula
#' Noise term line: y ~ Normal(mu, sigma)
#' Can handle attributes: response, noiseTerm
PFE_noiseTerm <- R6Class(
  classname = "PFE_noiseTerm", 
  inherit = PlanningFormulaElements,
     
   private = list(
     stateVersion = "0.1",
     response="",
     noiseTerm="",
     fontSize=NULL
   ),


   public = list(
     
     handleAttributes = c(pFAE$response, 
                          pFAE$noiseTerm,
                          pFAE$fontSize),
     
     initialize = function(id,
                           leftSide="", center=" ~ ", rightSide="",
                           latex=list(left="", center="", right=""),
                           description="",
                           emptyState = F){

       super$initialize(id, leftSide, center, rightSide,
                        latex, description, emptyState=emptyState)
     },
     

     changeAttribute = function(attrName, attr){
       ret <- list()
       #attr:
       #String
       if(attrName == pFAE$response){
         private$response <- attr
         # self$setLatex(type="left", value=attrLatex)
         changes <- self$termsToVerbal()
         if(changes[1]) 
           ret <- list.append(ret, list(side="left", content=self$display("left")))
       }
       #attr:
       #String
       else if(attrName == pFAE$noiseTerm){

         private$noiseTerm <- attr
         # self$setLatex(type="right", value=attrLatex)
         changes <- self$termsToVerbal()
         if(changes[3]) 
           ret <- list.append(ret, list(side="right", content=self$display("right")))
       }
       #attr:
       #list(id, fontSize)
       else if(attrName == pFAE$fontSize){
         if(!is.null(attr$id) && self$id == attr$id){
           private$fontSize <- attr$fontSize
           changes <- self$termsToVerbal()
           if(changes[1]) 
             ret <- list.append(ret, list(side="left", content=self$display("left")))
           if(changes[2]) 
             ret <- list.append(ret, list(side="center", content=self$display("center")))
           if(changes[3])
             ret <- list.append(ret, list(side="right", content=self$display("right")))
         }
       }
       return(ret)
     },
     

     setPrivateValues = function(response, noiseTerm, fontSize){
       private$response <- response
       private$noiseTerm <- noiseTerm
       private$fontSize <- fontSize
     },
     
     termsToVerbal = function(){
       sp <- "<span "
       if(!is.null(private$fontSize)) sp <- paste0(sp, "style=\"font-size: ",
                                                   private$fontSize,
                                                   "px;\"")
       sp <- paste0(sp, ">")

       left <- HTML(paste0(sp, private$response,"</span>"))
       center <- HTML(paste0(sp, "~","</span>"))
       right <- HTML(paste0(sp, private$noiseTerm,"</span>"))
       
       #latex
       # self$setLatex(type="left", value=private$response)
       # self$setLatex(type="center", value="~")
       # self$setLatex(type="right", value=private$noiseTerm)
       
       return(super$termsToVerbalCheck(left, center, right))
     },
     
     getInstance = function(){
       pFE <- PFE_noiseTerm$new(self$id, 
                              self$leftSide, self$center, self$rightSide, 
                              self$latex, 
                              self$description)
       pFE$setPrivateValues(private$response, private$noiseTerm, private$fontSize)
       return(pFE)
     },
     
     getState = function(uuid){
       superRet <- super$getState(uuid)

       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           response=private$response,
           noiseTerm=private$noiseTerm,
           fontSize=private$fontSize
         )
         
         for(el_name in names(superRet$PlanningFormulaElements)){
           el <- superRet$PlanningFormulaElements[[el_name]]
           if(!el_name %in% names(ret)) ret <- list.append(ret, el, el_name)
         }
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PFE_noiseTerm' = ret
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
       
       super$setState(state)
       private$response <- state$response
       private$noiseTerm <- state$noiseTerm
       private$fontSize <- state$fontSize
     },
     resetState = function(){
       if(!super$resetState()) return()
     }
     
   )
)

#' Display elements of Formula
#' Noise term line: mu ~ b0 + b1*sex ...
#' Can handle attributes: linPredName, predNew, predName, paraName, paraVector, linPredNewLine, removePred
PFE_linPred <- R6Class(
  classname = "PFE_linPred",
  inherit = PlanningFormulaElements,
                         
   private = list(
     stateVersion = "0.1",
     linName="&mu;",
     #list(predictor="", parameter="", pos="", vector=T/F, 
     #     subId, predColor=NULL, paraColor=NULL, fontSize=NULL)
     myterms = list(), 
     subIds = 1,
     displayesTermsInNewLine = T,
     ns = NULL,
     fontSize = NULL
   ),
 
   public = list(
     
     handleAttributes = c(pFAE$linPredName,
                          pFAE$predNew,
                          pFAE$predName,
                          pFAE$paraName,                                                
                          pFAE$paraVector,
                          pFAE$linPredNewLine,
                          pFAE$removePred,
                          pFAE$colorPred,
                          pFAE$colorPara,
                          pFAE$fontSize,
                          pFAE$linFontSize),

     
     initialize = function(ns, id,
                           leftSide="&mu;", center=" = ", rightSide="",
                           latex=list(left="", center="", right=""),
                           description="",
                           emptyState = F){
       super$initialize(id, leftSide, center, rightSide,
                        latex, description, emptyState=emptyState)
       if(emptyState) return()

       private$ns <- ns
     },
     
     changeAttribute = function(attrName, attr){
       ret <- list()
       
       if(attrName == pFAE$linPredName){

         private$linName <- attr
         tmp <- self$termsToVerbal()
         changes <- tmp$changes
         tmp <- tmp$ret
         # self$setLatex(type="left", value=attrLatex)
         if(changes[1])
           ret <- list.append(tmp, list(side="left", content=self$display("left")))
       }
       #attr
       #list(predictor="", parameter="", pos="", vector=T/F)
       else if(attrName == pFAE$predNew){

         attr$subId <- private$subIds
         private$subIds <- private$subIds +1
         
         private$myterms <- list.append(private$myterms, attr)
         tmp <- self$termsToVerbal()
         changes <- tmp$changes
         tmp <- tmp$ret
         if(changes[3])
           ret <- list.append(tmp, list(side="right", content=self$display("right")))
       }
       #attr
       #list(subId=NULL, predictor=c(""), newPredictor=c(""), vector=T/F/NULL)
       else if(attrName == pFAE$predName){

         changedIds <- self$changeTermsPred(attr)
         # changedIdsContent: list(list(side=id, content), ...)
         
         tmp <- self$termsToVerbal(changedIds)
         changes <- tmp$changes
         if(changes[3])
           ret <- tmp$ret
       }
       
       #attr
       #list(subId=NULL, predictor=c("")/NULL, parameter=""/NULL, newParameter)
       else if(attrName == pFAE$paraName){

         changedIds <- self$changeTermsPara(attr)
         # tmp$ret: list(list(side=id, content), ...)

         tmp <- self$termsToVerbal(changedIds)
         changes <- tmp$changes
         if(changes[3])
           ret <- tmp$ret
       }
       
       #attr
       #list(predictor=c("")/NULL, parameter=""/NULL, newParameter)
       else if(attrName == pFAE$paraVector){

         changedIds <- self$changeTermsVector(attr)
         # tmp$ret: list(list(side=id, content), ...)
         tmp <- self$termsToVerbal(changedIds)
         changes <- tmp$changes
         if(changes[3])
           ret <- tmp$ret
       }
       
       #attr
       # list(predictor=c("")/NULL, parameter=""/NULL, vector=T/F)
       else if(attrName == pFAE$linPredNewLine){

         if(is.null(attr) || is.na(attr)){
           
         }else if(any(c(F,T,"F","T","FALSE","TRUE",FALSE,TRUE,0,1) %in% attr)){
           private$displayesTermsInNewLine <- as.logical(attr)
           tmp <- self$termsToVerbal()
           changes <- tmp$changes
           if(changes[3])
             ret <- list.append(tmp$ret, list(side="right", content=self$display("right")))
         }else{
           print(paste0("wrong format: ", attr))
         }
       }
       
       #removes predictor via subId or predictors. 
       #Predictors have to match
       #attr
       # list(subId=NULL, predictor=c("")/NULL)
       else if(attrName == pFAE$removePred){

         if(is.null(attr) || is.na(attr)){
           ret <- list()
         }else{
           newTerms <- list()
           for(i in seq_len(length(private$myterms))){
             term <- private$myterms[[i]]
             if((!is.null(attr$subId) && str_trim(attr$subId) !="" && attr$subId == term$subId)|| 
                vectorEqual(attr$predictor, term$predictor)){
               ret <- list.append(ret, 
                                  list(side=private$ns(paste0("elementRight",self$id,"_", term$subId)), 
                                       content=NULL))
             }else{
               newTerms <- list.append(newTerms, term)
             }
           }
           private$myterms <- newTerms
         }
         self$termsToVerbal()
       }
       
       #Colorize predictor (default black)
       #attr
       # list(predictor, color)
       else if(attrName == pFAE$colorPred){

         if(is.null(attr) || is.na(attr)){
           ret <- list()
         }else{
           changedIds <- c()
           for(i in seq_len(length(private$myterms))){
             term <- private$myterms[[i]]
             if(!is.null(attr$predictor) && str_trim(attr$predictor) !="" && 
                vectorEqual(attr$predictor, term$predictor)){
               private$myterms[[i]]$predColor <- attr$color
               changedIds <- c(changedIds, term$subId)
             }
           }
           
           tmp <- self$termsToVerbal(changedIds)
           changes <- tmp$changes
           if(changes[3])
             ret <- tmp$ret
         }
       }
       
       #Colorize parameter (default green)
       #attr
       # list(parameter, color)
       else if(attrName == pFAE$colorPara){

         if(is.null(attr) || is.na(attr)){
           ret <- list()
         }else{
           changedIds <- c()
           for(i in seq_len(length(private$myterms))){
             term <- private$myterms[[i]]
             if(!is.null(attr$parameter) && str_trim(attr$parameter) !="" && 
                vectorEqual(attr$parameter, term$parameter)){
               private$myterms[[i]]$paraColor <- attr$color
               changedIds <- c(changedIds, term$subId)
             }
           }
           tmp <- self$termsToVerbal(changedIds)
           changes <- tmp$changes
           if(changes[3])
             ret <- tmp$ret
         }
       }
       #attr:
       #list(id, fontSize)
       else if(attrName == pFAE$fontSize){

         if(!is.null(attr$id) && self$id == attr$id){
           private$fontSize <- attr$fontSize
           tmp <- self$termsToVerbal()
           changes <- tmp$changes
           if(changes[1])
             ret <- list.append(ret, list(side="left", content=self$display("left")))
           if(changes[2])
             ret <- list.append(ret, list(side="center", content=self$display("center")))
           if(changes[3])
             ret <- list.append(ret, list(side="right", content=self$display("right")))
         }
       }
       #attr:
       #list(id, subId, fontSize)
       else if(attrName == pFAE$linFontSize){

         if(!is.null(attr$id) && self$id == attr$id){
           changedIds <- c()
           for(i in seq_len(length(private$myterms))){
             term <- private$myterms[[i]]
             if(!is.null(attr$subId)  && attr$subId==term$subId){
               private$myterms[[i]]$fontSize <- attr$fontSize
               changedIds <- c(changedIds, term$subId)
             }
           }
           tmp <- self$termsToVerbal(changedIds)
           changes <- tmp$changes
           if(any(changes))
             ret <- tmp$ret
         }
       }
       return(ret)
     },
     
     
     getTerm = function(pos){
       for(t in private$myterms){
         if(t$pos == pos)return(t)
       }
     },
     
     #returns a list of content for given subIds, if subIds not null
     #ret: list(list(side=id, content), ...)
     termsToVerbal = function(subIds=NULL){
       ret <- list()
       newOrder <- list()
       for(term in private$myterms){
         if(is.null(term$pos) || term$pos ==""){
           newOrder <- list.append(newOrder, term)
         }else{
           pos <- term$pos
           if(pos > length(newOrder)) pos <- length(newOrder)+1
           newOrder <- list.insert(newOrder, term, pos)
         }
       }
       str <- ""
       str_latex <- ""
       for(i in seq_len(length(newOrder))){
         term <- newOrder[[i]]
         str <- paste0(str, "<span id=\"", private$ns(paste0("elementRight",self$id,"_", term$subId)),"\" ")
         str <- paste0(str, " style=\"")
         if(private$displayesTermsInNewLine){
           str <- paste0(str, "display:table; ")
         }
         str <- paste0(str, "\">")
         
         divContent <- ""
         if(!is.null(term$fontSize)){
           divContent <- paste0("<span style=\"font-size:", term$fontSize,"px;\">")
         }
         if(is.null(term$paraCol)){
           divContent <- paste0(divContent, "<span class= \"formulaParameter\">")
         }else{
           divContent <- paste0(divContent, "<span style=\"color:", term$paraColor ,";\">")
         } 
         if(term$vector){
           divContent <- paste0(divContent, tags$b(term$parameter))
         }else{
           divContent <- paste0(divContent, term$parameter)
         }
         divContent <- paste0(divContent, "</span>")
         
         if(!is.null(term$predictor) &&  !("" %in%  term$predictor)){
           preds <- paste0("<span style=\"color:", 
                           ifelse(!is.null(term$predColor), term$predColor, BAYAS_COLORS$`--formula-color-1`),
                           ";\">",term$predictor ,"</span>")
           divContent <- paste0(divContent, "&sdot;", paste0(preds, collapse=":"))
         }
         if(i != length(newOrder))
           divContent <- paste0(divContent, " + ")
         
         str <- paste0(str, divContent, "</span>")
         
         if(term$subId %in% subIds){
           retItem <- list(
             side=private$ns(
               paste0("elementRight",
                      self$id,"_", 
                      term$subId)), 
             content = divContent)
           ret <- list.append(ret, retItem)
         }
       }
       
       sp <- "<span "
       if(!is.null(private$fontSize)) sp <- paste0(sp, "style=\"font-size: ",
                                                   private$fontSize,
                                                   "px;\"")
       sp <- paste0(sp, ">")
       
       left <- HTML(paste0(sp, private$linName,"</span>"))
       center <- HTML(paste0(sp, "=","</span>"))
       right <- HTML(paste0(sp, str,"</span>"))
       
       #latex
       # self$setLatex(type="left", value=private$linName)
       # self$setLatex(type="center", value="=")
       # self$setLatex(type="right", value=str_latex)
       
       changes <- super$termsToVerbalCheck(left, center, right)
       return(list(ret=ret, changes=changes))
     },
     
     
     #Change every term, where attr$id (if not null) or attr$predictor is present
     changeTermsPred = function(attr){
       changedIds <- c()
       for(i in seq_len(length(private$myterms))){
         term <- private$myterms[[i]]
         if((!is.null(attr$subId) && attr$subId==term$subId) ||
            (!is.null(attr$predictor) && attr$predictor %in% term$predictor)){
           term$predictor <- attr$newPredictor
           private$myterms[[i]] <- term
           changedIds <- c(changedIds, term$subId)
         }
       }
       return(changedIds)
     },
     
     #Change only the term with given parameter name
     # list(subId=NULL, predictor=c("")/NULL, parameter=""/NULL, newParameter)
     changeTermsPara = function(attr){
       if(is.null(attr$newParameter)) return(c())
       changedIds <- c()
       for(i in seq_len(length(private$myterms))){
         term <- private$myterms[[i]]
         if(is.null(attr$predictor) || str_trim(attr$predictor) ==""){
           if((!is.null(attr$subId) && attr$subId==term$subId) || 
              (!is.null(attr$parameter) && attr$parameter %in% term$parameter)){
             private$myterms[[i]]$parameter <- attr$newParameter
             changedIds <- c(changedIds, term$subId)
           }
         }else{
           if(vectorEqual(attr$predictor, term$predictor)){
             private$myterms[[i]]$parameter <- attr$newParameter
             changedIds <- c(changedIds, term$subId)
           }
         }
       }
       return(changedIds)
     },
     
     #Change only the term with given parameter name
     # list(predictor=c("")/NULL, parameter=""/NULL, vector=T/F)
     changeTermsVector = function(attr){
       if(is.null(attr$vector)) return(c())
       changedIds <- c()
       for(i in seq_len(length(private$myterms))){
         term <- private$myterms[[i]]
         if(is.null(attr$predictor) || str_trim(attr$predictor) ==""){
           if(vectorEqual(attr$parameter, term$parameter)){
             private$myterms[[i]]$vector <- attr$vector
             changedIds <- c(changedIds, term$subId)
           }
         }else{
           if(vectorEqual(attr$predictor, term$predictor)){
             private$myterms[[i]]$vector <- attr$vector
             changedIds <- c(changedIds, term$subId)
           }
         }
       }
       return(changedIds)
     },
     
     
     setPrivateValues = function(myterms, linName, subIds, 
                                 displayesTermsInNewLine, ns,
                                 fontSize){
       private$myterms <- myterms
       private$linName <- linName
       private$subIds <- subIds
       private$displayesTermsInNewLine <- displayesTermsInNewLine
       private$ns <- ns
       private$fontSize <- fontSize
     },
     
     getInstance = function(){
       pFE <- PFE_linPred$new(private$ns, self$id, 
                              self$leftSide, self$center, self$rightSide, 
                              self$latex, 
                              self$description)
       
       pFE$setPrivateValues(private$myterms, private$linName, 
                            private$subIds,
                            private$displayesTermsInNewLine,
                            private$ns,
                            self$fontSize)
       return(pFE)
     },
     
     getState = function(uuid){
       superRet <- super$getState(uuid)
       
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           linName=private$linName,
           myterms = private$myterms, 
           subIds = private$subIds,
           displayesTermsInNewLine = private$displayesTermsInNewLine,
           ns = private$ns,
           fontSize = private$fontSize
         )
         for(el_name in names(superRet$PlanningFormulaElements)){
           el <- superRet$PlanningFormulaElements[[el_name]]
           if(!el_name %in% names(ret)) ret <- list.append(ret, el, el_name)
         }
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PFE_linPred' = ret
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
       
       super$setState(state)
       private$linName <- state$linName
       private$myterms <- state$myterms
       private$subIds <- state$subIds
       private$displayesTermsInNewLine <- state$displayesTermsInNewLine
       private$ns <- state$ns
       private$fontSize <- state$fontSize
     },
     resetState = function(){
       if(!super$resetState()) return()
     }
     
    )
)


#' Display parameter prior
#' Noise term line: b1 ~ Normal(mu, sigma)
#' Can handle attributes: paraName, paraVector, paraDist, paraCenter
PFE_parameterDef <- R6Class(
  classname = "PFE_parameterDef", 
  inherit = PlanningFormulaElements,
                         
   private = list(
     stateVersion = "0.1",
     parameterName = NULL,
     paraDist = NULL,
     vector = F,
     color= BAYAS_COLORS$`--formula-color-3`, 
     fontSize=NULL
   ),
   
   public = list(
     
     handleAttributes = c(pFAE$paraName, 
                          pFAE$paraVector,
                          pFAE$paraDist,
                          pFAE$paraCenter,
                          pFAE$colorPara,
                          pFAE$fontSize),
     
     initialize = function(id,
                           leftSide="", center=" ~ ", rightSide="",
                           latex=list(left="", center="", right=""),
                           description="",
                           emptyState = F){
       super$initialize(id, leftSide, center, rightSide,
                        latex, description,
                        emptyState = emptyState)
     },
     
     changeAttribute = function(attrName, attr){
       ret <- list()
       
       #attr:
       #list(predictor=NULL, parameter, newParameter)
       #if parameter is NULL (or "") the new parameter is
       #also setted. Used for init
       if(attrName == pFAE$paraName){

         if(is.null(attr$parameter) || 
            str_trim(attr$parameter) == "" || 
            private$parameterName == attr$parameter){
           private$parameterName <- attr$newParameter
           changes <- self$termsToVerbal()
           if(changes[1])
             ret <- list.append(ret, list(side="left", content=self$display("left")))
         }
       }
       
       #attr:
       #list(parameter, vector)
       else if(attrName == pFAE$paraVector){

         if(!is.null(attr$parameter) &&
            private$parameterName == attr$parameter){
           private$vector <- attr$vector
           changes <- self$termsToVerbal()
           if(changes[1])
             ret <- list.append(ret, list(side="left", content=self$display("left")))
         }
       }
       #attr:
       #list(parameter, dist)
       else if(attrName == pFAE$paraDist){

         if(!is.null(attr$parameter) &&
            private$parameterName == attr$parameter){
           private$paraDist <- attr$dist
           changes <- self$termsToVerbal()
           if(changes[3])
             ret <- list.append(ret, list(side="right", content=self$display("right")))
         }
       }
       #attr:
       #list(parameter, color)
       else if(attrName == pFAE$colorPara){

         if(!is.null(attr$parameter) &&
            private$parameterName == attr$parameter){
           changes <- self$termsToVerbal()
           if(changes[1])
             ret <- list.append(ret, list(side="left", content=self$display("left")))
         }
         
       }
       #attr:
       #list(id, fontSize)
       else if(attrName == pFAE$fontSize){

         if(!is.null(attr$id) && self$id == attr$id){
           private$fontSize <- attr$fontSize
           changes <- self$termsToVerbal()
           if(changes[1])
             ret <- list.append(ret, list(side="left", content=self$display("left")))
           if(changes[2])
             ret <- list.append(ret, list(side="center", content=self$display("center")))
           if(changes[3])
             ret <- list.append(ret, list(side="right", content=self$display("right")))
         }
       }
       #attr:
       #list(id, center)
       else if(attrName == pFAE$paraCenter){
         if(!is.null(attr$id) && self$id == attr$id){
           self$center <- attr$center
           changes <- self$termsToVerbal()
           if(changes[2])
             ret <- list.append(ret, list(side="center", content=self$display("center")))
         }
       }
       return(ret)
     },
     
     
     termsToVerbal = function(){
       sp <- "<span "
       if(!is.null(private$fontSize)) sp <- paste0(sp, "style=\"font-size: ",
                                                   private$fontSize,
                                                   "px;\"")
       sp <- paste0(sp, ">")
     
       tmpLeftSide <- ""
       if(private$vector){
         tmpLeftSide <- paste(tags$b(HTML(private$parameterName)))
       }else{
         tmpLeftSide <- private$parameterName
       }
       if(is.null(private$color)) private$color <- BAYAS_COLORS$`--formula-color-3`
       tmpLeftSide <- paste0(
         tags$span(
           style=paste0("color:",private$color,";"), 
           HTML(tmpLeftSide)))
       
       left <- HTML(paste0(sp, tmpLeftSide,"</span>"))
       center <- HTML(paste0(sp, self$center,"</span>"))
       right <- HTML(paste0(sp, private$paraDist,"</span>"))
       
       #latex
       # self$setLatex(type="left", value=private$response)
       # self$setLatex(type="center", value="~")
       # self$setLatex(type="right", value=private$noiseTerm)
       
       return(super$termsToVerbalCheck(left, center, right))
     },
     

     getParameterName = function(){
       return(private$parameterName)
     },
     
     
     setPrivateValues = function(parameterName, paraDist, 
                                 vector, color, fontSize){
       private$parameterName <- parameterName
       private$paraDist <- paraDist
       private$vector <- vector
       private$color <- color
       private$fontSize <- fontSize
     },
     
     getInstance = function(){
       pFE <- PFE_parameterDef$new(self$id, 
                                   self$leftSide, self$center, self$rightSide, 
                                   self$latex,
                                   self$description)
       pFE$setPrivateValues(private$parameterName, private$paraDist, 
                            private$vector, private$color, 
                            private$fontSize)
       return(pFE)
     },
     
     getState = function(uuid){
       superRet <- super$getState(uuid)
       
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)
         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           parameterName = private$parameterName,
           paraDist = private$paraDist,
           vector = private$vector,
           color = private$color,
           fontSize = private$fontSize
         )
         for(el_name in names(superRet$PlanningFormulaElements)){
           el <- superRet$PlanningFormulaElements[[el_name]]
           if(!el_name %in% names(ret)) ret <- list.append(ret, el, el_name)
         }
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'PFE_parameterDef' = ret
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
       
       super$setState(state)
       private$parameterName <- state$parameterName
       private$paraDist <- state$paraDist
       private$vector <- state$vector
       private$color <- state$color
       private$fontSize <- state$fontSize
     },
     resetState = function(){
       if(!super$resetState()) return()
     }
     
   )
)


#' Display aux parameter prior
#' Noise term line: sd ~ Exponential(lambda)
#' Can handle attributes: paraName, paraVector, paraDist, paraCenter
PFE_auxDef <- R6Class(
  classname = "PFE_auxDef", 
  inherit = PlanningFormulaElements,
                            
  private = list(
    stateVersion = "0.1",
    parameterName = NULL,
    paraDist = NULL,
    vector = F,
    color= BAYAS_COLORS$`--formula-color-3`,
    fontSize=NULL
  ),
  
  public = list(
    
    handleAttributes = c(pFAE$paraName, 
                         pFAE$paraVector,
                         pFAE$paraDist,
                         pFAE$paraCenter,
                         pFAE$colorAux,
                         pFAE$fontSize),
    
    initialize = function(id,
                          leftSide="", center=" ~ ", rightSide="",
                          latex=list(left="", center="", right=""),
                          description="",
                          emptyState = F){
      super$initialize(id, leftSide, center, rightSide,
                       latex, description,
                       emptyState = emptyState)
    },
    
    changeAttribute = function(attrName, attr){
      ret <- list()
      
      #attr:
      #list(predictor=NULL, parameter, newParameter)
      #if parameter is NULL (or "") the new parameter is
      #also setted. Used for init
      if(attrName == pFAE$paraName){

        if(is.null(attr$parameter) || 
           str_trim(attr$parameter) == "" || 
           private$parameterName == attr$parameter){
          private$parameterName <- attr$newParameter
          # self$setLatex(type="left", value=attrLatex)
          changes <- self$termsToVerbal()
          if(changes[1])
            ret <- list.append(ret, list(side="left", content=self$display("left")))
        }
      }
      
      #attr:
      #list(parameter, vector)
      else if(attrName == pFAE$paraVector){
        
        if(!is.null(attr$parameter) &&
           private$parameterName == attr$parameter){
          private$vector <- attr$vector
          # if(private$vector){
          #   self$setLatex(type="left", value=wordToLatexUnderline(attrLatex))
          # }else{
          #   self$setLatex(type="left", value=attrLatex)
          # }
          changes <- self$termsToVerbal()
          if(changes[1])
            ret <- list.append(ret, list(side="left", content=self$display("left")))
        }
      }
      #attr:
      #list(parameter, dist)
      else if(attrName == pFAE$paraDist){

        if(!is.null(attr$parameter) &&
           private$parameterName == attr$parameter){
          private$paraDist <- attr$dist
          # self$setLatex(type="right", value=attrLatex)
          changes <- self$termsToVerbal()
          if(changes[3])
            ret <- list.append(ret, list(side="right", content=self$display("right")))
        }
      }
      #attr:
      #list(parameter, color)
      else if(attrName == pFAE$colorPara){

        if(!is.null(attr$parameter) &&
           private$parameterName == attr$parameter){
          changes <- self$termsToVerbal()
          if(changes[1])
            ret <- list.append(ret, list(side="left", content=self$display("left")))
        }
        
      }
      #attr:
      #list(id, fontSize)
      else if(attrName == pFAE$fontSize){

        if(!is.null(attr$id) && self$id == attr$id){
          private$fontSize <- attr$fontSize
          changes <- self$termsToVerbal()
          if(changes[1])
            ret <- list.append(ret, list(side="left", content=self$display("left")))
          if(changes[2])
            ret <- list.append(ret, list(side="center", content=self$display("center")))
          if(changes[3])
            ret <- list.append(ret, list(side="right", content=self$display("right")))
        }
      }       
      #attr:
      #list(id, center)
      else if(attrName == pFAE$paraCenter){
        if(!is.null(attr$id) && self$id == attr$id){
          self$center <- attr$center
          changes <- self$termsToVerbal()
          if(changes[2])
            ret <- list.append(ret, list(side="center", content=self$display("center")))
        }
      }
      return(ret)
    },
   
    
    termsToVerbal = function(){
      sp <- "<span "
      if(!is.null(private$fontSize)) sp <- paste0(sp, "style=\"font-size: ",
                                                  private$fontSize,
                                                  "px;\"")
      sp <- paste0(sp, ">")
      
      tmpLeftSide <- ""
      tmpLeftSideLatex <- ""
      if(private$vector){
        tmpLeftSide <- paste(tags$b(HTML(private$parameterName)))
        tmpLeftSideLatex <- wordToLatexUnderline(private$parameterName)
      }else{
        tmpLeftSide <- private$parameterName
        tmpLeftSideLatex <- private$parameterName
      }
      if(is.null(private$color)) private$color <- BAYAS_COLORS$`--formula-color-3`
      tmpLeftSide <- paste0(
        tags$span(
          style=paste0("color:",private$color,";"), 
          HTML(tmpLeftSide)))
      
      left <- HTML(paste0(sp, tmpLeftSide,"</span>"))
      center <- HTML(paste0(sp, self$center,"</span>"))
      right <- HTML(paste0(sp, private$paraDist,"</span>"))    
    
      return(super$termsToVerbalCheck(left, center, right))
    },
    
    getParameterName = function(){
      return(private$parameterName)
    },
    
    
    setPrivateValues = function(parameterName, vector, 
                                color, fontSize){
      private$parameterName <- parameterName
      private$vector <- vector
      private$color <- color
      private$fontSize <- fontSize
    },
    
    getInstance = function(){
      pFE <- PFE_auxDef$new(self$id, 
                            self$leftSide, self$center, self$rightSide, 
                            self$latex,
                            self$description)
      pFE$setPrivateValues(private$parameterName, 
                           private$vector, private$color,
                           private$fontSize)
      return(pFE)
    },
    
    getState = function(uuid){
      superRet <- super$getState(uuid)
      
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          parameterName = private$parameterName,
          paraDist = private$paraDist,
          vector = private$vector,
          color = private$color,
          fontSize = private$fontSize
        )
        for(el_name in names(superRet$PlanningFormulaElements)){
          el <- superRet$PlanningFormulaElements[[el_name]]
          if(!el_name %in% names(ret)) ret <- list.append(ret, el, el_name)
        }
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'PFE_auxDef' = ret
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
      
      super$setState(state)
      private$parameterName <- state$parameterName
      private$paraDist <- state$paraDist
      private$vector <- state$vector
      private$color <- state$color
      private$fontSize <- state$fontSize
    },
    resetState = function(){
      if(!super$resetState()) return()
    }
    
  )
)
