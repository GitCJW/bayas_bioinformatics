ModelCreatingDataFormula <- R6Class(
  classname = "ModelCreatingDataFormula", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    mcd = NULL, 
    
    responseName = NULL,
    responseDist = NULL,
    responseLink = NULL,
    
    responseFormulaId = NULL,
    linearPredictorId = NULL,
    predictorFormulaIdMapping = list(), #entry: predId, formulaId, formulaSubId
    parameterFormulaIdMapping = list(), #entry: paraId, formulaId, (formulaSubId)
    
    parameterIdsForResponse = c(), #put in id of para directly when aux parameters are created
    
    mode = "generative", # c("generative","prior")
    predictorLineType = "Multiple", # c("Multiple","Single")
    rxTriggerFormula = NULL,
    rxTriggerFormulaUI = NULL
  ),
  
  
  public = list(
   
   initialize = function(mcd,
                         emptyState = F){
     super$initialize()
     if(emptyState) return()
     
     private$mcd <- mcd
   },
   
   setMcd = function(mcd){
     private$mcd <- mcd
   },
   getMcd = function(){
     private$mcd
   },
   
   
   addPredictorFormulaIdMapping = function(predId, formulaId, name=NULL){
     private$predictorFormulaIdMapping <- list.append(private$predictorFormulaIdMapping, c(predId, formulaId), name)
   },
   removePredictorFormulaIdMapping = function(predId){
     newList <- list()
     for(i in private$predictorFormulaIdMapping){
       if(i[1]!=predId)
         newList <- list.append(newList, i)
     }
     private$predictorFormulaIdMapping <- newList
   },
   getPredictorFormulaIdMapping = function(name=NULL, startsWith=NULL, id=NULL){
     if(!is.null(startsWith)){
       names <- names(private$predictorFormulaIdMapping)
       l <- list()
       for(na in names){
         if(startsWith(na, startsWith)) l <- list.append(l, private$predictorFormulaIdMapping[[na]], na)
       }
       return(l)
     }
     if(!is.null(name)) return(private$predictorFormulaIdMapping[[name]])
     if(!is.null(id)){
       l <- list()
       for(map in private$predictorFormulaIdMapping){
         if(map[1]==id) 
           l <- list.append(l, map)
       }
       return(l)
     }
     return(private$predictorFormulaIdMapping)
   },
   isPredictorUsedInFormula = function(predId){
     for(i in private$predictorFormulaIdMapping){
       if(i[1]==predId) return(T)
     }
     return(F)
   },
   getUnusedPredictorsInFormula = function(){
     unused <- c()
     for(i in private$predictorFormulaIdMapping){
       if(!i[1] %in% private$mcd$getPredictorIds()) unused <- c(unused, i[1])
     }
     return(unused)
   },
   
   addParameterFormulaIdMapping = function(paraId, formulaId, name=NULL){
     private$parameterFormulaIdMapping <- list.append(private$parameterFormulaIdMapping, c(paraId, formulaId), name)
   },
   removeParameterFormulaIdMapping = function(paraId){
     newList <- list()
     for(i in private$parameterFormulaIdMapping){
       if(i[1]!=paraId)
         newList <- list.append(newList, i)
     }
     private$parameterFormulaIdMapping <- newList
   },
   getParameterFormulaIdMapping = function(name=NULL, startsWith=NULL, id=NULL){
     if(!is.null(startsWith)){
       names <- names(private$parameterFormulaIdMapping)
       l <- list()
       for(na in names){
         if(startsWith(na, startsWith)) l <- list.append(l, private$parameterFormulaIdMapping[[na]], na)
       }
       return(l)
     }
     if(!is.null(name)) return(private$parameterFormulaIdMapping[[name]])
     if(!is.null(id)){
       ret <- list()
       for(map in private$parameterFormulaIdMapping){
         if(map[1]==id) 
           ret <- list.append(ret, map)
       }
       return(ret)
     }
     return(private$parameterFormulaIdMapping)
   },
   isParameterUsedInFormula = function(paraId){
     for(i in private$parameterFormulaIdMapping){
       if(i[1]==paraId) return(T)
     }
     return(F)
   },
   getUnusedParametersInFormula = function(){
     unused <- c()
     for(i in private$parameterFormulaIdMapping){
       if(!i[1] %in% private$mcd$getParameterIds()) unused <- c(unused, i[1])
     }
     return(unused)
   },
   
   addParameterIdsForResponse = function(id){
     if(!id %in% private$parameterIdsForResponse) 
       private$parameterIdsForResponse <- c(private$parameterIdsForResponse, id)
   },
   removeParameterIdsForResponse = function(id=NULL){
     if(is.null(id)){
       private$parameterIdsForResponse <- c()
     }else{
       private$parameterIdsForResponse <- private$parameterIdsForResponse[private$parameterIdsForResponse != id]
     }
   },
   getParameterIdsForResponse = function(){
     return(private$parameterIdsForResponse)
   },
   
   
   getInsertId = function(parameter){

     insertId <- "end"
     if(length(private$parameterFormulaIdMapping) == 1){
       return(insertId)
     }
     
     interceptPos <- "end" #lastIntercept
     paraPos <- "end" #lastPara
     auxPos <- "end" #lastAux
     alt <- Inf
     
     formula <- private$mcd$getFormulaObject()
     for(paraMap in private$parameterFormulaIdMapping){
       
       para <- private$mcd$getParameter(paraMap[1])
       if(para$getId() == parameter$getId()) next
       element <- formula$getElement(paraMap[2])
       if(!class(element)[1] %in% c("PFE_parameterDef", "PFE_auxDef")) next
       if(para$getPriority() == parameter$getPriority()){
         pos <- formula$getPosOfPlanningFormulaElementById(element$getId())
         if(para$getType() == "intercept"){
           if(interceptPos[1] == "end"){
             interceptPos <- c(pos,pos)
           }else{
             interceptPos <- c(min(pos, interceptPos[1]),max(pos, interceptPos[2]))
           }
         }else if(para$getType() == "predictor"){
           if(paraPos[1] == "end"){
             paraPos <- c(pos,pos)
           }else{
             paraPos <- c(min(pos, paraPos[1]),max(pos, paraPos[2]))
           }
         }else if(para$getType() == "aux"){
           if(auxPos[1] == "end"){
             auxPos <- c(pos,pos)
           }else{
             auxPos <- c(min(pos, auxPos[1]),max(pos, auxPos[2]))
           }
         }
       }else if(para$getPriority() > parameter$getPriority()){
         alt <- min(alt, 
                    formula$getPosOfPlanningFormulaElementById(element$getId()))
       }
     }
     
     if(parameter$getType()=="intercept"){
       if(interceptPos[1] != "end") return(interceptPos[2]+1)
       if(paraPos[1] != "end") return(paraPos[1])
       if(auxPos[1] != "end") return(auxPos[1])
     }else if(parameter$getType()=="predictor"){
       if(paraPos[1] != "end") return(paraPos[2]+1)
       if(auxPos[1] != "end") return(auxPos[1])
     }else{
       if(auxPos[1] != "end") return(auxPos[2]+1)
     }
     return("end")
   },
   
   setMode = function(mode=c("Generative","Prior")){
     match.arg(mode)
     if(!equal(private$mode,mode)){
       private$mode <- mode
       self$doTrigger()
       self$triggerReactiveValue("rxTriggerFormulaUI")
     }
   },
   
   getMode = function(){
     return(private$mode)
   },
   
   
   setPredictorLineType = function(predictorLineType=c("Multiple","Single")){
     match.arg(predictorLineType)
     if(!equal(private$predictorLineType,predictorLineType)){
       private$predictorLineType <- predictorLineType
       formula <- private$mcd$getFormulaObject()
       formula$changeAttribute(id=private$linearPredictorId, attrName=pFAE$linPredNewLine,
                               attr=list(vector=predictorLineType!="Single"))
       self$triggerReactiveValue("rxTriggerFormulaUI")
     }
   },
   
   getPredictorLineType = function(){
     return(private$predictorLineType)
   },
   

   
   #formula: Object of 'PlanningFormula'
   updateFormula = function(){
     
     formula <- private$mcd$getFormulaObject()
     
     pFAE <- planningFormulaAttributeEnum()
     pFE <- planningFormulaEnum()

     #update response
     response <- private$mcd$getMcdResponse()
     responseName = response$getName()
     if(!is.null(responseName) && responseName=="") responseName <- "Response"
     responseDist = response$getDist()
     responseLink = response$getLink()
     
     if(is.null(private$responseFormulaId) && is.null(responseDist)) return()
     
     if(is.null(private$responseFormulaId)){
       element <- formula$createElement(pFE$glmResponse)
       element$setLatex(type="center", value="~")
       formula$addElement(element, where="start")
       private$responseFormulaId <- element$getId()
     }
     
     if(is.null(private$linearPredictorId) && !is.null(responseLink)){
       element <- formula$createElement(pFE$glmLinPred)
       element$setLatex(type="center", value="=")
       formula$addElement(element, where=2)
       private$linearPredictorId <- element$getId()
     }
     
     #response name
     if(!equal0(private$responseName, responseName)){
       private$responseName <- responseName
       formula$changeAttribute(id=private$responseFormulaId, attrName=pFAE$response, attr=responseName)
     }
     
     #response dist
     if(!equal0(private$responseDist, responseDist) || 
        private$responseDist %in% c(planningDistribtionsEnum("predictor")$Binomial,
                                    planningDistribtionsEnum("predictor")$Beta_Binomial)){
       private$responseDist <- responseDist
       
       paras <- getDistributionParameterForFormula(responseDist, T)
       if(responseDist %in% c(planningDistribtionsEnum("predictor")$Binomial,
                              planningDistribtionsEnum("predictor")$Beta_Binomial)){
         paras$fixPara$color <- BAYAS_COLORS$`--formula-color-1`
         
         ovs <- private$mcd$getOtherVariables()
         for(ov in ovs){
           if(!is.null(ov$getSpecialRole()) && 
              ov$getSpecialRole() == "BinomN"){
             paras$fixPara$name <- ov$getName()
             break
           }
         }
       }
       
       dString <- getDistributionNameForFormula(responseDist, BAYAS_COLORS$`--formula-color-2`, paras)
       dStringLatex <- getDistributionNameAsLatex(responseDist, paras)
       formula$changeAttribute(id=NULL, attrName=pFAE$noiseTerm, attr=dString)
     }
     
     #response link (in linpred)
     if(!equal0(private$responseLink, responseLink)){
       private$responseLink <- responseLink
       
       paras <- getDistributionParameterForFormula(responseDist, T)
       para <- paras[["mean"]]
       if(!is.null(para)){
         left <- getLinkFunctionTerm(responseLink, getParaSpanWithColor(para))
         leftLatex <- getLinkFunctionTerm(responseLink, para$name)
         formula$changeAttribute(id=private$linearPredictorId, attrName=pFAE$linPredName, 
                                 attr=tags$span(HTML(left)))
       }
     }

     
     ##update predictors
     #remove predictors
     unused <- self$getUnusedPredictorsInFormula()
     for(un in unused){
       removeId <- self$getPredictorFormulaIdMapping(id=un)
       for(r in removeId){
         if(length(r)==3){
           formula$changeAttribute(id=r[2], attrName=pFAE$removePred,
                                   attr=list(subId=r[3]))
         }else{
           formula$removeElement(r[2])
         }
       }
       self$removePredictorFormulaIdMapping(un)
     }
     removePara <- c()
     #remove redundant predictors
     for(i in private$predictorFormulaIdMapping){
       predId <- i[[1]]
       pred <- private$mcd$getPredictor(predId)
       if(pred$getAmIRedundant()){
         removeId <- self$getPredictorFormulaIdMapping(id=predId)
         for(r in removeId){
           if(length(r)==3){
             formula$changeAttribute(id=r[2], attrName=pFAE$removePred,
                                     attr=list(subId=r[3]))
           }else{
             formula$removeElement(r[2])
           }
         }
         removePara <- c(removePara, private$mcd$getParameterOfPredictor(predId)$getId())
         self$removePredictorFormulaIdMapping(predId)
       }
     }
     
     #add/change predictors
     for(pred in private$mcd$getPredictors()){
       if(pred$getAmIRedundant()) next
       predPara <- private$mcd$getParameterOfPredictor(pred$getId())
       if(self$isPredictorUsedInFormula(pred$getId())){
         #change
         predParaAttr <- list(name= predPara$getName(),
                          vector=predPara$isVector(),
                          priority=predPara$getPriority()) 
         
         if(pred$getType()=="predictor"){
           attr <- list()
           ids <- self$getPredictorFormulaIdMapping(id=pred$getId())
           for(id in ids){
             if(length(id) != 3) stop("not implemented")
             attr <- list(subId = id[3],
                          predictor=NULL,
                          newPredictor="",
                          vector=predParaAttr$vector)
             if(pred$isSlope()){
               attr$newPredictor <- paste0(pred$getNumerics(), collapse="&middot;")
             }
             id <- NULL
             if(pred$getPredLine() == 1)
               id <- formula$getPosOfPlanningFormulaElement(pFE$glmLinPred)[1]
             formula$changeAttribute(id=id, attrName=pFAE$predName, attr=attr)
           }
         }

       }else{
         #add
         nextPos <- length(private$mcd$getPredictors())+1
         predParaAttr <- list(name= predPara$getName(),
                          vector=predPara$isVector(),
                          priority=predPara$getPriority()) 
         
         if(pred$getType()=="intercept"){
           nextPos <- 1
           attr <- list(predictor="", parameter=predParaAttr$name, 
                        pos=nextPos, vector=predParaAttr$vector)
         }else if(pred$getType()=="predictor"){
           if(pred$isSlope()){
             attr <- list(predictor=paste0(pred$getNumerics(), collapse="&middot;"), parameter=predParaAttr$name, 
                          pos=nextPos, vector=predParaAttr$vector)
           }else{
             attr <- list(predictor="", parameter=predParaAttr$name, 
                          pos=nextPos, vector=predParaAttr$vector)
           }
         }
         
         insertIn <- NULL
         if(pred$getPredLine() == 1) 
           insertIn <- formula$getPosOfPlanningFormulaElement(pFE$glmLinPred)[1]
         formula$changeAttribute(id=insertIn, attrName=pFAE$predNew, attr=attr)

         linPred <- formula$getElement(insertIn)
         term <- linPred$getTerm(nextPos)
         
         self$addPredictorFormulaIdMapping(pred$getId(), c(insertIn,term$subId))
         self$addParameterFormulaIdMapping(predPara$getId(), c(insertIn,term$subId))
       }
     }
     
     
     
     ##update parameters
     #remove paras
     unused <- self$getUnusedParametersInFormula()
     for(un in unused){
       removeId <- self$getParameterFormulaIdMapping(id=un)
       for(r in removeId){
         if(length(r)==3)next
         formula$removeElement(r[2])
         self$removeParameterFormulaIdMapping(un)
       }
     }
     #remove redundant parameter
     for(i in removePara){
       removeId <- self$getParameterFormulaIdMapping(id=i)
       for(r in removeId){
         if(length(r)==3)next
         formula$removeElement(r[2])
         self$removeParameterFormulaIdMapping(i)
       }
     }
     
     #add/change paras
     for(para in private$mcd$getParameters()){
       element <- NULL
       
       #Create sampling statement, if not existing (para ~ Normal(...))
       add <- !self$isParameterUsedInFormula(para$getId())
       if(!add){
         add <- T
         paraId <- para$getId()
         elId <- self$getParameterFormulaIdMapping(id=paraId)
         for(id in elId){
           element <- formula$getElement(id[2])
           if(class(element)[1] %in% c("PFE_parameterDef","PFE_auxDef")) add <- F
         }
       }
       pred <- private$mcd$getPredictor(para$getPredId())
       if(!is.null(pred) && pred$getAmIRedundant()){
         add <- F
       }
       if(add){
         if(para$getType() %in% c("aux","intercept","predictor")){
           element <- NULL
           if(para$getType() == "aux"){
             element <- formula$createElement(pFE$auxAdd)
             element$setLatex(type="center", value=element$center)
           }else{
             element <- formula$createElement(pFE$parameterAdd)
             element$setLatex(type="center", value=element$center)
           }
           
           attr <- list(parameter=NULL, newParameter=para$getName())
           element$changeAttribute(attrName=pFAE$paraName, attr=attr)
           element$changeAttribute(attrName=pFAE$paraVector, 
                                   attr=list(parameter=para$getName(), 
                                             vector=para$isVector()))
           
           
           subs <- para$getSubs()
           dist <- c()
           distLatex <- ""
           for(sub_id in seq_along(subs)){
             sub <- subs[[sub_id]]
             tmpDist <- ""
             if(private$mode == "Prior"){
               tmpDist <- sub$getPrior()
             }else{
               tmpDist <- sub$getValueDistribution()
             }
             
             #dist from pDistEnum
             #distColor=string
             #para: list of list(name=string/value, vector=bool, color=string)
             
             distPara <- list()
             for(p in tmpDist$getParameter()){
               color <- BAYAS_COLORS$`--formula-color-4`
               if(sub$getAmIRedundant()) color <- BAYAS_COLORS$`--formula-color-5`
               newPara <- list(name=p$value, vector=F, color=color)
               distPara <- list.append(distPara, newPara)
             }
             
             subDist <- ""
             if(sub$getAmIRedundant()){
               next
             }else{
               subDist <- getDistributionNameForFormula(dist=tmpDist$getName(), 
                                                        distColor=BAYAS_COLORS$`--formula-color-2`, 
                                                        para=distPara)
               distLatex <- paste0(distLatex, getDistributionNameAsLatex(dist=tmpDist$getName(), 
                                                                         para=distPara))
             }
             dist <- c(dist, subDist)
           }
  
           distString <- ""
           if(length(dist)==1 && length(subs) > 1){
             distString <- paste0("<span>{ ",dist[1]," }</span>")
           }else if(length(dist)==1){
             distString <- dist
           }else if(length(dist)==2){
             distString <- paste0("<span>{ ",dist[1], " , ", dist[2], " }</span>")
           }else{
             distString <- paste0("<span>{ ",dist[1], " , ", dist[2], " , ... }</span>")
           }
           
           attr <- list(parameter=para$getName(), dist=distString)
           
           element$changeAttribute(attrName=pFAE$paraDist, attr=attr)
           
         }else if(para$getType() == "fixed"){
           stop("Not implemented")
           # element <- formula$createElement(pFE$parameterAdd)
           # element$changeAttribute(attrName=pFAE$paraName, attr=list(parameter=NULL, newParameter=para$getName()))
           # element$changeAttribute(attrName=pFAE$paraCenter, attr=list(id=element$getId(), center="="))
           # element$changeAttribute(attrName=pFAE$paraDist, attr=list(parameter=para$getName(), dist = para$getSubs()[[1]]$getValue()))
         }
         
         if(!is.null(element)){
           formula$addElement(element, where=self$getInsertId(para))
           self$addParameterFormulaIdMapping(para$getId(), element$getId())
         }
       }
       
  
       #Change content of elements
       paraId <- para$getId()
       elId <- self$getParameterFormulaIdMapping(id=paraId)
       for(id in elId){

         element <- formula$getElement(id[2])
         
         if(para$getType() %in% c("aux","predictor","intercept")){
           if(class(element)[1] == "PFE_linPred"){
             
             attr <- list(subId=last(id), predictor=NULL, parameter=NULL, 
                          newParameter=para$getName())

             formula$changeAttribute(id=element$getId(), attrName=pFAE$paraName, 
                                     attr=attr)

           }else if(class(element)[1] %in% c("PFE_auxDef","PFE_parameterDef")){
             attr <- list(parameter=NULL, newParameter=para$getName())
             
             formula$changeAttribute(id=element$getId(), attrName=pFAE$paraName, 
                                     attr=attr)
             formula$changeAttribute(attrName=pFAE$paraVector, 
                                     attr=list(parameter=para$getName(), 
                                               vector=para$isVector()))
            
             if(para$hasOnlyFixedValueSubs(type=tolower(private$mode))){
               formula$changeAttribute(id=element$getId(), attrName=pFAE$paraCenter, 
                                       attr=list(id=element$getId(), center="="))
             }else{
               formula$changeAttribute(id=element$getId(), attrName=pFAE$paraCenter, 
                                       attr=list(id=element$getId(), center="~"))
             }
             

             subs <- para$getSubs()
             dist <- c()
             distLatex <- ""
             for(sub_id in seq_along(subs)){
               sub <- subs[[sub_id]]
               tmpDist <- ""
               if(private$mode == "Prior"){
                 tmpDist <- sub$getPrior()
               }else{
                 tmpDist <- sub$getValueDistribution()
               }
               
               #dist from pDistEnum
               #distColor=string
               #para: list of list(name=string/value, vector=bool, color=string)
               
               distPara <- list()
               for(p in tmpDist$getParameter()){
                 color <- BAYAS_COLORS$`--formula-color-4`
                 if(sub$getAmIRedundant()) color <- BAYAS_COLORS$`--formula-color-5`
                 newPara <- list(name=p$value, vector=F, color=color)
                 distPara <- list.append(distPara, newPara)
               }

               subDist <- ""
               if(sub$getAmIRedundant()){
                 next
               }else{
                 subDist <- getDistributionNameForFormula(dist=tmpDist$getName(), 
                                                          distColor=BAYAS_COLORS$`--formula-color-2`, 
                                                          para=distPara)
                 
                 distLatex <- paste0(distLatex, getDistributionNameAsLatex(dist=tmpDist$getName(), 
                                                                           para=distPara))
               }
               dist <- c(dist,subDist)
             }
             
             distString <- ""
             if(length(dist)==1 && length(subs) > 1){
               distString <- paste0("<span>{ ",dist[1]," }</span>")
             }else if(length(dist)==1){
               distString <- dist
             }else if(length(dist)==2){
               distString <- paste0("<span>{ ",dist[1], " , ", dist[2], " }</span>")
             }else{
               distString <- paste0("<span>{ ",dist[1], " , ", dist[2], " , ... }</span>")
             }
             
             attr <- list(parameter=para$getName(), dist=distString)
             
             formula$changeAttribute(id=element$getId(), attrName=pFAE$paraDist, attr=attr)
           }
           
         }else if(para$getType() == "fixed"){
           stop("Not implemented")
           # formula$changeAttribute(id=element$getId(), attrName=pFAE$paraName, 
           #                         attr=list(parameter=NULL, newParameter=new))
           # 
           # formula$changeAttribute(id=element$getId(), attrName=pFAE$paraDist, 
           #                         attr=list(parameter=para$getName(), 
           #                                   dist = new))
         }
       }
      
     }
   },
   
   getUsedPredictors = function(){
     preds <- list()
     for(map in private$predictorFormulaIdMapping){
       pred <- private$mcd$getPredictor(map[1])
       preds <- list.append(preds, pred)
     }
     return(preds)
   },
   
   
   #returns an empty compiled brms model
   buildRstanFormula = function(seed){

     compiled_model <- tryCatch({

       link <- self$linkMapping(private$responseLink)
       family <- self$distFramilyMapping(private$responseDist, link)
       
       if(length(private$linearPredictorId[1]) == 0) return(NULL)
       
       
       #data
       dd <- private$mcd$getData()
       # dd[[1]] <- 1:(dim(dd)[1])
       
       # In a glm 'linPred1Id' is typical the expectation value
       interTerms <- c()
       linPred1Id <- private$linearPredictorId[1]
       linPred1_revised <- ""
       useIntercept1 <- F
       for(pred in private$predictorFormulaIdMapping){
         if(pred[2] == linPred1Id){
           predictor <- private$mcd$getPredictor(pred[1])
           if(predictor$getType() == "intercept"){
             useIntercept1 <- T
           }else{
             
             #### will be replaced
             if(linPred1_revised != ""){
               linPred1_revised <- paste0(linPred1_revised, " + ")
             } 
             predName <- predictor$getName()
             if(grepl(":", predName)){
               interTerms <- c(interTerms, predName)
             }
             
             
             #### replace by
             parameter <- private$mcd$getParameterOfPredictor(pred[1])
             predName <- predictor$getName()
             subs <- parameter$getSubs() 
             subNames <- names(subs)
             
             if(!(length(subs)==1 && names(subs)[[1]] == predictor$getName())){
               for(s_i in seq_along(subs)){
                 if(!subs[[s_i]]$getAmIRedundant()){
                   newCol <- paste0(predName,"..",subNames[[s_i]])
                   newCol <- gsub(":",".",newCol)
                   newCol2 <- newCol
                   
                   splittedPredName <- str_split(predName, ":")[[1]]
                   splittedSubName <- str_split(subNames[[s_i]], ":")[[1]]
                   
                   dd[[newCol]] <- 1
                   
                   for(sp_i in seq_along(splittedPredName)){
                     sP <- splittedPredName[[sp_i]]
                     sS <- splittedSubName[[sp_i]]
                     
                     oV <- private$mcd$getOtherVariable(predictor$getOVIds()[sp_i])
                     if(oV$getType()=="categorical"){
                       dd[[newCol]][dd[[sP]]!=sS] <- 0
                     }else{
                       newCol2 <- paste0(sP, ":", newCol2)
                     }
                   }
                   linPred1_revised <- paste0(linPred1_revised, newCol2, " + ")
                 }
               }
               linPred1_revised <- substr(linPred1_revised, 1, 
                                          nchar(linPred1_revised) - 3)
             }else{
               linPred1_revised <- paste0(linPred1_revised, predName)
             }
           }
         }
       }
       #... potential 'linPred2Id' for e.g. the auxiliary parameter
       #...
       
       
       
       if(!useIntercept1){
         if(linPred1_revised != ""){
           linPred1_revised <- paste0("-1 + ", linPred1_revised)
         }else{
           linPred1_revised <- paste0("-1")
         }
       }else{
         if(linPred1_revised != ""){
           linPred1_revised <- paste0("1 + ", linPred1_revised)
         }else{
           linPred1_revised <- paste0("1 ")
         }
       }
       
       dists <- planningDistribtionsEnum("all")
       if(private$responseDist %in% c(dists$Binomial, dists$Beta_Binomial)){
         
         varN <- "N"
         oVs <- private$mcd$getOtherVariables()
         for(ov in oVs){
           if(!is.null(ov$getSpecialRole()) &&
              ov$getSpecialRole() == "BinomN"){
             varN <- ov$getName()
             break
           } 
         }
         
         
         linPred1_revised <- paste0(private$responseName , " | trials(", varN,
                                    ") ~ ", linPred1_revised)
         
         dd[[varN]] <- as.numeric(dd[[varN]])
       }else{
         linPred1_revised <- paste0(private$responseName, " ~ ", linPred1_revised)
       }
       
       priors <- get_prior(formula=linPred1_revised, data=dd, family=family)
       
       terms <- linPred1_revised |> 
         formula(data=dd) |> 
         terms() |> 
         attr("term.labels")
       
       #set priors
       allParameters <- private$mcd$getParameters()
       for(para in allParameters){
         paraType <- para$getType()
         if(paraType=="aux"){
           subs <- para$getSubs()
           for(sub in subs){
             priorDist <- sub$getPrior()
             paraName <- str_split(para$getFormulaName(), "@")[[1]][2]
             if(!paraName %in% priors$class && localUse) browser()
             priors[priors$class==paraName,]$prior <- priorDist$getAsStanSyntax()
           }
         }else if(paraType=="predictor"){
           pred <- private$mcd$getPredictor(para$getPredId())
           
           oVIds <- pred$getOVIds()
           oVCont <- c()
           for(id in oVIds){
             oV <- private$mcd$getOtherVariable(id)
             if(oV$getType()=="cont") oVCont <- c(oVCont, oV$getName())
           }
           
           predName <- str_split(pred$getName(), ":")[[1]]
           subs <- para$getSubs()
           for(s_i in seq_along(subs)){
             sub <- subs[[s_i]]
             priorDist <- sub$getPrior()
             paraName <-  str_split(names(subs)[s_i], ":")[[1]]
             
             concName <- paste0(c(paste0(predName, collapse="."), 
                                  paste0(paraName, collapse=".")),
                                collapse="..", recycle0=T)
             
             # predParaName <- paste0(predName,paraName)
             # concName <- paste0(predParaName,collapse=":")
             
             # predNameCat <- predName[!predName %in% oVCont]
             # paraNameCat <- paraName[!predName %in% oVCont]
             
             if(length(oVCont)==length(predName)){
               concName <- paste0(predName, collapse=":")
             }else{
               predNameCont <- predName[predName %in% oVCont]
               predNameCont <- predNameCont[order(match(predNameCont, terms))]
               if(length(predNameCont) > 0){
                 concName <- paste0(paste0(predNameCont, collapse=":"), ":", concName)
               }
             }
             
             
             if(sub$getAmIRedundant()){
               if(concName %in% priors[priors$class=="b",]$coef){
                 if(localUse) browser()
                 stop("sub is marked as redundant in BAYAS but present in the formula")
               }
             }else{
               if(!concName %in% priors[priors$class=="b",]$coef){
                 if(localUse) browser()
                 stop("sub is not present in the formula, but has to be")
               }else{
                 priors[priors$class=="b" & priors$coef==concName,]$prior <- priorDist$getAsStanSyntax()
               }
             }
           }
         }else if(paraType=="intercept"){
           subs <- para$getSubs()
           for(sub in subs){
             priorDist <- sub$getPrior()
             priors[priors$class=="Intercept",]$prior <- priorDist$getAsStanSyntax()
           }
         }
       }
       
       compiled_model <- brms::brm(formula=linPred1_revised, data=dd, 
                                   family=family, prior=priors,
                                   chains=0, seed=seed)
       return(compiled_model)
     },
     
     error=function(err){
       return(NULL)
     })
     
     
     if(is.null(compiled_model)){
       showNotification("Something went wrong. The operator is notified.", type="error")
       malfunction_report(code=malfunctionCode()$planningFormula, msg="building planning formula",
                          type="error", askForReport=T)
     }
     return(compiled_model)
   },
   
   latexFormula = function(seed){

     if(length(private$linearPredictorId[1]) == 0) {
       if(localUse) browser()
       return()
     }
     
     latex <- "\\begin{dgroup*}\\breakingcomma\n"
     
     
     #response 
     paras <- getDistributionParameterForFormula(private$responseDist, T)
     responseDist <- getDistributionNameAsLatex(private$responseDist, paras)
     responseName <- private$responseName
     responseName <- wordToLatexConform(responseName)
     
     responseLatex <- paste0(responseName, " \\sim ", responseDist)
     responseLatex <- paste0("\t",responseLatex, "\n")
     
     latex <- paste0(latex, "\\begin{dmath*}\n",responseLatex, "\\end{dmath*}\n")
 
     
     # In a glm 'linPred1Id' is typical the expectation value
     linPred1Id <- private$linearPredictorId[1]
     useIntercept1 <- F
     predLatex <- ""
     for(pred in private$predictorFormulaIdMapping){
       if(pred[2] == linPred1Id){
         predictor <- private$mcd$getPredictor(pred[1])
         if(predictor$getType() == "intercept"){
           useIntercept1 <- T
         }else{

           parameter <- private$mcd$getParameterOfPredictor(pred[1])
           predName <- predictor$getName()
           
           predName <- wordToLatexConform(predName)
           
           sPredLatex <- ""
           if(parameter$isVector()){
             sPredLatex <- paste0(sPredLatex, "\\underline{b_{", predName, "}} \\\\ +")
           }else{
             sPredLatex <- paste0(sPredLatex, "b_{", predName, "} \\\\ +")
           }
           predLatex <- paste0(predLatex, sPredLatex)
         }
       }
     }

     if(useIntercept1){
       predLatex <- paste0("b_{(Intercept)} \\\\ +", predLatex)
     }
     
     if(nchar(predLatex) > 1){
       predLatex <- sub_str(predLatex, -5)
       predLatex <- paste0(predLatex, "\n")
     }
     
     paras <- getDistributionParameterForFormula(private$responseDist, T)
     para <- paras[["mean"]]
     leftLatex <- ""
     if(!is.null(para)){
       leftLatex <- getLinkFunctionTermToLatex(private$responseLink, para$latexName)
     }

     predLatex <- paste0(leftLatex, " = ", predLatex)

     latex <- paste0(latex, "\\begin{dmath*}\n\t", predLatex, "\\end{dmath*}\n")
     
     
     #... potential 'linPred2Id' for e.g. the auxiliary parameter
     #...
     
     
     

     #set priors
     allParameters <- private$mcd$getParameters()
     paraInterceptPrior <- list()
     paraPredPrior <- list()
     paraAuxPrior <- list()
     paraInterceptGenerative <- list()
     paraPredGenerative <- list()
     paraAuxGenerative <- list()
     predElements <- list()
     for(para in allParameters){
       paraType <- para$getType()
       paraPrio <- para$getPriority()
       
       if(paraType=="aux"){
         subs <- para$getSubs()

         paraName <- transformParaNameToLatex(para$getName())
         
         if(para$isVector()){
           paraName <- paste0("\\underline{", paraName, "}")
         }
         
         distLatex <- ""
         distGenerativeLatex <- ""
         for(s_i in seq_along(subs)){
           sub <- subs[[s_i]]
           priorDist <- sub$getPrior()
           generativeDist <- sub$getValueDistribution()
           if(!sub$getAmIRedundant()){
             distLatex <- paste0(distLatex, priorDist$getAsLatexSyntax() , " , ") #" , \\mathrel{\\phantom{+}}\\mathrel{\\phantom{.}}"
             distGenerativeLatex <- paste0(distGenerativeLatex, generativeDist$getAsLatexSyntax() , " , ") 
           }
         }
         distLatex <- sub_str(distLatex, -3) #-44
         distGenerativeLatex <- sub_str(distGenerativeLatex, -3) #-44
         
         sParaLatex <- ""
         sParaGenerativeLatex <- ""
         
         centerLatex <- ifelse(para$hasOnlyFixedValueSubs("prior"), " = ", " \\sim ")
         centerGenerativeLatex <- ifelse(para$hasOnlyFixedValueSubs("generative"), " = " , " \\sim ")
         if(para$isVector()){
           sParaLatex <- paste0(paraName, centerLatex ," \\{ ", distLatex, "\\}")
           sParaGenerativeLatex <- paste0(paraName, centerGenerativeLatex ,"\\{ ", distGenerativeLatex, " \\}")
         }else{
           sParaLatex <- paste0(paraName, centerLatex , distLatex)
           sParaGenerativeLatex <- paste0(paraName,centerGenerativeLatex, distGenerativeLatex)
         }

         lastSamePriorIndex <- 0
         namesParaAux <- names(paraAuxPrior)
         if(length(namesParaAux) > 0){
           namesParaAuxNum <- as.numeric(namesParaAux)
           lastSamePriorIndex <- max(which(namesParaAuxNum %in% paraPrio))
         }
         paraAuxPrior <- list.insert(paraAuxPrior, sParaLatex, lastSamePriorIndex+1, as.character(paraPrio))
         paraAuxGenerative <- list.insert(paraAuxGenerative, sParaGenerativeLatex, lastSamePriorIndex+1, as.character(paraPrio))
         
       }else if(paraType=="predictor"){
         pred <- private$mcd$getPredictor(para$getPredId())

         predName <- pred$getName() 
         predName <- wordToLatexConform(predName)
         
         predName <- paste0("b_{", predName, "}")
         
         if(para$isVector()){
           predName <- paste0("\\underline{", predName, "}")
         }
         
         
         subs <- para$getSubs()
         distLatex <- ""
         distGenerativeLatex <- ""
         for(s_i in seq_along(subs)){
           sub <- subs[[s_i]]
           priorDist <- sub$getPrior()
           generativeDist <- sub$getValueDistribution()
           if(!sub$getAmIRedundant()){
             distLatex <- paste0(distLatex, priorDist$getAsLatexSyntax(), " , ") #" , \\mathrel{\\phantom{+}}\\mathrel{\\phantom{.}}"
             distGenerativeLatex <- paste0(distGenerativeLatex, generativeDist$getAsLatexSyntax() , " , ") 
             if(length(subs) > 1){
               predElements <- list.append(predElements, c(predElements[[pred$getName()]], names(subs)[s_i]), pred$getName())
             }
           }
         }
         distLatex <- sub_str(distLatex, -3) #-44
         distGenerativeLatex <- sub_str(distGenerativeLatex, -3) #-44
         
         sParaLatex <- ""
         centerLatex <- ifelse(para$hasOnlyFixedValueSubs("prior"), " = ", " \\sim ")
         centerGenerativeLatex <- ifelse(para$hasOnlyFixedValueSubs("generative"), " = " , " \\sim ")
         if(para$isVector()){
           sParaLatex <- paste0(predName, centerLatex,"\\{ ", distLatex, " \\}")
           sParaGenerativeLatex <- paste0(predName, centerGenerativeLatex, "\\{ ", distGenerativeLatex, " \\}")
         }else{
           sParaLatex <- paste0(predName, centerLatex, distLatex)
           sParaGenerativeLatex <- paste0(predName, centerGenerativeLatex, distGenerativeLatex)
         }

         lastSamePriorIndex <- 0
         namesParaPred <- names(paraPredPrior)
         if(length(namesParaPred) > 0){
           namesParaAuxNum <- as.numeric(namesParaPred)
           lastSamePriorIndex <- max(which(namesParaAuxNum %in% paraPrio))
         }
         paraPredPrior <- list.insert(paraPredPrior, sParaLatex, lastSamePriorIndex+1, as.character(paraPrio))
         paraPredGenerative <- list.insert(paraPredGenerative, sParaGenerativeLatex, lastSamePriorIndex+1, as.character(paraPrio))
         
       }else if(paraType=="intercept"){
         subs <- para$getSubs()
         
         paraName <- "b_{(Intercept)}"
         
         if(para$isVector()){
           paraName <- paste0("\\underline{", paraName, "}")
         }
         
         distLatex <- ""
         distGenerativeLatex <- ""
         for(s_i in seq_along(subs)){
           sub <- subs[[s_i]]
           priorDist <- sub$getPrior()
           generativeDist <- sub$getValueDistribution()
           if(!sub$getAmIRedundant()){
             distLatex <- paste0(distLatex, priorDist$getAsLatexSyntax() , " , ") #" , \\mathrel{\\phantom{+}}\\mathrel{\\phantom{.}}"
             distGenerativeLatex <- paste0(distGenerativeLatex, generativeDist$getAsLatexSyntax() , " , ") 
           }
         }
         distLatex <- sub_str(distLatex, -3) #-44
         distGenerativeLatex <- sub_str(distGenerativeLatex, -3) #-44
         
         sParaLatex <- ""
         centerLatex <- ifelse(para$hasOnlyFixedValueSubs("prior"), " = ", " \\sim ")
         centerGenerativeLatex <- ifelse(para$hasOnlyFixedValueSubs("generative"), " = " , " \\sim ")
         if(para$isVector()){
           sParaLatex <- paste0(paraName, centerLatex, "\\{ ", distLatex, " \\}")
           sParaGenerativeLatex <- paste0(paraName, centerGenerativeLatex, "\\{ ", distGenerativeLatex, " \\}")
         }else{
           sParaLatex <- paste0(paraName, centerLatex,distLatex)
           sParaGenerativeLatex <- paste0(paraName, centerGenerativeLatex, distGenerativeLatex)
         }

         lastSamePriorIndex <- 0
         namesParaIntercept <- names(paraInterceptPrior)
         if(length(namesParaIntercept) > 0){
           namesParaInterceptNum <- as.numeric(namesParaIntercept)
           lastSamePriorIndex <- max(which(namesParaInterceptNum %in% paraPrio))
         }
         paraInterceptPrior <- list.insert(paraInterceptPrior, sParaLatex, lastSamePriorIndex+1, as.character(paraPrio))
         paraInterceptGenerative <- list.insert(paraInterceptGenerative, sParaGenerativeLatex, lastSamePriorIndex+1, as.character(paraPrio))
       }
     }
     

     latexPrior <- paste0("\\subsubsection{Inference model}", latex)
     for(el in paraInterceptPrior){
       latexPrior <- paste0(latexPrior, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     for(el in paraPredPrior){
       latexPrior <- paste0(latexPrior, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     for(el in paraAuxPrior){
       latexPrior <- paste0(latexPrior, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     latexPrior <- paste0(latexPrior, "\\end{dgroup*}\n")
     
     latexGenerative <- paste0("\\subsubsection{Data generation model}", latex)
     for(el in paraInterceptGenerative){
       latexGenerative <- paste0(latexGenerative, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     for(el in paraPredGenerative){
       latexGenerative <- paste0(latexGenerative, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     for(el in paraAuxGenerative){
       latexGenerative <- paste0(latexGenerative, "\\begin{dmath*}\\breakingcomma\n \t", el, "\n\\end{dmath*}\n")
     }
     latexGenerative <- paste0(latexGenerative, "\\end{dgroup*}\n")
     
     #Element names of vectors
     for(p_i in seq_along(predElements)){
       pred <- predElements[[p_i]]
       pred <- wordToLatexConform(pred)
       name <- names(predElements)[p_i]
       name <- wordToLatexConform(name)
       latexPrior <- paste0(latexPrior, "$\\underline{",name,"} = \\left\\{", paste0(paste0("\\text{",pred,"}"), collapse=" , \\allowbreak"), "\\right\\}$ \\\\ \n")
       latexGenerative <- paste0(latexGenerative, "$\\underline{",name,"} = \\left\\{", paste0(paste0("\\text{",pred,"}"), collapse=" , \\allowbreak"), "\\right\\}$ \\\\ \n")
     }
     
     latex <- paste0(latexGenerative, "\n\n", latexPrior)
     
     return(latex)
   },
   
   distFramilyMapping = function(dist, link){
     link <- unlist(link)
     distEnum <- planningDistribtionsEnum("all")
     if(dist == distEnum$Normal) return(brmsfamily("normal",link))
     
     if(dist == distEnum$Log_Normal) return(brmsfamily("lognormal",link))
     if(dist == distEnum$Gamma) return(brmsfamily("gamma",link))
     if(dist == distEnum$Inverse_Gaussian) return(brmsfamily("inverse.gaussian",link))
     
     if(dist == distEnum$Exponential) return(brmsfamily("exponential",link))
     
     if(dist == distEnum$Beta) return(brmsfamily("beta",link))
     
     if(dist == distEnum$Poisson) return(brmsfamily("poisson",link))
     if(dist == distEnum$Negative_Binomial) return(brmsfamily("negbinomial",link))
     
     if(dist == distEnum$Binomial) return(brmsfamily("binomial",link))
     if(dist == distEnum$Beta_Binomial) return(brmsfamily("beta_binomial",link))
     if(dist == distEnum$Bernoulli) return(brmsfamily("bernoulli",link))
   },
   linkMapping = function(link){
     linkFunction <- planningLinkEnum()
     
     if(link == linkFunction$cauchit) return(list(link="cauchit"))
     if(link == linkFunction$cloglog) return(list(link="cloglog"))
     if(link == linkFunction$identity) return(list(link="identity"))

     if(link == linkFunction$inv_square) return(list(link="1/mu^2"))
     if(link == linkFunction$inverse) return(list(link="inverse"))
     if(link == linkFunction$log) return(list(link="log"))
     if(link == linkFunction$logit) return(list(link="logit"))
     if(link == linkFunction$probit) return(list(link="probit"))
     if(link == linkFunction$sqrt) return(list(link="sqrt"))
     
     return(NULL)
   },
   
   drawResponeData = function(n, tmpData=NULL){

     check <- self$verifyFormula()
     if(!check$valid) return(list(F, msg="Invalid formula"))
     
     
     #any predictor available?
     if(is.empty(private$predictorFormulaIdMapping))
       return(list(F, msg="Missing predictor"))

     
     responseDist <- private$responseDist
     responseLink <- private$responseLink
     

     resp <- private$mcd$getMcdResponse()
     respDist <- resp$getDist()
     if(!equal0(responseDist,respDist)) return(list(F))
     
     dist <- ModelCreatingDataOtherVariableDistributionFactory(responseDist)
     useAlt <- getDistributionUsedForGLMUseAltParameters(responseDist)
     paras <- getDistributionParameterForFormula(responseDist, glm=T)
     
     distParas <- list()
     auxParas <- list()
     fixedParas <- list() #list(type=c("fix","var"), val=c("value", ov$getId())) 
     
     
     for(p_i in seq_along(paras)){
       p_name <- names(paras)[p_i]
       para <- paras[[p_i]]
       
       if(p_name=="mean"){
         linPred <- private$linearPredictorId
         preds <- private$predictorFormulaIdMapping
         for(pred in preds){
           if(pred[2]==linPred){
             predId <- pred[1]
             predObject <- private$mcd$getPredictor(predId)
             paraObject <- private$mcd$getParameterOfPredictor(predId)
             subs <- paraObject$getSubs()
             for(sub_i in seq_along(subs)){
               sub <- subs[[sub_i]]
               if(sub$getAmIRedundant()) next
               valDist <- sub$getValueDistribution()
               randomVal <- valDist$randomValue(n,F)
               name <- paste0(predObject$getName(), "@", names(subs)[[sub_i]])
               entry <- list(val=randomVal, ovIds=predObject$getOVIds(), attr=names(subs)[[sub_i]],
                             isSlope=predObject$isSlope())
               distParas <- list.append(distParas, entry)
             }
           }
         }
         
       }else if(p_name=="aux"){
         paraObject <- NULL
         for(p in private$parameterFormulaIdMapping){
           paraObject <- private$mcd$getParameter(p[1])
           if(is.null(paraObject)) next
           if(paraObject$getName() == para$name){
             break
           }
         }
         
         subs <- paraObject$getSubs()
         if(length(subs) >1){
           stop("Not implemented")
         }else{
           sub <- subs[[1]]
           valDist <- sub$getValueDistribution()
           randomVal <- valDist$randomValue(n,F)
           auxParas <- list.append(auxParas, randomVal, para$distParaName) 
         }

       }else if(p_name=="fixPara"){
         paraName <- para$name
         if(paraName=="N"){
           ovs <- private$mcd$getOtherVariables()
           for(ov in ovs){
             if(!is.null(ov$getSpecialRole()) &&
                ov$getSpecialRole() == "BinomN"){
               entry <- list(type="var", val=ov$getId(), name = paraName) 
               fixedParas <- list.append(fixedParas, entry, para$distParaName) 
               break
             }
           }
         }
       }else{
         stop("Not implemented")
       }
     }

     #draw samples
     samples <- c()
     
     i <- 1:n
     
     mu <- rep(0,n)
     if(n>10000) setProgress(value=0.1)
     for(d_i in seq_along(distParas)){
       pa <- distParas[[d_i]]

       valid <- T
       tmpMu <- pa$val[i]
       for(pa_i in seq_along(pa$ovIds)){
         id <- pa$ovIds[pa_i]
         attr_i <- str_split(pa$attr,":")[[1]][pa_i]

         ov <- private$mcd$getOtherVariable(id)
         cell <- private$mcd$getData(column=as.numeric(id), row=i)

         if(!is.null(tmpData)){
           varName <- private$mcd$getOtherVariable(as.numeric(id))$getName()
           cell <- tmpData[[varName]][i]
         }
         
         if(ov$isNumeric()){
           if(pa$isSlope){
             tmpMu <- tmpMu*as.numeric(cell)
           }else{
             stop("hm... no slope but a numeric is part of this predictor?")
           }
         }else{
           # if(any(cell != attr_i)){
           #   valid <- F
           #   break
           # }
           tmpMu[cell != attr_i] <- 0 
         }
       }
       if(valid){
         mu <- mu+tmpMu
       }
     }
     if(n>10000) setProgress(value=0.30)
     for(f_i in seq_along(fixedParas)){
       fixed <- fixedParas[[f_i]]
       if(fixed$type == "var"){
         val <- private$mcd$getData(column=as.numeric(fixed$val), row=i)
         if(!is.null(tmpData)){
           varName <- private$mcd$getOtherVariable(as.numeric(as.numeric(fixed$val)))$getName()
           val <- tmpData[[varName]][i]
         }
         val <- as.numeric(as.character(val))
         dist$setParameterValue(id=fixed$name, val=val, alternative=useAlt)
       }else if(fixed$type == "fix"){
         stop("Not implemented")
       }
     }
     if(n>10000) setProgress(value=0.50)
     for(a_i in seq_along(auxParas)){
       dist$setParameterValue(id=names(auxParas)[[a_i]], auxParas[[a_i]], alternative=useAlt)
     }

     if(n>10000) setProgress(value=0.70)
     mu <- applyLinkFunction(link=responseLink, inverse=T, value=mu)

     dist$setParameterValue(id=paras$mean$distParaName, val=mu, alternative=useAlt)
     dist$transform(!useAlt)

     
     draw <- rep(NA,n)
     

     dist$setSeed(private$mcd$getGenerateSeed())
     
     
     if(n>10000) setProgress(value=0.80)
     draw <- dist$randomValue(n,F)
     
     if(n>10000) setProgress(value=0.95)
     draw[mu==Inf] <- Inf
     draw[mu==-Inf] <- -Inf

     if(any(is.na(draw))){
       showNotification("Some response values couldn't be created.")
     }

     samples <- draw
     
     return(list(T,samples=samples))
   },
   
   
   #Verify formula 
   verifyFormula = function(){
     ret <- list(valid=T)
     
     preds <- self$getUsedPredictors()
     
     #Are used predictors valid?
     for(p in preds){
       if(p$isValid()$valid != "valid") ret$valid <- F
     }

     return(ret)
   },
   
   
   getLatex = function(){
     return(self$latexFormula())
   },
   
   doTrigger = function(){
     private$mcd$doTriggerFormula()
     self$triggerReactiveValue("rxTriggerFormula")
   },
   getReactive = function(type=NULL){
     if(is.null(type)){
       self$dependReactiveValue("rxTriggerFormula")
     }else if(type=="ui"){
       self$dependReactiveValue("rxTriggerFormulaUI")
     }
   },
   
   setClassAttr = function(rName, rDist, rLink, rFormulaId,
                           lPredId, predFIM, paraFIM, paraIFR,
                           mode, predLT){

     private$responseName <- rName
     private$responseDist <- rDist
     private$responseLink <- rLink
     private$responseFormulaId <- rFormulaId
     
     private$linearPredictorId <- lPredId
     private$predictorFormulaIdMapping <- predFIM
     private$parameterFormulaIdMapping <- paraFIM
     private$parameterIdsForResponse <- paraIFR

     private$mode <- mode
     private$predictorLineType <- predLT
   },
   
   getInstance = function(mcd=NULL){
     
     new <- NULL
     
     if(!is.null(mcd)){
       new <- ModelCreatingDataFormula$new(mcd)
     }else{
       new <- ModelCreatingDataFormula$new(private$mcd)
     }

     rName <- private$responseName
     rDist <- private$responseDist
     rLink <- private$responseLink
     rFormulaId <- private$responseFormulaId
     
     lPredId <- private$linearPredictorId
     predFIM <- private$predictorFormulaIdMapping
     paraFIM <- private$parameterFormulaIdMapping
     paraIFR <- private$parameterIdsForResponse
     
     mode <- private$mode
     predLT <- private$predictorLineType
     
     new$setClassAttr(rName, rDist, rLink, rFormulaId,
                      lPredId, predFIM, paraFIM, paraIFR,
                      mode, predLT)
     return(new)
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

         responseName = private$responseName,
         responseDist = private$responseDist,
         responseLink = private$responseLink,
         
         responseFormulaId = private$responseFormulaId,
         linearPredictorId = private$linearPredictorId,
         predictorFormulaIdMapping = private$predictorFormulaIdMapping,
         parameterFormulaIdMapping = private$parameterFormulaIdMapping,
         
         parameterIdsForResponse = private$parameterIdsForResponse,
         
         mode = private$mode, 
         predictorLineType = private$predictorLineType,
         
         #R6
         mcd = mcdState
       )
     }
     
     ret <- list(
       uuid = self$getUUID(),
       nextUUID = nextUUID,
       'ModelCreatingDataFormula' = ret
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
     
     private$responseName <- state$responseName
     private$responseDist <- state$responseDist
     private$responseLink <- state$responseLink
     
     private$responseFormulaId <- state$responseFormulaId
     private$linearPredictorId <- state$linearPredictorId
     private$predictorFormulaIdMapping <- state$predictorFormulaIdMapping
     private$parameterFormulaIdMapping <- state$parameterFormulaIdMapping
     
     private$parameterIdsForResponse <- state$parameterIdsForResponse
     
     private$mode <- state$mode
     private$predictorLineType <- state$predictorLineType
     
     #R6
     private$mcd <- state$mcd
   },
   resetState = function(){
     if(!super$resetState()) return()
     if(!is.null(private$mdc)) private$mcd$resetState()
   }
   
  )
)
