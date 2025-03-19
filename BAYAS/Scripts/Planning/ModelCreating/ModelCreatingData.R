distEnum <- planningDistribtionsEnum("response")

ModelCreatingData <- R6Class(
  classname = "ModelCreatingData", 
  inherit = ReactiveSerializationInterface,
  
   private = list(
     stateVersion = "0.1",

     
     modelName = NULL, #unique identifier
     data = NULL,
     dataMinPoints = 1,
     dataColHighlight=NULL,
     #Used when other variable data depends on another variable,
     #where errors occur (e.g. negative values for positive parameters)
     #list of list(oV.id, status="error"/"warning")
     otherVariableErrors = list(),
     
     mcdResponse = NULL,
     
     otherVariableIds = c(),
     otherVariable = list(),
     
     predictorIds = c(),
     predictors = list(),
     
     parameterIds = c(),
     parameters = list(),
     
     generateData = list(ssuVar=character(0), 
                         ssu=1, tdp=1),
     
     generateSeed = 123,
     
     generateDataAutomatically = TRUE,
     
     doRefreshAfterLoading = F,
     
     
     mcdSSD = NULL,
     
     visualizeData = list(yAxis=character(0),
                          xAxis=character(0),
                          groupBy=character(0),
                          plotType="Points"),
     
     
     #Report experiment inputs
     reportExpInp = list(onlyUsedVars = T,
                         onlyValidParas = T,
                         includeGenData = T,
                         replaceMinData = T,
                         includeSSD = T,
                         includeSSDResult = T,
                         onlyUsedGoals = T,
                         onlySSD = F),
     
     rxTriggerData = NULL,
     rxTriggerDataSelection = NULL,
     rxTriggerFormula = NULL,
     rxTriggerResponse = NULL,
     rxTriggerResponseName = NULL,
     rxTriggerResponseDist = NULL,
     rxTriggerResponseLink = NULL,
     rxTriggerOtherVariable = NULL, #remove, add
     rxTriggerOtherVariableStep = NULL,
     rxTriggerOtherVariableName = NULL,
     rxTriggerOtherVariableDist = NULL,
     rxTriggerOtherVariableError = NULL,
     rxTriggerPredictor = NULL,
     rxTriggerParameter = NULL,
     rxTriggerGenerateData = NULL,
     rxTriggerSSDGoals = NULL,
     rxTriggerVisualizeData = NULL,
     rxTriggerLoadMCD = NULL,

     
     lastOtherVariableId = NULL,
     mcdFormula = NULL,
     formulaObject = NULL, #"static",
     
     #only for local use
     silent = F
   ),
   
   public = list(
     
     hash = runif(1),
     
     initialize = function(modelName,
                           emptyState = F){
       super$initialize()

       
       if(emptyState) return()
       
       
       private$lastOtherVariableId <- NULL
       private$modelName <- modelName
       
       private$mcdResponse <- ModelCreatingDataResponse$new(self)
       private$mcdFormula <- ModelCreatingDataFormula$new(self)
       private$mcdSSD <- ModelCreatingDataSSD$new(mcd=self)
     },
     
     setData = function(data){
       self$triggerReactiveValue("rxTriggerData")
       private$data <- data
     },
     getData = function(column=NULL, row=NULL){
       data <- private$data
       if(!is.null(column)){
         if(column=="Response"){
           data <- data[,1]
         }else if(is.numeric(column) &&
                  column %in% private$otherVariableIds){
           id <- match(column, private$otherVariableIds) +1
           data <- data[[id]]
         }
       }
       if(!is.null(row)){
         if(is.null(column)){
           data <- data[row,,F]
         }else{
           data <- data[row]
         }
       }
       return(data)
     },
     getRandomDataset = function(N, seed){
       
       withr::with_seed(seed, {
         seed <- round(runif(3,-100000,100000))
       })
       
       checkN <- self$checkSampleSize(N)
       
       if(checkN[[1]]){
         
         #set temp random seeds of other variable and parameters
         tmpSeedOv <- list()
         tmpSeedPara <- list()
         oVs <- self$getOtherVariables()
         for(oV in oVs){
           tmpSeedOv <- list.append(tmpSeedOv, oV$getSeed(), as.character(oV$getId()))
           
           withr::with_seed(seed[1], {
             r <- round(runif(1,-100000,100000))
           })
           
           oV$setSeed(r, silent=T)
           oV$setRandomValues()
         }
         paras <- self$getParameters()
         for(p in paras){
           tmpList <- c()
           subs <- p$getSubs()
           
           withr::with_seed(seed[2], {
             r <- round(runif(length(subs)*2,-100000,100000))
           })
           
           for(s_i in seq_len(length(subs))){
             s <- subs[[s_i]]
             prior <- s$getPrior()
             val <- s$getValueDistribution()
             tmpList <- c(tmpList, prior$getSeed(), val$getSeed())
             prior$setSeed(r[s_i])
             val$setSeed(r[s_i+length(subs)])
             s$setPrior(prior, silent=T) #silent due to the call of 'activeUpdateAutomatically', to prevent calling twice
             s$setValueDistribution(val, silent=T)
           }
           
           tmpSeedPara <- list.append(tmpSeedPara, tmpList, as.character(p$getId()))
         }
  
         
                        
         #refresh data
         lcm <- N
         errorsOccur <- F
         
         ids <- list()
         for(oV in private$otherVariable){
           val <- oV$getDependsOnOV()
           if(is.null(val)) val <- ""
           ids <- list.append(ids, val, as.character(oV$getId()))
         }
         
         ids <- self$sortOtherVariableAccordingToDependency(ids)
         
         n <- names(ids)
         data <- self$getData()
         tmp <- as.data.frame(matrix(NA,nrow=N,ncol=ncol(data)))
         names(tmp) <- names(data)
         data <- tmp
         
         for(i in seq_len(length(ids))){
           val <- c()
           oV <- self$getOtherVariable(n[i])
           if(!is.null(oV$getType())){
             if(oV$getType() != "categorical"){
               dist <- oV$getDist()$getInstance()
               if(!is.null(dist)){
                 
                 #If this oV depends on another oV
                 if(!is.null(oV$getDependsOnOV())){
                   alt <- dist$getUseAlternative()
                   para <- dist$getParameter()
                   if(alt){
                     para <- dist$getAlternativeParameter()
                   }
                   errorsOccur <- F
                   for(pi in seq_len(length(para))){
                     p <- para[[pi]]
                     if(p$getType()=="variable"){
                       id <- p$getOtherVariableId()
                       if(is.null(id)) next
                       if(!is.null(id) && length(id) > 0) id <- as.numeric(id)
                       
                       valid <- self$isOtherVariableValidForDistParameter(
                         p,
                         oV$getId(),
                         id
                       )
                       if(valid$valid != "invalid"){
                         dependingoV <- self$getOtherVariable(as.numeric(p$getOtherVariableId()))
                         valuesOfOV <- dependingoV$getValues()
                         valuesOfOV <- as.numeric(self$getData(as.numeric(p$getOtherVariableId())))
                         dist$setParameterValue(pi, valuesOfOV, F, F)
                       }else{
                         dist$setParameterValueToDefault(pi, alt)
                         errorsOccur <- T
                         private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                                    list(id=oV$getId(), status="error"))
                       }
                     }
                   }
                   #If no error occur, but this oV depends on an oV, where error occurred
                   if(!errorsOccur && 
                      !is.null(private$otherVariableErrors) && 
                      length(private$otherVariableErrors) > 0){
                     for(err in private$otherVariableErrors){
                       if(oV$getDependsOnOV() %in% err$id){
                         private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                                    list(id=oV$getId(), status="warning"))
                         break;
                       }
                     }
                   }
                 }
                 
                 dist$randomValue(lcm) #Assign created value within distribution class
                 val <- dist$getValues(formatted=F)
               }
               data[[oV$getName()]] <- val
             }
             else{
               new_data <- self$setSubsetDataset(oV, lcm, localData=data)
               new_n <- names(new_data$df)
               for(n_i in new_n){
                 data[[n_i]] <- new_data$df[[n_i]][1:length(data[[n_i]])]
               }
             }
           }else{
             # if(localUse) browser()
             # self$setDataColumn(as.numeric(n[i]), rep("",lcm))
           }
         }
  
         # set response data
         tmpGenerateSeed <- private$generateSeed
         
         withr::with_seed(seed[3], {
           private$generateSeed <- round(runif(1,-1e7,1e7))
         })
         
         yData <- self$getMcdFormula()$drawResponeData(dim(data)[1], data)
         if(!yData[[1]]){
           # if(localUse) browser()
         }else{
           data[[1]] <- yData[[2]]
         }
         private$generateSeed <- tmpGenerateSeed
         
         
         
         #reset tmp random values
         # tmpSeedOv 
         # tmpSeedPara
         #set temp random seeds of other variable and parameters
         for(oV in oVs){
           r <- tmpSeedOv[[as.character(oV$getId())]]
           oV$setSeed(r, silent=T)
           oV$setRandomValues()
         }
         for(p in paras){
           r <- tmpSeedPara[[as.character(p$getId())]]
           subs <- p$getSubs()
           for(s_i in seq_len(length(subs))){
             s <- subs[[s_i]]
             prior <- s$getPrior()
             val <- s$getValueDistribution()
             prior$setSeed(r[s_i])
             val$setSeed(r[s_i+length(subs)])
             s$setPrior(prior, silent=T) #silent due to the call of 'activeUpdateAutomatically', to prevent calling twice
             s$setValueDistribution(val, silent=T)
           }
         }
         
         return(data)
       }else{
         #TODO
         warning("TODO: ModelCreatingData")
       }
     },
     setDataMinPoints = function(dp){
       private$dataMinPoints <- dp
     },
     getDataMinPoints = function(){
       return(private$dataMinPoints)
     },
     
     getLowestPossibleDataPoints = function(){
       c <- self$checkSampleSize(1)
       if(c[[1]]){
         return(1)
       }else{
         return(c[[2]][1])
       }
     },
     
     setOtherVariableErrors = function(err){
       private$otherVariableErrors <- err
     },
     getOtherVariableErrors = function(){ return(private$otherVariableErrors)},
     setDataColumn = function(column, values){
       max.l <- max(length(values), dim(private$data)[1])
       if(length(values) <= dim(private$data)[1]){
         values <- c(values, rep(NA, max.l-length(values)))
       }else{
         new.df <- data.frame(rep=rep(NA, max.l-dim(private$data)[1]))
         for(i in seq_len(dim(private$data)[2])){
           new.df[[i]] <- rep(NA, max.l-dim(private$data)[1])
         }
         colnames(new.df) <- colnames(private$data)
         private$data <- rbind(private$data,new.df)
       }
       if(column=="Response"){
         private$data[,1] <- values
       }else if(is.numeric(column) &&
                column %in% private$otherVariableIds){
         id <- match(column, private$otherVariableIds) +1
         if(is.null(values)){
           private$data[[id]] <- rep(NA, length(dim(private$data)[1]))
         }else{
           private$data[[id]] <- values
         }
       }
       row.has.na <- apply(private$data, 1, function(x){all(is.na(x))})
       private$data <- private$data[!row.has.na,,F]
       self$triggerReactiveValue("rxTriggerData")
     },
     setDataColumnByName = function(name, values){
       names <- names(private$data)
       for(oV in private$otherVariable){
         if(name == oV$getName()){
           self$setDataColumn(oV$getId(), values)
           break
         }
       }
     },
     resetData = function(){
       n <- names(private$data)
       private$data <- data.frame(a="")
       names(private$data) <- n
       self$triggerReactiveValue("rxTriggerData")
     },
     addDataColumn = function(col, name=NULL){
       d <- dim(private$data)
       private$data[[col]] <- rep(NA, d[1])
       if(!is.null(name) && is.numeric(col)){ 
         n <- names(private$data)
         n[[col]] <- name
         names(private$data) <- n
        }
       self$triggerReactiveValue("rxTriggerData")
     },
     #id:0 -> response
     removeDataColumn = function(id){
       if(id==0){
         id <- 1
       }else{
         id <- match(id, private$otherVariableIds) +1
       }
       private$data[[id]] <- NULL
       self$triggerReactiveValue("rxTriggerData")
     },
     #id:0 -> response
     highlightDataColumn = function(col, add=T){
       if(is.null(col)){
         private$dataColHighlight <- NULL
       }else{
         if(col==0){
           col <- 1
         }else{
           col <- match(col, private$otherVariableIds) +1
         }
         if(vectorEqual(private$dataColHighlight, col)) return()
         if(add){
           private$dataColHighlight <- c(private$dataColHighlight,col)
         }else{
           private$dataColHighlight <- col
         }
       }
       self$triggerReactiveValue("rxTriggerDataSelection")
     },
     setHighlightDataColumn = function(col){
       private$dataColHighlight <- col
     },
     getHighlightDataColumn = function(){
       return(private$dataColHighlight)
     },
     
     #type:"response", or id of otherVariable
     setHeader = function(type, header){
       if(type=="Response"){
         if(is.null(header) || str_trim(header)=="") header <- "Response"
         colnames <- names(private$data)
         if(length(colnames) > 0){
           colnames[1] <- header
           names(private$data) <- colnames
         }
       }else{
         id <- match(type, private$otherVariableIds) +1
         colnames <- names(private$data)
         colnames[id] <- header
         names(private$data) <- colnames
       }
       self$triggerReactiveValue("rxTriggerData")
     },
     
     setModelName = function(modelName){
       private$modelName <- modelName
     },
     getModelName = function(){ 
       return(private$modelName)
     },
     
     
     setMcdResponse = function(mcdResponse){
       private$mcdResponse = mcdResponse
       private$mcdResponse$setMcd(self)
     },
     getMcdResponse = function(){
       private$mcdResponse
     },
     
     
     
     ###############################################
     ########## Other Variable (and Data) ##########
     ###############################################
     
     #num: c(ids)
     setOtherVariableIds = function(num){
       private$otherVariableIds <- num
     },
     getOtherVariableIds = function(){
       return(private$otherVariableIds)
     },
     getOtherVariableIdsDependentOnThis = function(id){
       ids <- c()
       for(var in private$otherVariable){
         if(!is.null(var$getType()) && 
            var$getType() == "categorical" &&
            id %in% var$getDependsOnOV())
           ids <- c(ids, var$getId())
       }
       return(ids)
     },
     getOtherVariableIdsForDependency = function(){
       ids <- c()
       for(var in private$otherVariable){
         if(!is.null(var$getType()) && 
            (var$getType() != "categorical" || 
            var$getCategoricalModel()$isNumeric()))
           ids <- c(ids, var$getId())
       }
       return(ids)
     },
     getOtherVariableIdsForCategoricalSubgroups = function(){
       ids <- c()
       for(var in private$otherVariable){
         if(!is.null(var$getType()) && 
            var$getType() == "categorical")
           ids <- c(ids, var$getId())
       }
       return(ids)
     },
     #names of the oV of super groups of a subgroup categorical oV
     getOtherVariableNamesForCategoricalSubgroup = function(firstSuperGroupId){
       oV <- self$getOtherVariable(firstSuperGroupId)
       catModel <- oV$getCategoricalModel()
       names <- c()
       if(catModel$getType() == "subgroup"){
         super <- catModel$getSubgroupOf()
         names <- c(self$getOtherVariableNamesForCategoricalSubgroup(super), oV$getName())
       }else{
         names <- oV$getName()
       }
       return(names)
     },
     #Categorical and discrete distributions
     
     getOtherVariableIdsForReplacement = function(){
       ids <- c()
       names <- c()
       for(var in private$otherVariable){
         type <- var$getType()
         if(!is.null(type) && (type == "categorical")){
           catType <- var$getCategoricalModel()$getType()
           if(!is.null(catType) && catType %in% c("independent","subgroup")){
             ids <- c(ids, var$getId())
             names <- c(names, var$getName())
           }
         }
       }
       names(ids) <- names
       return(ids)
     },
     
  
     #Returns all ids of independent and subgroups, 
     #that are not already super groups or be a (nested) subgroup of id
     getOtherVariableIdsForSubgroupOV = function(id=NULL){
       ids <- c()
       names <- c()
       ancId <- self$getOtherVariable(id)$getDependsOnOV()
       for(oV in private$otherVariable){
         if(oV$getId() == id) next
         if(oV$getId() %in% ancId){
           ids <- c(ids, oV$getId())
           names <- c(names, oV$getName())
           next
         }
         type <- oV$getType()
         if(!is.null(type) && (type == "categorical")){
           cat <- oV$getCategoricalModel()
           catType <- cat$getType()
           if(!is.null(catType) && catType %in% c("independent","subgroup")){
             subs <- self$getOtherVariableIdsDependentOnThis(oV$getId())
             if(is.null(subs)){
               if(!self$ovHasCertainAncestor(oV$getId(), id)){
                 ids <- c(ids, oV$getId())
                 names <- c(names, oV$getName())
               }
             }
           }
         }
       }
       names(ids) <- names
       return(ids)
     },
     
     ovHasCertainAncestor = function(id, ancestorId){
       ov <- self$getOtherVariable(id)
       anc <- ov$getDependsOnOV()
       if(!is.null(anc)){
         if(anc==ancestorId) return(T)
         return(self$ovHasCertainAncestor(anc,ancestorId))
       }
       return(F)
     },
     
     # is dependency on other variable valid?
     #e.g. avoid cross references 
     #negative values for only positive parameters (sigma) etc.
     isOtherVariableValidForDistParameter = function(distParameter, distOVid, otherVarId){
       #Is id currently available? (e.g. it was just removed)
       if(!otherVarId %in% private$otherVariableIds){
         return(list(valid="invalid", 
                     message=paste0("This dependency is invalid.\n",
                                    "Dependent variable does not exist.")))
       }
       #cross references?
       crossRef <- self$crossReference(distOVid, otherVarId)
       
       if(crossRef){
         return(list(valid="invalid", 
                     message=paste0("This dependency is invalid.\n",
                                    "You have a cross reference. ",
                                    "The choosen variable depends (in)direct on this variable.")))
       }
       oV <- self$getOtherVariable(otherVarId)
       if(is.null(oV)) return(NULL)
       oVDist <- NULL
       ovMin <- NULL
       ovMax <- NULL
       if(oV$getType()=="categorical"){
         catModel <- oV$getCategoricalModel()
         if(catModel$isNumeric()){
           val <- catModel$getValuesForNumericDependency()
           ovMin <- min(val)
           ovMax <- max(val)
         }else{
           return(list(valid="invalid", 
                       message=paste0("This dependency is invalid.\n Your variable contains non numeric values.")))
         }
       }else{
         oVDist <- oV$getDist()
         ovMin <- oVDist$getMin()
         ovMax <- oVDist$getMax()
       }
       if(ovMin >= distParameter$min_val && 
          ovMax <= distParameter$max_val){
         return(list(valid="valid"))
       }else if(!is.null(oVDist$getValues()) &&
                distParameter$min_val <= min(oVDist$getValues()) &&
                distParameter$max_val >= max(oVDist$getValues())){
         return(list(valid="warning", 
                     message=paste0("This dependency can be inappropriate.\n The choosen variable 'could' contain values, ",
                                    "that are not applicable for this parameter.\n ",
                                    "Please choose another variable or make sure the choosen variable always matches the requirements.")))
       }else{
         return(list(valid="invalid", 
                     message=paste0("This dependency is invalid.\n Your variable contains values, ",
                     "that are not applicable for this parameter.\n ",
                     "Please choose another variable or make sure the variable matches the requirement.")))
       }
     },
     
     crossReference = function(distOVid, otherVarId){
       oV <- self$getOtherVariable(otherVarId)
       if(is.null(oV)) return(F)
       dependingIds <- oV$getDependsOnOV()
       if(distOVid %in% dependingIds) return(T)
       for(id in dependingIds){
         crossRef <- self$crossReference(distOVid, id)
         if(crossRef) return(T)
       }
       return(F)
     },
     
  
     setLastOtherVariableId = function(id){
       private$lastOtherVariableId <- id
     },
     
     getLastOtherVariableId = function(){
       return(private$lastOtherVariableId)
     },
     
     createOtherVariable = function(){
       nextId <- max(c(private$otherVariableIds,0))+1
       name <- self$getOtherVariableNames(name="unnamed")
       newOV <- ModelCreatingDataOtherVariable$new(nextId, self,name=name)
       return(newOV)
     },
     
     addOtherVariable = function(var, ignoreAdd=F){
       private$otherVariableIds <- c(private$otherVariableIds, var$getId())
       private$otherVariable <- list.append(private$otherVariable, var)
       self$triggerReactiveValue("rxTriggerOtherVariable")
       
       id <- match(var$getId(), private$otherVariableIds) +1
       if(!ignoreAdd) self$addDataColumn(id, var$getName())
       if(self$doRefresh()) self$refreshData()
     },
     
     removeOtherVariable = function(id=NULL, var=NULL){
       if(is.null(id)){
         if(is.null(var)){
           warning("Could not remove variable, since both parameter 'id' and 'var' are NULL.")
         }else{
           id <- var$getId()
         }
       }

       if(id %in% private$otherVariableIds){
         newList <- list()
         for(i in private$otherVariable){
           if(i$getId() != id) newList <- list.append(newList, i)
         }
         private$otherVariable <- newList
         self$removeDataColumn(id)
         private$otherVariableIds <- private$otherVariableIds[private$otherVariableIds!=id]
         if(self$doRefresh()) self$refreshData() 
       }
       self$triggerReactiveValue("rxTriggerOtherVariable")
     },
     
     updateDependentOtherVariable = function(id){
       oV <- self$getOtherVariable(id)
       
       ids <- self$getOtherVariableIdsDependentOnThis(id)
       if(is.null(ids) || length(ids)==0) return()
       
       if(!is.null(oV$getType()) && 
          oV$getType() == "categorical"){
         
         for(i in ids){
           oVD <- self$getOtherVariable(i)
           catModel <- oVD$getCategoricalModel()
           if(catModel$getType() == "replacement") return()
           
           catModel$setSubgroupOfNames(self$getOtherVariableNamesForCategoricalSubgroup(oV$getId()),new=T)
           catModel$setUpperGroupElements(oV$getCategoricalModel()$getValues(),new=T)
           
           oVD$setCategoricalModel(catModel$getInstance())
         }
       }else{
         for(i in ids){
           oVD <- self$getOtherVariable(i)
           oVD$clear()
         }
       }
     },
     
     setOtherVariables = function(oVs){
       private$otherVariable <- oVs
       self$triggerReactiveValue("rxTriggerOtherVariable")
     },
     
     getOtherVariables = function(){
       return(private$otherVariable)
     },
     
     getOtherVariable = function(id){
       if(is.null(id)) return(NULL)
       for(v in private$otherVariable){
         if(v$getId()==id) return(v)
       }
     },
     
     getOtherVariableByName = function(name){
       if(is.null(name)) return(NULL)
       for(v in private$otherVariable){
         if(v$getName()==name) return(v)
       }
     },
     
     #returns other variable names
     #if name is setted, returns name if available (not in use)
     #or an alternative name (adding "_" recursively)
     getOtherVariableNames = function(name=NULL, ignore=c()){
       names <- c()
       for(oV in private$otherVariable){
         names <- c(names, oV$getName())
       }
       if(is.null(name)){
         return(names)
       }else{
         names <- names[!names %in% ignore]
         while(name %in% names){
           if(grepl(".*_[0-9]+$",name)){
             hits <- gregexpr("[0-9]+$", name)[[1]]
             if(length(hits)!=1 || hits > -1){
               index <- last(hits)
               length <- last(attr(hits, "match.length"))
               sub <- substr(name, index, nchar(name))
               sub_num <- as.numeric(sub)+1
               name <- paste0(substr(name, 0, index-1), sub_num) 
             }else{
               name <- paste0(name,"_0")
             }
           }else{
             name <- paste0(name,"_0")
           }
         }
         return(name)
       }
     },
  
     
     getMinDatapointsOfVar = function(id){
       oV <- self$getOtherVariable(id)
       if(is.null(oV$getType()))return(1)
       if(oV$getType() == "categorical"){
         catModel <- oV$getCategoricalModel()
         if(catModel$getType() == "independent"){
           return(sum(catModel$getValuesFrequency()))
         }else if(catModel$getType() == "subgroup"){
           #from most super group to subgroup
           oVids <- c(oV$getId())
           oVSuper <- oV
           while(!is.null(oVSuper$getDependsOnOV())){
             oVSuperId <- oVSuper$getDependsOnOV()
             oVSuper <- self$getOtherVariable(oVSuperId)
             oVids <- c(oVSuperId, oVids)
           }
           return(self$getMinDatapointsOfCatVar(oVids))
         }else{
           return(1)
         }
       }else{
         return(oV$getMinDatapoints())
       }
     },
     
     getMinDatapointsOfCatVarAdvanced = function(id, el=NULL, iCall=F){
       #Check for the smallest minDP
       for(i in 1:1e6){
         if(self$checkDPForOV(i, id, el) > 0){
           return(i)
         }
       }
       
       return(NULL)

     },
     
     checkDPForOV = function(dp, ovId, el){
       oV <- self$getOtherVariable(ovId)
       catModel <- oV$getCategoricalModel()
       val <- catModel$getValues()
       freq <- catModel$getValuesFrequency()
       capped <- catModel$getCapped()
       
       for(e in el){ 
         freq <- freq[[e]]
         val <- val[[e]]
         capped <- capped[[e]]
       }
       
       depId <- self$getOtherVariableIdsDependentOnThis(oV$getId())
       if(is.null(depId)){
         sum <- sum(freq)
         if(capped){
           if(dp >= sum) return(sum)
         }else{
           if(dp %% sum == 0) return(sum)
         }
         return(-1)
       }else{
         for(dId in depId){
           sum <- 0
           for(i in seq_len(length(val))){
             s <- self$checkDPForOV(dp, dId, c(el,val[[i]]))
             if(s==-1) return(-1)
             sum <- sum + (s*freq[[i]])
           }
           if(capped){
             if(dp >= sum) return(sum)
           }else{
             if(dp %% sum == 0) return(sum)
           }
         }
         return(dp)
       }
       
     },
     
     getMinDatapointsOfCatVar = function(ids, el=NULL){
       oV <- self$getOtherVariable(ids[1])
       catModel <- oV$getCategoricalModel()
       val <- catModel$getValues()
       freq <- catModel$getValuesFrequency()
       
       for(e in el){ 
         freq <- freq[[e]]
         val <- val[[e]]
       }
       
       sum <- 0
       if(length(ids)==1){
         # for(e in el) freq <- freq[[e]]
         return(sum(freq))
       }else{
         for(i in seq_len(length(val))){
           sum <- sum+self$getMinDatapointsOfCatVar(ids=ids[-1],el=c(el,val[[i]]))*freq[[i]]
         }
       }
       return(sum)
     },
     
     getFactorOfDatapointsOfVar = function(id){
       oV <- self$getOtherVariable(id)
       if(is.null(oV$getType()))return(0)
       if(oV$getType() == "categorical"){
         catModel <- oV$getCategoricalModel()
         if(catModel$getType() == "independent"){
           if(catModel$getCapped()) return(c(0:sum(catModel$getValuesFrequency())))
           return(c(0,sum(catModel$getValuesFrequency())))
         }else if(catModel$getType() == "subgroup"){
           #from most super group to subgroup
           oVids <- c(oV$getId())
           oVSuper <- oV
           while(!is.null(oVSuper$getDependsOnOV())){
             oVSuperId <- oVSuper$getDependsOnOV()
             oVSuper <- self$getOtherVariable(oVSuperId)
             oVids <- c(oVSuperId, oVids)
           }
           return(self$getFactorOfDatapointsOfCatVar(oVids))
         }else{
           return(0) #categorical: replacement
         }
       }else{
         return(oV$getMinDatapoints())
       }
     },
     
     getFactorOfDatapointsOfCatVar = function(ids, el=NULL){
       
       oV <- self$getOtherVariable(ids[1])
       catModel <- oV$getCategoricalModel()
       val <- catModel$getValues()
       freq <- catModel$getValuesFrequency()
       capped <- catModel$getCapped()
       for(e in el){ 
         freq <- freq[[e]]
         capped <- capped[[e]]
         val <- val[[e]]
       }
       
       sum <- c()
       if(length(ids)==1){
         if(capped){
           return(0:sum(freq))
         }else{
           return(sum(freq))
         }
       }else{
         forSum <- 0
         for(i in seq_len(length(val))){
           subVals <- self$getFactorOfDatapointsOfCatVar(ids=ids[-1],el=c(el,val[[i]]))
           if(any(is.null(forSum) | is.na(forSum))) stop("Error ...")
           forSum <- max(forSum) + subVals*freq[[i]] 
           if(capped){
             sum <- c(sum, forSum)
           }else{
             sum <- sum+forSum
           } 
         }
       }
       return(sum)
     },
     
     #If categorical variables (subgroups) exists,
     #the super group will be generated independently from the subgroup
     #therefore it is created like 'a,b,a,b,a,b' not 'a,a,a,b,b,b'
     #This function creates the data in correct order
     setSubsetDataset = function(oV, dp, localData=NULL){
       #For categorical type of "replacement"
       catModel <- oV$getCategoricalModel()
       if(!is.null(catModel)){
         catType <- catModel$getType()
         if(!is.null(catType) && catType == "replacement"){
           replaceId <- as.numeric(catModel$getReplaceValuesOf())
           replaceOV <- self$getOtherVariable(replaceId)
           
           if(!is.null(localData)){ 
             df <- localData
             val <- catModel$getReplacedValues(replaceOV, df=df)
             name <- oV$getName()
             df[[name]] <- val
             return(list(df=df))
           }else{
             df <- self$getData()
             val <- catModel$getReplacedValues(replaceOV, df=df)
             self$setDataColumn(oV$getId(), val)
             return()
           }
         }
       }else{
         stop("Error.")
       }
       
       
       oVids <- c(oV$getId())
       oVSuper <- oV
       while(oV$getCategoricalModel()$getType() == "subgroup" &&
             !is.null(oVSuper$getDependsOnOV())){
         oVSuperId <- oVSuper$getDependsOnOV()
         oVSuper <- self$getOtherVariable(oVSuperId)
         oVids <- c(oVSuperId, oVids)
       }
  
       check <- self$checkSampleSize(0)
       minC <- check[[2]][1]
       gFactor <- floor(dp/minC)
       if(gFactor==0) stop()
       newData <- self$setTableDataSubgroup(oV$getId(), dp=dp, gFactor=gFactor, 
                                             rest=dp-(minC*gFactor),
                                            localData=localData)
       return(newData)
     },
     
     
     setTableDataSubgroup = function(id, el=NULL, dp=NULL, 
                                      ignore=F, gFactor=NULL, rest=0,
                                     localData=NULL){
       df <- data.frame()
       
       oV <- self$getOtherVariable(id[1])
       catModel <- oV$getCategoricalModel()
       values <- catModel$getValues()
       distributed <- catModel$getValuesDistributed()
       freq <- catModel$getValuesFrequency()
       randomize <- catModel$getRandomize()
       randomizeParameter <- catModel$getRandomParameter()
       seed <- catModel$getSeed()
       
       
       for(e in el){
         values <- values[[e]]
         freq <- freq[[e]]
         randomize <- randomize[[e]]
         distributed <- distributed[[e]]
         if(is.list(randomizeParameter)) randomizeParameter <- randomizeParameter[[e]]
         if(is.list(seed)) seed <- seed[[e]]
       }
       
       
       #if ov has parents: next
       if(!ignore && !is.null(oV$getDependsOnOV())) return()
       
       childs <- self$getOtherVariableIdsDependentOnThis(oV$getId())
       #remove replacement childs
       newChilds <- c()
       for(c in childs){
         ovChild <- self$getOtherVariable(c)
         catModelChild <- ovChild$getCategoricalModel()
         typeChild <- catModelChild$getType()
         if(typeChild=="replacement") next
         newChilds <- c(newChilds,c)
       }
       childs <- newChilds
  
       if(is.null(childs)){
         new <- c()
         if(randomize && distributed=="equally"){
           newRest <- rest - min(rest, length(values))
           if(is.null(dp)) dp <- (length(values)*gFactor) + min(rest, length(values))
           rest <- newRest
           new <- self$getRandomizedElements(dp=dp, values=values, 
                                             randomizeParameter=randomizeParameter,
                                             seed=seed)
         }
         else{
           new <- rep(values, freq)
           if(!is.null(dp)){
             # if(!is.null(gFactor) && gFactor != dp/length(new)) stop() 
             gFactor  <- dp/length(new)
           }
           newRest <- rest - min(rest, length(new))
           new <- c(rep(new, gFactor), new[seq_len(min(rest, length(new)))])
           rest <- newRest
         }
         new <- factor(new, levels=unique(values))
         # sorted_new <- new[sort(new)]
         sorted_new <- sort(new)
         sorted_new <- as.character(sorted_new)
         df <- data.frame(sorted_new)
         names(df) <- oV$getName()
         
         
         if(ignore) return(list(df=df,rest=rest))
       }
       else{
         #number of childs sould be 1 
         for(child in childs){
           df_c <- data.frame()
  
           
           if(randomize && distributed=="equally"){
             
             possDp <- NULL
             
             if(is.null(dp)){
               dpT <- 0
               for(e in seq_len(length(values))){
                 value <- values[[e]]
                 df_depth <- self$setTableDataSubgroup(child, el=c(el,value), 
                                                        dp=NULL, ignore=T, gFactor=gFactor,
                                                        rest=rest, localData=localData)
                 rest <- df_depth$rest
                 dpT <- dpT + dim(df_depth$df)[1]
               }
               dp <- dpT
             }
             if(!is.null(dp))
               possDp <- self$getDPForOV(self$getOtherVariable(last(id)), dp, el=el)
             
             possDp2 <- self$getDPCombinationsForOV(possDp, dp)
             
             
             new <- self$getRandomizedElementsForParent(poss=possDp2,
                                                        randomizeParameter=randomizeParameter,
                                                        seed=seed)
               
             table <- table(new)
             for(e in seq_len(length(values))){
               value <- values[[e]]
               df_depth <- self$setTableDataSubgroup(child, el=c(el,value), 
                                                      dp=table[[values[[e]]]], ignore=T,
                                                     localData=localData) 
               rest <- df_depth$rest
               
               
               df_depth <- cbind(new=value,df_depth$df)
               new.n <- names(df_depth)
               new.n[1] <- oV$getName()
               names(df_depth) <- new.n
              
               
               if(is.empty(df_c)){
                 df_c <- df_depth
               }else{
                 df_c <- rbind(df_c,df_depth)
               }
             }
           }
           else{
             
             if(!is.null(dp) && is.null(gFactor)){
               minC <- 0
               for(e in seq_len(length(values))){
                 value <- values[[e]]
                 minC <- minC + self$getMinDatapointsOfCatVarAdvanced(child, c(el,value))
               }
               gFactor <- floor(dp/minC)
               rest <- dp-(minC*gFactor)
             }
             
             for(e in seq_len(length(values))){
               value <- values[[e]]
               freq_e <- freq[[e]]
               df_depth <- self$setTableDataSubgroup(child, el=c(el,value), dp=NULL, ignore=T, #dp = NULL 
                                                      gFactor=gFactor, rest=rest,
                                                     localData=localData)
               rest <- df_depth$rest
               
               
               df_depth <- cbind(new=value,df_depth$df)
               new.n <- names(df_depth)
               new.n[1] <- oV$getName()
               names(df_depth) <- new.n
               
               i_df_depth <- df_depth
               for(f in seq_len(freq_e-1)){
                 df_depth <- rbind(df_depth,i_df_depth)
               }
               
               if(is.empty(df_c)){
                 df_c <- df_depth
               }else{
                 df_c <- rbind(df_c,df_depth)
               }
             }
           }
           
           if(is.empty(df)){
             df <- df_c
           }else{
             df <- cbind(df,df_c)
           }
         }
       }
  
      
       if(ignore){
         return(list(df=df, rest=rest))
       }else{
         if(is.null(localData)){
           df_n <- names(df)
           for(na in df_n){
             self$setDataColumnByName(na, as.character(df[[na]]))
           }
         }
         return(list(df=df, rest=rest))
       }
     },
  
     
     #returns the possible datapoints for a certain oV
     getDPForOV = function(oV, dp, el=NULL){
       
       catModel <- oV$getCategoricalModel()
       values <- catModel$getValues()
       
       for(e in el){
         values <- values[[e]]
       }
       
       comb <- self$getAllSubCombinations(oV$getId())
  
       ret <- list()
       for(value in values){
         poss <- c()
         for(co in comb){
           co <- co[-1]
           if(length(co) > 0){
             min <- self$getMinDatapointsOfCatVar(co, c(el,value))
             factor <- self$getFactorOfDatapointsOfCatVar(co, c(el,value))
             if(is.empty(poss)) {
               poss <- self$getAllPossDp(min, factor, dp) 
             }else{
               poss <- intersect(poss, self$getAllPossDp(min, factor, dp))
             }
           }
         }
         if(!is.null(poss)){
           poss <- sort(poss)
           ret <- list.append(ret, poss, value)
         }
       }
  
       return(ret)
     },
     
     #returns all sub/occ. combinations
     getAllSubCombinations = function(ovId){
       dependingIds <- self$getOtherVariableIdsDependentOnThis(last(ovId))
       if(!is.null(dependingIds)){
         ret <- list()
         for(i in dependingIds){
           depOv <- self$getOtherVariable(i)
           depCatModel <- depOv$getCategoricalModel()
           if(!is.null(depCatModel))
             type <- depCatModel$getType()
           if(!is.null(type) && type %in% c("independent","subgroup"))
             ret <- list.append(ret, unlist(self$getAllSubCombinations(c(ovId,i))))
         }
         return(ret)
       }else{
         return(ovId)
       }
     },
     
     #returns all sub/occ. combinations
     getDPCombinationsForOV = function(comb, dp){
       ret <- list()
       
       for(val in names(comb)){
         if(is.empty(ret)){
           for(num in comb[[val]]){
             ret <- list.append(ret, list(val=val, num=num) )
           }
         }else{
           newRet <- list()
           for(r in ret){
             for(num in comb[[val]]){
               if((val == last(names(comb)) && 
                 r$num+num == dp) ||
                 (val != last(names(comb)) &&
                   r$num+num <= dp)){
                 newRet <- list.append(newRet, list(val=c(r$val,val), num=c(r$num, num)))
               }
             }
           }
           ret <- newRet
         }
       }
       return(ret)
     },
     
     getAllPossDp = function(min, fac, maxDp){
       
       maxLoops <- ceiling((maxDp-min) / max(fac))
       mult <- 1:maxLoops
       val <- c()
       for(m in mult){
         val <- c(val, min + fac*m)
       }
       val <- unique(val)
       val <- val[val <= maxDp]
       return(val)
     },
     
     
     getRandomizedElements = function(dp, values, randomizeParameter, seed){
       new <- values
       dp <- dp-length(values)
       randomizeParameter <- randomizeParameter/100
       if(length(dp) > 0 && dp > 0){
         invPara <- 1/(1+(randomizeParameter))
         
         withr::with_seed(seed, {
           prob <- runif(length(values), invPara, 1)
           
           new_c <- sample(values, dp, replace=T, 
                           prob=prob)
         })

         new <- c(new, new_c)
         
         flag <- T
         while(flag){
           tt <- table(new)
           flag <- sapply(1:length(tt), function(i){
             tt_s <- tt[-i]
             ratio <- tt_s/tt[i]
             any(c(ratio >(1+randomizeParameter), ratio < invPara))
           })
           flag2 <- sapply(1:length(tt), function(i){
             tt_s <- tt[-i]
             diff <- abs(tt_s-tt[i])
             any(diff > 1)
           })
           flag <- any(flag) && any(flag2)
           if(flag){
             maxEl <- names(tt[tt==max(tt)])[1]
             minEl <- names(tt[tt==min(tt)])[1]
             tmp <- new[new==maxEl]
             new[new==maxEl][length(tmp)] <- minEl
           }
         }
       }
       return(new)
     },
     
     
     getRandomizedElementsForParent = function(poss, randomizeParameter, seed=seed){
       rP <- randomizeParameter/100
       newPoss <- list()
       for(p in poss){
         flag <- T
         for(n1_c in seq_len(length(p$num)-1)){
           n <- p$num[n1_c]
           c_n <- p$num[n1_c:length(p$num)]
           if(any((n+n*rP) < c_n |
                  n > c_n+c_n*rP)){
             flag <- F
             break
           }
         }
         if(flag) newPoss <- list.append(newPoss, p)
       }
       withr::with_seed(seed, {
         sa <- sample(1:length(newPoss), 1)
       })
       new <- newPoss[[sa]]
       new <- rep(new$val, new$num)
       return(new)
     },
     
     
     #Refresh data automatically?
     doRefresh = function(){
       if(private$doRefreshAfterLoading) return(T)
       dp <- self$getValidTotalDatapoints()
       private$generateDataAutomatically && 
          (is.numeric(dp) && dp<10000)
     },
     
     ###############################################
     # If any 'OtherVariable' changes, iterate through
     # all and add/update its data to 'data'
     refreshData = function(){
       #Verify for errors in dependencies
       private$otherVariableErrors <- list()
       
       #Validation of categorical vars
       #Numeric validation is done further down
       for(oV in private$otherVariable){
         type <- oV$getType()
         
         if(!is.null(type) && type == "categorical"){
           catModel <- oV$getCategoricalModel()
           if(is.null(catModel)){
             private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                        list(id=oV$getId(), status="error"))
             next
           }
           valid <- catModel$isValid()
           if(!valid$valid){
             private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                        list(id=oV$getId(), status="error"))
           }
           
           #If randomization is falsely used
           randAvail <- oV$isRandomizeAvailableForCatagorical()
           if(!randAvail$valid && any(unlist(catModel$getRandomize()))){
             private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                        list(id=oV$getId(), status="warning"))
           }
         }
       }
  
       
       lcm <- list()
       lcm_ignored <- c()
       
       min_dp <- list(self$getValidTotalDatapoints())
       ids <- list()
       for(oV in private$otherVariable){
         lcm <- list.append(lcm,self$getFactorOfDatapointsOfVar(oV$getId()), as.character(oV$getId()))
         if(!is.null(oV$getType()) && oV$getType() == "categorical" && 
            !is.null(oV$getCategoricalModel()$getType()) &&
            oV$getCategoricalModel()$getType()=="subgroup")
           lcm_ignored <- c(lcm_ignored, oV$getDependsOnOV())
  
         min_dp <- list.append(min_dp, self$getMinDatapointsOfVar(oV$getId()), as.character(oV$getId()))
         val <- oV$getDependsOnOV()
         if(is.null(val)) val <- ""
         ids <- list.append(ids, val, as.character(oV$getId()))
       }
       
       
       n <- names(lcm)
       lcm_new <- list()
       for(i in seq_len(length(lcm))){
         if(!n[i] %in% lcm_ignored){
           lcm_new <- list.append(lcm_new, unique(lcm[[i]]), n[i])
         }
       }
  
       
       min <- max(unlist(min_dp))
       lcm_val <- Lcm.vec(min_dp)
  
       global_dp <- min
       
       for(dp in min:lcm_val){
         valid <- T
         for(n in names(lcm_new)){
           local_min <- min_dp[[n]]* floor(dp / min_dp[[n]])
           if(!dp %in% (local_min+lcm_new[[n]])){
             valid <- F
             break
           }
         }
         if(valid){
           global_dp <- dp
           break
         }
       }
  
       lcm <- global_dp
  
       errorsOccur <- F
       
       ids <- self$sortOtherVariableAccordingToDependency(ids)
  
       n <- names(ids)
       if(is.empty(n)){
         resp <- self$getData()[,1]
         self$resetData() # except response?
         if(!is.null(resp) && length(resp) > 0){
           na <- colnames(private$data)
           df <- data.frame(resp)
           names(df) <- na
           private$data <- df
         }
       }
       for(i in seq_len(length(ids))){
         val <- c()
         oV <- self$getOtherVariable(n[i])
         if(!is.null(oV$getType())){
           if(oV$getType() != "categorical"){
             dist <- oV$getDist()
             if(!is.null(dist)){
               
               #If this oV depends on another oV
               if(!is.null(oV$getDependsOnOV())){
                 alt <- dist$getUseAlternative()
                 para <- dist$getParameter()
                 if(alt){
                   para <- dist$getAlternativeParameter()
                 }
                 errorsOccur <- F
                 for(pi in seq_len(length(para))){
                   p <- para[[pi]]
                   if(p$getType()=="variable"){
                     id <- p$getOtherVariableId()
                     if(is.null(id))next
                     if(!is.null(id) && length(id) > 0) id <- as.numeric(id)
  
                     valid <- self$isOtherVariableValidForDistParameter(
                       p,
                       oV$getId(),
                       id
                     )
                     if(valid$valid != "invalid"){
                       dependingoV <- self$getOtherVariable(as.numeric(p$getOtherVariableId()))
                       valuesOfOV <- dependingoV$getValues()
                       valuesOfOV <- as.numeric(self$getData(as.numeric(p$getOtherVariableId())))
                       dist$setParameterValue(pi, valuesOfOV, F, F)
                     }else{
                       dist$setParameterValueToDefault(pi, alt)
                       errorsOccur <- T
                       private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                         list(id=oV$getId(), status="error"))
                     }
                   }
                 }
                 #If no error occur, but this oV depends on an oV, where error occurred
                 if(!errorsOccur && 
                    !is.null(private$otherVariableErrors) && 
                    length(private$otherVariableErrors) > 0){
                   for(err in private$otherVariableErrors){
                     if(oV$getDependsOnOV() %in% err$id){
                       private$otherVariableErrors <- list.append(private$otherVariableErrors,
                                                         list(id=oV$getId(), status="warning"))
                       break;
                     }
                   }
                 }
               }
  
               oV$getDist()$randomValue(lcm) #Assign created value within distribution class
               val <- oV$getDist()$getValues(formatted=F)
             }
             self$setDataColumn(as.numeric(n[i]), val) 
           }else{
             self$setSubsetDataset(oV, lcm)
           }
         }else{
           self$setDataColumn(as.numeric(n[i]), rep("",lcm))
         }
       }
       
       #order data
       # self$orderData(ids)
       
       #shorten data to maximum of lcm
       if(lcm > 0 && dim(self$getData())[1] > lcm){
         self$setData(self$getData()[1:lcm,,F])
       }
       
       self$triggerReactiveValue("rxTriggerOtherVariableError")
     },
     
  
     #list: named list with names=id of OV and elements
     #depending ids of other OV
     #Returns a sorted list, where last elements
     #depends on the first elements
     sortOtherVariableAccordingToDependency = function(list, count=0, init=T){
       if(count >= length(list)**2) return(list) #should never happen
       n <- names(list)
       newList <- list
       pos <- 0
       for(i in seq_len(length(list)-1)){
         for(j in (i+1):length(list)){
           if(n[j] %in% list[[i]]) pos <- j
         }
         if(pos != 0){
           el <- newList[[i]]
           newList[[i]] <- NULL
           newList <- list.insert(newList, el, pos, n[i])
           newList <- self$sortOtherVariableAccordingToDependency(newList,count+1,init=F)
           break
         }
       }
       #reorder due to replacement variables
       #they should handled at the end of categorical variables
       if(init){
         nListNum <- list()
         nListCat <- list()
         nListCatRep <- list()
         n <- names(newList)
         for(i in seq_len(length(newList))){
           el <- newList[[i]]
           oV <- self$getOtherVariable(n[[i]])
           if(!is.null(oV$getType()) && oV$getType() == "categorical"){
             catModel <- oV$getCategoricalModel()
             if(!is.null(catModel$getType()) && catModel$getType() == "replacement"){
               nListCatRep <- list.append(nListCatRep, el, n[[i]])
             }else{
               nListCat <- list.append(nListCat, el, n[[i]])
             }
           }else{
             nListNum <- list.append(nListNum, el, n[[i]])
           }
         }
         newList <- c(nListCat,nListCatRep,nListNum)
       }
       return(newList)
     },
     
     
     orderData = function(ids){
       names <- c()
       data <- self$getData()
       for(id in names(ids)){
         ov <- self$getOtherVariable(id)
         new_name <- ov$getName()
         type <- ov$getType()
         if(!is.null(type) && type=="categorical"){
           dataC <- self$getData(column=as.numeric(id))
           levels <- unique(dataC)
           data[[new_name]] <- factor(dataC, levels)
         }
         names <- c(names, new_name)
       }
       sorted_data <- data %>% 
         arrange(!!!syms(names))
  
       self$setData(sorted_data)
     },
     
     
     #checks if a given sample size is valid due to
     #categorical variables that are uncapped.
     #returns a list(flag=T/F, c(lowerPossible,upperPossible))
     checkSampleSize = function(sampleSize){
       
       lcm <- list()
       lcm_ignored <- c()
       
       min_dp <- list()
       ids <- list()
       for(oV in private$otherVariable){
         if(!is.null(oV$getType()) && oV$getType() != "categorical") next
         
         lcm <- list.append(lcm,self$getFactorOfDatapointsOfVar(oV$getId()), as.character(oV$getId()))
         if(!is.null(oV$getType()) && oV$getType() == "categorical" && 
            !is.null(oV$getCategoricalModel()$getType()) &&
            oV$getCategoricalModel()$getType()=="subgroup")
           lcm_ignored <- c(lcm_ignored, oV$getDependsOnOV())
         
         min_dp <- list.append(min_dp, self$getMinDatapointsOfVar(oV$getId()), as.character(oV$getId()))
         val <- oV$getDependsOnOV()
         if(is.null(val)) val <- ""
         ids <- list.append(ids, val, as.character(oV$getId()))
       }
       
       n <- names(lcm)
       lcm_new <- list()
       for(i in seq_len(length(lcm))){
         if(!n[i] %in% lcm_ignored){
           lcm_new <- list.append(lcm_new, unique(lcm[[i]]), n[i])
         }
       }
       
       if(is.empty(min_dp)) min_dp <- list(a=1)
       
       min <- max(unlist(min_dp))
       lcm_val <- Lcm.vec(min_dp)
       
       if(min > sampleSize){
         return(list(F,c(min,min)))
       }
       
       global_dp <- min
       
       a <- max(min, floor(sampleSize / lcm_val) * lcm_val)
       b <- ceiling(sampleSize / lcm_val) * lcm_val
       
       if(sampleSize %in% c(a,b)) return(list(T))
       
       lowerPoss <- a
       upperPoss <- b
       
       for(dp in a:b){
         valid <- T
         for(na in names(lcm_new)){
           local_min <- min_dp[[na]]* floor(dp / min_dp[[na]])
           if(!dp %in% (local_min+lcm_new[[na]])){
             valid <- F
             break
           }
         }
         if(valid){
           global_dp <- dp
           if(dp < sampleSize) lowerPoss <- dp
           if(dp==sampleSize) return(list(T))
           if(dp > sampleSize){
             upperPoss <- dp
             break
           } 
         }
       }
       return(list(F, c(lowerPoss,upperPoss)))
     },
  
     
     ###############################################
     ################## Predictor ##################
     ###############################################
     
     createPredictor = function(){
       nextId <- max(c(private$predictorIds,0))+1
       newOV <- ModelCreatingDataPredictor$new(nextId, self)
       return(newOV)
     },
     
     setPredictors = function(preds){
       private$predictors <- preds
     },
     
     addPredictor = function(pred){
       private$predictorIds <- c(private$predictorIds, pred$getId())
       private$predictors <- list.append(private$predictors, pred)
       self$triggerReactiveValue("rxTriggerPredictor")
     },
     
     removePredictor = function(id=NULL, pred=NULL){
       if(is.null(id)){
         if(is.null(pred)){
           warning("Could not remove variable, since both attributes 'id' and 'pred' are NULL.")
         }else{
           id <- pred$getId()
         }
       }
       if(id %in% private$predictorIds){
         newList <- list()
         for(i in private$predictors){
           if(i$getId() != id) newList <- list.append(newList, i)
         }
         private$predictors <- newList
         private$predictorIds <- private$predictorIds[private$predictorIds!=id]
       }
       self$triggerReactiveValue("rxTriggerPredictor")
     },
     
     getPredictors = function(){
       return(private$predictors)
     },
     
     setPredictorIds = function(ids){
       private$predictorIds <- ids
     },
     
     getPredictorIds = function(){
       return(private$predictorIds)
     },
     
     getPredictor = function(id){
       if(is.null(id)) return(NULL)
       for(v in private$predictors){
         if(v$getId()==id) return(v)
       }
     },
     
     getPredictorByName = function(name){
       if(is.null(name)) return(NULL)
       for(v in private$predictors){
         if(v$getName()==name) return(v)
       }
     },
     
     # Does an intercept already exist?
     predictorHasIntercept = function(){
       for(v in private$predictors){
         if(v$getType()=="intercept") return(T)
       }
       return(F)
     },
     
     
     isPredictorDuplicate = function(id=NULL, pred=NULL){
       if(is.null(pred) && is.null(pred)) stop("id or pred have to be given")
       if(is.null(pred)) pred <- self$getPredictor(id)
       if(is.null(id)) id <- pred$getId()
       ov1 <- pred$getOVIds()
       if(is.empty(ov1)) return(F)
       predictors <- self$getPredictors()
       for(p in predictors){
         if(p$getId() != id){
           ov2 <- p$getOVIds()
           if(all(ov1 %in% ov2) && all(ov2 %in% ov1)) return(T)
         }else{
           return(F)
         }
       }
       return(F)
     },
     
     validationOfPredictors = function(duplicateAnnotation=F){
       listOfPreds <- c()
       listOfPredsName <- c()
       invalidList <- c()
       warnList <- c()
       
  
       predictors <- self$getPredictors()
       for(pred in predictors){
         listOfPreds <- c(listOfPreds,pred$getId())
         n <- pred$getName()
         if(is.null(n) || n=="") n <- "(unused)"
         listOfPredsName <- c(listOfPredsName,n)
         valid <- pred$isValid()
         if(valid$valid == "error") {
           invalidList <- c(invalidList, pred$getId())
         }else if(valid$valid == "warning") {
           warnList <- c(warnList, pred$getId())
         }else{
           if(self$isPredictorDuplicate(pred=pred)){
             invalidList <- c(invalidList, pred$getId())
             if(duplicateAnnotation){
               listOfPredsName[length(listOfPredsName)] <-
                 paste0(listOfPredsName[length(listOfPredsName)], " (duplicate)")
             }
           }
         }
       }
       names(listOfPreds) <- listOfPredsName
      
       errorWarningList <- list()
       for(li in seq_len(length(listOfPreds))){
         el <- listOfPreds[[li]]
         n <- names(listOfPreds)[[li]]
         if(el %in% invalidList){
           errorWarningList$invalid <- list.append(errorWarningList$invalid, el, n, T)
         }else if(el %in% warnList){
           errorWarningList$warning <- list.append(errorWarningList$warning, el, n, T)
         }else{
           errorWarningList$valid <- list.append(errorWarningList$valid, el, n, T)
         }
       }
       errorWarningList_sorted <- list()
       for(n in c("valid","warning","invalid")){
         if(n %in% names(errorWarningList))
         errorWarningList_sorted[[n]] <- errorWarningList[[n]]
       }
       return(errorWarningList_sorted)
     },
     

     
     getOtherVariablesforPredictors = function(){
       ov <- self$getOtherVariables()
       ovIds <- c()
       ovNames <- c()
       
       for(i in ov){
         ovIds <- c(ovIds, i$getId())
         ovNames <- c(ovNames, i$getName())
       }
       ovIds <- c(ovIds, "")
       ph <- ovNames[1:min(length(ovNames),3)]
       if(is.null(ph)) ph <- ""
       names(ovIds) <- c(ovNames, paste0(ph,collapse=" "))
       return(ovIds)
     },
     
     
     ###############################################
     ################## Parameter ##################
     ###############################################
     createParameter = function(formulaName=""){
       nextId <- max(c(private$parameterIds,0))+1
       newPara <- ModelCreatingDataParameter$new(id=nextId, mcd=self, formulaName=formulaName)
       return(newPara)
     },
     
     addParameter = function(para){
       private$parameterIds <- c(private$parameterIds, para$getId())
       private$parameters <- list.append(private$parameters, para)
       self$doTriggerParameter(para$getId())
     },
     
     setParameters = function(paras){
       private$parameters <- paras
     },
     
     #formulaOrder=T: Sort by para priority: When equal: Intercept < predictor < aux
     getParameters = function(formulaOrder=F){
       if(formulaOrder){
         newOrder <- list()
         for(para in private$parameters){
           bool <- T
           for(newParaId in seq_len(length(newOrder))){
             newPara <- newOrder[[newParaId]]
             if(para$getPriority() < newPara$getPriority()){
               bool <- F
             }else if(para$getPriority() == newPara$getPriority()){
               if(para$getType()=="intercept" ||
                  newPara$getType()=="aux"){
                 bool <- F
               }
             }
             if(!bool){
               newOrder <- list.insert(newOrder, para, newParaId)
               break
             } 
           }
           if(bool){
             newOrder <- list.append(newOrder, para)
           }
         }
         return(newOrder)
       }else{
         return(private$parameters) 
       }
     },
     
     getParameter = function(paraId){
       for(para in private$parameters){
         if(para$getId()==paraId) return(para)
       }
     },
     
     getParameterOfPredictor = function(predId){
       for(para in private$parameters){
         if(!is.null(para$getPredId()) &&
            para$getPredId() == predId) return(para)
       }
     },
     
     getParameterSubByTree = function(tree, returnNULL=F, parameters=NULL){
       if(is.null(parameters)) parameters <- private$parameters
       retSubs <- list()
       
       selected <- get_selected(tree, format="slices")
       
       for(ss in selected){
         subTree <- tree
         while(is.list(ss)){
           subTree <- subTree[[names(ss)]]
           ss <- ss[[1]]
         }
         paraId <- attr(subTree, "bayasParaId")
         subName <- attr(subTree, "bayasSubName")
         
         if(returnNULL && is.null(paraId))
           return(NULL)
         
         for(para in parameters){
           if(para$getId()==paraId){
             if(is.null(subName)){
               ss <- para$getSingleSub()
               if(is.null(ss)){
                 retSubs <- list.append(retSubs, para)
               }else{
                 retSubs <- list.append(retSubs, para$getSingleSub(), names(para$getSubs())[1])
               }
             }else{
               retSubs <- list.append(retSubs, para$getSubs()[[subName]], subName)
             }
             break
           }
         }
       }
       return(retSubs)
     },
     getParameterSubNameByTree = function(selected,parameters=NULL){
       if(is.null(parameters)) parameters <- private$parameters
       anc <- attr(selected[[1]],  which="ancestry")
       name <- selected[[1]][1]
       for(para in parameters){
         if(!is.empty(anc) && para$getName() == anc){
           name <- str_split(name, "<sub>")[[1]][2]
           name <- str_split(name, "</sub>")[[1]][1]
           return(name)
         }else if(para$getName() == name){
           return(NULL)
         }
       }
     },
     
     setParameterIds = function(ids){
       private$parameterIds <- ids
     },
     
     getParameterIds = function(){
       return(private$parameterIds)
     },
     
     removeParameter = function(id=NULL, para=NULL){
       if(is.null(id)){
         if(is.null(para)){
           warning("Could not remove variable, since both parameter 'id' and 'para' are NULL.")
         }else{
           id <- para$getId()
         }
       }
       
       if(id %in% private$parameterIds){
         newList <- list()
         for(i in private$parameters){
           if(i$getId() != id) newList <- list.append(newList, i)
         }
         private$parameters <- newList
         private$parameterIds <- private$parameterIds[private$parameterIds!=id]
       }
       self$triggerReactiveValue("rxTriggerParameter")
     },
     
     
     
     
     
     ###############################################
     ################ Generate data ################
     ###############################################
     
     setGenerateData = function(generateData, type=c("ssuVar","ssu","tdp")){
       change <- F
       if(is.null(type)){
         for(i in 1:3){
           if(!equal0(private$generateData[[i]],generateData[[i]])){
             private$generateData <- generateData
             change <- T
             break
           }
         }
       }else{
         match.arg(type)
         if(!equal0(private$generateData[[type]], generateData)){
           private$generateData[[type]] <- generateData
           change <- T
           if(type != "tdp"){
             unitVar <- private$generateData$ssuVar
             unitNumber <- private$generateData$ssu
             if(!is.null(unitVar) && !is.empty(unitVar) && unitVar != ""){
               oV <- self$getOtherVariable(unitVar)
               
               unit <- self$getMinDatapointsOfVar(unitVar)
               if(!is.numeric(unitNumber) || unitNumber < 1) unitNumber <- 1
               
               val <- unit*unitNumber
               
               private$generateData$tdp <- val 
             }
           }
         }
       }
       if(change) self$doTriggerGenerateData() 
     },
     
     getGenerateData = function(type=c("ssuVar","ssu","tdp")){
       if(is.null(type)){
         return(private$generateData)
       }else{
         match.arg(type)
         return(private$generateData[[type]])
       }
     },
     
     #Returns the setted total datapoints if valid,
     #otherwise min datapoints
     getValidTotalDatapoints = function(){
       tdp <- private$generateData[["tdp"]]
       checkedDP <- self$checkSampleSize(tdp)
       if(!is.null(tdp) && checkedDP[[1]]){
         return(tdp)
       }else{
         return(checkedDP[[2]][1])
         # return(self$getDataMinPoints())
       }
     },
     
     setGenerateSeed = function(seed){
       private$generateSeed <- seed
       self$triggerReactiveValue("rxTriggerPredictor")
     },
     getGenerateSeed = function(){
       private$generateSeed
     },
     
     setGenerateDataAutomatically = function(flag, silent=F){
       private$generateDataAutomatically <- flag
       # if(self$doRefresh() && !silent) self$refreshData()
     },
     getGenerateDataAutomatically = function(){
       private$generateDataAutomatically
     },
     
     
     
     ###############################################
     ################### Formula ###################
     ###############################################
     
     #PlanningFormula object
     setFormulaObject = function(formula){
       if(is.null(private$formulaObject)){
         private$formulaObject <- formula
       }else{
         stop("Should just setted once")
       }
     },
     getFormulaObject = function(){
       return(private$formulaObject)
     },
     
     setMcdFormula = function(mcdFormula){
       private$mcdFormula <- mcdFormula
     },
     getMcdFormula = function(){
       return(private$mcdFormula)
     },
     
     
     
     ###############################################
     ##################### SSD #####################
     ###############################################
     
     setMcdSSD = function(mcdSSD){
       private$mcdSSD <- mcdSSD
     },
     getMcdSSD = function(){
       return(private$mcdSSD)
     },
     
     
     
     
     ###############################################
     ################ Visualization ################
     ###############################################
     
     setVisualizeData = function(visualizeData, 
                                 type=c("yAxis","xAxis",
                                        "groupBy","plotType")){
       change <- F
       if(is.null(type)){
         for(i in 1:4){
           if(!equal0(private$visualizeData[[i]], visualizeData[[i]])){
             private$visualizeData <- visualizeData
             change <- T
             break
           }
         }
       }else{
         match.arg(type)
         if(!equal0(private$visualizeData[[type]], visualizeData)){
           private$visualizeData[[type]] <- visualizeData
           change <- T
         }
       }
       if(change) 
         self$doTriggerVisualizeData()
     },
     
     getVisualizeData = function(type=c("yAxis","xAxis",
                                        "groupBy","plotType")){
       if(is.null(type)){
         return(private$visualizeData)
       }else{
         match.arg(type)
         return(private$visualizeData[[type]])
       }
     },
     
     
     
     
     
     ###############################################
     ################## Reporting ##################
     ###############################################
     
     getReportExpInp = function(){
       return(private$reportExpInp)
     },
     
     setReportExpInp = function(flags){
       if(!is.null(flags) && length(flags) == 8 &&
          vectorEqual(names(flags), c("onlyUsedVars", "onlyValidParas", "includeGenData", "replaceMinData", "includeSSD", "includeSSDResult", "onlyUsedGoals", "onlySSD"))){
         private$reportExpInp <- flags
       }
     },
     
     
     
     doTriggerResponse = function(type=c("name", "step",
                                         "dist","link")){
       match.arg(type)
       if(type=="name"){
         self$triggerReactiveValue("rxTriggerResponseName")
       }else if(type=="step"){
         self$triggerReactiveValue("rxTriggerResponse")
       }else if(type=="dist"){
        self$triggerReactiveValue("rxTriggerResponseDist")
       }else if(type=="link"){
         self$triggerReactiveValue("rxTriggerResponseLink")
       }
     },
     
     doTriggerOtherVariable = function(id, type=c("name", "step",
                                                  "dist")){
       match.arg(type)
       private$lastOtherVariableId <- id
       if(type=="name"){
         self$updateDependentOtherVariable(id)
         self$triggerReactiveValue("rxTriggerOtherVariableName")
       }else if(type=="step"){
         self$triggerReactiveValue("rxTriggerOtherVariableStep")
       }else if(type=="dist"){
         self$triggerReactiveValue("rxTriggerOtherVariableDist")
         #if any data in a ModelCreatingDataOtherVariable that has dependencies of other variables
         #also update them
         self$updateDependentOtherVariable(id)
  
         if(self$doRefresh()) self$refreshData()
       }
     },
     
     doTriggerPredictor = function(id, type=c("","name","involvedVars","info")){
       
       if(id %in% private$predictorIds)
         self$triggerReactiveValue("rxTriggerPredictor")
  
     },
     
     doTriggerParameter = function(id){
       if(is.null(id) || id %in% private$parameterIds)
         self$triggerReactiveValue("rxTriggerParameter")
     },
     
     doTriggerFormula = function(){
       self$triggerReactiveValue("rxTriggerFormula")
     },
     
     doTriggerGenerateData = function(){
       self$triggerReactiveValue("rxTriggerGenerateData")
     },
     
     doTriggerSSDGoals = function(){
       self$triggerReactiveValue("rxTriggerSSDGoals")
     },
     
     doTriggerVisualizeData = function(){
       self$triggerReactiveValue("rxTriggerVisualizeData")
     },
     
     doTriggerLoadMCD = function(){
       self$triggerReactiveValue("rxTriggerLoadMCD")
     },
  
     getLatex = function(id){

       latex <- list()
       
       latex$preFormula <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/head_formula.txt"))
       latex$preSSD <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/head_ssd.txt"))
       
       #Formula
       latex$formula <- self$getLatexFormula(id=id, sub="sub")

       #Other variable
       latex$ov <- self$getLatexOtherVariables(id=id,sub="sub")
      
       #Parameter (only the seeds)
       latex$parameter <- self$getLatexParameter(id=id,sub="sub")
       
       #Generated data
       latex$data <- self$getLatexDataset(id=id, sub="sub")
       
       #SSD
       latex$ssd <- self$getLatexSSD(id=id, sub="sub")
       
       return(latex)
     },
     
     getLatexFormula = function(id, sub){
       latex <- self$getMcdFormula()$getLatex()
       pre <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Formula.txt"))
       latex <- paste0("\\", sub, "section{Formula}\n", pre, latex)
     },
     
     getLatexOtherVariables = function(id, sub){

       oVs <- self$getOtherVariables()
       latexList <- list(pre="", ov=list())
       
       if(is.empty(oVs)) return(latexList)
       
       latexList$pre <- paste0("\\", sub, "section{Definition of (other) variables }\n")
       
       latexList$preVariable <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables.txt"))
       latexList$preVariableNum <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables_num.txt"))
       latexList$preVariableCat <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables_cat.txt"))
       latexList$preVariableCatInd <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables_cat_ind.txt"))
       latexList$preVariableCatSub <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables_cat_subgroup.txt"))
       latexList$preVariableCatRep <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/Variables_cat_replace.txt"))
       
       for(ov in oVs){
         if(is.null(ov$getType())) next
   
         latex <- ""
         
         #categorical
         catModel <- ov$getCategoricalModel()
         if(!is.null(catModel)){
           
           type <- catModel$getType()
           #independent
           if(type == "independent"){
             values <- catModel$getValues()
             
             latex <- paste0("\\",sub,"subsection{`",wordToLatexConform(ov$getName()), "'}\n")
             latex <- paste0(latex, "Type: ", wordToLatexItalic("Categorical")," \\\\\n")
             latex <- paste0(latex, "Elements: ", wordToLatexItalic(paste0(values, collapse = ", "), wordToLatexConform=T), " \\\\\n")
             
             #Equal
             equallyExplicit <- catModel$getValuesDistributed()
             
             #Explicit
             if(equallyExplicit == "explicit"){
               latex <- paste0(latex, "Frequency of elements: ", wordToLatexItalic("Explicit")," \\\\\n")
               distributed <- catModel$getValuesFrequency()
               valFreq <- paste0(values, "(", distributed, ")")
               latex <- paste0(latex, "Elements (frequency): ", wordToLatexItalic(paste0(valFreq, collapse = ", "), wordToLatexConform=T), " \\\\\n")
             }
             #equally
             else{
               latex <- paste0(latex, "Frequency of elements: ", wordToLatexItalic("Equal")," \\\\\n")
             }
             
             #capped
             capped <- catModel$getCapped()
             capped <- ifelse(capped, "Yes", "No")
             latex <- paste0(latex, "Capped: ", wordToLatexItalic(capped), " \\\\\n")
             
             #randomized?
             randomized <- catModel$getRandomize()
             randomizedT <- ifelse(randomized, "Yes", "No")
             latex <- paste0(latex, "Randomize frequency: ", wordToLatexItalic(randomizedT), " \\\\\n")
             
             #Variation + random seed
             if(randomized){
               randPara <- catModel$getRandomParameter()
               randSeed <- catModel$getSeed()
               
               latex <- paste0(latex, "Randomize frequency parameter: ", wordToLatexItalic(randPara), " \\\\\n")
               latex <- paste0(latex, "Randomize frequency seed: ", wordToLatexItalic(randSeed), " \\\\\n")
             }
             
           }
           #subgroup
           else if(type == "subgroup"){
             
             #Explicitly for elements?
             values <- catModel$getValues()
             
             latex <- paste0("\\",sub,"subsection{`",wordToLatexConform(ov$getName()), "'}\n")
             latex <- paste0(latex, "Type: ", wordToLatexItalic("Categorical subgroup"), "\\\\\n")
             dep <- ov$getDependsOnOV()
             depOv <- self$getOtherVariable(dep)
             latex <- paste0(latex, "Subgroup of: `", wordToLatexConform(depOv$getName()), "' \\\\\n")
             
             explicitly <- catModel$getSubgroupExplicit()
             explicitlyT <- ifelse(explicitly, "Yes", "No")
             latex <- paste0(latex, "Elements are explicitly defined: ", wordToLatexItalic(explicitlyT), " \\\\\n")
             if(!explicitly){

               #Equal
               equallyExplicit <- catModel$getValuesDistributed()
               distributed <- catModel$getValuesFrequency()
               
               #capped
               capped <- catModel$getCapped()

               #randomized?
               randomized <- catModel$getRandomize()
               
               while(is.list(values)){
                 values <- values[[1]]
                 equallyExplicit <- equallyExplicit[[1]]
                 distributed <- distributed[[1]]
                 capped <- capped[[1]]
                 randomized <- randomized[[1]]
               }
               
               #Values
               latex <- paste0(latex, "Elements: ", wordToLatexItalic(paste0(values, collapse = ", "), wordToLatexConform=T), " \\\\\n")
               
               #Explicit
               if(equallyExplicit == "explicit"){
                 latex <- paste0(latex, "Frequency of elements: ", wordToLatexItalic("Explicit"), "\\\\\n")
                 valFreq <- paste0(values, "(", distributed, ")")
                 latex <- paste0(latex, "Elements (frequency): ", wordToLatexItalic(paste0(valFreq, collapse = ", "), wordToLatexConform=T), " \\\\\n")
               }
               #equally
               else{
                 latex <- paste0(latex, "Frequency of elements: ",wordToLatexItalic("Equal"), "\\\\\n")
               }
               
               capped <- ifelse(capped, "Yes", "No")
               latex <- paste0(latex, "Capped: ", wordToLatexItalic(capped), " \\\\\n")
               
               randomizedT <- ifelse(randomized, "Yes", "No")
               latex <- paste0(latex, "Randomize frequency: ", wordToLatexItalic(randomizedT), " \\\\\n")

               #Variation + random seed
               if(randomized){
                 randPara <- catModel$getRandomParameter()
                 randSeed <- catModel$getSeed()
                 
                 latex <- paste0(latex, "Randomize frequency parameter: ", wordToLatexItalic(randPara), " \\\\\n")
                 latex <- paste0(latex, "Randomize frequency seed: ", wordToLatexItalic(randSeed), " \\\\\n")
               }
 
             }else{
               tableData <- catModel$getTableData(wordToLatexConform(ov$getName()))
               caption <- paste0("Elements of subgroup `", wordToLatexConform(ov$getName()), 
                                 "' for its super group `",wordToLatexItalic(wordToLatexConform(depOv$getName()), wordToLatexConform=T) ,"'.")
               label <- paste0("id-",id,"_model-", self$getModelName(), "_ovDef-",ov$getName())
               latex <- paste0(latex, "Elements: see also table~\\ref{", label, "}\\\\\n")
               latex <- paste0(latex, dfToLatexTable(tableData, caption=caption, label=label))
               varSub <- self$getLatexOtherVariablesSubgroup(catModel)
               varSub <- paste0("\n", varSub)
               latex <- paste0(latex, paste0(varSub, collapse=""))
             }
           }
           #replacement
           else if(type == "replacement"){
             
             replaceOf <- ov$getDependsOnOV()
             replaceOfOv <- self$getOtherVariable(replaceOf)
      
             replaceOfoVCatModel <- replaceOfOv$getCategoricalModel()
             valid <- replaceOfoVCatModel$isValid()
             
             df <- data.frame()
             
             if(valid$valid){
               df <- replaceOfoVCatModel$getTableData(replaceOfOv$getName())
               
               n <- names(df)
               df <- cbind(df, df[[dim(df)[2]]])
               names(df) <- c(n, ov$getName())
               
               df[[dim(df)[2]]] <- ov$getCategoricalModel()$getReplacedValues(replaceOfOv)
             }
             

             caption <- paste0("Elements of replacement variable `", wordToLatexConform(ov$getName()), 
                               "' for its super group `",wordToLatexItalic(replaceOfOv$getName(), wordToLatexConform=T) ,"'.")
             label <- paste0("id-",id,"_model-", self$getModelName(), "_ovDef-",ov$getName())
             
             latex <- paste0("\\",sub,"subsection{`",wordToLatexConform(ov$getName()), "'}\n")
             latex <- paste0(latex, "Type: ", wordToLatexItalic("Categorical replacement"), "\\\\\n")
             latex <- paste0(latex, "Replaced values of: `", wordToLatexConform(replaceOfOv$getName()), "' \\\\\n")
             latex <- paste0(latex, "Elements: see table~\\ref{", label, "}\\\\\n")
             latex <- paste0(latex, dfToLatexTable(df, caption=caption, label=label))
           }
           
         }
         #non categorical
         else{
 
           latex <- paste0("\\",sub,"subsection{`",wordToLatexConform(ov$getName()), "'}\n")
           
           latex <- paste0(latex, "Type: ", wordToLatexItalic("Numerical"), "\\\\\n")

           dist <- ov$getDist()
           distLatex <- dist$getAsLatexSyntax(mcd=self)
           latex <- paste0(latex, "Generated by: $", wordToLatexConform(ov$getName())," \\sim ", distLatex, "$\\\\\n")
            
           # negative values
           negVal <- dist$getNegateValues()
           if(negVal) latex <- paste0(latex, "Generated values will be ",wordToLatexItalic("negated"), "\\\\\n")
           
           # seed
           randSeed <- dist$getSeed()
           latex <- paste0(latex, "Seed: ", wordToLatexItalic(randSeed), " \\\\\n")
           
         }
         
         latex <- paste0(latex, "\n")
         catType <- NULL
         if(ov$getType() == "categorical"){
           catType <- ov$getCategoricalModel()$getType()
         }
         latex <- list(use=ov$isInUse(), ovType=ov$getType(), catType=catType, latex=latex)
         latexList$ov <- list.append(latexList$ov, latex, ov$getName())
       }
   
       return(latexList)
     },
     getLatexOtherVariablesSubgroup = function(catModel, elementNames=NULL, retSubs=list()){
       #Values
       values <- catModel$getValues()
       
       if(is.null(elementNames)){
         for(el in names(values)){
           ret <- self$getLatexOtherVariablesSubgroup(catModel, c(elementNames, el), retSubs)
           if(length(ret) > 1){
             retSubs <- list.append.vector(retSubs, ret)
           }else{
             retSubs <- list.append(retSubs, ret)
           }
         }
       }else{
         
         equallyExplicit <- catModel$getValuesDistributed()
         distributed <- catModel$getValuesFrequency()
         capped <- catModel$getCapped()
         randomized <- catModel$getRandomize()
         randPara <- catModel$getRandomParameter()
         randSeed <- catModel$getSeed()
         
         for(el in elementNames){
           values <- values[[el]]
           equallyExplicit <- equallyExplicit[[el]]
           distributed <- distributed[[el]]
           capped <- capped[[el]]
           randomized <- randomized[[el]]
           randPara <- randPara[[el]]
           randSeed <- randSeed[[el]]
         }
         
         if(is.list(values)){
           for(el in names(values)){
             ret <- self$getLatexOtherVariablesSubgroup(catModel, c(elementNames, el), retSubs)
             if(length(ret) > 1){
               retSubs <- list.append.vector(retSubs, ret)
             }else{
               retSubs <- list.append(retSubs, ret)
             }
           }
         }else{
           latex <- paste0("For super elements: ", wordToLatexBold(paste0(elementNames, collapse=":")), " \\\\\n")
           
           #Values
           latex <- paste0(latex, "Elements: ", wordToLatexItalic(paste0(values, collapse = ", ")), " \\\\\n")
           
           #Explicit
           if(equallyExplicit == "explicit"){
             latex <- paste0(latex, "Frequency of elements: ", wordToLatexItalic("Explicit"), "\\\\\n")
             valFreq <- paste0(values, "(", distributed, ")")
             latex <- paste0(latex, "Elements (frequency): ", wordToLatexItalic(paste0(valFreq, collapse = ", ")), " \\\\\n")
           }
           #equally
           else{
             latex <- paste0(latex, "Frequency of elements: ", wordToLatexItalic("Equal"), "\\\\\n")
           }
           
           capped <- ifelse(capped, "Yes", "No")
           latex <- paste0(latex, "Capped: ", wordToLatexItalic(capped), " \\\\\n")
           
           randomizedT <- ifelse(randomized, "Yes", "No")
           latex <- paste0(latex, "Randomize frequency: ", wordToLatexItalic(randomizedT), " \\\\\n")
           
           #Variation + random seed
           if(randomized){
             randPara <- catModel$getRandomParameter()
             randSeed <- catModel$getSeed()
             
             latex <- paste0(latex, "Randomize frequency parameter: ", wordToLatexItalic(randPara), " \\\\\n")
             latex <- paste0(latex, "Randomize frequency seed: ", wordToLatexItalic(randSeed), " \\\\\n")
           }
           return(latex)
         }
       }
       
       return(retSubs)
     },
     
     
     getLatexParameter = function(id, sub){

       pre <- "\\subsection{Parameter seeds} \n"
       
       latex <- ""
       
       paras <- self$getParameters()
       for(para in paras){

         predName <- ""
         if(!is.null(para$getPredId())){
           pred <- self$getPredictor(para$getPredId())
           predName <- pred$getName() 
           predName <- wordToLatexConform(predName)
           
           predName <- paste0("b_{", predName, "}")
           
           if(para$isVector()){
             predName <- paste0("\\underline{", predName, "}")
           }
         }else{
           predName <- transformParaNameToLatex(para$getName())
         }
  
         
         seedsPrior <- c()
         seedsDataGen <- c()
         subs <- para$getSubs()

         paraTable <- "\\begin{table} [!htbp] \n \\begin{center} \n  \\begin{tabular}{||c c c||} \n "
         paraTable <- paste0(paraTable, "\\hline $", predName, "$ & Generate data & Inference \\\\ \\hline \n")
         for(sub_i in seq_along(subs)){
           sub <- subs[[sub_i]]
           sub_name <- names(subs)[[sub_i]]
           priorDist <- sub$getPrior()$getSeed()
           dataGenDist <- sub$getValueDistribution()$getSeed()
           paraTable <- paste0(paraTable, "  ", sub_name, " & ", priorDist," & ", dataGenDist , " \\\\ \n")
         }

         paraTable <- paste0(paraTable, "  \\hline \n")
         paraTable <- paste0(paraTable, "  \\end{tabular} \n")
         paraTable <- paste0(paraTable, " \\end{center} \n")
         paraTable <- paste0(paraTable, " \\caption{Used seeds for parameter `$", predName, "$'.} \n")
         paraTable <- paste0(paraTable, "\\end{table} \n")
         
         latex <- paste0(latex, paraTable)
       }
       
       return(list(
         pre = pre,
         latex = latex
       ))
     },
     
     getLatexDataset = function(id, sub){

       pre <- "\\subsection{Example data} \n"
       pre <- paste0(pre, readTxtFile(paste0(report_folder,"/GeneralTex/Planning/GenData.txt")))

       
       copy <- self$getInstance()
       #remove unused ov
       rmOv <- c()
       for(ov in copy$getOtherVariables()){
         if(!ov$isInUse()) rmOv <- c(rmOv, ov$getId())
       }

       for(idOv in rmOv){
         
         oV <- self$getOtherVariable(idOv)
         
         #from most super group to subgroup
         removeList <- c()
         checkids <- oV$getId()
         while(!is.empty(checkids)){
           removeList <- c(removeList, checkids)
           newCheckids <- c()
           for(i in checkids){
             ids <- cMCD$getOtherVariableIdsDependentOnThis(i)
             newCheckids <- c(newCheckids, ids)
           }
           checkids <- newCheckids
         }

         for(rm_id in rev(removeList)){
           copy$removeOtherVariable(rm_id)
         }
       }
       
       
       check <- copy$checkSampleSize(1)
       minDP <- 1
       if(!check[[1]]) minDP <- check[[2]][1]
       minDatasetWOUnusedOV <- copy$getRandomDataset(minDP, 1234)
       minDatasetWOUnusedOV <- format(minDatasetWOUnusedOV, digits=3)
       
       modelName <- wordToLatexConform(self$getModelName())
       
       caption <- paste0("Minimum example dataset of `", wordToLatexItalic(modelName), "'.")
       label <- paste0("id-",id,"_model-", self$getModelName(), "_minDatasetWOUnusedOV")
       minDatasetLatexWOUnusedOV <- dfToLatexTable(df=minDatasetWOUnusedOV, caption=caption, label=label)
       
       
       check <- self$checkSampleSize(1)
       minDP <- 1
       if(!check[[1]]) minDP <- check[[2]][1]
       minDataset <- self$getRandomDataset(minDP, 1234)
       genDataset <- self$getData()
       minDataset <- format(minDataset, digits=3)
       genDataset <- format(genDataset, digits=3)
       
 
       caption <- paste0("Minimum example dataset of `", wordToLatexItalic(modelName), "'.")
       label <- paste0("id-",id,"_model-", self$getModelName(), "_minDataset")
       minDatasetLatex <- dfToLatexTable(df=minDataset, caption=caption, label=label)
       
       caption <- paste0("Example dataset of `", wordToLatexItalic(modelName), "'.")
       label <- paste0("id-",id,"_model-", self$getModelName(), "_exampleDataset")
       genDatasetLatex <- dfToLatexTable(df=genDataset, caption=caption, label=label)
       
       responseSeed <- paste0("Response seed: ", wordToLatexItalic(private$generateSeed), "\\\\\n")

       return(list(
         pre = pre,
         minDatasetLatexWOUnusedOV = minDatasetLatexWOUnusedOV,
         minDataset = minDatasetLatex,
         genDataset = genDatasetLatex,
         responseSeed = responseSeed))
     },
     
     getLatexSSD = function(id, sub){
       
       latex <- list()
       
       latexPre <- paste0("\\", sub, "section{Sample size determination} \n")
       
       preSSD <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/SSD.txt"))
       preSSDGoal <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/SSD_goal.txt"))
       preSSDResult <- readTxtFile(paste0(report_folder,"/GeneralTex/Planning/SSD_result.txt"))
       
       latexPre <- paste0(latexPre, preSSD, preSSDGoal)
       
       mcdSSD <- private$mcdSSD
       
       mcd <- mcdSSD$getMcd()
       #If mcdOff is not null, use this instead (otherwise e.g. changed goals could falsely reported)
       if(!is.null(mcdSSD$getMcdOff())) mcd <- mcdSSD$getMcdOff()
       
       add <- list(goals=list(), result=list())
       
       
       #Parameters
       power <- mcdSSD$getPower()
       maxN <- mcdSSD$getMaxN()
       acc <- mcdSSD$getAccuarcy()
       seed <- mcdSSD$getSeed()
       
       latexPre <- paste0(latexPre, "Power: ", wordToLatexItalic(power), "\\\\\n")
       latexPre <- paste0(latexPre, "Max N: ", wordToLatexItalic(maxN), "\\\\\n")
       latexPre <- paste0(latexPre, "Approximate number simulations: ", wordToLatexItalic(acc), "\\\\\n")
       latexPre <- paste0(latexPre, "Seed: ", wordToLatexItalic(seed), "\\\\\n")
       
       #UsedGoals
       latexPre <- paste0(latexPre, "\\", sub, "subsection{Sample size determination goals} \n")
       
       latex$latexPre <- latexPre
       
       goals <- mcdSSD$getGoals(used=T)
       for(g in goals){
         if(!g$getIsValid()) next
         goalName <- g$getName()
         groupA <- g$getParametersA(twoListFormat = T, asFormulaNames=T)
         groupB <- g$getParametersB(twoListFormat = T, asFormulaNames=T)
         groupA <- formulaParameterToLatex(groupA)
         groupB <- formulaParameterToLatex(groupB)
         gType <- g$getType()
         ropeExclude <- g$getRopeExcludeInclude()
         ropeLower <- g$getRopeLower()
         ropeUpper <- g$getRopeUpper()
         hdiMass <- g$getHDI()
         prec <- g$getPrecWidth()
         
         goalPlot <- removeUnnecessaryEnvInPlot(g$getPlot())
         
         add$goals <- list.append(add$goals, goalPlot, goalName)
         
         caption <- paste0("A single goal `", wordToLatexConform(goalName), "' of the sample size determination. ")
         
         #caption rope vs prec?
         if(gType == "rope"){
           caption <- paste0(caption, "Histograms of the expected difference of the parameters of the ", 
                             wordToLatexItalic("inference"), " model and the ", 
                             wordToLatexItalic("data generation") ," model before sampling. ")
           if(ropeExclude=="exclude"){
             caption <- paste0(caption, "The red rectangle represents the ROPE. ")
           }else{
             caption <- paste0(caption, "The green rectangle represents the ROPE. ")
           }
         }else{
           caption <- paste0(caption, "Histograms of the expected difference of the parameters of the ", 
                             wordToLatexItalic("inference"), " model before sampling. ",
                             "The blue part of the histogram and the black bar show the ", hdiMass, 
                             "\\% credible interval. The goal is reached and thus the sample is considered sufficient if the width ",
                             "of this interval is less than or equal to ", prec,". ")
         }
         
         label <- paste0("id-",id,"_model-", mcd$getModelName(), "_goal-",goalName)
         plotId <- paste0(label,"_",id)
         plotLatex <- plotToTex(plotId=plotId, plot=goalPlot, caption=caption, label=label)
         
         gLatex <- paste0("Goal ", wordToLatexItalic(goalName, wordToLatexConform=T),", see figure~\\ref{", label, "} \\\\\n")
         gLatex <- paste0(gLatex,"Parameters in group `A': ", paste0(groupA, collapse=", "), "\\\\\n")
         gLatex <- paste0(gLatex,"Parameters in group `B': ", paste0(groupB, collapse=", "), "\\\\\n")
         
         if(gType == "rope"){
           gLatex <- paste0(gLatex,"Goal type: ", wordToLatexItalic("ROPE") , "\\\\\n")
           gLatex <- paste0(gLatex,"ROPE type: ", wordToLatexItalic(str_to_title(ropeExclude)) , "\\\\\n")
           gLatex <- paste0(gLatex,"ROPE lower bound: " , wordToLatexItalic(ropeLower) , "\\\\\n")
           gLatex <- paste0(gLatex,"ROPE upper bound: " , wordToLatexItalic(ropeUpper) , "\\\\\n")
           gLatex <- paste0(gLatex,"Probability mass: " , wordToLatexItalic(hdiMass) , "\\%\\\\\n")
         }else{
           gLatex <- paste0(gLatex,"Goal type: " , wordToLatexItalic("Precision") , "\\\\\n")
           gLatex <- paste0(gLatex,"Precision width: " , wordToLatexItalic(prec, wordToLatexConform=T) , "\\\\\n")
           gLatex <- paste0(gLatex,"Probability mass: " , wordToLatexItalic(hdiMass) , "\\%\\\\\n")
         }
         gLatex <- paste0(gLatex, plotLatex)
         latex$goals <- list.append(latex$goals, list(used=g$getInUse(), latex=gLatex), goalName)
       }

       #SSD Result
       ssd <- mcdSSD$getSsdObject()
       if(!is.null(ssd)){
         resultLatex <- paste0("\\", sub, "subsection{Sample size determination result} \n")
         resultLatex <- paste0(resultLatex, " ", preSSDResult)
         
         bestCandidate <- NULL
         credibleInterval <- NULL
         resultsSSD <- ssd$intern$resultsSSD
         resultsSSDHigh <- resultsSSD[resultsSSD$tendency == 1,]
         if(dim(resultsSSDHigh)[1] > 0){
           bestCandidate <- min(resultsSSDHigh$N)
           power <- ssd$intern$resultsPowerBinomial[ssd$intern$resultsPowerBinomial$N==bestCandidate,]
           credibleInterval <- c(power$powerLow, power$powerHigh)
         }
         
         pB <- ssd$intern$resultsPowerBinomial
         pB <- pB[pB$N == ssd$intern$N_high,]
         
         labelTable <- paste0("id-",id,"_model-", mcd$getModelName(), "_resultTable")
         
         #Result plot
         caption <- paste0("Sample sizes (horizontal axis) simulated in sample size determination (`", wordToLatexConform(mcd$getModelName()),"'), and power (vertical axis) reached for these sample sizes. ",
                           "Each circle marks the fraction of trials with the indicated sample size in which the goal was achieved. ",
                           "Vertical error bars are 90\\% credible intervals of power. \n",
                           "The blue dashed line shows the desired power and the blue circle/bar shows the smallest of the tested sample sizes at which this power is probably surpassed. ",
                           "See table~\\ref{",labelTable,"} for more details on the number of simulations per sample size.")
         labelPlot <- paste0("id-",id,"_model-", mcd$getModelName(), "_", id ,"_resultGraph")
         resultPlot <- plotResults(ssd, plotTriangles=F)
         add$result <- removeUnnecessaryEnvInPlot(resultPlot)
         plotLatex <- plotToTex(plotId=labelPlot, plot= resultPlot, caption=caption, label=labelPlot)
         

         #Result table
         caption <- paste0("Sample size determination of experiment `", wordToLatexConform(mcd$getModelName()), "'. ",
                           "Simulated ", wordToLatexItalic("N"), " with its number of simulations and empirical power.")
         tt <- ssd$intern$resultsSSD
         tt <- tt[,1:4]
         tt$power <- formatC(tt$power, digits=3)
         names(tt) <- c("N", "\\#Simulations", "Power", "Certain")
         summaryTableLatex <- dfToLatexTable(tt, caption=caption, label=labelTable)
         
         if(!is.null(bestCandidate)){
           resultLatex <- paste0(resultLatex, "Result of sample size determination of `", wordToLatexConform(mcd$getModelName()), 
                                 "', see figure~\\ref{",labelPlot , "} and table~\\ref{", labelTable, "}\\\\\n")
           resultLatex <- paste0(resultLatex, "Potential ", wordToLatexItalic("N"), ": ", wordToLatexItalic(bestCandidate), "\\\\\n")
           resultLatex <- paste0(resultLatex, "90\\%-credible interval of power: ", wordToLatexItalic(paste0(formatC(credibleInterval[1], digits=3), " - ", 
                                                                                                             formatC(credibleInterval[2], digits=3))) ,"\\\\\n")
           resultLatex <- paste0(resultLatex, "Power interval width: ", wordToLatexItalic(formatC(credibleInterval[2]-credibleInterval[1], digits=3)), "\\\\\n")
           resultLatex <- paste0(resultLatex, "Number of simulations for this N: ", wordToLatexItalic(resultsSSD$i[resultsSSD$N==bestCandidate]), "\\\\\n")
         }else if(ssd$intern$NMaxTooLow){
           resultLatex <- paste0("Potential ", wordToLatexItalic("N"), " candidate: None; 'Max N' reached \\\\\n",
                                 "Estimated N greater 'Max N': ", wordToLatexItalic(ssd$extern$N), "\\\\\n")
         }else{
           resultLatex <- paste0("Potential ", wordToLatexItalic("N"), " candidate: None \\\\\n")
         }
         latex$latexPost <- resultLatex
         latex$latexPost <- paste0(latex$latexPost, plotLatex)
         latex$latexPost <- paste0(latex$latexPost, summaryTableLatex)
       }
       
       return(list(latex=latex, add=add))
     },
     
     getReactive = function(type=c("data", "dataSelection", 
                                   "otherVariableError",
                                   "formula",
                                   "responseStep", "responseName",
                                   "responseDist", "responseLink",
                                   "otherVariable",
                                   "otherVariableStep", "otherVariableName",
                                   "otherVariableDist",
                                   "predictor","predictorName",
                                   "predictorInfo",
                                   "parameter",
                                   "generateData",
                                   "ssdGoals",
                                   "visualizeData",
                                   "loadMCD")){
       match.arg(type)
       
       if(type=="data"){
         return(self$dependReactiveValue("rxTriggerData"))
       }else if(type=="dataSelection"){
         return(self$dependReactiveValue("rxTriggerDataSelection"))
       }else if(type=="otherVariableError"){
         return(self$dependReactiveValue("rxTriggerOtherVariableError"))
       }else if(type=="formula"){
         return(self$dependReactiveValue("rxTriggerFormula"))
       }else if(type=="responseStep"){
         return(self$dependReactiveValue("rxTriggerResponse"))
       }else if(type=="responseName"){
         return(self$dependReactiveValue("rxTriggerResponseName"))
       }else if(type=="responseDist"){
         return(self$dependReactiveValue("rxTriggerResponseDist"))
       }else if(type=="responseLink"){
         return(self$dependReactiveValue("rxTriggerResponseLink"))
       }else if(type=="otherVariable"){
         return(self$dependReactiveValue("rxTriggerOtherVariable"))
       }else if(type=="otherVariableStep"){
         return(self$dependReactiveValue("rxTriggerOtherVariableStep"))
       }else if(type=="otherVariableName"){
         return(self$dependReactiveValue("rxTriggerOtherVariableName"))
       }else if(type=="otherVariableDist"){
         return(self$dependReactiveValue("rxTriggerOtherVariableDist"))
       }else if(type=="predictor"){
         return(self$dependReactiveValue("rxTriggerPredictor"))
       }else if(type=="parameter"){
         return(self$dependReactiveValue("rxTriggerParameter"))
       }else if(type=="generateData"){
         return(self$dependReactiveValue("rxTriggerGenerateData"))
       }else if(type=="ssdGoals"){
         return(self$dependReactiveValue("rxTriggerSSDGoals"))
       }else if(type=="visualizeData"){
         return(self$dependReactiveValue("rxTriggerVisualizeData"))
       }else if(type=="loadMCD"){
         return(self$dependReactiveValue("rxTriggerLoadMCD"))
       }
     },
     
     getInstance = function(){
       new <- ModelCreatingData$new(private$modelName)
       
       new$setModelName(self$getModelName())
       new$setData(self$getData())
       new$setDataMinPoints(self$getDataMinPoints())
       new$setHighlightDataColumn(self$getHighlightDataColumn()) #new set
       new$setOtherVariableErrors(self$getOtherVariableErrors()) #new set
       
       new$setGenerateData(self$getGenerateData(NULL), type=NULL)
       new$setGenerateSeed(self$getGenerateSeed())
       new$setGenerateDataAutomatically(self$getGenerateDataAutomatically(), silent=T)
       
       
       new$setVisualizeData(self$getVisualizeData(NULL), type=NULL)
       
       new$setReportExpInp(self$getReportExpInp())
       
       new$setLastOtherVariableId(self$getLastOtherVariableId()) #new set
       
       
       mcdResponse <- self$getMcdResponse()$getInstance()
       mcdResponse$setMcd(new)
       new$setMcdResponse(mcdResponse)
       
       
       new$setMcdFormula(self$getMcdFormula()$getInstance(mcd=new))
       new$setFormulaObject(self$getFormulaObject()$getInstance())
       new$setMcdSSD(self$getMcdSSD()$getInstance(mcd=new))
       
       
       oVs <- self$getOtherVariables()
       for(ov in oVs){
         ovNew <- ov$getInstance()
         ovNew$setMcd(new)
         new$addOtherVariable(ovNew, ignoreAdd=T)
       }
  
       preds <- self$getPredictors()
       for(pred in preds){
         ovPred <- pred$getInstance(new)
         new$addPredictor(ovPred)
       }
  
       paras <- self$getParameters()
       for(para in paras){
         ovPara <- para$getInstance(mcd=new)
         new$addParameter(ovPara)
       }

       return(new)
     },
     
     setInstance = function(model){
       self$setModelName(model$getModelName())
       self$setData(model$getData())
       self$setDataMinPoints(model$getDataMinPoints())
       self$setHighlightDataColumn(model$getHighlightDataColumn()) #new set
       self$setOtherVariableErrors(model$getOtherVariableErrors()) #new set
       
       self$setGenerateData(model$getGenerateData(NULL), type=NULL)
       self$setGenerateSeed(model$getGenerateSeed())
       self$setGenerateDataAutomatically(model$getGenerateDataAutomatically(), silent=T)
       
       self$setVisualizeData(model$getVisualizeData(NULL), type=NULL)
       
       self$setReportExpInp(model$getReportExpInp())
       
       self$setLastOtherVariableId(model$getLastOtherVariableId()) #new set
       
       #set this mcd objects
       mcdResponse <- model$getMcdResponse()$getInstance()
       mcdResponse$setMcd(self)
       self$setMcdResponse(mcdResponse)
       self$doTriggerResponse("name")
       self$doTriggerResponse("dist")
       self$doTriggerResponse("link")
       
       
       oVs <- model$getOtherVariables()
       for(ov in oVs){
         ov$setMcd(self)
       }
       self$setOtherVariableIds(model$getOtherVariableIds())
       self$setOtherVariables(model$getOtherVariables()) #new set
       
       
       preds <- model$getPredictors()
       for(pred in preds){
         pred$setMcd(self)
       }
       self$setPredictors(model$getPredictors()) #new set
       self$setPredictorIds(model$getPredictorIds()) #new set
       
       
       paras <- model$getParameters()
       newParaList <- list()
       for(para in paras){
         # para$setMcd(self)
         newParaList <- list.append(newParaList, para$getInstance(mcd=self))
       }
       self$setParameters(newParaList) #new set
       self$setParameterIds(model$getParameterIds()) #new set
       
       self$doTriggerParameter(NULL)
       
       mcdFormula <- model$getMcdFormula()$getInstance()
       mcdFormula$setMcd(self)
       self$setMcdFormula(mcdFormula)
       
       self$doTriggerFormula()
       
       self$getFormulaObject()$setInstance(model$getFormulaObject()$getInstance())
       
       self$setMcdSSD(model$getMcdSSD()$getInstance(self))
       self$doTriggerSSDGoals()
     },
     
     compareTo = function(cT, exact=F){

       if(private$modelName != cT$getModelName()) return(F)
       
       if(!is.null(private$mcdResponse) &&
          !private$mcdResponse$compareTo(cT$getMcdResponse())) return(F)
       
       if(!vectorEqual(private$otherVariableIds, cT$getOtherVariableIds())) return(F)
       for(i in seq_along(private$otherVariableIds)){
         if(!private$otherVariable[[i]]$compareTo(cT$getOtherVariables()[[i]], exact=exact)) return(F)
       }
       
       if(!vectorEqual(private$predictorIds, cT$getPredictorIds())) return(F)
       for(i in seq_along(private$predictorIds)){
         if(!private$predictors[[i]]$compareTo(cT$getPredictors()[[i]], exact=exact)) return(F)
       }

       if(!vectorEqual(private$parameterIds, cT$getParameterIds())) return(F)
       for(i in seq_along(private$parameterIds)){
         if(!private$parameters[[i]]$compareTo(cT$getParameters()[[i]], exact=exact)) return(F)
       }
       
       if(private$generateSeed != cT$getGenerateSeed()) return(F)
       
       if(!is.null(private$mcdSSD) &&
          !private$mcdSSD$compareTo(cT$getMcdSSD(), exact=exact)) return(F)

       if(!exact) return(T)
       

       # data = NULL
       # dataMinPoints = 1
       # generateDataAutomatically = TRUE
       # generateData = list(ssuVar=character(0), 
       #                     ssu=1, tdp=1)
       
       return(T)
     },
     
     getState = function(uuid){
       nextUUID <- uuid
       ret <- list()
       
       if(self$getUUID() == -1){
         
         nextUUID <- nextUUID + 1
         self$setUUID(uuid)

         mcdResponseState <- NULL
         if(!is.null(private$mcdResponse)){
           mcdResponseState <- private$mcdResponse$getState(nextUUID)
           nextUUID <- mcdResponseState$nextUUID
         }

         mcdSSDState <- NULL
         if(!is.null(private$mcdSSD)){
           mcdSSDState <- private$mcdSSD$getState(nextUUID)
           nextUUID <- mcdSSDState$nextUUID
         }
         
         mcdFormulaState <- NULL
         if(!is.null(private$mcdFormula)){
           mcdFormulaState <- private$mcdFormula$getState(nextUUID)
           nextUUID <- mcdFormulaState$nextUUID
         }
         
         formulaObjectState <- NULL
         if(!is.null(private$formulaObject)){
           formulaObjectState <- private$formulaObject$getState(nextUUID)
           nextUUID <- formulaObjectState$nextUUID
         }
         
         otherVariableState <- list()
         for(aa in private$otherVariable){
           cState <- aa$getState(nextUUID)
           otherVariableState <- list.append(otherVariableState, cState)
           nextUUID <- cState$nextUUID
         }
         predictorsState <- list()
         for(aa in private$predictors){
           cState <- aa$getState(nextUUID)
           predictorsState <- list.append(predictorsState, cState)
           nextUUID <- cState$nextUUID
         }
         parametersState <- list()
         for(aa in private$parameters){
           cState <- aa$getState(nextUUID)
           parametersState <- list.append(parametersState, cState)
           nextUUID <- cState$nextUUID
         }

         
         ret <- list(
           uuid = uuid, 
           stateVersion = private$stateVersion,
           
           modelName = private$modelName,
           data = private$data,
           dataMinPoints = private$dataMinPoints,
           dataColHighlight = private$dataColHighlight,
           otherVariableErrors = private$otherVariableErrors,
           
           otherVariableIds = private$otherVariableIds,
           predictorIds = private$predictorIds,
           parameterIds = private$parameterIds,
           generateData = private$generateData,
           generateSeed = private$generateSeed,
           generateDataAutomatically = private$generateDataAutomatically,
           doRefreshAfterLoading = private$doRefreshAfterLoading,
           visualizeData = private$visualizeData,
           lastOtherVariableId = private$lastOtherVariableId,
           silent = private$silent,
           reportExpInp = private$reportExpInp,
           
           #R6
           mcdResponse = mcdResponseState,
           otherVariable = otherVariableState,
           predictors = predictorsState,
           parameters = parametersState,
           mcdSSD = mcdSSDState,
           mcdFormula = mcdFormulaState,
           formulaObject = formulaObjectState
           
         )
       }
       
       ret <- list(
         uuid = self$getUUID(),
         nextUUID = nextUUID,
         'ModelCreatingData' = ret
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
       
       private$modelName <- state$modelName
       private$data <- state$data
       private$dataMinPoints <- state$dataMinPoints
       private$dataColHighlight <- state$dataColHighlight
       private$otherVariableErrors <- state$otherVariableErrors
       
       private$otherVariableIds <- state$otherVariableIds
       private$predictorIds <- state$predictorIds
       private$parameterIds <- state$parameterIds
       private$generateData <- state$generateData
       private$generateSeed <- state$generateSeed
       private$generateDataAutomatically <- state$generateDataAutomatically
       private$doRefreshAfterLoading <- state$doRefreshAfterLoading
       private$visualizeData <- state$visualizeData
       private$lastOtherVariableId <- state$lastOtherVariableId
       private$silent <- state$silent
       private$reportExpInp <- state$reportExpInp
       
       #R6
       private$mcdResponse <- state$mcdResponse
       private$otherVariable <- state$otherVariable
       private$predictors <- state$predictors
       private$parameters <- state$parameters
       private$mcdSSD <- state$mcdSSD
       private$mcdFormula <- state$mcdFormula
       private$formulaObject <- state$formulaObject
       
     },
     resetState = function(){
       if(!super$resetState()) return()
       if(!is.null(private$mcdResponse)) private$mcdResponse$resetState()
       if(!is.null(private$mcdSSD)) private$mcdSSD$resetState()
       if(!is.null(private$mcdFormula)) private$mcdFormula$resetState()
       if(!is.null(private$formulaObject)) private$formulaObject$resetState()
       for(aa in private$otherVariable){
         aa$resetState()
       }
       for(aa in private$predictors){
         aa$resetState()
       }
       for(aa in private$parameters){
         aa$resetState()
       }
     }
     
   )

)


