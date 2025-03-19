ReportProgressModel <- R6Class(
  classname = "ReportProgressModel", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    nextId = 0,
    items = list(),
    recommendedItems = list(),
    updateProgressPanel = T,

    #Only variable
    changeInItems = 0,
    
    listIdMapping = list(), #named list. Names= ids from items, elements= ids from recommendedItems
    
    selectedList = c() # finally selected items in right order
  ),
  
  public = list(
    
    #initNextId: int
    #initItems: ReportProgressModelItem
    initialize = function(initNextId = NULL, initItems = NULL,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      if(!is.null(initNextId)) private$nextId <- initNextId
      if(!is.null(initItems)) private$items <- initItems

    },
    
    getItems = function(recommended=F){
      if(recommended){
        return(private$recommendedItems)
      }else{
        return(private$items)
      }
    },
    
    getItemsOfPerIterationDataModel = function(id){
      ids <- c()  
    
      for(i in private$recommendedItems){
        if(equal(i$getpDIM_id(), id)) ids <- c(ids,i$getId())
      }
      for(i in private$items){
        if(equal(i$getpDIM_id(), id)) ids <- c(ids,i$getId())
      }
      return(ids)
    },
    
    getItem = function(id){
      if(is.null(id)) return(NULL)
      for(i in private$recommendedItems){
        if(i$getId() == id) return(i)
      }
      for(i in private$items){
        if(i$getId() == id) return(i)
      }
      return(NULL)
    },
    
    
    addItem = function(id, 
                       moduleType,
                       dataModel_id=NULL, pDIM_id=NULL, pDIM_name=NULL, 
                       planningName=NULL,
                       imgFile, type,
                       object, singleton, clicked=NULL, recommended, silent = F){

      isNew <- T
      
      clicked_item <- F
      if(!is.null(clicked)) clicked_item <- clicked
      newItem <- ReportProgressModelItem$new(RPM = self, id=id, 
                                             moduleType = moduleType, 
                                             dataModel_id=dataModel_id, 
                                             planningName = planningName,
                                             pDIM_id=pDIM_id, pDIM_name=pDIM_name,
                                             imgFile=imgFile, type=type, 
                                             object=object, singleton=singleton,
                                             clicked=clicked_item)
      if(recommended){
        pos <- self$posOfItem(newItem, private$recommendedItems)
        if(!is.null(pos)){
          newItem$setClicked(private$recommendedItems[[pos]]$getClicked(), dontTrigger=T)
          self$removeMappingEntry(private$recommendedItems[[pos]]$getId())
          
          private$recommendedItems[[pos]] <- newItem
          isNew <- F
        }else{
          private$recommendedItems <- list.append(private$recommendedItems, newItem)
        }
        
        #Mapping for item and recommended item list
        itemIdOfOtherList <- self$sameElementInOtherList(newItem)
        if(!is.null(itemIdOfOtherList)){
          private$listIdMapping <- list.append(private$listIdMapping, 
                                               newItem$getId(), paste0(itemIdOfOtherList,""))
          if(!silent) self$triggerReactiveValue("listIdMapping")
        }
        
        if(!silent) self$triggerReactiveValue("recommendedItems")
      }else{
        pos <- self$posOfItem(newItem, private$items)
        
        itemIdOfOtherList <- self$sameElementInOtherList(newItem, otherList=private$recommendedItems)
        if(is.null(clicked)){
          clicked <- F
          if(!is.null(itemIdOfOtherList)){
            otherItem <- self$getItem(itemIdOfOtherList)
            clicked <- otherItem$getClicked()
          }
          clicked <- clicked || (!is.null(pos) && private$items[[pos]]$getClicked())
          newItem$setClicked(clicked, dontTrigger=T)
        }

        
        if(!is.null(pos)){
          self$removeMappingEntry(private$items[[pos]]$getId())
          private$items[[pos]] <- newItem
          isNew <- F
        }else{
          private$items <- list.append(private$items, newItem)
        }
        
        #Mapping for item and recommended item list
        itemIdOfOtherList <- self$sameElementInOtherList(newItem)
        if(!is.null(itemIdOfOtherList)){
          private$listIdMapping <- list.append(private$listIdMapping, 
                                               itemIdOfOtherList, paste0(newItem$getId(),""))
          if(!silent) self$triggerReactiveValue("listIdMapping")
        }
        
        if(!silent) self$triggerReactiveValue("items")
      }
      
      if(!silent) self$triggerReactiveValue("changeInItems")
      return(isNew)
    },
    
    #Remove the item(s) with the given ids.
    removeItem = function(ids, recommended=F, silent = F){
      
      new_list <- list()
      
      if(recommended){
        for(i in private$recommendedItems){
          if(!i$getId() %in% ids) 
            new_list <- list.append(new_list,i)
        }
        private$recommendedItems <- new_list
        
        self$removeMappingEntry(ids)
        if(!silent) self$triggerReactiveValue("recommendedItems")
      }else{
        p <- private
        for(i in private$items){
          if(!i$getId() %in% ids) 
            new_list <- list.append(new_list,i)
        }
        private$items <- new_list
        
        self$removeMappingEntry(ids)
        if(!silent) self$triggerReactiveValue("items")
      }
    },
    
    removeMappingEntry = function(ids, silent = F){
      #Remove also mapping entry
      if(length(private$listIdMapping) > 0){
        newListIdMapping <- list()
        n <- names(private$listIdMapping)
        for(i in 1:length(private$listIdMapping)){
          el <- private$listIdMapping[[i]]
          if(!n[i] %in% ids) 
            newListIdMapping <- list.append(newListIdMapping, el, paste0(n[i],""))
        }
        private$listIdMapping <- newListIdMapping
      }
      if(!silent) self$triggerReactiveValue("listIdMapping")
    },
    
    setListIdMapping = function(mapping, silent=F){
      private$listIdMapping <- mapping
      if(!silent) self$triggerReactiveValue("listIdMapping")
    },
    getListIdMapping = function(){
      return(private$listIdMapping)
    },
    
    #If item does not exists, return NULL
    posOfItem = function(newItem, list){
      if(!newItem$getSingleton()) return(NULL)
      for(i in seq_along(list)){
        if(self$equalItems(newItem, list[[i]]))return(i)
      }
      return(NULL)
    },
    
    equalItems = function(a,b){
      a$getDataModel_id() == b$getDataModel_id() &&
        a$getpDIM_id() == b$getpDIM_id() &&
        a$getType() == b$getType() 
    },
    
    #Returns, if true, the id
    sameElementInOtherList = function(item, otherList=NULL){
      if(!item$getSingleton()) return(NULL)
      if(is.null(otherList)){
        otherList <- private$items
        for(i in private$items){
          if(i$getId()==item$getId()){
            otherList <- private$recommendedItems
            break
          }
        }
      }

      for(i in otherList){
        if(self$equalItems(i, item)) return(i$getId())
      }
      return(NULL)
    },
 
    setNextId = function(id){
      private$nextId <- id
    },
    getNextId = function(){
      private$nextId
    },
    
    incNextId = function(silent = F){
      private$nextId <- private$nextId+1
      if(!silent) self$triggerReactiveValue("nextId")
    },
    
    getUpdateProgressPanel = function(){
      private$updateProgressPanel
    },
    
    setUpdateProgressPanel = function(flag, silent = F){
      private$updateProgressPanel <- flag
      if(!silent) self$triggerReactiveValue("updateProgressPanel")
    },
    
    setChangeInItems = function(num){
      private$changeInItems <- num
    },
    getChangeInItems = function(){
      return(private$changeInItems)
    },
    
    triggerChangeInItems = function(silent = F){
      private$changeInItems <- private$changeInItems +1 
      if(!silent) self$triggerReactiveValue("changeInItems")
    },

    #Returns the id from the item of the other list if existing, otherwise NULL
    mapToOtherList = function(item){
      return(self$sameElementInOtherList(item))
    },
    
    setSelectedList = function(list, silent=F){
      if(any(is.na(list))) stop()
      
      
      if(is.null(list) && is.null(private$selectedList)) return()
      
      if(!is.null(list) && !is.null(private$selectedList) &&
         length(list) == length(private$selectedList) &&
         vectorEqual(list, private$selectedList)){
        return()
      }
      private$selectedList <- list
      if(!silent) self$triggerReactiveValue("selectedList")
    },
    
    getSelectedList = function(list){
      return(private$selectedList)
    },
    
    saveImagesOfItems = function(dataModel, mCDList){
      for(item in private$items){
        self$saveImagesOfSingleItem(dataModel, mCDList, item)
      }
      for(item in private$recommendedItems){
        self$saveImagesOfSingleItem(dataModel, mCDList, item)
      }
    },
    
    #Should called only within this class
    saveImagesOfSingleItem = function(dataModel, mCDList, item){

      tEnum <- reportTypeEnum()
      
      imgFileThumbnail <- item$getImgFile()
      imgFileThumbnail <- substr(imgFileThumbnail, 7, nchar(imgFileThumbnail))
      imgFileThumbnail <- paste0(report_folder, imgFileThumbnail)
      
      imgFile <- item$getImgFile()
      imgFile <- substr(imgFile, 18, nchar(imgFile)-4)
      
      if(item$getType() %in% c(tEnum$preplot, tEnum$previewppc, tEnum$ppc, 
                               tEnum$mp, tEnum$pairs, tEnum$pvp, tEnum$priorpc,
                               tEnum$singleEffect, tEnum$prediction)){
        plot <- item$getObject()
        plot <- plot$div$plot
        
        
        #full resolution plot
        plotToTex(plotId=imgFile, plot=plot, caption=NULL, ignoreTex = T)
        
        #preview icon
        ggsave(imgFileThumbnail, plot, device="jpeg", width=100, height=100, units="px", dpi=25)
        
      }else if(item$getType() == tEnum$validation){
        obj <- item$getObject()
        res <- obj$add
        pIDM <- dataModel$get.perIterationDataModel(id=item$getpDIM_id())
        modelValidationItem(pIDM, res$res_dev, imgFileThumbnail)
      }else if(item$getType() == tEnum$planningExperiment){
        
        planName <- item$getPlanningName()
        mcd <- mCDList$getMCD(planName)
        
        #If mcdOff is not null, use this instead (otherwise e.g. changed goals could falsely reported)
        if(!is.null(mcd$getMcdSSD()$getMcdOff())) mcd <- mcd$getMcdSSD()$getMcdOff()
        
        div <- item$getObject()
        divObj <- div$obj
        
        for(g_i in seq_along(divObj$ssd$goals)){
          plot <- divObj$ssd$goals[[g_i]]
          goalName <- names(divObj$ssd$goals)[g_i]
          label <- paste0("model-", mcd$getModelName(), "_goal-",goalName)
          plotId <- paste0("id-", item$getId(), "_",label,"_",item$getId())
          
          #full resolution plot
          plotToTex(plotId=plotId, plot=plot, caption=NULL, ignoreTex = T)
        }
        
        #ssd result
        if(!is.empty(divObj$ssd$result)){
          labelPlot <- paste0("id-", item$getId(), "_model-", mcd$getModelName(), "_", item$getId() ,"_resultGraph")
          plotToTex(plotId=labelPlot, plot=divObj$ssd$result, caption=NULL, ignoreTex = T)
        }

      }
    },
    
    checklistMapping = function(item){
      tEnum <- reportTypeEnum()
      type <- item$getType()
      if(type == tEnum$planningExperiment) return(1)
      if(type == tEnum$preplot) return(2)
      if(type == tEnum$formula) return(3)
      if(type == tEnum$validation) return(4)
      if(type == tEnum$previewppc) return(5)
      if(type == tEnum$ppc) return(5)
      if(type == tEnum$priorpc) return(6)
      if(type == tEnum$pvp) return(7)
      if(type == tEnum$mp) return(8)
      if(type %in% c(tEnum$effectMatrix,tEnum$singleEffect)) return(9)
      if(type == tEnum$prediction) return(10)
      if(type == tEnum$modelComparison) return(11)
      return(-1)
    },
    
    getInstance =  function(){
      newInstance <- ReportProgressModel$new()
      
      newInstance$setNextId(private$nextId)
      newInstance$setUpdateProgressPanel(private$updateProgressPanel)
      newInstance$setChangeInItems(private$changeInItems)
      newInstance$setListIdMapping(private$listIdMapping, silent=T)
      newInstance$setSelectedList(private$selectedList, silent=T)

      
      for(aa in private$items){
        newInstance$addItem(id=aa$getId(), 
                            moduleType = aa$getModuleType(),
                            dataModel_id=aa$getDataModel_id(), pDIM_id=aa$getpDIM_id(), pDIM_name=aa$getpDIM_name(), 
                            planningName = aa$getPlanningName(),
                            imgFile=aa$getImgFile(), type=aa$getType(),
                            object=aa$getObject(), singleton=aa$getSingleton(), clicked = aa$getClicked(), recommended=F)
        newItem <- newInstance$getItem(aa$getId())
        newItem$setBlankData(aa$getBlankData())
        newItem$setIndividualData(aa$getIndividualData())
      }

      for(aa in private$recommendedItems){
        newInstance$addItem(id=aa$getId(), 
                            moduleType = aa$getModuleType(),
                            dataModel_id=aa$getDataModel_id(), pDIM_id=aa$getpDIM_id(), pDIM_name=aa$getpDIM_name(), 
                            planningName = aa$getPlanningName(),
                            imgFile=aa$getImgFile(), type=aa$getType(),
                            object=aa$getObject(), singleton=aa$getSingleton(), clicked = aa$getClicked(), recommended=T)
        newItem <- newInstance$getItem(aa$getId())
        newItem$setBlankData(aa$getBlankData())
        newItem$setIndividualData(aa$getIndividualData())
      }
      
      return(newInstance)
    },
    
    setInstance = function(instance){
      self$setNextId(instance$getNextId())
      self$setUpdateProgressPanel(instance$getUpdateProgressPanel())
      self$setChangeInItems(instance$getChangeInItems())
      self$setSelectedList(instance$getSelectedList())
      
      private$items <- NULL
      private$recommendedItems <- NULL
      for(aa in instance$getItems()){
        self$addItem(id=aa$getId(), 
                     moduleType = aa$getModuleType(),
                     dataModel_id=aa$getDataModel_id(), pDIM_id=aa$getpDIM_id(), pDIM_name=aa$getpDIM_name(), 
                     planningName = aa$getPlanningName(),
                     imgFile=aa$getImgFile(), type=aa$getType(),
                     object=aa$getObject(), singleton=aa$getSingleton(), clicked = aa$getClicked(), recommended=F)
        newItem <- self$getItem(aa$getId())
        newItem$setBlankData(aa$getBlankData())
        newItem$setIndividualData(aa$getIndividualData())
      }
      
      for(aa in instance$getItems(recommended=T)){
        self$addItem(id=aa$getId(), 
                     moduleType = aa$getModuleType(),
                     dataModel_id=aa$getDataModel_id(), pDIM_id=aa$getpDIM_id(), pDIM_name=aa$getpDIM_name(), 
                     planningName = aa$getPlanningName(),
                     imgFile=aa$getImgFile(), type=aa$getType(),
                     object=aa$getObject(), singleton=aa$getSingleton(), clicked = aa$getClicked(), recommended=T)
        newItem <- self$getItem(aa$getId())
        newItem$setBlankData(aa$getBlankData())
        newItem$setIndividualData(aa$getIndividualData())
      }
      self$setListIdMapping(instance$getListIdMapping(), silent=T)
      
      self$triggerReactiveValues()
    },
    

    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)

        itemsState <- list()
        for(aa in private$items){
          cState <- aa$getState(nextUUID)
          itemsState <- list.append(itemsState, cState)
          nextUUID <- cState$nextUUID
        }
        
        recommendedItemsState <- list()
        for(aa in private$recommendedItems){
          cState <- aa$getState(nextUUID)
          recommendedItemsState <- list.append(recommendedItemsState, cState)
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          nextId = private$nextId,
          updateProgressPanel = private$updateProgressPanel,
          changeInItems = private$changeInItems,
          listIdMapping = private$listIdMapping, 
          selectedList = private$selectedList,
          
          #R6
          items = itemsState,
          recommendedItems = recommendedItemsState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ReportProgressModel' = ret
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
      
      private$nextId = state$nextId
      private$updateProgressPanel = state$updateProgressPanel
      private$changeInItems = state$changeInItems
      private$listIdMapping = state$listIdMapping
      private$selectedList = state$selectedList
      
      #R6
      private$items = state$items
      private$recommendedItems = state$recommendedItems
    },
    resetState = function(){
      if(!super$resetState()) return()
      for(aa in private$items){
        aa$resetState()
      }
      for(aa in private$recommendedItems){
        aa$resetState()
      }
    }
    
  )                 
)


















##
ReportProgressModelItem <- R6Class(
  classname = "ReportProgressModelItem", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    RPM = NULL,
    id = 0, # unique id
    moduleType = c("planning", "evaluation", "blank"),
    dataModel_id=NULL,
    pDIM_id=NULL,
    pDIM_name=NULL,
    planningName=NULL,
    imgFile=NULL,
    type=NULL,
    object=NULL,
    singleton=F,
    clicked=F,
    blankData=NULL, #list(header, text, binding=c("prev", "no", "next"))
    
    individualData = NULL #No R6
  ),
  
  public = list(
   
    initialize = function(RPM, id, 
                          moduleType,
                          dataModel_id=NULL, pDIM_id=NULL, pDIM_name=NULL, 
                          planningName=NULL,
                          imgFile, type=NULL, 
                          object=NULL, singleton=F, clicked=F,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
 
      private$RPM <- RPM
      private$id <- id
      private$moduleType <- moduleType
      private$dataModel_id <- dataModel_id
      private$planningName <- planningName
      private$pDIM_id <- pDIM_id
      private$pDIM_name <- pDIM_name
      private$imgFile <- imgFile
      private$type <- type
      private$object <- object
      private$singleton <- singleton
      private$clicked <- clicked
    },
    
    getRPM = function(){private$RPM},
    getId = function(){private$id},
    getModuleType = function(){private$moduleType},
    getDataModel_id = function(){private$dataModel_id},
    getpDIM_id = function(){private$pDIM_id},
    getpDIM_name = function(){private$pDIM_name},
    getPlanningName = function(){private$planningName},
    getImgFile = function(){private$imgFile},
    getType = function(){private$type},
    getSingleton = function(){private$singleton},
    getObject = function(adjust=F){
      if(adjust){
        self$adjustedObject()
      }else{
        private$object 
      }
    },
    getClicked = function(){private$clicked},
    getBlankData = function(){private$blankData},
    getIndividualData = function(){private$individualData},
    
    
    setRPM = function(RPM){
      private$RPM <- RPM
    },
    setImgFile = function(img){
      private$RPM$triggerChangeInItems()
      private$imgFile <- img
    },
    setClicked = function(flag, dontTrigger=F){
      if(!dontTrigger) 
        private$RPM$triggerChangeInItems()
      private$clicked <- flag
    },
    setObject = function(newObject, dontTrigger=F){
      private$object <- newObject
      if(!dontTrigger)
        private$RPM$triggerChangeInItems()
    },
    toggleClicked = function(){
      private$RPM$triggerChangeInItems()
      private$clicked <- !private$clicked
    },
    setBlankData = function(blankData){
      private$blankData <- blankData
    },
    setIndividualData = function(data){
      private$individualData <- data
    },
    
    adjustedObject = function(){
      tEnum <- reportTypeEnum()
      objNew <- private$object
      
      if(private$type == tEnum$planningExperiment){
        indData <- private$individualData
        latex <- private$object$latex
        
        newLatex <- ""
        
        if(indData$defined$resp){
          newLatex <- latex$preFormula
        }
        if(indData$defined$ssd){
          newLatex <- paste0(newLatex, latex$preSSD)
        }
        
        #formula
        newLatex <- paste0(newLatex, latex$formula)
        
        #ov
        newLatex <- paste0(newLatex, latex$ov$pre)
        ovNewLatex <- ""
        ovPreFlags <- list(preVariable=F, preVariableNum=F,preVariableCat=F,
                           preVariableCatInd=F, preVariableCatSub=F, preVariableCatRep=F)
        
        for(ov in latex$ov$ov){
          if(!indData$preSel$onlyUsedVars || ov$use){
            ovPreFlags$preVariable <- T
            ovNewLatex <- paste0(ovNewLatex, ov$latex)
            if(ov$ovType == "categorical"){
              ovPreFlags$preVariableCat <- T
              if(ov$catType == "independent"){
                ovPreFlags$preVariableCatInd <- T
              }else if(ov$catType == "subgroup"){
                ovPreFlags$preVariableCatSub <- T
              }else if(ov$catType == "replacement"){
                ovPreFlags$preVariableCatRep <- T
              }else{
                if(localUse()) browser() #rm, if never called
              }
            }else{
              ovPreFlags$preVariableNum <- T
            }
          }
        }
        if(ovPreFlags$preVariableCatSub) ovPreFlags$preVariableCatInd <- F
        ovPreNewLatex <- ""
        if(ovPreFlags$preVariable) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariable)
        if(ovPreFlags$preVariableNum) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariableNum)
        if(ovPreFlags$preVariableCat) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariableCat)
        if(ovPreFlags$preVariableCatInd) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariableCatInd)
        if(ovPreFlags$preVariableCatSub) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariableCatSub)
        if(ovPreFlags$preVariableCatRep) ovPreNewLatex <- paste0(ovPreNewLatex, latex$ov$preVariableCatRep)
        newLatex <- paste0(newLatex, ovPreNewLatex, ovNewLatex)
        
        
        #Parameter seeds
        newLatex <- paste0(newLatex, latex$parameter$pre, latex$parameter$latex)
        
        
        #Gen data
        if(indData$preSel$includeGenData){
          newLatex <- paste0(newLatex, latex$data$pre)
          newLatex <- paste0(newLatex, latex$data$responseSeed)
          
          if(indData$preSel$replaceMinData){
            if(indData$preSel$onlyUsedVars){
              newLatex <- paste0(newLatex, latex$data$minDatasetLatexWOUnusedOV)
            }else{
              newLatex <- paste0(newLatex, latex$data$minDataset)
            }
          }else{
            newLatex <- paste0(newLatex, latex$data$genDataset)
          }
        }
        
        #Estimate 'N'
        if(indData$preSel$includeSSD){
          newLatex <- paste0(newLatex, latex$ssd$latex$latexPre)
          
          #goals
          for(g in latex$ssd$latex$goals){
            if(!indData$preSel$onlyUsedGoals || g$used){
              newLatex <- paste0(newLatex, g$latex)
            }
          }
        }
        
        
        #Estimate 'N' result
        if(indData$preSel$includeSSDResult && indData$preSel$includeSSD){
          newLatex <- paste0(newLatex, latex$ssd$latex$latexPost)
        }
        
        newLatex <- paste0(newLatex, "\n\\clearpage")
        
        objNew$latex <- newLatex
      }
      return(objNew)
    },
    
    getHoverInfo = function(){
      if(private$moduleType == "planning"){
        return(paste0(private$planningName))
      }else if(private$moduleType == "evaluation"){
        modelName <- private$pDIM_name
        if(is.null(modelName) || modelName == "") modelName <- "-"
        dataName <- private$dataModel_id
        if(!is.numeric(dataName)){
          dataName <- basename(dataName)
        }else{
          dataName <- "-"
        }

        return(paste0(dataName, "\n",modelName,"\n",private$type))
      }else{
        return(paste0(""))
      }
    
    },
    checklistMapping = function(){
      return(private$RPM$checklistMapping(self))
    },
    
    
    getInstance =  function(RPM){
      newInstance <- ReportProgressModelItem$new(RPM=RPM, id=private$id, 
                                                 moduleType = private$moduleType,
                                                 dataModel_id=private$dataModel_id, 
                                                 pDIM_id=private$pDIM_id, pDIM_name=private$pDIM_name, 
                                                 planningName=private$planningName,
                                                 imgFile=private$imgFile,  type=private$type, 
                                                 object=private$object, singleton=private$singleton, clicked=private$clicked)

      newInstance$setBlankData(private$blankData)
      newInstance$setIndividualData(private$individualData)
      return(newInstance)
    },
    
    setInstance = function(instance){
      private$id <- instance$getId()
      private$moduleType <- instance$getModuleType()
      private$dataModel_id <- instance$getDataModel_id()
      private$pDIM_id <- instance$getpDIM_id()
      private$pDIM_name <- instance$getpDIM_name()
      private$planningName <- instalce$getPlanningName()
      private$imgFile <- instance$getImgFile()
      private$type <- instance$getType()
      private$object <- instance$getObject()
      private$singleton <- instance$getSingleton()
      private$clicked <- instance$getClicked()
      private$blankData <- instance$getBlankData()
      private$individualData <- instance$getIndividualData()
      
      #R6
      private$RPM <- instance$getRPM()
    },
    
    
    getState = function(uuid){
      nextUUID <- uuid+1
      ret <- list()
      
      if(self$getUUID() == -1){
        self$setUUID(uuid)
        
        RPMState <- NULL
        if(!is.null(private$RPM)){
          RPMState <- private$RPM$getState(nextUUID)
          nextUUID <- RPMState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          id = private$id, # unique id
          moduleType = private$moduleType,
          dataModel_id = private$dataModel_id,
          pDIM_id = private$pDIM_id,
          pDIM_name = private$pDIM_name,
          planningName = private$planningName,
          imgFile = private$imgFile,
          type = private$type,
          object = private$object,
          singleton = private$singleton,
          clicked = private$clicked,
          blankData = private$blankData,
          individualData = private$individualData,
          
          #R6
          RPM = RPMState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ReportProgressModelItem' = ret
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
      private$moduleType <- state$moduleType
      private$dataModel_id <- state$dataModel_id
      private$pDIM_id <- state$pDIM_id
      private$pDIM_name <- state$pDIM_name
      private$planningName <- state$planningName
      private$imgFile <- state$imgFile
      private$type <- state$type
      private$object <- state$object
      private$singleton <- state$singleton
      private$clicked <- state$clicked
      private$blankData <- state$blankData
      private$individualData <- state$individualData
      
      #R6
      private$RPM = state$RPMState
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$RPM)) private$RPM$resetState()
    }
    
  )                 
)

