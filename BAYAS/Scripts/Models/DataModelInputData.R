
#Data model only for the input data (raw and long-format)
#Will be assigned to each 'PerIterationDataModel' and to DataModel for global use (for the upload page)
DataModelInputData <- R6Class(
  classname = "DataModelInputData", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    
    dataModel = NULL,
    
    perIterationDataModel = NULL,
    
    #Path to tmp file of uploaded/used data 
    tmpDataPath = NULL,
    
    
    #(Not necessarily unique) path of uploaded/used data 
    #Will be passed to pDIM
    currentDataPath = NULL,

    #decimal separator
    decSep = "point",
    
    #cell separator
    cellSep = "comma",
    
    # Raw users input
    excelTable = NULL,
    
    # Cleaned users input data
    # Cleaned input data, maybe renamed columns, removed lines cause of missing values
    longFormat = NULL,
    
    # Dataframe with columns "variable"(name), "type", "lower", "upper", response
    inputProperties = NULL
  ),
  
  
  public = list(
    
    initialize = function(dataModel = NULL, perIterationDataModel = NULL, emptyState=F){
      super$initialize()
      if(emptyState) return()
      
      private$dataModel <- dataModel
      private$perIterationDataModel <- perIterationDataModel
    },
    
    getDataModel = function(){
      return(private$dataModel)
    },
    setDataModel = function(dataModel, silent=F){
      private$dataModel <- dataModel
      if(!silent) self$triggerReactiveValue("dataModel")
    },
    
    getPerIterationDataModel = function(){
      return(private$perIterationDataModel)
    },
    setPerIterationDataModel = function(perIterationDataModel, silent=F){
      private$perIterationDataModel <- perIterationDataModel
      if(!silent) self$triggerReactiveValue("perIterationDataModel")
    },
    

    getCurrentDataPath = function(){
      return(private$currentDataPath)
    },
    setCurrentDataPath = function(currentDataPath, silent=F){
      private$currentDataPath <- currentDataPath
      if(!silent) self$triggerReactiveValue("currentDataPath")
    },
    
    getTmpDataPath = function(){
      return(private$tmpDataPath)
    },
    setTmpDataPath = function(tmpDataPath, silent=F, triggerEqual=F){
      if(!triggerEqual && !vectorEqual(private$tmpDataPath, tmpDataPath)) triggerEqual <- T
      private$tmpDataPath <- tmpDataPath
      if(!silent && triggerEqual) self$triggerReactiveValue("tmpDataPath")
    },
    
    getDecSep = function(){
      return(private$decSep)
    },
    setDecSep = function(decSep, silent=F, triggerEqual=F){
      if(!triggerEqual && !equal0(private$decSep, decSep)) triggerEqual <- T
      private$decSep <- decSep
      if(decSep == private$cellSep) self$setCellSep("semicolon")
      if(!silent && triggerEqual) self$triggerReactiveValue("decSep")
    },
    
    
    getCellSep = function(){
      return(private$cellSep)
    },
    setCellSep = function(cellSep, silent=F, triggerEqual=F){
      if(!triggerEqual && !equal0(private$cellSep, cellSep)) triggerEqual <- T
      private$cellSep <- cellSep
      if(cellSep == private$decSep) self$setDecSep("point")
      if(!silent && triggerEqual) self$triggerReactiveValue("cellSep")
    },
    
    
    getExcelTable = function(){
      return(private$excelTable)
    },
    setExcelTable = function(excelTable, silent=F){
      private$excelTable <- excelTable
      if(!silent) self$triggerReactiveValue("excelTable")
    },
    
    
    getLongFormat = function(categoricalAsCharacters = T, completeCases=F){
      dd <- private$longFormat
      
      if(categoricalAsCharacters){
        cEnum <- characteristicEnum()
        if(!is.null(dim(private$inputProperties)[1])){
          for(i in seq_len(dim(private$inputProperties)[1])){
            type <- private$inputProperties$type[i]
            var <- private$inputProperties$variable[i]
            if(!is.null(type) && type == cEnum$Categorical)
              dd[[var]] <- as.character(dd[[var]])
          }
        }
      }
      if(completeCases){
        dd <- dd %>%
          mutate(across(where(is.character), str_trim)) %>%
          mutate_if(is.character, list(~na_if(.,""))) %>%
          na.omit
      }
      return(dd)
    },
    getLongFormatVariable = function(varName, completeCases=F){
      dd <- self$getLongFormat()
      varNames <- colnames(dd)
      dd <- dd[,varNames %in% varName, drop=F]
      if(completeCases){
        dd <- dd %>%
          mutate(across(where(is.character), str_trim)) %>%
          mutate_if(is.character, list(~na_if(.,""))) %>%
          na.omit
      }
      return(dd)
    },
    setLongFormat = function(longFormat, silent=F, asIt = F){
      if(asIt){
        private$longFormat <- longFormat
      }else{
        private$longFormat <- self$cleanInputData(longFormat, decSep=private$decSep)
      }
      if(!silent) self$triggerReactiveValue("longFormat")
    },

    
    getInputProperties = function(){
      return(private$inputProperties)
    },
    getInputProperty = function(name = NULL, index = NULL, type = c("type","lower","upper")){
      if(is.null(name)){
        if(is.null(index)) return()
        name <- private$inputProperties$variable[index]
      }
      return(private$inputProperties[[type]][private$inputProperties$variable==name])
    },
    setInputProperties = function(inputProperties, silent=F){
      private$inputProperties <- inputProperties
      if(!silent) self$triggerReactiveValue("inputProperties")
    },
    setGuessedInputProperties = function(silent=F){
      private$inputProperties <- data.frame(variable = character(0),
                                            type = character(0),
                                            lower = character(0),
                                            upper = character(0),
                                            response = character(0))

      for(col in colnames(private$longFormat)){
        # guessed <- self$analyseInputdata(private$longFormat[[col]])
        guessed <- self$analyseInputdata(self$getLongFormatVariable(col, completeCases=T)[[1]])
        private$inputProperties[col,] <- c(col, guessed$characteristic, guessed$lowerLimit, guessed$upperLimit, F)
      }
      private$inputProperties[1,5] <- T
      
      if(!silent) self$triggerReactiveValue("inputProperties")
    },
    changeInputProperties = function(name = NULL, index = NULL, type = c("type","lower","upper","response"), value, silent=F){
      inp <- private$inputProperties
      if(is.null(name) && is.null(index)) return()
      if(is.null(name)) name <- inp[[1]][index]
      if(!name %in% inp$variable) return()
      match.arg(type)
      inp[[type]][inp$variable == name] <- value
      if(type == "response" && value) inp[[type]][inp$variable != name] <- F
      private$inputProperties <- inp
      if(!silent) self$triggerReactiveValue("inputProperties")
    },

    getColnames = function(){
      return(colnames(private$longFormat))
    },
    getResponseVariable = function(onlyName = F){
      inp <- private$inputProperties
      if(onlyName) return(inp[inp$response=="TRUE",]$variable)
      return(inp[inp$response=="TRUE",])
    },
    getOtherVariables = function(varName=NULL, onlyName = F){
      inp <- private$inputProperties
      if(!is.null(varName)) inp <- inp[inp$variable %in% varName,]
      if(onlyName) return(inp[inp$response=="FALSE",]$variable)
      return(inp[inp$response=="FALSE",])
    },
    
    ## static
    cleanInputData = function(x, allowOnlyComplete = F, decSep){
      if(global_browser) browser()
      colnames(x) <- gsub("[[:space:]]", "_", colnames(x))
      
      #remove columns with just one value
      if(allowOnlyComplete){
        x <- x[complete.cases(x),]
        
        remove_c <- c()
        for(i in seq_along(x[1,])){
          if(length(unique(x[,i]))==1) remove_c <- c(remove_c,i)
        }
        if(length(remove_c)>0){
          x <- x[,-remove_c, drop=F]
        }
      }
      
      #if every non-empty row is parsable to numeric --> as.numeric
      #Also considering the chosen cell separator
      for(i in seq_len(dim(x)[2])){
        tmp <- x[,i]
        tmp[is.na(str_trim(tmp))] <- NaN
        tmp[str_trim(tmp)==""] <- NaN
        
        if(decSep == "comma") tmp <- str_replace_all(tmp, ",", ".")
        tmpNumeric <- as.numeric(tmp)
        if(all(!is.na(tmpNumeric) | is.nan(tmpNumeric))){
          x[,i] <- tmpNumeric
        }
      }
      
      return(x)
    },
    
    analyseInputdata = function(vector){
      cEnum <- characteristicEnum()
      
      # Integer, Continuous, Categorical
      characteristic <- cEnum$Categorical
      
      #Remove empty entries
      vector <- vector[str_trim(vector)!=""]
      
      # Convert to numeric
      vector_numeric <- suppressWarnings(as.numeric(vector))
      vector_numeric <- suppressWarnings(vector_numeric[!is.na(vector_numeric)])
      
      
      # Check if 100% of the members are numbers
      # T: Check if characteristic is whether int or continuous
      # F: Accept the data as categorical
      if(length(vector_numeric) > 0  & (length(vector_numeric) / length(vector)) ==1){
        characteristic <- cEnum$Discrete
        if(!all(vector_numeric %% 1 == 0 )) characteristic <- cEnum$Continuous
      }
      
      
      if(characteristic == cEnum$Categorical){
        lowerLimit <- 0
        upperLimit <- "INF"
      }else{
        if(characteristic == cEnum$Discrete){
          lowerLimit <- 0
        }else{
          lowerLimit <- ">0"
        }
        min <- min(vector_numeric)
        if(min < 0) lowerLimit <- "-INF"
        if(min == 0) lowerLimit <- "0"
        
        # upper limit
        upperLimit <- "INF"
        max <- max(vector_numeric)
        
        if(max < 1 && min >= 0){
          if(characteristic == cEnum$Discrete){
            upperLimit <- 1
          }else{
            upperLimit <- "<1"
          }
        } 
        if(min < 0) upperLimit <- "INF"
      }
      
      result <- list(
        characteristic = characteristic,
        lowerLimit= lowerLimit,
        upperLimit = upperLimit
      )
      
      return(result)
    },
    
    
    getInstance = function(dataModel = NULL, perIterationDataModel = NULL){
      new <- DataModelInputData$new(dataModel = dataModel, perIterationDataModel = perIterationDataModel)
      
      
      new$setCurrentDataPath(private$currentDataPath, silent=T)
      new$setTmpDataPath(private$tmpDataPath, silent=T)
      new$setDecSep(private$decSep, silent=T)
      new$setCellSep(private$cellSep, silent=T)
      new$setExcelTable(private$excelTable, silent=T)
      new$setLongFormat(private$longFormat, silent=T, asIt=T)
      new$setInputProperties(private$inputProperties, silent=T)
      
      return(new)
    },
    
    setInstance = function(instance){

      private$dataModel <- instance$getDataModel()
      private$currentDataPath <- instance$getCurrentDataPath()
      private$tmpDataPath <- instance$getTmpDataPath()
      private$decSep <- instance$getDecSep()
      private$cellSep <- instance$getCellSep()
      private$excelTable <- instance$getExcelTable()
      private$longFormat <- instance$getLongFormat()
      private$inputProperties <- instance$getInputProperties()
      
      self$triggerReactiveValues()
    },
    
    getState = function(uuid){

      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        dataModelState <- NULL
        if(!is.null(private$dataModel)){
          dataModelState <- private$dataModel$getState(nextUUID)
          nextUUID <- dataModelState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          currentDataPath = private$currentDataPath,
          tmpDataPath = private$tmpDataPath,
          decSep = private$decSep,
          cellSep = private$cellSep,
          excelTable = private$excelTable,
          longFormat = private$longFormat,
          inputProperties = private$inputProperties,
          
          dataModel = dataModelState#,
          # perIterationDataModel = private$perIterationDataModel
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'DataModelInputData' = ret
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
      
      private$dataModel <- state$dataModel
      private$currentDataPath = state$currentDataPath
      private$tmpDataPath = state$tmpDataPath
      private$decSep = state$decSep
      private$cellSep = state$cellSep
      private$excelTable = state$excelTable
      private$longFormat = state$longFormat
      private$inputProperties = state$inputProperties
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$dataModel)) private$dataModel$resetState()
    }
    
  )

  
)

