ModelCreatingDataPredictor <- R6Class(
  classname = "ModelCreatingDataPredictor", 
  inherit = SerializationInterface,
  
    private = list(
      stateVersion = "0.1",
      id = NULL,
      mcd = NULL,
      name = NULL,
      type=NULL, #c("intercept", "predictor")
      slope=T,
      
      predLine = 1, #always 1 in a regular glm
      
      oVIds = NULL #ids of OV that are used for this predictor
    ),
    
    public = list(
      
      initialize = function(id, mcd,
                            emptyState = F){
        super$initialize()
        if(emptyState) return()
        
        private$id <- id
        private$mcd <- mcd
      },
      
      setId = function(id, silent=F){
        private$id <- id
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      getId = function(){
        private$id
      },
      
      setMcd = function(mcd){
        private$mcd <- mcd
      },
      getMcd = function(){
        private$mcd
      },
      
      setName = function(name, silent=F){
        private$name <- name
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      setNameBasedOnOvs = function(){
        if(private$type=="intercept") return()
        names <- c()
        for(id in private$oVIds){
          oV <- private$mcd$getOtherVariable(id)
          names <- c(names, oV$getName())
        }
        name <- paste0(names, collapse=":")
        self$setName(name)
      },
      getName = function(){
        private$name
      },

      setType = function(type, silent=F){
        private$type <- type
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      getType = function(){
        private$type
      },
      
      setSlope = function(){
        private$slope <- F
        for(ovid in private$oVIds){
          ov <- private$mcd$getOtherVariable(ovid)
          # if(!is.null(ov$getType()) &&
          #    ov$getType() !="categorical"){
          if(ov$isNumeric()){
            private$slope <- T
            return()
          }
        }
      },
      
      isSlope = function(){
        return(private$slope)
      },
      
      getNumerics = function(){
        if(self$isSlope()){
          nums <- c()
          for(ovid in private$oVIds){
            ov <- private$mcd$getOtherVariable(ovid)
            # if(!is.null(ov$getType()) &&
            #    ov$getType() !="categorical"){
            if(ov$isNumeric()){
              nums <- c(nums, ov$getName())
            }
          }
          return(nums)
        }
        return(NULL)
      },
      
      setPredLine = function(line){
        private$predLine <- line
      },
      
      getPredLine = function(){
        return(private$predLine)
      },
      
      setOVIds = function(ids, setName=T, silent=F){
        private$oVIds <- ids
        if(setName){
          if(private$type=="intercept"){
            self$setName("(Intercept)", silent=silent)
          }else{
            names <- c()
            for(id in private$oVIds){
              oV <- private$mcd$getOtherVariable(id)
              names <- c(names, oV$getName())
            }
            name <- paste0(names, collapse=":")
            self$setName(name, silent=silent)
          }
        }
        self$setSlope()
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      addOVIds = function(id, setName=T, silent=F){
        private$oVIds <- c(private$oVIds,id)
        if(setName){
          names <- c()
          for(id in private$oVIds){
            oV <- private$mcd$getOtherVariable(id)
            names <- c(names, oV$getName())
          }
          name <- paste0(names, collapse=":")
          self$setName(name, silent=silent)
        }
        self$setSlope()
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      getOVIds = function(){
        private$oVIds
      },
      removeOVId = function(id, setName=T, silent=F){
        private$oVIds <- private$oVIds[private$oVIds != id]
        if(setName){
          names <- c()
          for(id in private$oVIds){
            oV <- private$mcd$getOtherVariable(id)
            if(!is.null(oV))
              names <- c(names, oV$getName())
          }
          name <- paste0(names, collapse=":")
          self$setName(name, silent=silent)
        }
        if(!silent) private$mcd$doTriggerPredictor(private$id, "")
      },
      
      isValid = function(){
        err <- private$mcd$getOtherVariableErrors()
        valid <- "valid"
        msg <- ""
        if(is.empty(private$oVIds) && 
           private$type=="predictor") 
          return(list(valid="error", msg="No involved variable!"))
        
        for(ovids in private$oVIds){
          for(e in err){
            if(ovids == e[[1]]){
              if(e[[2]] == "error"){
                return(list(valid="error", 
                            msg="Some involved variables are corrupt."))
              }else{
                valid <- "warning"
                msg <- "Some involved variables are inaccurate."
              }
            }
          }
          if(valid=="valid"){
            oV <- private$mcd$getOtherVariable(ovids)
            if(is.null(oV$getType())){
              valid <- "warning"
            }else if(oV$getType() == "categorical"){
              catModel <- oV$getCategoricalModel()
              #If categorical model contains only a single value
              #Actually no error, only in case of a predictor
              elements <- c()
              if(catModel$getType() == "replacement"){
                for(re in catModel$getReplaceValues()){
                  rep <- re[2]
                  if(rep=="") rep <- re[1]
                  elements <- c(elements, rep)
                }
              }else{
                elements <- unlist(catModel$getValues())
              }
              if(length(unique(elements)) < 2){
                return(list(valid="error", 
                            msg="Each involved categorical variable must have at least <b>two</b> elements."))
              }
            }
          }
        }
        if(self$getAmIRedundant()){
          return(list(valid="warning", 
                      msg="This predictor is completely redundant and is not displayed in the formula."))
        }
        return(list(valid=valid,msg=msg))
      },
      
      
      getInfo = function(){
        getInfoOfPredictor(private$mcd, private$oVIds, private$type)
      },
      
      getInfoPlot = function(){
        getInfoPlotOfPredictor(private$mcd, private$oVIds, private$type)
      },
      
      getInfoPlotCaption = function(){
        getInfoPlotCaption()
      },
      
      getUniqueElements = function(){
        df <- data.frame()
        for(ovid in private$oVIds){
          ov <- private$mcd$getOtherVariable(ovid)
          ovType <- ov$getType()
          if(!is.null(ovType)){
            vals <- private$mcd$getData(ov$getId())
            if(ovType != "categorical" || ov$isNumeric()){
              vals <- rep(ov$getName(), length(vals))
            }
            tmpdf <- data.frame(vals)
            names(tmpdf) <- ov$getName()
            if(is.empty(df)){
              df <- tmpdf
            }else{
              df <- cbind(df, tmpdf)
            }
          }
        }
        if(is.empty(df)) return(NULL)
        inter <- interaction(df, sep=":")
        return(unique(inter))
      },
      
      #Redundant, if this predictor has
      #only redundant parameters
      getAmIRedundant = function(){
        para <- private$mcd$getParameterOfPredictor(private$id)
        return(para$getAmIRedundant())
        # subs <- para$getSubs()
        # if(length(subs)==1){
        #   return(subs[[1]]$getAmIRedundant())
        # }
        # return(F)
      },
      
      doTrigger = function(){
        private$mcd$doTriggerPredictor(private$id, "")
      },

      getInstance = function(mcd){
        new <- ModelCreatingDataPredictor$new(self$getId(), mcd)
        new$setName(private$name, silent=T)
        new$setType(private$type, silent=T)
        new$setOVIds(private$oVIds, silent=T)
        new$setPredLine(private$predLine)
        return(new)
      },
      
      compareTo = function(cT, exact=F){

        if(private$name != cT$getName()) return(F)
        if(private$type != cT$getType()) return(F)
        if(private$slope != cT$isSlope()) return(F)
        if(private$predLine != cT$getPredLine()) return(F)
   
        if(!vectorEqual(private$oVIds, cT$getOVIds())) return(F)

        
        if(!exact) return(T)
        if(private$id != cT$getId()) return(F)
        
        return(T)
      },
      
      getState = function(uuid){
        nextUUID <- uuid
        ret <- list()
        
        if(self$getUUID() == -1){
          
          nextUUID <- nextUUID + 1
          self$setUUID(uuid)

          mcdState <- NULL
          if(!is.null(private$mcdSSD)){
            mcdState <- private$mcdSSD$getState(nextUUID)
            nextUUID <- mcdState$nextUUID
          }
          
          ret <- list(
            uuid = uuid, 
            stateVersion = private$stateVersion,
            
            id = private$id,
            name = private$name,
            type = private$type,
            slope = private$slope,
            predLine = private$predLine, 
            oVIds = private$oVIds,
            
            #R6
            mcd = mcdState
          )
        }
        
        ret <- list(
          uuid = self$getUUID(),
          nextUUID = nextUUID,
          'ModelCreatingDataPredictor' = ret
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
        private$mcd <- state$mcd
        private$name <- state$name
        private$type <- state$type
        private$slope <- state$slope
        private$predLine <- state$predLine
        private$oVIds <- state$oVIds 
      },
      resetState = function(){
        if(!super$resetState()) return()
        if(!is.null(private$mdc)) private$mcd$resetState()
      }
      
    )
)



getInfoOfPredictor = function(mcd, ovIds, predType){
  
  if(!is.null(predType)){
    if(predType == "intercept"){
      return("The (overall) intercept is the value of the response variable when all independent variables are set to zero.")
    }
  }else{
    return("")
  }
  
  if(is.empty(ovIds)) return("No variable selected.")
  
  ovs <- list()
  ovsName <- c()
  ovsType <- c()
  ovsElements <- list() # for categorical
  for(i in ovIds){
    ov <- mcd$getOtherVariable(i)
    ovs <- list.append(ovs, ov)
    ovsName <- c(ovsName, ov$getName())
    
    if(is.null(ov$getType())){
      return(paste0("This predictor refers to an undefined variable. ",
                    "Please fix it before proceeding with predictors."))
    }
    
    ovType <- ifelse(!ov$isNumeric(), "cat", "num")
    ovsType <- c(ovsType, ovType)
    
    el <- character(0)
    if(ovType=="cat"){
      catModel <- ov$getCategoricalModel()
      el <- mcd$getData(ov$getId())
    }
    ovsElements <- list.append(ovsElements, el)
  }
  
  if(is.empty(ovs)){
    return("")
  }
  
  ret <- ""
  
  if(length(ovsType)==1){
    if("num" %in% ovsType){
      ret <- paste0(ret, "This predictor is a single slope for '", ovsName, "'.")
    }else{
      shownEl <- unique(ovsElements[[1]])
      shownEl_sub <- shownEl[1:(min(length(shownEl),3))]
      el_text <- paste0(shownEl_sub, collapse=", ")
      el_text <- paste0("'",el_text,"'")
      if(length(shownEl) > 3) el_text <- paste0(el_text, ", etc.")
      el_text <- paste0("(",el_text, ")")
      ret <- paste0(ret, "This predictor contains a set of indicators (also known as dummy) for each* element of '", 
                    ovsName, "' ", el_text,". \n ", 
                    "")
      ret <- paste0(ret, "\n\n","* For better performance of sampling the first element is skipped in some cases to avoid autocorrelation.")
    }
  }
  else{
    
    if(all(ovsType=="num")){
      names <- paste0(ovsName, collapse="*")
      ret <- paste0("This predictor is a single slope for the multiplication of '", names, "'.")
    }else if("num" %in% ovsType){

      t <- table(ovsType)
      if(t[["num"]] == 1){
        ret <- paste0("This predictor contains a set of slopes for '", ovsName[ovsType=="num"],"' ")
      }else{
        nums <- ovsName[ovsType=="num"]
        nums <- paste0("'", nums, "'")
        nums_names <- paste0(first(nums,-1), collapse=", ")
        nums_names <- paste0(nums_names, " and ",last(nums))
        ret <- paste0("This predictor contains a set of slopes for the multiplicaton of ",
                      nums_names, " ")
      }
      ret <- paste0(ret, "for each element of ")
      
      cats <- ovsName[ovsType=="cat"]
      ovsName <- paste0("'",cats,"'")
      names <- ovsName
      
      ovsElementsNew <- list()
      for(i in 1:length(ovsElements)){
        if(ovsType[i]=="cat")
          ovsElementsNew <- list.append(ovsElementsNew, ovsElements[[i]])
      }
      ovsElements <- ovsElementsNew
      dat <- data.frame(ovsElements[[1]])
      
      if(length(ovsName) > 1){
        names <- paste0(first(ovsName,-1), collapse=", ")
        names <- paste0(names, " and ", last(ovsName))
        
        for(e in 2:length(ovsElements)){
          dat[[e]] <- ovsElements[[e]]
        }
      }

      dat <- unique(dat)
      shownEl_sub <- c()
      for(i in 1:(min(dim(dat)[1],3))){
        shownEl_sub <- c(shownEl_sub, paste0(dat[i,],collapse=":"))
      }
      
      shownEl_sub <- paste0("'",shownEl_sub,"'")
      el_text <- paste0(shownEl_sub, collapse=", ")
      if(dim(dat)[1] > 3) el_text <- paste0(el_text, ", etc.")
      el_text <- paste0("(",el_text, ")")
      
      ret <- paste0(ret, names, " ", el_text,". \n ", 
                    "")
      ret <- paste0(ret, "\n\n","* For better performance of sampling the first element is skipped in some cases to avoid autocorrelation.")
    }else{
      ovsName <- paste0("'",ovsName,"'")
      names <- paste0(first(ovsName,-1), collapse=", ")
      names <- paste0(names, " and ", last(ovsName))
      dat <- data.frame(ovsElements[[1]])
      for(e in 2:length(ovsElements)){
        dat[[e]] <- ovsElements[[e]]
      }
      dat <- unique(dat)
      shownEl_sub <- c()
      for(i in 1:(min(dim(dat)[1],3))){
        shownEl_sub <- c(shownEl_sub, paste0(dat[i,],collapse=":"))
      }
      
      shownEl_sub <- paste0("'",shownEl_sub,"'")
      el_text <- paste0(shownEl_sub, collapse=", ")
      if(dim(dat)[1] > 3) el_text <- paste0(el_text, ", etc.")
      el_text <- paste0("(",el_text, ")")
      ret <- paste0(ret, "This predictor contains a set of indicators (also known as dummy) for each* element of combinations of ", 
                    names, " ", el_text,". \n ", 
                    "")
      ret <- paste0(ret, "\n\n","* For better performance of sampling the first element is skipped in some cases to avoid autocorrelation.")
    }
    
  }

  return(ret)
}


getInfoPlotOfPredictor = function(mcd, ovIds, predType){
  
  if(is.empty(ovIds)) return(ggplot())
  
  
  ovs <- list()
  ovsName <- c()
  ovsType <- c()
  ovsElements <- list() # for categorical
  for(i in ovIds){
    ov <- mcd$getOtherVariable(i)
    ovs <- list.append(ovs, ov)
    ovsName <- c(ovsName, ov$getName())
    
    ovType <- ifelse(ov$getType()=="categorical", "cat", "num")
    ovsType <- c(ovsType, ovType)
    
    el <- character(0)
    if(ovType=="cat"){
      catModel <- ov$getCategoricalModel()
      el <- mcd$getData(ov$getId())
    }
    ovsElements <- list.append(ovsElements, el)
  }
  
  if(is.empty(ovs)){
    ggplot()
  }
  
  y_name=mcd$getMcdResponse()$getName()
  if(is.null(y_name) || y_name == "") y_name <- "Response"
  x_name=""
  num=NULL
  cat=NULL
  
  nums <- ovsName[ovsType=="num"]
  if(!is.empty(nums)){
    num <- paste0(nums, collapse=":")
    x_name <- num
  }
  
  if("cat" %in% ovsType){
    ovsElementsNew <- list()
    for(i in 1:length(ovsElements)){
      if(ovsType[i]=="cat")
        ovsElementsNew <- list.append(ovsElementsNew, ovsElements[[i]])
    }
    ovsElements <- ovsElementsNew
    dat <- data.frame(ovsElements[[1]])
    
    if(length(ovsElements) > 1){
      for(e in 2:length(ovsElements)){
        dat[[e]] <- ovsElements[[e]]
      }
    }
    
    dat <- unique(dat)
    shownEl_sub <- c()
    for(i in 1:(min(dim(dat)[1],3))){
      shownEl_sub <- c(shownEl_sub, paste0(dat[i,],collapse=":"))
    }
    
    shownEl_sub <- paste0("'",shownEl_sub,"'")
    
    cat <- shownEl_sub
  }
  
  gg <- getInfoPlot(y_name, x_name, num, cat)

  return(gg)
}


getInfoPlot = function(y_name, x_name, num=NULL, cat=NULL){

  if(is.null(cat)) cat <- ""
    
  cat_s <- cat
  if(length(cat) > 3) cat_s <- cat[1:3]
  
  gg <- ggplot()
  
  if(is.null(num)){
    
    intercepts <- c(0.5,-0.375, 0.25)
    intercepts <- intercepts[1:length(cat_s)]
    x_axis <- c(-2,2)
    
    y_axis <- sapply(1:length(intercepts), function(i) rep(intercepts[i],2))
    colnames(y_axis) <- cat_s
    y_axis <- as.data.frame(y_axis)
    
    
    res <- tidyr::pivot_longer(y_axis, all_of(cat), names_to="variable", values_to="y")
    res$x <- rep(x_axis, each=length(intercepts))
    
    gg <- ggplot(res)
    for(i in 1:length(intercepts)){
      poly_data <- data.frame(x=rep(x_axis,each=2), 
                              y=c(y_axis[1,i]-0.1,
                                  y_axis[1,i]+0.1,
                                  y_axis[2,i]+0.1,
                                  y_axis[2,i]-0.1))
      
      gg <- gg + geom_polygon(data=poly_data, mapping=aes(x=x,y=y), fill=BAYAS_COLORS$`--modelCreatingPlot-color-values-1`) + 
        annotate("text", x=x_axis[2]-0.75, y=y_axis[2,i]+0.15, label=cat_s[i]) 
    }
    
    gg <- gg + 
      geom_line(aes(x=x, y=y, group=variable)) +
      geom_hline(yintercept=0) +
      geom_vline(xintercept=0) +
      xlab(x_name) + ylab(y_name) +
      scale_x_continuous(expand = c(0,0)) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  else{
    
    slope <- c(0.5,-0.375, 0.25)
    slope <- slope[1:length(cat_s)]
    x_axis <- c(-2,2)
    
    y_func <- function(x, b) 0.5+x*b
    y_axis <- sapply(1:length(slope), function(i) y_func(x_axis, slope[i]))
    colnames(y_axis) <- cat_s
    y_axis <- as.data.frame(y_axis)
    
    res <- data.frame()
    if(dim(y_axis)[2] ==1){
      res <- y_axis
      colnames(res) <- "y"
      res$variable <- ""
    }else{
      res <- tidyr::pivot_longer(y_axis, all_of(cat), names_to="variable", values_to="y")
    }
    res$x <- rep(x_axis, each=length(slope))
    
    gg <- ggplot(res)
    for(i in 1:length(slope)){
      poly_data <- data.frame(x=rep(x_axis,each=2), 
                              y=c(y_axis[1,i]-0.1,
                                  y_axis[1,i]+0.1,
                                  y_axis[2,i]+0.1,
                                  y_axis[2,i]-0.1))

      gg <- gg + geom_polygon(data=poly_data, mapping=aes(x=x,y=y), fill=BAYAS_COLORS$`--modelCreatingPlot-color-values-1`) +
        annotate("text", x=x_axis[2]-0.75, y=y_axis[2,i]+0.1, label=cat_s[i]) 
    }

    gg <- gg + 
      geom_line(aes(x=x, y=y, group=variable)) +
      geom_hline(yintercept=0) +
      geom_vline(xintercept=0) +
      xlab(x_name) + ylab(y_name) +
      scale_x_continuous(expand = c(0,0)) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  return(gg)
}


getInfoPlotCaption = function(){
  paste0("A schematic representation of the effect of the predictor ",
         "on the response variable. \n", 
         "For inidicator variables only up to 3 elements are shown.")
}