
saveSession <- function(dataModel, file, encrypt=T){

  # if(global_browser) browser()
  
  # Evaluation model
  stateEval <- dataModel$getState(1)
  
  #un-nest R6 objects
  retEval <- unnestR6(stateEval)
  retEval <- retEval$unnestList
  

  # Planning model
  id <- stateEval$nextUUID
  
  name <- cMCD$getModelName()
  if(!(!is.null(name) && name == "_tmp"))
    mCDList$addMCDList(cMCD, name=NULL)
  statePlan <- mCDList$getState(id)
  
  #un-nest R6 objects
  retPlan <- unnestR6(statePlan)
  retPlan <- retPlan$unnestList
  
  
  
  #testing
  {
    flag <- flag2 <- flag3 <- F

    tmpFile <- paste0(dirname(dirname(getwd())), "/tmp/all/")
    if(flag){
      for(aa_id in seq_along(retEval)){
        aa <- retEval[[aa_id]]
        saveRDS(aa, file=paste0(tmpFile, "_", aa_id))
      }
    }

    if(flag2){
      index <- 143
      so <- retEval[[index]]$value
      for(aa_id in seq_along(so)){
        aa <- so[[aa_id]]
        saveRDS(aa, file=paste0(tmpFile, "_so_", aa_id))
      }
    }
    
    if(flag3){
      index <- 143
      index2 <- 8
      index3 <- 23
      so <- retEval[[index]]$value[[index2]][[index3]]
      for(aa_id in seq_along(so)){
        aa <- so[[names(so)[aa_id]]]
        saveRDS(aa, file=paste0(tmpFile, "_so_layer", aa_id))
        aa_id <- aa_id+1
      }
    }
  }
  

  state <- list(retEval = retEval, retPlan = retPlan)

  # Write to local
  if(encrypt){
    key <- readRDS(paste0(dirname(getwd()),"/PW/key.key"))
    saveRDS(state, file=file)
    encrypt(inputFile=file, outputFile=file, key=key)
  }else{
    saveRDS(state, file=file)
  }
  dataModel$resetState()
  mCDList$resetState()
  
  
  #Check whether reset works
  if(localUse){
    stateEval2 <- dataModel$getState(1)
    retEval2 <- unnestR6(stateEval2)
    retEval2 <- retEval2$unnestList
    if(length(retEval) != length(retEval2)) browser()
    dataModel$resetState()
    
    
    #Check whether reset works
    statePlan2 <- mCDList$getState(id)
    retPlan2 <- unnestR6(statePlan2)
    retPlan2 <- retPlan2$unnestList
    if(length(retPlan) != length(retPlan2)) browser()
    mCDList$resetState()
  }

  return(T)
}

saveObject <- function(obj, file, encrypt=T){
  
  state <- obj$getState(1)
  
  #un-nest R6 objects
  retObj <- unnestR6(state)$unnestList

  state <- list(retObj = retObj)

  # Write to local
  if(encrypt){
    key <- readRDS(paste0(dirname(getwd()),"/PW/key.key"))
    saveRDS(state, file=file)
    encrypt(inputFile=file, outputFile=file, key=key)
  }else{
    saveRDS(state, file=file)
  }
  obj$resetState()
  
  return(T)
}
  

unnestR6 <- function(state, unnestList = list()){
  if(is.list(state) && length(state) == 3 && !is.null(names(state)[2]) && names(state)[2] == "nextUUID"){

    unlist_state <- unlist(state[[3]])
    if(any(grepl("nextUUID", names(unlist_state)))){
      ret <- unnestR6(state[[3]], unnestList)
      unnestList <- ret$unnestList
      state[[3]] <- ret$state
    }
    
    id <- as.character(state[[1]])
    if(!id %in% names(unnestList) && !is.empty(state[[3]]) &&
       !(length(state[[3]]) == 1 && is.na(state[[3]]))){
      unnestList <- list.append(unnestList,
                                list(class = names(state)[3], value = state[[3]]),
                                id)
    }
    state[[3]] <- NA
  }else{
    
    unlist_state <- unlist(state)
    
    if(any(grepl("nextUUID", names(unlist_state)))){
      for(st_id in seq_along(state)){
        st <- state[[st_id]]
        unlist_state <- unlist(st)
        if(any(grepl("nextUUID", names(unlist_state)))){
          ret <- unnestR6(st, unnestList)
          unnestList <- ret$unnestList
          state[[st_id]] <- ret$state
        }
      }
    }
    
  }
  

  return(list(state=state, unnestList=unnestList))
}


loadSession <- function(dataModel, file, decrypt=T){

  # if(global_browser) browser()
  
  state <- NULL
  if(decrypt){
    key <- readRDS(paste0(dirname(getwd()),"/PW/key.key"))
    
    status <- tryCatch(
      {
        decrypt(inputFile=file, outputFile=file, key=key)
        T
      },
      error = function(exc){
        F
      }
    )
    if(!status){
      showNotification("Can't read this file. Either an incorrect format or corrupted data.", type="error")
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="decryption failed",
                         type="error")
      if(localUse) browser()
      return(list(F, msg="Can't read this file."))
    }
  }
  state <- tryCatch(
    {
      readRDS(file=file)
    },
    error = function(exc){
      NULL
    }
  )
  
  if(is.null(state)){
    showNotification("Can't read this file.", type="error")
    malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="read RDS failed",
                       type="error")
    if(localUse) browser()
    return(list(F, msg="Can't read this file."))
  }


  status <- tryCatch({

    ## Planning
    statePlan <- state$retPlan
    
    cMCDUUID <- -1
    objList <- list()
    
    for(st in statePlan){
      class_def <- get(st$class)
      newObj <- class_def$new(emptyState=T)
      objList <- list.append(objList, newObj, as.character(st$value$uuid))
      if(st$class == "ModelCreatingDataList") cMCDUUID <- st$value$uuid
    }
    
    for(st_id in seq_along(statePlan)){
      st <- statePlan[[st_id]]
      loadSingleObject(objList[[st_id]], st, objList, statePlan)
    }
    
    mCDList$setInstance(objList[[as.character(cMCDUUID)]])
    cMCD$setModelName("_tmp")
    cMCD$doTriggerLoadMCD()

    ## Evaluation
    stateEval <- state$retEval
    dataModelUUID <- -1
    objList <- list()
    
    for(st in stateEval){
      class_def <- get(st$class)
      newObj <- class_def$new(emptyState=T)
      objList <- list.append(objList, newObj, as.character(st$value$uuid))
      if(st$class == "DataModel") dataModelUUID <- st$value$uuid
    }
    
    for(st_id in seq_along(stateEval)){
      st <- stateEval[[st_id]]
      loadSingleObject(objList[[st_id]], st, objList, stateEval)
    }
   
    dataModel$setInstance(objList[[as.character(dataModelUUID)]])
    
    #write excelTable from cPIDM
    dM <- dataModel$getDataModelInputData()
    tP <- dM$getTmpDataPath()

    if(!is.null(tP)){
      
      #replace xlsx with csv
      tP$file <- str_replace(tP$file, ".xlsx", ".csv")
      
      data <- dM$getExcelTable()
      write.table(data, tP$file, row.names=F, col.names=F,
                  sep = ",", dec = ".")
      
      tP$type <- "csv"
      tP$sheet <- NULL
      dM$setTmpDataPath(tP)
      dM$setDecSep("point", silent=T)
      dM$setCellSep("comma", silent=T)
    }
    
    #write report items images to file
    dataModel$get.reportProgressModel()$saveImagesOfItems(dataModel, mCDList)
    
    list(T, msg="")
  },
  error =  function(exc){
    msg <- ""
    if(exc$message == "versionTooOld"){
      msg <- "Your version is too old."
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="version too old",
                         type="info")
    }else{
      msg <- "Can't read this file."
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="setting states failed",
                         type="error")
      if(localUse) browser()
    }
    list(F, msg=msg)
  })
  
  return(status)
}

loadObject <- function(obj, objClassName, file, decrypt=T){
  
  state <- NULL
  if(decrypt){
    key <- readRDS(paste0(dirname(getwd()),"/PW/key.key"))
    
    status <- tryCatch(
      {
        decrypt(inputFile=file, outputFile=file, key=key)
        T
      },
      error = function(exc){
        F
      }
    )
    if(!status){
      showNotification("Can't read this file. Either an incorrect format or corrupted data.", type="error")
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="decryption failed",
                         type="error")
      if(localUse) browser()
      return(list(F, msg="Can't read this file."))
    }
  }
  state <- tryCatch(
    {
      readRDS(file=file)
    },
    error = function(exc){
      NULL
    }
  )
  
  if(is.null(state)){
    showNotification("Can't read this file.", type="error")
    malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="read RDS failed",
                       type="error")
    if(localUse) browser()
    return(list(F, msg="Can't read this file."))
  }
  
  

  status <- tryCatch({
    topUuid <- -1
    objList <- list()
    
    state <- state$retObj
    
    for(st in state){
      class_def <- get(st$class)
      newObj <- class_def$new(emptyState=T)
      objList <- list.append(objList, newObj, as.character(st$value$uuid))
      if(st$class == objClassName) topUuid <- st$value$uuid
    }
    
    for(st_id in seq_along(state)){
      st <- state[[st_id]]
      loadSingleObject(objList[[st_id]], st, objList, state)
    }
    obj$setInstance(objList[[as.character(topUuid)]])
    
    list(T, msg="")
  },
  error =  function(exc){
    msg <- ""
    if(exc$message == "versionTooOld"){
      msg <- "Your version is too old."
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="version too old",
                         type="info")
    }else{
      msg <- "Can't read this file."
      malfunction_report(code=malfunctionCode()$incorrectBAYASFile, msg="setting states failed",
                         type="error")
      if(localUse) browser()
    }
    list(F, msg=msg)
  })
  
  return(status)
}

loadSingleObject <- function(obj, objState, objList, state){
  
  va_names <- names(objState$value)
  for(val_id in seq_along(objState$value)){
    val <- objState$value[[val_id]]
    val_name <- va_names[val_id]
    if(val_name != "uuid"){
      unlist_state <- unlist(val)
      if(any(grepl("nextUUID", names(unlist_state)))){
        objState$value[[val_id]] <- loadSingleObjectDepth(val, objList)
      }
    }
  }
  obj$setState(objState$value)
  
  return(T)
}

loadSingleObjectDepth <- function(a, objList){

  if(is.list(a) && length(a) == 3 && !is.null(names(a)[2]) && names(a)[2] == "nextUUID"){
    
    id <- as.character(a$uuid)
    return(objList[[id]])
    
  }else{
    for(el_id in seq_along(a)){
      el <- a[[el_id]]
      
      unlist_state <- unlist(el)
      if(any(grepl("nextUUID", names(unlist_state)))){
        a[[el_id]] <- loadSingleObjectDepth(el, objList)
      }
      
    }
  }
  return(a)
}



encrypt <- function(inputFile, outputFile, key){
  file_content <- readBin(inputFile, raw(), file.size(inputFile))
  encrypted_content <- sodium::data_encrypt(file_content, 
                                            key = key, 
                                            nonce = random(24))
  packed <- sodium_pack(encrypted_content)
  writeBin(packed, outputFile) 
}
decrypt <- function(inputFile, outputFile, key){
  encrypted_raw <- readBin(inputFile, raw(), file.size(inputFile))
  unpacked <- sodium_unpack(encrypted_raw)
  decrypted_content <- sodium::data_decrypt(unpacked, key)
  writeBin(decrypted_content, outputFile)
}

drop_attributes <- function(x) {
  attributes(x) <- NULL
  x
}
sodium_pack <- function(x) {
  c(attr(x, "nonce", exact = TRUE), drop_attributes(x))
}
sodium_unpack <- function(x) {
  i <- seq_len(24L)
  ret <- x[-i]
  attr(ret, "nonce") <- x[i]
  ret
}

getHash <- function(state){
  return(rlang::hash(unlistEmptyEnv(state)))
}

unlistEmptyEnv <- function(state){
  if(is.list(state)){
    for(ss in state){
      state <- unlistEmptyEnv(ss)
    }
  }else{
    if(is.environment(state)) return(NULL)
    if("environment" %in% attributes(state)) attr(state, "environment") <- NULL
  }
  return(state)
}


