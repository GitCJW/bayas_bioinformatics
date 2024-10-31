SerializationInterface <- R6Class(
  classname = "SerializationInterface", 
  
  private = list(
    uuid = -1
  ),
  
  
  public = list(
    
    initialize = function(){
    },
    
    getUUID = function(){
      return(private$uuid)
    },
    setUUID = function(id){
      private$uuid <- id
    },
    
    getState = function(uuid){
      stop("Should be implemented in concrete class.")
    },
    setState = function(state){
      stop("Should be implemented in concrete class.")
    },
    resetState = function(){
      if(private$uuid == -1) return(F)
      private$uuid <- -1
      return(T)
    },
    
    checkVersion = function(classVersion, stateVersion, stop=F){
      ret <- list(needsConversion=F, conversionPossible=F, conversionList=c())
      if(is.null(classVersion) || is.null(stateVersion)){
        ret$needsConversion <- T
        ret$conversionPossible <- F
      }else{
        if(classVersion != stateVersion){
          classVersionSplit <- str_split(classVersion, "\\.")[[1]]
          stateVersionSplit <- str_split(stateVersion, "\\.")[[1]]
          classVersionSplitNum <- as.numeric(classVersionSplit)
          stateVersionSplittNum <- as.numeric(stateVersionSplit)
          if(classVersionSplitNum[1] != stateVersionSplittNum[1]){
            ret$needsConversion <- T
            ret$conversionPossible <- F
          }else{
            ret$needsConversion <- T
            ret$conversionPossible <- T
            diff <- classVersionSplitNum[2] - stateVersionSplittNum[2]
            name <- paste0(class(self)[1],"ConvertFrom")
            for(i in seq_len(diff)){
              from <- paste0(stateVersionSplittNum[1], ".", stateVersionSplittNum[2]+(i-1))
              to <- paste0(stateVersionSplittNum[1], ".", stateVersionSplittNum[2]+i)
              ret$conversionList <- c(ret$conversionList, paste0(name, from, "To", to))
            }
          }
        }
      }
      if(stop && ret$needsConversion && !ret$conversionPossible) stop("versionTooOld")
      return(ret)
    }
    
    
  )
)