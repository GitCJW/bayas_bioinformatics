
DataModelPlotModel <- R6Class(
  classname = "DataModelPlotModel", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.3",
    
    dataModel = NULL,
    
    #next plot id
    id = c(raw=1,pairs=1,pvp=1,mp=1,ppc=1),
    
    # Plot history of raw input plots
    # data.frame containing the name and plot
    plot_history = list(),
    
    # Plot history of Pairs plots
    # data.frame containing the name and plot
    plot_history_pairs = list(),
    
    # Plot history of PVP (prior_vs_posterior) plots
    # data.frame containing the name and plot
    plot_history_pvp = list(),
    
    # Plot history of MP plots
    # data.frame containing the name and plot
    plot_history_mp = list(),
    
    # Plot history of PPC plots
    # data.frame containing the name and plot
    plot_history_ppc = list()
  ),
  
  public = list(
    
    initialize = function(dataModel,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$dataModel <- dataModel
    },
    
    getNextId = function(type=c("raw","pairs","pvp","mp","ppc")){
      match.arg(type)
      private$id[type] <- private$id[type]+1
      return(private$id[type]-1)
    },
    
    getId = function(){
      return(private$id)
    },
    setId = function(id){
      private$id <- id
    },
    
    set.plot_history = function(history, x, silent=F){
      if(history=="raw"){
        private$plot_history <- x
      }else if(history=="pairs"){
        private$plot_history_pairs <- x
      }else if(history=="mp"){
        private$plot_history_mp <- x
      }else if(history=="ppc"){
        private$plot_history_ppc <- x
      }else if(history=="pvp"){
        private$plot_history_pvp <- x
      }else{
        stop("not yet implemented")
      }
      if(!silent) self$triggerHistory(history)
    },
    add.plot_history = function(history, plotname, id=NULL, tabTitle=NULL, plot, csvFile=NULL, silent=F,
                                mpInfo = NULL, pairsInfo = NULL, pvpInfo = NULL, ppcInfo = NULL){
      

      #Remove large objects for saving with rds
      plot <- removeUnnecessaryEnvInPlot(plot)
      
      newPlotObj <- list(plot_name = plotname, id = id, tabTitle = tabTitle, 
                         plot = plot, csvFile = csvFile, mpInfo = mpInfo, pairsInfo = pairsInfo,
                         pvpInfo = pvpInfo, ppcInfo = ppcInfo)
      
      if(history=="raw"){
        private$plot_history <- list.append(private$plot_history, newPlotObj)
      }else if(history=="pairs"){
        tmp <- private$plot_history_pairs
        private$plot_history_pairs <- list.append(private$plot_history_pairs, newPlotObj)
      }else if(history=="mp"){
        tmp <- private$plot_history_mp
        private$plot_history_mp <- list.append(private$plot_history_mp, newPlotObj)
      }else if(history=="ppc"){
        tmp <- private$plot_history_ppc
        private$plot_history_ppc <- list.append(private$plot_history_ppc, newPlotObj)
      }else if(history=="pvp"){
        tmp <- private$plot_history_pvp
        private$plot_history_pvp <- list.append(private$plot_history_pvp, newPlotObj)
      }else{
        stop("not yet implemented")
      }
      

      if(!silent) self$triggerHistory(history)
    },
    remove.plot_history = function(history, plotname, silent=F){
      if(history=="raw"){
        tmp <- private$plot_history
      }else if(history=="pairs"){
        tmp <- private$plot_history_pairs
      }else if(history=="mp"){
        tmp <- private$plot_history_mp
      }else if(history=="ppc"){
        tmp <- private$plot_history_ppc
      }else if(history=="pvp"){
        tmp <- private$plot_history_pvp
      }else{
        stop("not yet implemented")
      }
      
      tmp_new <- list()
      for(tt in tmp){
        if(plotname != tt$plot_name) tmp_new <- list.append(tmp_new, tt)
      }
      tmp <- tmp_new

      if(history=="raw"){
        private$plot_history <- tmp
      }else if(history=="pairs"){
        private$plot_history_pairs <- tmp
      }else if(history=="mp"){
        private$plot_history_mp <- tmp
      }else if(history=="ppc"){
        private$plot_history_ppc <- tmp
      }else if(history=="pvp"){
        private$plot_history_pvp <- tmp
      }
      if(!silent) self$triggerHistory(history)
    },
    get.plot_history = function(history){
      if(history=="raw"){
        return(private$plot_history)
      }else if(history=="pairs"){
        return(private$plot_history_pairs)
      }else if(history=="mp"){
        return(private$plot_history_mp)
      }else if(history=="ppc"){
        return(private$plot_history_ppc)
      }else if(history=="pvp"){
        return(private$plot_history_pvp)
      }else{
        stop("not yet implemented")
      }
    },
    get.plot_by_name = function(history, plotname){
      if(history=="raw"){
        tmp <- private$plot_history
      }else if(history=="pairs"){
        tmp <- private$plot_history_pairs
      }else if(history=="mp"){
        tmp <- private$plot_history_mp
      }else if(history=="ppc"){
        tmp <- private$plot_history_ppc
      }else if(history=="pvp"){
        tmp <- private$plot_history_pvp
      }else{
        stop("not yet implemented")
      }
      for(tt in tmp){
        if(tt$plot_name == plotname){
          return(tt$plot)
        }
      }
    },
    get.raw_plot_file_name = function(plotname){
      for(tt in private$plot_history){
        if(tt$plot_name == plotname){
          return(tt$csvFile)
        }
      }
    },
    get.next_plot = function(history, plotname=NULL, id=NULL){
      if(history=="raw"){
        tmp <- private$plot_history
      }else if(history=="pairs"){
        tmp <- private$plot_history_pairs
      }else if(history=="mp"){
        tmp <- private$plot_history_mp
      }else if(history=="ppc"){
        tmp <- private$plot_history_ppc
      }else if(history=="pvp"){
        tmp <- private$plot_history_pvp
      }else{
        stop("not yet implemented")
      }
      
      plotNames <- c()
      smallerId <- -1
      biggerId <- Inf
      for(tt in tmp){
        if(!is.null(id)){
          if(tt$id < id){
            smallerId <- max(smallerId, tt$id)
          }else{
            biggerId <- min(biggerId, tt$id)
          }
        }
        plotNames <- c(plotNames, tt$plot_name)
      }
      if(!is.null(id)){
        if(biggerId < Inf){
          return(biggerId)
        }else if(smallerId > -1){
          return(smallerId)
        }else{
          return(NULL)
        }
      }
      
      index <- match(plotname, plotNames)
      if(is.na(index)) last(plotNames)
        
      if(index == 1){
        if(length(plotNames) > 1){
          return(plotNames[2])
        }else{
          return(NULL)
        }
      }else{
        return(plotNames[index-1])
      }
    },

    
    triggerHistory = function(history){
      if(history=="raw"){
        self$triggerReactiveValue("plot_history")
      }else if(history=="pairs"){
        self$triggerReactiveValue("plot_history_pairs")
      }else if(history=="mp"){
        self$triggerReactiveValue("plot_history_mp")
      }else if(history=="ppc"){
        self$triggerReactiveValue("plot_history_ppc")
      }else if(history=="pvp"){
        self$triggerReactiveValue("plot_history_pvp")
      }
    },
    
    getInstance =  function(dataModel){
      newInstance <- DataModelPlotModel$new(dataModel)
      
      newInstance$setId(private$id)
      
      newInstance$set.plot_history(history = "raw", private$plot_history)
      newInstance$set.plot_history(history = "pairs", private$plot_history_pairs)
      newInstance$set.plot_history(history = "pvp", private$plot_history_pvp)
      newInstance$set.plot_history(history = "mp", private$plot_history_mp)
      newInstance$set.plot_history(history = "ppc", private$plot_history_ppc)

      return(newInstance)
    },
    
    setInstance = function(instance){
      
      self$setId(instance$getId())
      
      self$set.plot_history(history = "raw", instance$get.plot_history("raw"))
      self$set.plot_history(history = "pairs", instance$get.plot_history("pairs"))
      self$set.plot_history(history = "pvp", instance$get.plot_history("pvp"))
      self$set.plot_history(history = "mp", instance$get.plot_history("mp"))
      self$set.plot_history(history = "ppc", instance$get.plot_history("ppc"))

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
          
          id = private$id,
          plot_history = private$plot_history,
          plot_history_pairs = private$plot_history_pairs,
          plot_history_pvp = private$plot_history_pvp,
          plot_history_mp = private$plot_history_mp,
          plot_history_ppc = private$plot_history_ppc,

          dataModel = dataModelState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'DataModelPlotModel' = ret
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
      private$plot_history <- state$plot_history
      private$plot_history_pairs <- state$plot_history_pairs
      private$plot_history_pvp <- state$plot_history_pvp
      private$plot_history_mp <- state$plot_history_mp
      private$plot_history_ppc <- state$plot_history_ppc

      private$dataModel <- state$dataModel
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$dataModel)) private$dataModel$resetState()
    }
  )
  
)