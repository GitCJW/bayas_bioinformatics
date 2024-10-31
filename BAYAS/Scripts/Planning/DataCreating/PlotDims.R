PlotDims <- R6Class(classname = "PlotDims", 

  public = list(
    x_cat = NULL, #is x categorical
    y_cat = NULL, #is y categorical
    same_var = NULL, #is x==y
    limXMin = NULL, #plot min x limit, nothing to do with categorical elements
    limXMax = NULL, #plot max x limit, nothing to do with categorical elements
    limYMin = NULL, #plot min y limit, nothing to do with categorical elements
    limYMax = NULL, #plot max y limit, nothing to do with categorical elements
    xCatElements = NULL, # if x is categorical, holds selected elements
    yCatElements = NULL, # if y is categorical, holds selected elements
    
    initialize = function(x_cat=F,y_cat=F,same_var=NULL,dims=NULL,xEl=NULL,yEl=NULL){
      self$x_cat <- x_cat
      self$y_cat <- y_cat
      self$same_var <- same_var
      self$limXMin <- dims[1]
      self$limXMax <- dims[2]
      self$limYMin <- dims[3]
      self$limYMax <- dims[4]
      self$xCatElements <- xEl
      self$yCatElements <- yEl
    },
    
    setValues = function(x_cat=NULL,y_cat=NULL,same_var=NULL,limXMin=NULL,limXMax=NULL,limYMin=NULL,limYMax=NULL,xEl=NULL,yEl=NULL, setNULL=F){
      if(!is.null(x_cat) || setNULL) self$x_cat <- x_cat
      if(!is.null(y_cat) || setNULL) self$x_cat <- y_cat
      if(!is.null(same_var) || setNULL) self$same_var <- same_var
      if(!is.null(limXMin) || setNULL) self$limXMin <- limXMin
      if(!is.null(limXMax) || setNULL) self$limXMax <- limXMax
      if(!is.null(limYMin) || setNULL) self$limYMin <- limYMin
      if(!is.null(limYMax) || setNULL) self$limYMax <- limYMax
      if(!is.null(xEl) || setNULL) self$xCatElements <- xEl
      if(!is.null(yEl) || setNULL) self$yCatElements <- yEl
      
      #Adjust min and max
      if(!is.null(self$limXMin) && !is.null(self$limXMax) && self$limXMin == self$limXMax){
        if(self$limXMin == 0){
          self$limXMin <- -0.1
          self$limXMax <- 0.1
        }else{
          self$limXMin <- self$limXMin-abs(self$limXMin)/10
          self$limXMax <- self$limXMin+abs(self$limXMin)/10
        }
      }
      if(!is.null(self$limYMin) && !is.null(self$limYMax) && self$limYMin == self$limYMax){
        if(self$limYMin == 0){
          self$limYMin <- -1
          self$limYMax <- 1
        }else{
          # self$limYMin <- self$limYMin-abs(self$limYMin)/10
          # self$limYMax <- self$limYMin+abs(self$limYMin)/10
          self$limYMin <- self$limYMin-1
          self$limYMax <- self$limYMin+1
        }
      }
    },
    
    getNumDims = function(){
      return(c(self$limXMin,self$limXMax,self$limYMin,self$limYMax))
    }

  )

)