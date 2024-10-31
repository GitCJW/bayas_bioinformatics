bayasColors <- function(theme=c("cerulean", "zephyr")){
  match.arg(theme)
  
  ret <- list()
  
  if(theme=="cerulean"){
    
    ret$`--navbar-bg` <- NULL
    
    ret$`--font-error` <- "#c71c22"
    ret$`--font-warning` <- "#dd5600"
    
    ret$`--bs-btn-bg` <- "#2fa4e7"
    
    ret$`--highlight-selection` <- "#d1eaff"
    
    ret$`--plotPreHist-color-1` <- "lightblue"
    ret$`--plotPreHist-color-2` <- "black"
    
    ret$`--plotDiff_range_a` <- "grey"
    ret$`--plotDiff_range_b` <- "#377EB8"
    
    ret$`--plotVerificationLoo-good` <- "#178acc"
    ret$`--plotVerificationLoo-ok` <- "grey"
    ret$`--plotVerificationLoo-bad` <- "red"
    ret$`--plotVerificationLoo-veryBad` <- "darkred"
    
    ret$`--tb-cell-colors-1` <- "#B13433"
    ret$`--tb-cell-colors-2` <- "#8D472B"
    ret$`--tb-cell-colors-3` <- "#675E24"
    ret$`--tb-cell-colors-4` <- "#45731E"
    ret$`--tb-cell-colors-5` <- "#2E7F18"
    
    ret$`--samplingESS-colors-1` <- "darkred"
    ret$`--samplingESS-colors-2` <- "darkkhaki"
    ret$`--samplingESS-colors-3` <- "#93c16c"
    ret$`--samplingESS-colors-4` <- "darkgreen"

    ret$`--effectMatrix-colors-1` <- "#e65050"
    ret$`--effectMatrix-colors-2` <- "#fff"
    ret$`--effectMatrix-colors-3` <- "#377EB8"

    ret$`--creatingData-colors-1` <- "#fff"
    ret$`--creatingData-colors-2` <- "black"
    ret$`--creatingData-colors-3` <- "red"
    ret$`--creatingData-colors-4` <- "#FF6666"

    ret$`--formula-color-1` <- "black"
    ret$`--formula-color-2` <- "#033c73"
    ret$`--formula-color-3` <- "#73a839"
    ret$`--formula-color-4` <- "#555"
    ret$`--formula-color-5` <- "darkgrey"
    
    ret$`--modelCreatingPlot-color-1` <- "lightblue"
    ret$`--modelCreatingPlot-color-2` <- "grey"
    ret$`--modelCreatingPlot-color-3` <- "orange"
    ret$`--modelCreatingPlot-color-4` <- "blue"
    ret$`--modelCreatingPlot-color-5` <- "red"
    
    ret$`--modelCreatingPlot-color-values-1` <- "#999999"
    ret$`--modelCreatingPlot-color-values-2` <- "#03396c"
    ret$`--modelCreatingPlot-color-values-3` <- "#8F2727"
    ret$`--modelCreatingPlot-color-values-4` <- "#E69F00"
    ret$`--modelCreatingPlot-color-values-5` <- "#56B4E9"

    ret$`--modelCreatingPlotROPE-color-values-1` <- "darkred"
    ret$`--modelCreatingPlotROPE-color-values-2` <- "darkgreen"

  }else if(theme=="zephyr"){
    
    ret$`--navbar-bg` <- "#3459e6" #--bs-primary: 
    
    ret$`--font-error` <- "#da292e"
    ret$`--font-warning` <- "#f4bd61"
    
    ret$`--highlight-selection` <- "#f0f3ff"
      
    ret$`--bs-btn-bg` <- "#3459e6"
    
    ret$`--plotPreHist-color-1` <- "#d9e1ff"
    ret$`--plotPreHist-color-2` <- "black"
    
    ret$`--plotDiff_range_a` <- "grey"
    ret$`--plotDiff_range_b` <- "#3459e6"
    
    ret$`--plotVerificationLoo-good` <- "#d9e1ff"
    ret$`--plotVerificationLoo-ok` <- "grey"
    ret$`--plotVerificationLoo-bad` <- "red"
    ret$`--plotVerificationLoo-veryBad` <- "darkred"
    
    ret$`--tb-cell-colors-1` <- "#B13433"
    ret$`--tb-cell-colors-2` <- "#8D472B"
    ret$`--tb-cell-colors-3` <- "#675E24"
    ret$`--tb-cell-colors-4` <- "#45731E"
    ret$`--tb-cell-colors-5` <- "#2E7F18"
    
    ret$`--samplingESS-colors-1` <- "darkred"
    ret$`--samplingESS-colors-2` <- "darkkhaki"
    ret$`--samplingESS-colors-3` <- "#93c16c"
    ret$`--samplingESS-colors-4` <- "darkgreen"
    
    ret$`--effectMatrix-colors-1` <- "#e65050"
    ret$`--effectMatrix-colors-2` <- "#fff"
    ret$`--effectMatrix-colors-3` <- "#3459e6"
    
    ret$`--creatingData-colors-1` <- "#fff"
    ret$`--creatingData-colors-2` <- "black"
    ret$`--creatingData-colors-3` <- "red"
    ret$`--creatingData-colors-4` <- "#FF6666"
    
    ret$`--formula-color-1` <- "black"
    ret$`--formula-color-2` <- "#3459e6"
    ret$`--formula-color-3` <- "#2fb380"
    ret$`--formula-color-4` <- "#555"
    ret$`--formula-color-5` <- "darkgrey"
    
    ret$`--modelCreatingPlot-color-1` <- "#d9e1ff"
    ret$`--modelCreatingPlot-color-2` <- "grey"
    ret$`--modelCreatingPlot-color-3` <- "orange"
    ret$`--modelCreatingPlot-color-4` <- "blue"
    ret$`--modelCreatingPlot-color-5` <- "red"
    
    ret$`--modelCreatingPlot-color-values-1` <- "#999999"
    ret$`--modelCreatingPlot-color-values-2` <- "#03396c"
    ret$`--modelCreatingPlot-color-values-3` <- "#8F2727"
    ret$`--modelCreatingPlot-color-values-4` <- "#E69F00"
    ret$`--modelCreatingPlot-color-values-5` <- "#3459e6"
    
    ret$`--modelCreatingPlotROPE-color-values-1` <- "darkred"
    ret$`--modelCreatingPlotROPE-color-values-2` <- "darkgreen"
    
  }
  
  return(ret)
}

setBayasBayesplotColors <- function(theme=c("cerulean", "zephyr")){
  
  if(theme=="cerulean"){
    color_scheme_set(scheme = "blue")
  }else if(theme=="zephyr"){

    zyphrColor_scheme <- c("#d2daf9","#a4b4f4", "#778fee", 
                           "#4969e9","#1c44e3", "#112988")
    
    color_scheme_set(zyphrColor_scheme)
  }
  
}