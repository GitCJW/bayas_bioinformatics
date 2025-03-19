init_creatingData_function <- function(input, output, session, dataModel){
  
  ## usedDS stores the raw data. Changes in the limits or removing points will not change the data in usedDS.
  ## Removing a point will lead to an index in usedDS of -1. 
  ## The limits and dropping cells are just applied to filteredDS.
  usedDS <<- reactiveVal(NULL)
  filteredDS <<- reactiveVal(NULL)
  allUsedDS <<- list() #named list (name = input$planningDatasetName) of lists (data, id, datasetProp)
  plotDS <- reactiveVal(NULL)
  abcd <- PlotDims$new()
  plotDims <- reactiveVal(abcd) 
  firePlotDims <- reactiveVal(T)
  usedDSHeaders <- reactiveVal(c())
  usedDSHeadersOld <- reactiveVal(c())
  selectedTwoVars <- reactiveVal(c())
  selectedExample <- 1
  gg <- ggplot()
  oldNumberOfCol <<- 0
  
  hoverPoints <- reactiveVal(c())
  plotBrushingPRec <- reactiveVal(F)
  
  #Obsever list
  propTableObservers <<- list()
  
  ## Stores data for input properties table
  ## Data.frame of 6 rows per column (variable)
  ## Column name, rounding, min, max, type, completeness (string)
  datasetProperties <<- reactiveVal(NULL) 
  
  #Generated data, that will be submitted
  generatedDataForSubmitting <<- reactiveVal(NULL)
  submittedData <<- reactiveVal(data.frame(row=numeric(0),value=character(0),stringsAsFactors=F))
  replicateData <<- reactiveVal(NULL)
  
  
  # ---------------------------- #
  
  # Binding of tabpanel of dataset and brushing data and sidebar CollapsePanels
  observeEvent(input$planningCollapseProperties, {
    if(input$planningCollapseProperties == "planningCollapsePanelCreateData"){
      updateTabsetPanel(session, "planningCreatingDataTabsetPanel", selected="Dataset")
    }else if(input$planningCollapseProperties == "planningCollapsePanelBrushingData"){
      updateTabsetPanel(session, "planningCreatingDataTabsetPanel", selected="Brushing data")
    }else if(input$planningCollapseProperties == "planningCollapsePanelVisualizeData"){
      updateTabsetPanel(session, "planningCreatingDataTabsetPanel", selected="Visualize data")
    }
  })
  
  observeEvent(input$planningCreatingDataTabsetPanel, {
    if(input$planningCreatingDataTabsetPanel == "Dataset"){
      updateCollapse(session, id="planningCollapseProperties", open="planningCollapsePanelCreateData")
    }else if(input$planningCreatingDataTabsetPanel == "Brushing data"){
      updateCollapse(session, id="planningCollapseProperties", open="planningCollapsePanelBrushingData")
    }else if(input$planningCreatingDataTabsetPanel == "Visualize data"){
      updateCollapse(session, id="planningCollapseProperties", open="planningCollapsePanelVisualizeData")
    }
  })
  
  ## Craeting new dataset
  observeEvent(input$planningNewData, {
    selectedExample <<-1
    showModal(
      modalDialog(title = NULL, size="l", easyClose=T,
            tags$div(tags$h5("Choose a dataset"), style="text-align:center;"),
            tags$div(style="overflow-y:auto; overflow-x:hidden; max-height:450px;",
              fluidRow(style="padding-bottom:10px;",
                column(4, style="min-width:min-content",
                       imageButtonTitle("planningExampleDataSet_1",
                                        paste0("Images","/Planning/Examples/Empty.png"),
                                        "Empty dataset",
                                        selected=T)),
                column(4, style="min-width:min-content",
                       imageButtonTitle("planningExampleDataSet_2",
                                        paste0("Images","/Planning/Examples/TwoVars1.png"),
                                        "Simple two column dataset")),
                column(4, style="min-width:min-content",
                       imageButtonTitle("planningExampleDataSet_3",
                                        paste0("Images","/Planning/Examples/Group1.png"),
                                        "Single group dataset"))
              ),
              fluidRow(style="width:inherit;",
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_4",
                                               paste0("Images","/Planning/Examples/ResponseEmmptyX.png"),
                                               "Response with empty X")),
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_5",
                                               paste0("Images","/Planning/Examples/ThreeVars1.png"),
                                               "Simple two column dataset")),
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_6",
                                               paste0("Images","/Planning/Examples/ThreeVars2.png"),
                                               "Two groups dataset"))
              ),
              fluidRow(style="width:inherit;",
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_7",
                                               paste0("Images","/Planning/Examples/Empty.png"),
                                               "Empty dataset")),
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_8",
                                               paste0("Images","/Planning/Examples/Empty.png"),
                                               "Simple two column dataset")),
                       column(4, style="min-width:min-content",
                              imageButtonTitle("planningExampleDataSet_9",
                                               paste0("Images","/Planning/Examples/Empty.png"),
                                               "Single group dataset"))
              )
            ),
            footer = fluidRow(
              column(2,tags$div(modalButton("Cancel"), style="float:left;")),
              column(8,tags$div()),
              column(2,actionButton("planningSelectAnDatasetExample","Finish",class="btn btn-default action-button btn-primary shiny-bound-input"))
            )
      )
    )
    
  })
  
  #Init examples dataset clicks
  # runningIndexDatasets <<- 1 
  selectedIndexDataset <<- 1
  oldSelected <<- NULL
  sapply(1:6, function(i){
    observeEvent(input[[paste0("planningExampleDataSet_",i)]], {
      if(selectedExample == i){ #Second click --> choose this example
        setNewDataset(i)
      }else{ # First click --> highlight example
        shinyjs::removeClass(selector="button.planningExampleBtn", class="planningSelectedExampleBtn")
        shinyjs::addClass(paste0("planningExampleDataSet_",i), class="planningSelectedExampleBtn")
        selectedExample <<- i
      }
    })
  })
  
  #New Dataset button. Opens a modal with several examples
  observeEvent(input$planningSelectAnDatasetExample,{
    setNewDataset(selectedExample)
  })
  
  setNewDataset <- function(i){
    removeModal()
    dataset <- getExampleData(i)
    if(dim(dataset)[1] > 0){
      dataset$hiddenIndex <- 1:dim(dataset)[1]
    }else if(dim(dataset)[2] > 0){
      dataset$hiddenIndex <- numeric()
    }
    index <- length(allUsedDS)+1
    l <- list(dataset,index,NULL) #list() --> NULL
    allUsedDS[[index]] <<- l
    selectedIndexDataset <<- index
    enable("planningDatasetName")
    
    flag <- F
    newName <- paste0("Dataset_",i)
    n <- names(allUsedDS)
    if(newName %in% n && input$planningCreatedDatasets != "") flag <-T
    i <- 0
    while(flag){
      if(!paste0(newName, "(",i,")") %in% n){
        newName <- paste0(newName, "(",i,")")
        flag <- F
      }else{
        i <- i+1
      }
    }
    updateTextInput(session, "planningDatasetName",label=NULL, value=newName)
  }
  
  
  #Name of dataset
  observeEvent(input$planningDatasetName, ignoreInit=T, {
    newName <- input$planningDatasetName
    if(gsub("^\\s+|\\s+$", "", newName) == "") return() #trim and check if empty string
    flag <- F
    n <- names(allUsedDS)[-selectedIndexDataset]
    if(newName %in% n && input$planningCreatedDatasets != "") flag <-T
    i <- 0
    if(flag){
      while(flag){
        if(!paste0(newName, "(",i,")") %in% n){
          newName <- paste0(newName, "(",i,")")
          updateTextInput(session, "planningDatasetName",label=NULL, value=newName)
          flag <- F
        }else{
          i <- i+1
        }
      }
    }else{
      if(length(allUsedDS) > 0){
        n <- names(allUsedDS)
        if(is.null(n)) n <- rep("",length(allUsedDS))
        n[selectedIndexDataset] <- newName
        names(allUsedDS) <<- n
        updateSelectInput(session, "planningCreatedDatasets", choices = n, selected=newName)
        updateSelectInput(session, "createdDatasetsInPlanning", choices = c("",n), selected="")
      }
    }
  })
  
  #Select dataset
  observeEvent(input$planningCreatedDatasets, {
    if(!is.null(oldSelected) && oldSelected %in% names(allUsedDS)){
      allUsedDS[[oldSelected]][[1]] <<- usedDS()
      allUsedDS[[oldSelected]][[3]] <<- datasetProperties()
    }
    n <- names(allUsedDS)
    index <- match(input$planningCreatedDatasets,n)
    selectedIndexDataset <<- index
    usedDS(allUsedDS[[selectedIndexDataset]][[1]])
    if(length(allUsedDS[[selectedIndexDataset]])==3){
      datasetProperties(allUsedDS[[selectedIndexDataset]][[3]])
    }else{
      datasetProperties(NULL)
    } 
    updateTextInput(session, "planningDatasetName",value=input$planningCreatedDatasets)
    oldSelected <<- input$planningCreatedDatasets
  })
  
  #Remove dataset
  observeEvent(input$planningRemoveDataset, {
    if(!is.null(input$planningCreatedDatasets) && input$planningCreatedDatasets != ""){
      allUsedDS[[input$planningCreatedDatasets]] <<- NULL
      if(length(allUsedDS)==0){
        usedDS(NULL)
        datasetProperties(NULL)
        updateTextInput(session, "planningDatasetName", value="")
        shinyjs::disable("planningDatasetName")
        oldSelected <<- NULL
        existingOfAddCol <<- F # Reset flag, so that the add "new column" column will be added again
        session$sendCustomMessage(type = "resetValue", message = "planningCreatedDatasets")
      }
      updateSelectInput(session, "planningCreatedDatasets", choices=names(allUsedDS))
    }
  })
   
  #Download current selected dataset
  #TODO-1 download filteredData instead
  output$planningDownloadDataset <- downloadHandler(
    filename = function() {
      paste0(input$planningDatasetName,".csv")
    },
    content = function(file) {
      write.csv(removeHiddenIndex(filteredDS()), file, row.names=F)
    }
  )
  
  #Download all datasets
  output$planningDownloadDatasets <- downloadHandler(
    filename = function() {
      "datasets.zip"
    },
    content = function(fname) {
      tmpDir <- tempdir()
      fs <- list()
      n <- names(allUsedDS)
      for (i in 1:length(allUsedDS)) {
        path <- paste0(tmpDir,"/",n[i], ".csv")
        fs[[i]] <- path
        dat <- filterData(allUsedDS[[i]])
        write.csv(dat, path)
      }
      zip(zipfile=fname, files=fs, flags="-j")
    },
    contentType = "application/zip"
  )
  
  
  # Dataset table
  pageLength <<- 10
  pageStart <<- 0
  output$planningSummarizePlot <- renderDT({
    tmp <- removeHiddenIndex(filteredDS())
    if(is.null(tmp)) return()
    # tmp <- tmp[ , !(colnames(tmp) %in% c("hiddenIndex")), drop=F]
    datatable(tmp,
              editable=list(target = "cell", disable = list(columns = 0)),
              filter="bottom",
              options=list(ordering=T, lengthMenu=c(5,10,20), pageLength=pageLength, displayStart=pageStart, #, stateSave =T
                           columnDefs = list(list(orderable=TRUE, targets=0)))) 
  }, server=F) 

  # Update filtered data, when usedDS changed or filter
  observe({
    isolate(u <- usedDS())
    datProp <- datasetProperties()
    filteredDS(filterData(list(usedDS(),-1,datProp)))
    isolate(dat <- filteredDS())
    cols <- intersect(colnames(datProp),colnames(dat))
    if(length(cols) > 0){
      for(i in cols){
        colDat <- dat[,i]
        #change type
        datProp[5,i] <- getVarType(colDat)
      }
      datasetProperties(datProp)
    }
  })
  
  #dL dataList (data, id, inputProperties) id is irrelevant
  # returning value is a data.frame that contains the same columns as dL[[1]] (usedDS)
  # with an additional column "hiddenIndex"
  filterData <- function(dL){
    tmp <- dL[[1]]
    prop <- dL[[3]]
    if("hiddenIndex" %in% colnames(tmp)) tmp <- tmp[tmp$hiddenIndex > -1,,drop=F] 
    if(!is.null(tmp) && dim(tmp)[2] >0){ #&& !is.na(tmp)
      cnames <- intersect(colnames(tmp)[!colnames(tmp) %in% "hiddenIndex"],colnames(prop))
      if(!is.null(dim(prop))){
        for(i in cnames){
          #TODO only for numerics... ignore categ?
          suppressWarnings(numerics <- !is.na(as.numeric(tmp[,i])))
          numTmp <- as.numeric(tmp[numerics,i])
          if(length(numTmp)==0)next;
          numTmp <- round(numTmp/as.numeric(prop[2,i]))*as.numeric(prop[2,i])
          numTmp[less(numTmp, prop[3,i])] <- as.numeric(prop[3,i])
          numTmp[greater(numTmp, prop[4,i])] <- as.numeric(prop[4,i])
          tmp[numerics,i] <- numTmp
        }
      }
      return(tmp)
    }else{
      return(data.frame(stringsAsFactors=F))
    }
  }
  
  # Observe changes by user in table
  observeEvent(input$planningSummarizePlot_cell_edit, {
    res <- input$planningSummarizePlot_cell_edit
    if("hiddenIndex" %in% usedDSHeaders()[1:res$col]) res$col <-res$col+1
    dat <- usedDS()
    if(trimws(res$value) == "")res$value <- NA 
    # warning("Not necessarily numeric")
    numVal <-as.numeric(res$value)
    if(!is.na(numVal)){
      dat[res$row,res$col] <- numVal
    }else{
      dat[res$row,res$col] <- res$value
    }
    usedDS(dat)
  })
  
  observeEvent(input$planningSummarizePlot_state, {
    inp <- input$planningSummarizePlot_state
    pageLength <<- inp$length
    pageStart <<- inp$start
  })
  

  #Observe changes in the dataset
  #Main part is creating plotDS()
  observe(priority=1,{
    tmp <- usedDS()
    selectedRows <- input$planningSummarizePlot_rows_selected
    if(is.null(tmp)){
      #TODO remove prop table entries?
      plotDS(NULL)
      usedDSHeaders(NULL)
      return()
    }
    
    if(dim(tmp)[1]>0){
      tmp$hiddenIndex <- 1:dim(tmp)[1] #Hint-1
      usedDS(tmp)
    }
    usedDSHeaders(colnames(usedDS()))
    x <- input$planningBrushingX
    y <- input$planningBrushingY
    xCat <- input$planningPlotXAxisLimitsCat
    yCat <- input$planningPlotYAxisLimitsCat
    addPoints <- input$planningRadioButtonAdd
    
    if(is.null(x) || length(usedDSHeaders())==0){
      plotDS(NULL)
    }else{
      if(x %in% usedDSHeaders() && y %in% usedDSHeaders() && length(unique(c(x,y)))==2){
        fraction <- input$planningBrushingFraction
        tmp <-  filteredDS()[c(x,y,"hiddenIndex")]
        
        xType <- getVarType(tmp[,x])
        yType <- getVarType(tmp[,y])
        
        if(xType == "Categorical"){
          xCat[xCat=="(Empty)"] <- NA 
          tmp <- tmp[tmp[,x] %in% xCat,]
        }
        if(yType == "Categorical"){
          yCat[yCat=="(Empty)"] <- NA 
          tmp <- tmp[tmp[,y] %in% yCat,]
        }
        
        #Plot just a fraction of data points
        samples <- getFracIndex(1:length(tmp[,1]),ceiling(length(tmp[,1])*fraction))
        tmp <- tmp[samples,]
        
        if(is.null(tmp) || dim(tmp)[1]==0){
          plotDS(NULL)
          return()
        }
        
        #Handle missing values
        #If both vars have missing values, drop them
        #If just one var has a missing value colorize it red and put the value to mean or 0 in case there are no other values
        tmp <- tmp[!(is.na(tmp[,1]) & is.na(tmp[,2])),] #Drop
        if(dim(tmp)[1]==0) plotDS(data.frame(stringsAsFactors=F))
        if(dim(tmp)[1]>0 && (any(is.na(tmp[,1])) >0 || any(is.na(tmp[,2])) >0 )){
          tmp$hiddenColor <- 1
          tmp$hiddenColor[(is.na(tmp[,1]))] <- 2
          tmp$hiddenColor[(is.na(tmp[,2]))] <- 3
        }
        
        #Highlight selected values
        if(length(selectedRows)>0){
          index  <- filteredDS()$hiddenIndex[selectedRows]
          tmp$hiddenSize <- 1.5
          tmp$hiddenSize[tmp$hiddenIndex %in% index] <- 3.5
        }
              
        warning("For non numeric?")
        
        for(i in 1:2){
          tmp_not_na <- tmp[!is.na(tmp[,i]),i]
          valType <- xType
          if(i==2) valType <- yType
          if(valType=="Categorical"){
            if(length(tmp_not_na)==0){
              tmp[is.na(tmp[,i]),i] <- "(Empty)" #"(Empty_row)"
            }else{
              tmp[is.na(tmp[,i]),i] <- "(Empty)"
            }
          }else{
            if(length(tmp_not_na)==0){
              tmp[is.na(tmp[,i]),i] <- 0
            }else{
              tmp[is.na(tmp[,i]),i] <- mean(tmp_not_na)
            }
          }
        }
        plotDS(tmp)
      }
      # Same var
      else if(x %in% colnames(usedDS()) && y %in% colnames(usedDS()) && x==y){
        if(is.null(removeHiddenIndex(filteredDS()))){
          plotDS(NULL)
          return()
        }
        tmp <- removeHiddenIndex(filteredDS())[x]
        xType <- getVarType(tmp[,x])

        if(xType == "Categorical"){
          cat <- yCat
          if(addPoints) cat <- xCat
          if("(Empty)" %in% cat) cat[cat=="(Empty)"] <- NA
          tmp <- tmp[tmp[,x] %in% cat,,drop=F]
        }else{
          tmp[,x] <- as.numeric(tmp[,x])
        }
        
        #Handle missing values
        #If both vars have missing values, drop them
        #If just one var has a missing value colorize it red and put the value to mean or 0 in case there are not other values
        if(input$planningRadioButtonAdd){
          tmp <- tmp[!is.na(tmp[,1]),,drop=F] #Drop
        }
        if(is.null(dim(tmp))) tmp <- NULL
        
        if(is.null(dim(tmp)) || dim(tmp)[1]==0){
          plotDS(data.frame(stringsAsFactors=F));
        }else if(dim(tmp)[1]>0 && any(is.na(tmp[,1])) > 0){
          tmp$hiddenColor <- 1
          tmp$hiddenColor[is.na(tmp[,1])] <- 4
        }
        
        #Highlight selected values
        if(length(selectedRows)>0){
          selectedRows <- selectedRows[selectedRows<=dim(tmp)[1]]
          tmp$hiddenSize <- 1.5
          tmp$hiddenSize[selectedRows] <- 3.5
        }
        
        warning("For non numeric?")
        
        if(is.null(tmp)){
          plotDS(NULL)
        }else{
          tmp_not_na <- tmp[!is.na(tmp[,1]),1]
          if(xType=="Categorical"){
            tmp[is.na(tmp[,1]),1] <- "(Empty)" 
          }else{
            if(length(tmp_not_na)==0){
              tmp[is.na(tmp[,1]),1] <- 0
            }else{
              tmp[is.na(tmp[,1]),1] <- mean(tmp_not_na)
            }
          }
          plotDS(tmp)
        }
      }
    }
    
 
    #edit datasetProperties
    if(!is.null(usedDS())){
      colnames <- colnames(usedDS())[!colnames(usedDS()) %in% "hiddenIndex"]
      new_df <- as.data.frame(matrix(NA,6,length(colnames)),
                              stringsAsFactors=F)
      isolate(dat <- datasetProperties())
      new_df[2,] <- "0.01"
      new_df[3,] <- -Inf
      new_df[4,] <- Inf
      new_df[5,] <- "Continuous"
      colnames(new_df) <- colnames

      isolate(totalLength <- dim(usedDS())[1])
      if(length(colnames)==0){
        datasetProperties(new_df)
      }else{
        for(i in 1:length(colnames)){
          new_df[1,i] <- colnames[i]
          if(!is.null(dim(dat)[2]) && dim(dat)[2] >=i){
            if(!is.na(dat[2,i])) new_df[2,i] <- dat[2,i]
            if(!is.na(dat[3,i])) new_df[3,i] <- dat[3,i]
            if(!is.na(dat[4,i])) new_df[4,i] <- dat[4,i]
            if(!is.na(dat[5,i])) new_df[5,i] <- dat[5,i]
          }
          t <- usedDS()[,colnames[i]]
          new_df[6,i] <- paste0(length(t[!is.na(t)])," / ",totalLength) 
        }
        datasetProperties(new_df)
      }
    }
  })


  
  flagLimitInputs <- T
  fireLimitInputs <- reactiveVal(T)
  observe(priority=2,{
    print("diminputs")
    numMinX <- input$planningPlotXAxisLimitsMin
    numMaxX <- input$planningPlotXAxisLimitsMax
    numMinY <- input$planningPlotYAxisLimitsMin
    numMaxY <- input$planningPlotYAxisLimitsMax
    catX <- input$planningPlotXAxisLimitsCat
    catY <- input$planningPlotYAxisLimitsCat
    flagLimitInputs <<- F
    isolate(if(fireLimitInputs()) fireLimitInputs(F) else fireLimitInputs(T))
  })
  
  #Change plotDims automatically, depends on:
  #filtereDS
  #Axis vars
  #Num axis --> min and max
  #Cat axis --> selected items
  #Add, adjust_to, delete
  #And adjust also the "automatically adjustment" afterwards
  observe(priority=1,{
    print("edit plotDims...")
    abcd.t <- abcd
    fDS <- filteredDS()
    xAxis <- input$planningBrushingX
    yAxis <- input$planningBrushingY
    fireLimitInputs()
    isolate({
      numMinX <- input$planningPlotXAxisLimitsMin
      numMaxX <- input$planningPlotXAxisLimitsMax
      numMinY <- input$planningPlotYAxisLimitsMin
      numMaxY <- input$planningPlotYAxisLimitsMax
      catX <- input$planningPlotXAxisLimitsCat
      catY <- input$planningPlotYAxisLimitsCat
    })
    adjustLimits <- flagLimitInputs & input$planningPlotYAxisLimitsAuto
    flagLimitInputs <<- T
    
    addPoints <- input$planningRadioButtonAdd
    adjustPoints <- input$planningRadioButtonAdjust
    deletePoints <- input$planningRadioButtonDelete
    
    xVar <- NULL
    yVar <- NULL
    
    if(!is.null(xAxis) && !is.null(yAxis) && all(c(xAxis,yAxis) %in% colnames(fDS)) && xAxis != ""){
      xVar <- getVarType(fDS[,xAxis])
      yVar <- getVarType(fDS[,yAxis])
    }

    #Case 0: No data selected, or wrong column selected
    if(is.null(xVar)){
      return()
    }
    #unusable inputs?
    if(xVar != "Categorical"){
      if((!is.numeric(numMinX) || !is.numeric(numMaxX)) && !input$planningPlotYAxisLimitsAuto)return()
    }
    if(yVar != "Categorical"){
      if((!is.numeric(numMinY) || !is.numeric(numMaxY)) && !input$planningPlotYAxisLimitsAuto)return()
    }
    
    adjustAfter <- F


    #Case 1: Same var selected twice --> x==y
    if(xAxis == yAxis){
      #Case 1.1: numerical var is selected
      if(xVar != "Categorical"){
        tmpData <- as.numeric(fDS[,xAxis])
        tmpData <- tmpData[!is.na(tmpData)]
        #Case 1.1.1: Add --> x axis shows range of numeric var (min,max); y axis shows density (min>=0,max)
        if(addPoints){
          p <- ggplot_build(ggplot(data=data.frame(x=tmpData), aes(x=x)) + 
                              geom_histogram(aes(y=..density..), bins=30))
          if(adjustLimits){
            abcd$setValues(x_cat=F,y_cat=F,same_var=T,
                           limXMin=min(tmpData),limXMax=max(tmpData),limYMin=0,limYMax=max(p$data[[1]]$density),
                           setNULL=T)
          }else{
            abcd$setValues(x_cat=F,y_cat=F,same_var=T,
                           limXMin=numMinX,limXMax=numMaxX,limYMin=numMinY,limYMax=numMaxY,
                           setNULL=T)
            a <- list(min(tmpData),max(tmpData),0,max(p$data[[1]]$density))
            b <- list(numMinX,numMaxX,numMinY,numMaxY)
            if(dimQuasiEqual(a,b)) adjustAfter <- T
          }
        }
        #Case 1.1.2: Adjust_to/delete --> x aixs shows number of points 1:N; y axis shows numeric points
        else{
          if(adjustLimits){
            abcd$setValues(x_cat=F,y_cat=F,same_var=T,
                           limXMin=1,limXMax=length(fDS[,xAxis]),limYMin=min(tmpData),limYMax=max(tmpData),
                           setNULL=T)
          }else{
            abcd$setValues(x_cat=F,y_cat=F,same_var=T,
                           limXMin=numMinX,limXMax=numMaxX,limYMin=numMinY,limYMax=numMaxY,
                           setNULL=T)
            a <- list(1,length(fDS[,xAxis]),min(tmpData),max(tmpData))
            b <- list(numMinX,numMaxX,numMinY,numMaxY)
            if(dimQuasiEqual(a,b)) adjustAfter <- T
          }
        }
      }
      #Case 1.2: categorical var is selected
      else{
        tmpData <- fDS[,xAxis]
        #Case 1.2.1: Add --> x axis show selected elements of var (inputPicker), y axis show count (min>=0,max)
        if(addPoints){
          #remove NA, empty strings
          tmpData <- tmpData[!trimws(tmpData, which="both") %in% c("",NA)]
          p <- table(tmpData)#
          uni <- names(p)
          if(adjustLimits){
            abcd$setValues(x_cat=T,y_cat=F,same_var=T,
                           limXMin=1,limXMax=length(uni),limYMin=0,limYMax=max(p),xEl=uni,
                           setNULL=T)
          }else{
            abcd$setValues(x_cat=T,y_cat=T,same_var=T,
                           limXMin=1,limXMax=length(catX),limYMin=numMinY,limYMax=numMaxY,xEl=catX,
                           setNULL=T)
            a <- list(0,max(p),uni,NULL)
            b <- list(numMinY,numMaxY,catX,NULL)
            if(dimQuasiEqual(a,b)) adjustAfter <- T
          }
        }
        #Case 1.2.2: Adjust_to/delete --> x axis shows number of points 1:N, y shows the categorics (inputPicker)
        else{
          if(adjustLimits){
            choices <- unique(tmpData)
            if(NA %in% choices) choices <- c(setdiff(choices,NA),"(Empty)")
            
            abcd$setValues(x_cat=F,y_cat=T,same_var=T,
                           limYMin=1,limYMax=length(choices),limXMin=1,limXMax=length(tmpData),yEl=choices,
                           setNULL=T)
          }else{
            abcd$setValues(x_cat=F,y_cat=T,same_var=T,
                           limYMin=1,limYMax=length(catY),limXMin=numMinX,limXMax=numMaxX,yEl=catY,
                           setNULL=T)
            a <- list(1,length(tmpData),unique(tmpData),NULL)
            b <- list(numMinX,numMaxX,catY,NULL)
            if(dimQuasiEqual(a,b)) adjustAfter <- T
          }
        }
      }
    }
    #Case 2: x != y
    else{
      tmpDataX <- fDS[,xAxis]
      tmpDataY <- fDS[,yAxis]
      #Case 2.1: x:num, y:cat
      if(xVar!="Categorical" && yVar=="Categorical"){
        tmpDataX <- tmpDataX[!is.na(tmpDataX)]
        choices <- unique(tmpDataY)
        if(NA %in% choices) choices <- c(setdiff(choices,NA),"(Empty)")
        if(all(is.na(fDS[is.na(fDS[,yAxis]),xAxis]))) choices <- setdiff(choices,"(Empty)")
        
        #Case 2.1.1: Add/Adjust_to/delete --> x axis shows range of num var (min,max); y axis show elements of y (inputPicker)
        if(adjustLimits){
          abcd$setValues(x_cat=F,y_cat=T,same_var=F,
                         limYMin=1,limYMax=length(unique(tmpDataY)),limXMin=min(tmpDataX),limXMax=max(tmpDataX),yEl=choices,
                         setNULL=T)
        }else{
          abcd$setValues(x_cat=F,y_cat=T,same_var=F,
                         limYMin=1,limYMax=length(catY),limXMin=numMinX,limXMax=numMaxX,yEl=catY,
                         setNULL=T)
          a <- list(min(tmpDataX),max(tmpDataX),choices,NULL)
          b <- list(numMinX,numMaxX,catY,NULL)
          if(dimQuasiEqual(a,b)) adjustAfter <- T
        }
      }
      #Case 2.2: x:cat, y:num
      else if(yVar!="Categorical" && xVar=="Categorical"){
        tmpDataY <- tmpDataY[!is.na(tmpDataY)]
        choices <- unique(tmpDataX)
        if(NA %in% choices) choices <- c(setdiff(choices,NA),"(Empty)")
        if(all(is.na(fDS[is.na(fDS[,xAxis]),yAxis]))) choices <- setdiff(choices,"(Empty)")
        
        #Case 2.2.1: Add/Adjust_to/delete --> x axis show elements of y (inputPicker); y axis shows range of num var (min,max) 
        if(adjustLimits){
          abcd$setValues(x_cat=T,y_cat=F,same_var=F,
                         limXMin=1,limXMax=length(unique(tmpDataX)),limYMin=min(tmpDataY),limYMax=max(tmpDataY),xEl=choices,
                         setNULL=T)
        }else{
          abcd$setValues(x_cat=T,y_cat=F,same_var=F,
                         limXMin=1,limXMax=length(catX),limYMin=numMinY,limYMax=numMaxY,xEl=catX,
                         setNULL=T)
          a <- list(min(tmpDataY),max(tmpDataY),choices,NULL)
          b <- list(numMinY,numMaxY,catX,NULL)
          if(dimQuasiEqual(a,b)) adjustAfter <- T
        }
      }
      #Case 2.3: x,y num
      else if(xVar!="Categorical" && yVar!="Categorical"){
        tmpDataX <- tmpDataX[!is.na(tmpDataX)]
        tmpDataY <- tmpDataY[!is.na(tmpDataY)]
        #Case 2.3.1: Add/Adjust_to/delete --> x,y axis shows range of num var (min,max) 
        if(adjustLimits){
          abcd$setValues(x_cat=F,y_cat=F,same_var=F,
                         limXMin=min(tmpDataX),limXMax=max(tmpDataX),limYMin=min(tmpDataY),limYMax=max(tmpDataY),
                         setNULL=T)
        }else{
          abcd$setValues(x_cat=F,y_cat=F,same_var=F,
                         limXMin=numMinX,limXMax=numMaxX,limYMin=numMinY,limYMax=numMaxY,
                         setNULL=T)
          a <- list(min(tmpDataX),max(tmpDataX),min(tmpDataY),max(tmpDataY))
          b <- list(numMinX,numMaxX,numMinY,numMaxY)
          if(dimQuasiEqual(a,b)) adjustAfter <- T
        }
      }
      #Case 2.4: x,y cat
      else{
        #Case 2.4.1: Add/Adjust_to/delete --> x,y axis show elements of x,y (inputPicker)
        choicesX <- unique(tmpDataX)
        choicesY <- unique(tmpDataY)
        if(NA %in% choicesX) choicesX <- c(setdiff(choicesX,NA),"(Empty)")
        if(NA %in% choicesY) choicesY <- c(setdiff(choicesY,NA),"(Empty)")
        if(all(is.na(fDS[is.na(fDS[,xAxis]),yAxis]))) choicesX <- setdiff(choicesX,"(Empty)")
        if(all(is.na(fDS[is.na(fDS[,yAxis]),xAxis]))) choicesY <- setdiff(choicesY,"(Empty)")
        
        if(adjustLimits){
          abcd$setValues(x_cat=T,y_cat=T,same_var=F,
                         limXMin=0,limXMax=length(choicesX),0,limYMax=length(choicesY),
                         xEl=choicesX, yEl=choicesY,
                         setNULL=T)
        }else{
          abcd$setValues(x_cat=T,y_cat=T,same_var=F,
                         limXMin=0,limXMax=length(catX),0,limYMax=length(catY),
                         xEl=catX, yEl=catY,
                         setNULL=T)
          a <- list(choicesX, choicesY,NULL,NULL)
          b <- list(catX, catY,NULL,NULL)
          if(dimQuasiEqual(a,b)) adjustAfter <- T
        }
      }
    }
    if(adjustAfter || adjustLimits){
      updatePrettyCheckbox(session,"planningPlotYAxisLimitsAuto",value=T)
      print("update nums")
      #Update numerics
      numDims <- abcd$getNumDims()
      updateNumericInput(session,"planningPlotXAxisLimitsMin",value=numDims[1])
      updateNumericInput(session,"planningPlotXAxisLimitsMax",value=numDims[2])
      updateNumericInput(session,"planningPlotYAxisLimitsMin",value=numDims[3])
      updateNumericInput(session,"planningPlotYAxisLimitsMax",value=numDims[4])
      updatePickerInput(session,"planningPlotXAxisLimitsCat", selected=abcd$xCatElements)
      updatePickerInput(session,"planningPlotYAxisLimitsCat", selected=abcd$yCatElements)
    }else{
      updatePrettyCheckbox(session,"planningPlotYAxisLimitsAuto",value=F)
    }
    isolate(firePlotDims(!firePlotDims()))
    plotDims(abcd)
  })

  
  #Properties input for each single column
  propertiesOverview(session, input, output)
  
  #Remove empty rows (at least one row have to be left)
  observeEvent(input$removeEmptyRows, {
    dat <- removeHiddenIndex(filteredDS())
    if(dim(dat)[1] == 0)return()
    usedDat <- usedDS()
    s <- vapply(1:dim(dat)[1], function(i){all(is.na(dat[i,]))}, FUN.VALUE=F)
    ## Everything na, keep just one empty row
    if(all(s)){
      new_dat <- as.data.frame(matrix(NA,1,dim(usedDat)[2]),stringsAsFactors=F)
      colnames(new_dat) <- colnames(usedDat)
      usedDS(new_dat)
    }else{
      index <- filteredDS()$hiddenIndex[s]
      dat <- usedDat[!usedDat$hiddenIndex %in% index,]
      rownames(dat) <- 1:dim(dat)[1]
      usedDS(dat)
    }
  })
  
  # Remove rows with empty cell (complete.cases)
  observeEvent(input$removeRowsOfNA, {
    fData <- filteredDS()
    data <- usedDS()
    if(dim(fData)[1] == 0)return()
    fData <- fData[complete.cases(fData),]
    if(dim(fData)[1]==0){
      new_dat <- as.data.frame(matrix(NA,1,dim(data)[2]),stringsAsFactors=F)
      colnames(new_dat) <- colnames(usedDat)
      usedDS(new_dat)
    }else{
      index <- fData$hiddenIndex
      data <- data[data$hiddenIndex %in% index,]
      rownames(data) <- 1:dim(data)[1]
      usedDS(data)
    }
    
  })
  
  
  #Observe changes in dataset colnames
  observe({
    headers <- usedDSHeaders()
    headers <- headers[!(headers %in% c("hiddenIndex"))]
    isolate(old <- usedDSHeadersOld())
    if(length(old) != length(headers)){
      if(length(headers) >1){
        y <- headers[2]
      }else{
        y <- headers[1]
      }
      x <- headers[1]
      
      if(!is.null(y) && is.na(y)) y<-NULL
      if(!is.null(x) && is.na(x)) x<-NULL
      if(is.null(x) && is.null(y)){
        session$sendCustomMessage(type = "resetValue", message = "planningBrushingX")
        session$sendCustomMessage(type = "resetValue", message = "planningBrushingY")
        updateSelectInput(session, "planningBrushingX", choices=c(""), selected="")
        updateSelectInput(session, "planningBrushingY", choices=c(""), selected="")
      }else{
        updateSelectInput(session, "planningBrushingX", choices=headers, selected=x)
        updateSelectInput(session, "planningBrushingY", choices=headers, selected=y)
      }

    }else{
      x_pos <- match(input$planningBrushingX, old)
      y_pos <- match(input$planningBrushingY, old)
      updateSelectInput(session, "planningBrushingX", choices=headers, selected=headers[x_pos])
      updateSelectInput(session, "planningBrushingY", choices=headers, selected=headers[y_pos])
    }
    usedDSHeadersOld(headers)
  })
  

  
  ##Brushing options
  
  #change size of brush rectangle
  radiusIsEqual <<- T
  lockRadiosEqual <<- c(F,F)
  showBrushRec <- reactiveVal(F)
  showUntil <<- -1
  showUntilFire <- reactiveVal(0)
  observeEvent(input$planningBrushingRadius, ignoreInit=T, {
    if(!lockRadiosEqual[1]){
      lockRadiosEqual[2] <<- T
      showBrushRec(T)
      showUntilFire(showUntilFire()+1)
      pD <- abcd$getNumDims()
      hoverPoints(c(pD[[2]]-(pD[[2]]-pD[[1]])/2, pD[[4]]-(pD[[4]]-pD[[3]])/2))
    } 
    if(!lockRadiosEqual[1] && radiusIsEqual){
      updateSliderInput(session, "planningBrushingRadius2", value=input$planningBrushingRadius)
    }
    if(lockRadiosEqual[1]) lockRadiosEqual[1] <<- F
  })
  observeEvent(input$planningBrushingRadius2, ignoreInit=T, {
    if(!lockRadiosEqual[2]){
      lockRadiosEqual[1] <<- T
      showBrushRec(T)
      showUntilFire(showUntilFire()+1)
      pD <- abcd$getNumDims()
      hoverPoints(c(pD[[2]]-(pD[[2]]-pD[[1]])/2, pD[[4]]-(pD[[4]]-pD[[3]])/2))
    } 
    if(!lockRadiosEqual[2] && radiusIsEqual){
      updateSliderInput(session, "planningBrushingRadius", value=input$planningBrushingRadius2)
    }
    if(lockRadiosEqual[2]) lockRadiosEqual[2] <<- F
  })
  observeEvent(input$planningBrushingRadiusEqual, ignoreInit=T, {
    radiusIsEqual <<- if(radiusIsEqual) F else T 
    if(radiusIsEqual){
      updateActionButton(session, "planningBrushingRadiusEqual", icon=icon("equals"))
    }else{
      updateActionButton(session, "planningBrushingRadiusEqual", icon=icon("not-equal"))
    }
  })
  #show the brush rectangle for a short time period
  observe({
    showUntilFire()
    if(showBrushRec()){
      showUntil <<- as.numeric(Sys.time()) +2
      shinyjs::delay(2000, showBrushRec(F))
      plotBrushingPRec(T)
    }else{
      if(as.numeric(Sys.time()) >= showUntil){
        plotBrushingPRec(F)
      }else{
        shinyjs::delay(max(0,(showUntil-as.numeric(Sys.time()))*1000), showBrushRec(F))
      }
    }
  }) 

  #Change axis selection ui for min and max or categorical
  observe(priority = -1,{
    
    prop <- datasetProperties()
    selAxisX <- input$planningBrushingX
    selAxisY <- input$planningBrushingY
    addPoints <- input$planningRadioButtonAdd
    xType <- prop[5,selAxisX]
    yType <- prop[5,selAxisY]
    
    pDNum <- abcd$getNumDims()
    isolate(fD <- filteredDS())
    if(is.null(selAxisX) || is.null(selAxisY) || is.null(xType) || is.null(yType)){
      output$planningPlotXAxisLimitsUI <- renderUI(tags$div())
      output$planningPlotYAxisLimitsUI <- renderUI(tags$div())
      return()
    }
    
    if(!is.null(xType) && is.character(xType) && (xType != "Categorical" || (xType == "Categorical" && !is.null(selAxisX) && !is.null(selAxisY) && selAxisX == selAxisY  && !addPoints))){
      output$planningPlotXAxisLimitsUI <- renderUI({
        tagList(
          column(6,numericInput("planningPlotXAxisLimitsMin",label=NULL, value=ifelse(!is.na(pDNum[[1]]),pDNum[[1]],-10)), style="width:50%;margin-bottom:-15px;padding:0px 5px 0px 5px;"),
          column(6,numericInput("planningPlotXAxisLimitsMax",label=NULL, value=ifelse(!is.na(pDNum[[2]]),pDNum[[2]],10)), style="width:50%;margin-bottom:-15px;padding:0px 5px 0px 5px;")
        )
      })
    }else{
      output$planningPlotXAxisLimitsUI <- renderUI({
        if(selAxisX == selAxisY){
          tmp <- fD[,selAxisX]
          tmp <- tmp[!is.na(tmp) & tmp!=""]
          choices <- unique(tmp)
        }else{
          isolate({
            choices <- unique(fD[,selAxisX])
            if(NA %in% choices) choices <- c(setdiff(choices,NA),"(Empty)")
            if(all(is.na(fD[is.na(fD[,selAxisX]),selAxisY]))) choices <- setdiff(choices,"(Empty)")
          })
        }
        selected <- abcd$xCatElements
        tagList(
          column(12,pickerInput("planningPlotXAxisLimitsCat",label=NULL, choices=choices,selected=selected,multiple=T,options=pickerOptions(actionsBox=T)), style="margin-bottom:-15px;")
        )
      })
    }
    if(!is.null(yType) && is.character(yType) && (yType != "Categorical" || (yType == "Categorical" && !is.null(selAxisX) && !is.null(selAxisY) && selAxisX == selAxisY && addPoints))){
      output$planningPlotYAxisLimitsUI <- renderUI({
        tagList(
          column(6,numericInput("planningPlotYAxisLimitsMin",label=NULL, value=ifelse(!is.na(pDNum[[3]]),pDNum[[3]],-10)), style="width:50%;margin-bottom:-15px;padding:0px 5px 0px 5px;"),
          column(6,numericInput("planningPlotYAxisLimitsMax",label=NULL, value=ifelse(!is.na(pDNum[[4]]),pDNum[[4]],10)), style="width:50%;margin-bottom:-15px;padding:0px 5px 0px 5px;")
        )
      })
    }else{
      output$planningPlotYAxisLimitsUI <- renderUI({
        isolate(choices <- unique(fD[,selAxisY]))
        if(NA %in% choices) choices <- c(setdiff(choices,NA),"(Empty)")
        if(selAxisX != selAxisY && all(is.na(fD[is.na(fD[,selAxisY]),selAxisX]))) choices <- setdiff(choices,"(Empty)")

        
        selected <- abcd$yCatElements
        tagList(
          column(12,pickerInput("planningPlotYAxisLimitsCat",label=NULL, choices=choices,selected=selected,multiple=T,options=pickerOptions(actionsBox=T)), style="margin-bottom:-15px;")
        )
      })
    }
  })
  
  #SelectInputs of X and Y axis
  observeEvent(input$planningBrushingX, ignoreNULL = F, {
    if(!is.null(input$planningBrushingX) && input$planningBrushingX!="") x <- input$planningBrushingX else x <- NULL
    if(!is.null(input$planningBrushingY) && input$planningBrushingY!="") y <- input$planningBrushingY else y <- NULL
    selectedTwoVars(c(x,y))
  })
  observeEvent(input$planningBrushingY, ignoreNULL = F, {
    if(!is.null(input$planningBrushingX) && input$planningBrushingX!="") x <- input$planningBrushingX else x <- NULL
    if(!is.null(input$planningBrushingY) && input$planningBrushingY!="") y <- input$planningBrushingY else y <- NULL
    selectedTwoVars(c(x,y))
  })
  #If one of X or Y changes
  observe({
    if(is.null(selectedTwoVars()[1])){
      session$sendCustomMessage(type = "resetValue", message = "planningBrushingAdjustTo")
      updateSelectInput(session,"planningBrushingAdjustTo",choices=c(""), selected="")
    }else{
      updateSelectInput(session,"planningBrushingAdjustTo",choices=selectedTwoVars(), selected=selectedTwoVars()[1])
    }
  })
  
  #Render brushing plot 
  output$planningBruhsingPlot <- renderPlot({
    print("overall plot...")
    pDS <- plotDS()
    if(is.null(pDS))return(ggplot()) #TODO verify
    firePlotDims()
    pD <- plotDims()$getNumDims()
    vars <- selectedTwoVars()
    if(is.na(vars) || is.null(vars) || length(vars)==0 || dim(pDS)[1] == 0){ #remove dim(pDS)[1] == 0?
      gg <<- ggplot(data=pDS)
    }else if(!is.null(vars) && vars[1] == vars[2]){ #same variable selected
      brushPlotSingle()
    }else{
      brushPlotTwo()
    }
    
    pD <- abcd$getNumDims()
    if(is.num(pD)){
      if(dim(pDS)[1] == 0){
        if(!abcd$x_cat) gg <<- gg + xlim(pD[1],pD[2])
        if(!abcd$y_cat) gg <<- gg + ylim(pD[3],pD[4])
      }else{
        if(!abcd$x_cat && !abcd$y_cat){
          gg <<- gg + coord_cartesian(xlim=c(pD[1],pD[2]),ylim=c(pD[3],pD[4]))
        }else if(!abcd$x_cat){
          gg <<- gg + coord_cartesian(xlim=c(pD[1],pD[2]))
        }else if(!abcd$y_cat){ 
          gg <<- gg + coord_cartesian(ylim=c(pD[3],pD[4]))
        }
      }
    }

    #Area plot
    if(plotBrushingPRec()){
      xMin <- as.numeric(input$planningBrushingMinX)
      xMax <- as.numeric(input$planningBrushingMaxX)
      yMin <- as.numeric(input$planningBrushingMinY)
      yMax <- as.numeric(input$planningBrushingMaxY)
      radius <- c(input$planningBrushingRadius,input$planningBrushingRadius2)
      
      xPlot <- c(pD[[1]],pD[[2]])
      yPlot <- c(pD[[3]],pD[[4]])
      x <- hoverPoints()[1]
      y <- hoverPoints()[2]
      
      xRad <- ((xPlot[2] - xPlot[1])/2) * (radius[1]/100)
      yRad <- ((yPlot[2] - yPlot[1])/2) * (radius[2]/100)
      xRange <- c(min(max(xMin,x-xRad),xMax),max(min(xMax,x+xRad),xMin))
      yRange <- c(min(max(yMin,y-yRad),yMax),max(min(yMax,y+yRad),yMin))

      gg <<- gg + annotate(geom = "rect",
                           xmin = xRange[1], ymin = yRange[1], xmax = xRange[2], ymax = yRange[2],
                           fill = "light blue", alpha = 0.5)
    }
    return(gg)
  })

  
  #Variable selected twice for X and Y --> X==>Y
  brushPlotSingle <- function(){
    var <- selectedTwoVars()[1]
    pDS <- plotDS()
    pD <- abcd$getNumDims()
    index_name <- "index"
    if(var == "index") index_name <- "index_index"
    varType <- getVarType(pDS[,var])
    
    if(varType == "Categorical"){
      if(input$planningRadioButtonAdd){
        if(is.null(pDS) || dim(pDS)[1]==0){
          new_data <- data.frame(x=numeric(),stringsAsFactors=F)
          colnames(new_data) <- var
          gg <<- ggplot(data=new_data, aes_string(x=var)) 
        }else{
          new_data <- data.frame(x=pDS[,var],stringsAsFactors=F)
          colnames(new_data) <- var
          gg <<- ggplot(data=new_data, aes_string(x=var)) + 
            geom_bar(aes_string(x=var),  colour=BAYAS_COLORS$`--creatingData-colors-2`, fill=BAYAS_COLORS$`--creatingData-colors-1`)
        }
      }else{
        if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
          new_data <- data.frame(x=pDS[,var],hiddenColor=pDS[,"hiddenColor"], hiddenSize=pDS[,"hiddenSize"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenColor", "hiddenSize", index_name)
          lev <- levels(as.factor(new_data[,var]))
          numberSelectedElements <- match("(Empty)",lev)
          data_arrow <- new_data[new_data$hiddenColor == 4,c(var,index_name)]
          data_arrow[[var]] <- numberSelectedElements + 0.0025*(pD[[4]]-pD[[3]])
          new_data$hiddenColor <- ifelse(new_data$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, color="hiddenColor", size="hiddenSize")) + 
            scale_color_identity() + scale_size_identity() +
            geom_point(data=data_arrow, aes_string(x=index_name,y=var),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)
        }else if("hiddenColor" %in% colnames(pDS)){
          new_data <- data.frame(x=pDS[,var],hiddenColor=pDS[,"hiddenColor"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenColor", index_name)
          lev <- levels(as.factor(new_data[,var]))
          numberSelectedElements <- match("(Empty)",lev)
          data_arrow <- new_data[new_data$hiddenColor == 4,c(var,index_name)]
          data_arrow[[var]] <- numberSelectedElements + 0.0025*(pD[[4]]-pD[[3]])
          new_data$hiddenColor <- ifelse(new_data$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, color="hiddenColor")) + scale_color_identity() +
            geom_point(data=data_arrow, aes_string(x=index_name,y=var),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)
        }else if("hiddenSize" %in% colnames(pDS)){
          new_data <- data.frame(x=pDS[,var], hiddenSize=pDS[,"hiddenSize"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenSize", index_name)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, size="hiddenSize")) + scale_size_identity()
        }else{
          new_data <- data.frame(x=pDS[,var], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, index_name)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var))
        }
      }
    }
    #NUM
    else{ 
      if(input$planningRadioButtonAdd){
        if(is.null(pDS) || dim(pDS)[1]==0){
          new_data <- data.frame(x=numeric(),stringsAsFactors=F)
          colnames(new_data) <- var
          gg <<- ggplot(data=new_data, aes_string(x=var)) 
        }else{
          new_data <- data.frame(x=as.numeric(pDS[,var]),stringsAsFactors=F)
          colnames(new_data) <- var
          gg <<- ggplot(data=new_data, aes_string(x=var)) + 
            geom_histogram(aes(y=..density..),  colour=BAYAS_COLORS$`--creatingData-colors-2`, fill=BAYAS_COLORS$`--creatingData-colors-1`, bins=30) +
            geom_density(alpha=.2, fill=BAYAS_COLORS$`--creatingData-colors-4`)
        }
      }else{
        if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
          new_data <- data.frame(x=pDS[,var],hiddenColor=pDS[,"hiddenColor"], hiddenSize=pDS[,"hiddenSize"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenColor", "hiddenSize", index_name)
          new_data[,var] <- as.numeric(new_data[,var])
          data_arrow <- new_data[new_data$hiddenColor == 4,c(var,index_name)]
          data_arrow[[var]] <- data_arrow[[var]] + 0.0025*(pD[[4]]-pD[[3]])
          new_data$hiddenColor <- ifelse(new_data$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, color="hiddenColor", size="hiddenSize")) + 
            scale_color_identity() + scale_size_identity() +
            geom_point(data=data_arrow, aes_string(x=index_name,y=var),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)
        }else if("hiddenColor" %in% colnames(pDS)){
          new_data <- data.frame(x=pDS[,var],hiddenColor=pDS[,"hiddenColor"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenColor", index_name)
          new_data[,var] <- as.numeric(new_data[,var])
          data_arrow <- new_data[new_data$hiddenColor == 4,c(var,index_name)]
          data_arrow[[var]] <- data_arrow[[var]] + 0.0025*(pD[[4]]-pD[[3]])
          new_data$hiddenColor <- ifelse(new_data$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, color="hiddenColor")) + scale_color_identity() +
            geom_point(data=data_arrow, aes_string(x=index_name,y=var),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)
        }else if("hiddenSize" %in% colnames(pDS)){
          new_data <- data.frame(x=pDS[,var], hiddenSize=pDS[,"hiddenSize"], index=1:dim(pDS)[1],stringsAsFactors=F)
          colnames(new_data) <- c(var, "hiddenSize", index_name)
          new_data[,var] <- as.numeric(new_data[,var])
          gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var, size="hiddenSize")) + scale_size_identity()
        }else{
          if(length(pDS[,var])==0){
            gg <<- ggplot()
          }else{
            new_data <- data.frame(x=pDS[,var], index=1:dim(pDS)[1],stringsAsFactors=F)
            colnames(new_data) <- c(var, index_name)
            new_data[,var] <- as.numeric(new_data[,var])
            gg <<- ggplot(data=new_data) + geom_point(aes_string(x=index_name,y=var))
          }
        }
      }
    }
  }
  
  brushPlotTwo <- function(){
    set.seed(123)
    vars <- selectedTwoVars()
    pDS <- plotDS()
    pD <- abcd$getNumDims()
    varTypeX <- getVarType(pDS[,vars[1]])
    varTypeY <- getVarType(pDS[,vars[2]])
   
    #Case 1: Num and Num
    if(varTypeX != "Categorical" && varTypeY != "Categorical"){
      data_arrow_x <- pDS[pDS$hiddenColor == 2,c(vars[1],vars[2])]
      data_arrow_x[[vars[2]]] <- data_arrow_x[[vars[2]]] + 0.0025*(pD[[4]]-pD[[3]])
      data_arrow_y <- pDS[pDS$hiddenColor == 3,c(vars[1],vars[2])]
      data_arrow_y[[vars[2]]] <- data_arrow_y[[vars[2]]] + 0.0025*(pD[[4]]-pD[[3]])

      if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_point(aes_string(x=vars[1],y=vars[2], color="hiddenColor", size="hiddenSize")) + scale_color_identity() + scale_size_identity() +
          geom_point(data=data_arrow_y, aes_string(x=vars[1],y=vars[2]),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5) +
          geom_point(data=data_arrow_x, aes_string(x=vars[1],y=vars[2]),shape="\u2194",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)

      }else if("hiddenColor" %in% colnames(pDS)){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_point(aes_string(x=vars[1],y=vars[2], color="hiddenColor", size=1.5)) + scale_color_identity() + scale_size_identity() +
          geom_point(data=data_arrow_y, aes_string(x=vars[1],y=vars[2]),shape="\u2195",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5) +
          geom_point(data=data_arrow_x, aes_string(x=vars[1],y=vars[2]),shape="\u2194",size=7, color=BAYAS_COLORS$`--creatingData-colors-3`,alpha=0.5)
      }else if("hiddenSize" %in% colnames(pDS)){
        gg <<- ggplot(data=pDS) + geom_point(aes_string(x=vars[1],y=vars[2], size="hiddenSize")) + scale_size_identity() + scale_color_identity()
      }else{
        gg <<- ggplot(data=pDS) + geom_point(aes_string(x=vars[1],y=vars[2]), size=1.5) + scale_color_identity() + scale_size_identity()
      }
    }
    #Case 2: Num and Cat
    else if(varTypeX != "Categorical" && varTypeY == "Categorical"){
      if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor", size="hiddenSize"), width=0, height=0.1) + 
          scale_color_identity() + scale_size_identity() #+
      }else if("hiddenColor" %in% colnames(pDS)){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor"), size=1.5, width=0, height=0.1) + 
          scale_color_identity() + scale_size_identity() #+
      }else if("hiddenSize" %in% colnames(pDS)){
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], size="hiddenSize"), width=0, height=0.1) + 
          scale_size_identity() + scale_color_identity()
      }else{
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2]), size=1.5, width=0, height=0.1) + 
          scale_color_identity() + scale_size_identity()
      }
    }
    #Case 3: Cat and Num
    else if(varTypeX == "Categorical" && varTypeY != "Categorical"){
      if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor", size="hiddenSize"), width=0.1, height=0) + 
          scale_color_identity() + scale_size_identity() #+
      }else if("hiddenColor" %in% colnames(pDS)){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor"), size=1.5, width=0.1, height=0) + 
          scale_color_identity() + scale_size_identity() #+
      }else if("hiddenSize" %in% colnames(pDS)){
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], size="hiddenSize"), width=0.1, height=0) + 
          scale_size_identity() + scale_color_identity()
      }else{
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2]), size=1.5, width=0.1, height=0) + 
          scale_color_identity() + scale_size_identity()
      }
    }
    #Case 4: Cat and Cat
    else{
      if(all(c("hiddenColor","hiddenSize") %in% colnames(pDS))){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor", size="hiddenSize"), width=0.1, height=0.1) + 
          scale_color_identity() + scale_size_identity() 
      }else if("hiddenColor" %in% colnames(pDS)){
        pDS$hiddenColor <- ifelse(pDS$hiddenColor == 1, BAYAS_COLORS$`--creatingData-colors-2`,BAYAS_COLORS$`--creatingData-colors-3`)
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], color="hiddenColor"), size=1.5, width=0.1, height=0.1) + 
          scale_color_identity() + scale_size_identity() 
      }else if("hiddenSize" %in% colnames(pDS)){
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2], size="hiddenSize"), width=0.1, height=0.1) + 
          scale_size_identity() + scale_color_identity()
      }else{
        gg <<- ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2]), size=1.5, width=0.1, height=0.1) + 
          scale_color_identity() + scale_size_identity()
      }
    }
  }
  # vars <- c("x","y")
  # pDS <- data.frame(x=rnorm(30),y=rep(c("a","b"),each=15))
  # set.seed(123)
  # ggplot(data=pDS) + geom_jitter(aes_string(x=vars[1],y=vars[2]), size=1.5, width=0, height=0.05) +
  #   scale_color_identity() + scale_size_identity()
  # ggplot(data=pDS) + geom_point(aes_string(x=vars[1],y=vars[2]), size=1.5) +
  #   scale_color_identity() + scale_size_identity()

  #Click event of brushing plot
  observeEvent(input$planningBruhsingPlotClick, {
    #If there is no plot data, do nothing
    if(is.null(plotDS())){
      showNotification("Please create a new dataset and at least one column before.", type="warning")
      return()
    }
    isolate(tmp <- usedDS())
    #Click coor
    x <- input$planningBruhsingPlotClick$x
    y <- input$planningBruhsingPlotClick$y
    pD <- abcd$getNumDims()
    
    #axis vars
    xAxis <- input$planningBrushingX
    yAxis <- input$planningBrushingY
    
    xType <- getVarType(tmp[,xAxis])
    yType <- getVarType(tmp[,yAxis])
    
    brushPoints <- input$planningBrushingNumberPoints
    xMin <- if(is.na(as.numeric(input$planningBrushingMinX))) NULL else as.numeric(input$planningBrushingMinX)
    xMax <- if(is.na(as.numeric(input$planningBrushingMaxX))) NULL else as.numeric(input$planningBrushingMaxX)
    yMin <- if(is.na(as.numeric(input$planningBrushingMinY))) NULL else as.numeric(input$planningBrushingMinY)
    yMax <- if(is.na(as.numeric(input$planningBrushingMaxY))) NULL else as.numeric(input$planningBrushingMaxY)
    
    xPlot <- c(pD[[1]],pD[[2]])
    yPlot <- c(pD[[3]],pD[[4]])
    
    radius <- c(input$planningBrushingRadius,input$planningBrushingRadius2)
    

    xRad <- ((xPlot[2] - xPlot[1])/2) * (radius[1]/100)
    yRad <- ((yPlot[2] - yPlot[1])/2) * (radius[2]/100)
  
    xRange <- c(min(max(xMin,x-xRad),xMax),max(min(xMax,x+xRad),xMin))
    yRange <- c(min(max(yMin,y-yRad),yMax),max(min(yMax,y+yRad),yMin))

    cnames <- colnames(tmp)
    
    #Differ between adding, adjustment and removing
    #add
    if(input$planningRadioButtonAdd){
      #Case 1 same var selected twice
      if(xAxis==yAxis){
        xVal <- runif(brushPoints,min=xRange[1],max=xRange[2])
        #Case 1.1 Cat
        if(xType == "Categorical"){
          selCat <- input$planningPlotXAxisLimitsCat
          xVal <- strInPoints(selCat,xVal) 
          xVal <- xVal[complete.cases(xVal)]
        }
        #Case 1.2 Cont
        #Add to data
        matches <- match(xAxis,cnames)
        newLines <- matrix(NA, length(xVal), length(cnames))
        newLines[,matches[1]] <- xVal
      }
      #Case 2 two diff vars
      else{
        xVal <- runif(brushPoints,min=xRange[1],max=xRange[2])
        yVal <- runif(brushPoints,min=yRange[1],max=yRange[2])
        xVal <- xVal[complete.cases(xVal)&complete.cases(yVal)]
        yVal <- yVal[complete.cases(xVal)&complete.cases(yVal)]
        
        #Case 2.1 Cont - Cont
        if(xType != "Categorical" && yType != "Categorical"){
          #Nothing to do here...
        }
        #Case 2.2 Cat - Cat
        else if(xType == "Categorical" && yType == "Categorical"){
          selCat <- input$planningPlotXAxisLimitsCat
          xVal <- strInRange(selCat,xRange)
          selCat <- input$planningPlotYAxisLimitsCat
          yVal <- strInRange(selCat,yRange)
          
          if(is.null(xVal) || is.null(yVal)){
            return()
          }else{
            xVal <- sample(xVal, brushPoints, replace=T)
            yVal <- sample(yVal, brushPoints, replace=T)
          }
        }
        #Case 2.3 Cont - Cat
        else if(xType != "Categorical" && yType == "Categorical"){
          selCat <- input$planningPlotYAxisLimitsCat
          yVal <- strInRange(selCat,yRange)
          if(is.null(yVal) || length(yVal)==0){
            return()
          }
          xVal <- xVal[complete.cases(xVal)]
          yVal <- sample(yVal, length(xVal),replace=T)
        }
        #Case 2.4 Cat - Cont
        else{
          selCat <- input$planningPlotXAxisLimitsCat
          xVal <- strInRange(selCat,xRange)
          if(is.null(xVal) || length(xVal)==0){
            xVal <- numeric(0)
            yVal <- numeric(0)
            return()
          }
          yVal <- yVal[complete.cases(yVal)]
          xVal <- sample(xVal, length(yVal),replace=T)
        }

        #Add to data
        matches <- match(c(xAxis,yAxis),cnames)
        newLines <- as.data.frame(matrix(NA, length(xVal), length(cnames)))
        newLines[,matches[1]] <- xVal
        newLines[,matches[2]] <- yVal
      }
      
      newLines <- as.data.frame(newLines,stringsAsFactors=F)
      colnames(newLines) <- cnames
      pageStart <<- dim(tmp)[1]#init "page" of data table
      tmp<-rbind(tmp,newLines)
      setUsedDS(tmp)
      # usedDS(tmp)
    }
    #adjust
    else if(input$planningRadioButtonAdjust){ 
      adjustTo <- input$planningBrushingAdjustTo
      onlyNa <- input$planningSwitchOnlyNA
      xMin <- xRange[1]
      xMax <- xRange[2]
      yMin <- yRange[1]
      yMax <- yRange[2]
      
      isolate(data <- usedDS())
      isolate(filteredData <- filteredDS())

      #Case 1 same var selected twice
      if(xAxis==yAxis){
        
        xAxis <- "hiddenIndex"
        adjustTo <- "hiddenIndex"
        
        #Case 1.1 Cat
        if(xType == "Categorical"){
          selCat <- input$planningPlotYAxisLimitsCat
          if(onlyNa){
            index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin & is.na(filteredData[,yAxis]))
          }else{
            index <-(!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin)
          }
          index <- filteredData$hiddenIndex[index]
          l <- length(data[data$hiddenIndex %in% index,yAxis])
          ret <- strInRange(selCat, yRange)
          if(is.null(ret)) return()
          ret <- sample(ret,l,replace=T)
          data[data$hiddenIndex %in% index,yAxis] <- ret
        }
        #Case 1.2 Cont
        else{
          if(onlyNa){
            index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin & is.na(filteredData[,yAxis]))
          }else{
            index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin)
          }
          index <- filteredData$hiddenIndex[index]
          l <- length(data[data$hiddenIndex %in% index,yAxis])
          data[data$hiddenIndex %in% index,yAxis] <-  runif(l, yMin, yMax)
        }
      }
      #Case 2 two diff vars
      else{
        #Case 2.1 Cont - Cont
        if(xType != "Categorical" && yType != "Categorical"){
          a <- xAxis
          b <- yAxis
          if(yAxis == adjustTo){
            a <- yAxis
            b <- xAxis
          }
          index <- (!(is.na(filteredData[,a] & is.na(filteredData[,b]))) & filteredData[,a] <= xMax & filteredData[,a] >= xMin)
          if(onlyNa) index <- (index & is.na(filteredData[,b]))
          index <- filteredData$hiddenIndex[index]
          l <- length(data[data$hiddenIndex %in% index,b])
          data[data$hiddenIndex %in% index,b] <-  runif(l, yMin, yMax)
        }
        #Case 2.2 Cat - Cat
        else if(xType == "Categorical" && yType == "Categorical"){
          showNotification("Categorical variables are not adjustable!",type="error")
        }
        #Case 2.3 Cont - Cat
        else if(xType != "Categorical" && yType == "Categorical"){
          # if(yAxis == adjustTo){
            showNotification("Categorical variables are not adjustable!",type="error")
          # }else{
          #   index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin)
          #   if(onlyNa) index <- (index & is.na(filteredData[,yAxis]))
          #   index <- filteredData$hiddenIndex[index]
          #   l <- length(data[data$hiddenIndex %in% index,yAxis])
          #   data[data$hiddenIndex %in% index,yAxis] <-  runif(l, yMin, yMax)
          # }
        }
        #Case 2.4 Cat - Cont
        else{
          showNotification("Categorical variables are not adjustable!",type="error")
        }
      }
      usedDS(data)
    }
    #remove
    else if(input$planningRadioButtonDelete){ 
      adjustTo <- input$planningBrushingAdjustTo
      
      onlyNa <- input$planningSwitchOnlyNA
      xMin <- xRange[1]
      xMax <- xRange[2]
      yMin <- yRange[1]
      yMax <- yRange[2]
      
      isolate(data <- usedDS())
      isolate(filteredData <- filteredDS())
      
      #Case 1 same var selected twice
      if(xAxis==yAxis){
        xAxis <- "hiddenIndex"
        #Case 1.1 Cat
        if(xType == "Categorical"){
          selCatY <- input$planningPlotYAxisLimitsCat
          ret <- strInRange(selCatY, yRange)
          index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin & filteredData[,yAxis] %in% ret)
          if(onlyNa){ 
            showNotification("You can not remove empty values (Only NA).", type="error")
            return()
          }
          index <- filteredData$hiddenIndex[index]
          data[data$hiddenIndex %in% index,yAxis] <- NA
        }
        #Case 1.2 Cont
        else{
          index <- (!(is.na(filteredData[,xAxis] & is.na(filteredData[,yAxis]))) & filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin & filteredData[,yAxis] <= yMax & filteredData[,yAxis] >= yMin)
          if(onlyNa){ 
            showNotification("You can not remove empty values (Only NA).", type="error")
            return()
          }
          index <- filteredData$hiddenIndex[index]
          data[data$hiddenIndex %in% index,yAxis] <- NA
        }
      }
      #Case 2 two diff vars
      else{
        #Case 2.1 Cont - Cont
        if(xType != "Categorical" && yType != "Categorical"){
          tmpData <- filteredData[,c(xAxis,yAxis,"hiddenIndex")]
          
          a <- xAxis
          b <- yAxis
          if(onlyNa){ 
            
            tmpData <- tmpData[(is.na(tmpData[[a]]) | is.na(tmpData[[b]])) & !(is.na(tmpData[[a]]) & is.na(tmpData[[b]])),]
            mA <-mean(filteredData[[a]], na.rm=T)
            mB <-mean(filteredData[[b]], na.rm=T)
            tmpData[is.na(tmpData[[a]]),a] <- if(is.nan(mA)) 0 else mA
            tmpData[is.na(tmpData[[b]]),b] <- if(is.nan(mB)) 0 else mB
            
            ind <- (tmpData[[a]] >= xMin & tmpData[[a]] <= xMax)  &
              (tmpData[[b]] >= yMin & tmpData[[b]] <= yMax)
            ind[is.na(ind)] <- F
            index <- tmpData$hiddenIndex[ind]
          }else{
            mA <-mean(filteredData[[a]], na.rm=T)
            mB <-mean(filteredData[[b]], na.rm=T)
            tmpData[is.na(tmpData[[a]]),a] <- if(is.nan(mA)) 0 else mA
            tmpData[is.na(tmpData[[b]]),b] <- if(is.nan(mB)) 0 else mB
            
            ind <- (tmpData[[a]] >= xMin & tmpData[[a]] <= xMax)  &
              (tmpData[[b]] >= yMin & tmpData[[b]] <= yMax)
            ind[is.na(ind)] <- F
            index <- tmpData$hiddenIndex[ind]
          }
        
          # index <- filteredData$hiddenIndex[ind]
          data[data$hiddenIndex %in% index,c(a,b)] <- NA
        }
        #Case 2.2 Cat - Cat
        else if(xType == "Categorical" && yType == "Categorical"){
          selCatX <- input$planningPlotXAxisLimitsCat
          selCatY <- input$planningPlotYAxisLimitsCat
          retX <- strInRange(selCatX, xRange, withEmpty=T)
          retY <- strInRange(selCatY, yRange, withEmpty=T)
          if(onlyNa){
            indexX <- indexY <- c()
            if(NA %in% retX)
              indexX <- filteredData$hiddenIndex[filteredData[,xAxis] %in% NA & filteredData[,yAxis] %in% retY]
            if(NA %in% retY)
              indexY <- filteredData$hiddenIndex[filteredData[,xAxis] %in% retX & filteredData[,yAxis] %in% NA]
            
            index <- c(indexX, indexY)
          }else{
            index <- filteredData$hiddenIndex[filteredData[,xAxis] %in% retX & 
                                                filteredData[,yAxis] %in% retY]
          }
          data[data$hiddenIndex %in% index,c(xAxis,yAxis)] <- NA
        }
        #Case 2.3 Cont - Cat
        else if(xType != "Categorical" && yType == "Categorical"){
          selCat <- input$planningPlotYAxisLimitsCat
          mA <-mean(filteredData[[xAxis]], na.rm=T)
          index <- is.na(filteredData[,xAxis]) & mA <= xMax & mA >= xMin
          if(!onlyNa){
            index <- index |(filteredData[,xAxis] <= xMax & filteredData[,xAxis] >= xMin)
          }
          ret <- NA
          if(!onlyNa){
            ret <- strInRange(selCat, yRange, withEmpty=T)
          }
          if(onlyNa){
            index <- index | filteredData[,yAxis] %in% ret
          }else{
            index <- index & filteredData[,yAxis] %in% ret
          }
          index <- filteredData$hiddenIndex[index]
          data[data$hiddenIndex %in% index,c(xAxis,yAxis)] <- NA
        }
        #Case 2.4 Cat - Cont
        else{
          selCat <- input$planningPlotXAxisLimitsCat
          mB <-mean(filteredData[[yAxis]], na.rm=T)
          index <- is.na(filteredData[,yAxis]) & mB <= yMax & mB >= yMin
          if(!onlyNa){
            index <- index |(filteredData[,yAxis] <= yMax & filteredData[,yAxis] >= yMin)
          }
          ret <- NA
          if(!onlyNa){
            ret <- strInRange(selCat, xRange, withEmpty=T)
          }
          if(onlyNa){
            index <- index | filteredData[,xAxis] %in% ret
          }else{
            index <- index & filteredData[,xAxis] %in% ret
          }
          l <- data.frame(filteredData$hiddenIndex[index])
          index <- filteredData$hiddenIndex[index]
          data[data$hiddenIndex %in% index,c(xAxis,yAxis)] <- NA
        }
      }
      usedDS(data)
      
      
    }
  })
  

  #For Ctrl option
  observeEvent(input$planningBruhsingPlotHover, {
    x <- input$planningBruhsingPlotHover$x
    y <- input$planningBruhsingPlotHover$y
    # print0(x," ",y)
    hoverPoints(c(x,y))
  })
  

  lock <<- F
  #Key binding x for toggle add/adjust/delete
  observe({
    inp <- input$keyDown
    # print(inp)
    
    # Just in case, 'Brushing data' is selected
    if(isolate(input$planningCollapseProperties == "planningCollapsePanelBrushingData") && !is.null(inp)){
      if(inp == 65){
      #   updatePrettyCheckbox(session, "planningRadioButtonAdd", value=T)
      # }else if(inp == 83){
      #   updatePrettyCheckbox(session, "planningRadioButtonAdjust", value=T)
      #   isolate({
      #     if(input$planningRadioButtonAdjust){
      #       isolate(sel <- selectedTwoVars())
      #       if(length(sel) > 1){
      #         if(input$planningBrushingAdjustTo == sel[1]){
      #           updateSelectInput(session,"planningBrushingAdjustTo",selected=sel[2])
      #         }else{
      #           updateSelectInput(session,"planningBrushingAdjustTo",selected=sel[1])
      #         }
      #       }
      #     }
      #   })
      #   isolate(session$sendCustomMessage(type = "resetValue", message = "keyDown"))
      # }else if(inp == 68){
      #   updatePrettyCheckbox(session, "planningRadioButtonDelete", value=T)
      }
      #Ctrl key for displaying brush rectangle
      else if(inp == 17){ 
        if(lock) return()
        plotBrushingPRec(T)
        lock <<- T
      }else if(inp == 78){ 
        # updatePrettySwitch(session, "planningSwitchOnlyNA",value=!input$planningSwitchOnlyNA)
        # isolate(session$sendCustomMessage(type = "resetValue", message = "keyDown"))
      }
    }

  })
  

  #Key binding for release
  observe({
    if(!is.null(input$keyUp) && input$keyUp == 17){
      isolate(session$sendCustomMessage(type = "resetValue", message = "keyDown"))
      plotBrushingPRec(F)
      isolate(session$sendCustomMessage(type = "resetValue", message = "keyUp"))
    }
    if(lock)lock <<-F
  })

  
  
  #Radio buttons (checkboxes) for add, adjust and delete
  observeEvent(input$planningRadioButtonAdd, priority=11,{
    if(input$planningRadioButtonAdd){
      shinyjs::disable("planningBrushingAdjustTo")
      updatePrettyCheckbox(session, "planningRadioButtonAdjust", value=F)
      updatePrettyCheckbox(session, "planningRadioButtonDelete", value=F)
    }else{
      if(!(input$planningRadioButtonAdjust | input$planningRadioButtonDelete)){
        updatePrettyCheckbox(session, "planningRadioButtonAdd", value=T)
      }
    }
  })
  observeEvent(input$planningRadioButtonAdjust, priority=11,{
    if(input$planningRadioButtonAdjust){
      shinyjs::enable("planningBrushingAdjustTo")
      updatePrettyCheckbox(session, "planningRadioButtonAdd", value=F)
      updatePrettyCheckbox(session, "planningRadioButtonDelete", value=F)
    }else{
      if(!(input$planningRadioButtonAdd | input$planningRadioButtonDelete)){
        updatePrettyCheckbox(session, "planningRadioButtonAdjust", value=T)
      }
    }
  })  
  observeEvent(input$planningRadioButtonDelete, priority=11,{
    if(input$planningRadioButtonDelete){
      shinyjs::disable("planningBrushingAdjustTo")
      updatePrettyCheckbox(session, "planningRadioButtonAdjust", value=F)
      updatePrettyCheckbox(session, "planningRadioButtonAdd", value=F)
    }else{
      if(!(input$planningRadioButtonAdjust | input$planningRadioButtonAdd)){
        updatePrettyCheckbox(session, "planningRadioButtonDelete", value=T)
      }
    }
  })

  
  
  #Helper inside init_creatingData_function
  setUsedDS <- function(data){
    #Parse colums to correct type
    prop <- datasetProperties()
    for(n in colnames(prop)){
      if(prop[5,n] != "Categorical"){
        data[,n] <- as.numeric(data[,n])
      }
    }
    usedDS(data)
  }
  
}





# Table of input properties. For each variable a column with 
# a remove button
# Min max limits 
# Type (Conti, Discrete, Categ)
# Fill / Replace button to create data easily
# Status of rows filled for this variable (e.g. 0/50)
existingOfAddCol <<- F
propertiesOverview <- function(session, input, output){

  createRowHeaders <<- T
  
  observe({
    dat <- datasetProperties()
    isolate(numberCol <- max(0,length(colnames(usedDS()))-1)) #-1 due to column "hiddenIndex"
    if(is.null(dat) ){ #length(dat)==0 is true for empty datasets (no columns)
      if(is.null(usedDS())){
        #remove Uis
        lapply(1:7, function(i){
          removeUI(paste0("#planningPropInputAddInPropColCell",i), immediate=T)
        })
        lapply(1:max(oldNumberOfCol,1), function(i){
          lapply(1:7, function(j){
            removeUI(paste0("#planningPropertiesOverviewCell",j,i), immediate=T)
          })
        })
        oldNumberOfCol <<- numberCol
      }
      return()
    } 
    
      if(createRowHeaders){
        # row names 
        insertUI(selector="#planningPropertiesOverviewRow1", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell10", tags$label("")))
        insertUI(selector="#planningPropertiesOverviewRow2", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell20", tags$label("Remove", style="padding-top:8px;")))
        insertUI(selector="#planningPropertiesOverviewRow3", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell30", tags$label("Precision", style="padding-top:8px;")))
        insertUI(selector="#planningPropertiesOverviewRow4", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell40", tags$label("Min / Max", style="padding-top:8px;")))
        insertUI(selector="#planningPropertiesOverviewRow5", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell50", tags$label("Type", style="padding-top:8px;")))
        insertUI(selector="#planningPropertiesOverviewRow6", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell60", tags$label("Fill / Replace", style="padding-top:8px;")))
        insertUI(selector="#planningPropertiesOverviewRow7", where="afterBegin", 
                 planningPropertiesOverviewCell("planningPropertiesOverviewCell70", tags$label("Completeness", style="padding-top:8px;")))
        createRowHeaders <<-F
      }

    
    
    if(numberCol > oldNumberOfCol){

      #remove addCol
      lapply(1:7, function(i){
        removeUI(paste0("#planningPropInputAddInPropColCell",i), immediate=T)
      })
      
      lapply((oldNumberOfCol+1):numberCol, function(i){
        removeButtonID <- paste0("planningPropInputRemoveInPropCol",i) 
        columnNameID <- paste0("planningPropInputTextInputChangingName",i) 
        precisionID <- paste0("planningPropInputTextInputPrecision",i)
        minID <- paste0("planningPropInputNumericInputMin",i) 
        maxID <- paste0("planningPropInputNumericInputMax",i) 
        selectType <- paste0("planningPropInputSelectInputType",i) 
        fillBtn <- paste0("planningPropInputFillBtn",i)
        completeness <- paste0("planningPropInputCompleteness",i)

        # editable title 
        insertUI(selector="#planningPropertiesOverviewRow1", where="beforeEnd", 
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell1",i),
                   tags$div(textAreaInput(columnNameID, label = NULL, value=dat[1,i], width="100%", resize="vertical", height="40px"),style="margin-bottom:-15px; font-weight:bold;"))
        )
        
        insertUI(selector="#planningPropertiesOverviewRow2",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell2",i),
                   tags$div(actionButton(removeButtonID, label = "Remove", width="100%")))
        )

        insertUI(selector="#planningPropertiesOverviewRow3",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell3",i),
                                                tags$div(style="margin-bottom:-15px;",
                                                         sliderTextInput(precisionID, label = NULL,choices='^'(10,c(3:-10)),selected=dat[2,i], width="100%")))
        )
        
        insertUI(selector="#planningPropertiesOverviewRow4",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell4",i),
                   tags$div(
                     textInputHeight(minID,dat[3,i], margin=c(0,0,2,0)),
                     textInputHeight(maxID,dat[4,i])))
        )
        
        insertUI(selector="#planningPropertiesOverviewRow5",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell5",i),
                   tags$div(style="margin-bottom:-15px;",
                            selectInput(selectType, label = NULL, choices=c("Continuous","Discrete","Categorical"), selected=dat[5,i], width="100%")))
        )
        
        insertUI(selector="#planningPropertiesOverviewRow6",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell6",i),
                   tags$div(actionButton(fillBtn, label = "Fill / Replace", width="100%")))
        )
        
        insertUI(selector="#planningPropertiesOverviewRow7",where="beforeEnd",
                 planningPropertiesOverviewCell(paste0("planningPropertiesOverviewCell7",i),
                   tags$div(id=completeness, dat[6,i], style="text-align:center;padding-top:8px;"))
        )
        
        shinyjs::show("planningPropertiesOverview")
        shinyjs::show("planningPropertiesOverviewGeneral")
      })
      
      #add addCol
      insertUI(selector="#planningPropertiesOverviewRow1", where="beforeEnd",
               planningPropertiesOverviewCell(id="planningPropInputAddInPropColCell1",
                 tags$div(actionButton("planningPropInputAddInPropCol", label = "New column", width="100%")))
               )
      lapply(2:7, function(i){
        insertUI(paste0("#planningPropertiesOverviewRow",i), where="beforeEnd",
                 planningPropertiesOverviewCell(id=paste0("planningPropInputAddInPropColCell",i),tags$div()))
      })

      #add observers
      addObserversToPropColumn(session, input, output, (oldNumberOfCol+1):numberCol)
      
    }else if(numberCol < oldNumberOfCol){
      
      #remove
      lapply((numberCol+1):oldNumberOfCol, function(i){
        lapply(1:7, function(j){
          removeUI(paste0("#planningPropertiesOverviewCell",j,i), immediate=T)
        })
      })
    }else{

      if(!existingOfAddCol){
        #add addCol
        insertUI(selector="#planningPropertiesOverviewRow1", where="beforeEnd",
                 planningPropertiesOverviewCell(id="planningPropInputAddInPropColCell1",
                                                tags$div(actionButton("planningPropInputAddInPropCol", label = "New column", width="100%")))
        )
        lapply(2:7, function(i){
          insertUI(paste0("#planningPropertiesOverviewRow",i), where="beforeEnd",
                   planningPropertiesOverviewCell(id=paste0("planningPropInputAddInPropColCell",i),tags$div()))
        })
      }

      shinyjs::show("planningPropertiesOverview")
      shinyjs::show("planningPropertiesOverviewGeneral")
      
      #add observers
      addObserversToPropColumn(session, input, output, 0)
    }
    
    #update 
    if(numberCol >0){
      lapply(1:numberCol, function(i){
        columnNameID <- paste0("planningPropInputTextInputChangingName",i) 
        precisionID <- paste0("planningPropInputTextInputPrecision",i)
        minID <- paste0("planningPropInputNumericInputMin",i) 
        maxID <- paste0("planningPropInputNumericInputMax",i) 
        selectType <- paste0("planningPropInputSelectInputType",i) 
        completeness <- paste0("planningPropInputCompleteness",i)
        
        #update col name
        updateTextAreaInput(session,columnNameID,value=dat[1,i])
        
        #update precision 
        if(!is.null(input[[precisionID]]) && dat[2,i] != input[[precisionID]]){
          updateSliderTextInput(session, precisionID, selected=as.numeric(dat[2,i]))
        } 
        
        #update min max
        updateTextInput(session, minID, value=dat[3,i])
        updateTextInput(session, maxID, value=dat[4,i])

        #update type
        updateSelectInput(session,selectType,selected=dat[5,i])
        
        #update completeness
        shinyjs::html(completeness, dat[6,i])
      })
    }
      
    oldNumberOfCol <<- numberCol
  })
    
}

planningPropertiesOverviewCell <- function(id, content){
  tags$div(id=id,style="min-width:130px !important; max-width:130px !important; float:left; margin:5px;",
           content)
}

addObserversToPropColumn <- function(session, input, output, numbers){
  
  existingOfAddCol <<- T
  selCol <<- reactiveVal(NULL)
  sapply(numbers, function(i){
    
    #Rename
    if(!is.null(propTableObservers[[paste0("planningPropInputTextInputChangingName",i)]])) propTableObservers[[paste0("planningPropInputTextInputChangingName",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputTextInputChangingName",i)]] <<- observeEvent(input[[paste0("planningPropInputTextInputChangingName",i)]], ignoreInit=T,{
      
      df <- datasetProperties()
      cnames <- colnames(df)
      new_name <- input[[paste0("planningPropInputTextInputChangingName",i)]]
      
      if(grepl(" ", new_name)){
        showNotification("Please use underscore \"_\" instead of blanks.",type="error")
        return()
      }
      
      if(!(new_name %in% cnames) && new_name != "" && !new_name %in% c("hiddenIndex","hiddenSize")){
          tmp <- usedDS()
          cnamesDS <- colnames(tmp)
          cnamesDS[cnamesDS==df[1,i]] <- new_name
          colnames(tmp) <- cnamesDS #c(cnames,"hiddenIndex")
          usedDS(tmp)
          cnames[i] <- new_name
      }
    })
    
    #Remove
    if(!is.null(propTableObservers[[paste0("planningPropInputRemoveInPropCol",i)]])) propTableObservers[[paste0("planningPropInputRemoveInPropCol",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputRemoveInPropCol",i)]] <<- observeEvent(input[[paste0("planningPropInputRemoveInPropCol",i)]], ignoreInit=T, {
      tmp <- usedDS()
      colname <- datasetProperties()[1,i]
      tmp <- tmp[,!colnames(tmp) %in% colname ,drop=F]
      usedDS(tmp)
      datasetProperties(datasetProperties()[-i])
    })
    
    #Precision
    if(!is.null(propTableObservers[[paste0("planningPropInputTextInputPrecision",i)]])) propTableObservers[[paste0("planningPropInputTextInputPrecision",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputTextInputPrecision",i)]] <<- observeEvent(input[[paste0("planningPropInputTextInputPrecision",i)]], ignoreInit=T, {
      datProp <- datasetProperties()
      datProp[2,i] <- input[[paste0("planningPropInputTextInputPrecision",i)]]
      datasetProperties(datProp)
    })
    
    #Min / Max
    if(!is.null(propTableObservers[[paste0("planningPropInputNumericInputMin",i)]])) propTableObservers[[paste0("planningPropInputNumericInputMin",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputNumericInputMin",i)]] <<- observeEvent(input[[paste0("planningPropInputNumericInputMin",i)]], {
      #check in delay
      shinyjs::delay(1000, {
        if(grepl(",",input[[paste0("planningPropInputNumericInputMin",i)]]) && !is.na(as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMin",i)]],",",".")))){
          updateTextInput(session, paste0("planningPropInputNumericInputMin",i), value=as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMin",i)]],",",".")))
        }else if(is.na(as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMin",i)]],",","."))) && input[[paste0("planningPropInputNumericInputMin",i)]] != "-Inf"){
          updateTextInput(session, paste0("planningPropInputNumericInputMin",i), value="-Inf")
        }
      })
      adjustMinMaxLimitsForBrushing(input[[paste0("planningPropInputTextInputChangingName",i)]], "min")
      datProp <- datasetProperties()
      datProp[3,i] <- input[[paste0("planningPropInputNumericInputMin",i)]]
      datasetProperties(datProp)
    })
    if(!is.null(propTableObservers[[paste0("planningPropInputNumericInputMax",i)]])) propTableObservers[[paste0("planningPropInputNumericInputMax",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputNumericInputMax",i)]] <<- observeEvent(input[[paste0("planningPropInputNumericInputMax",i)]], {
      #check in delay
      shinyjs::delay(1000, {
        if(grepl(",",input[[paste0("planningPropInputNumericInputMax",i)]]) && !is.na(as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMax",i)]],",",".")))){
          updateTextInput(session, paste0("planningPropInputNumericInputMax",i), value=as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMin",i)]],",",".")))
        }else if(is.na(as.numeric(str_replace_all(input[[paste0("planningPropInputNumericInputMax",i)]],",","."))) && input[[paste0("planningPropInputNumericInputMax",i)]] != "Inf"){
          updateTextInput(session, paste0("planningPropInputNumericInputMax",i), value="Inf")
        }
      })
      adjustMinMaxLimitsForBrushing(input[[paste0("planningPropInputTextInputChangingName",i)]], "max")
      datProp <- datasetProperties()
      datProp[4,i] <- input[[paste0("planningPropInputNumericInputMax",i)]]
      datasetProperties(datProp)    
    })
    # Change also min and max limits of the min/max ui elements for brushing
    adjustMinMaxLimitsForBrushing <- function(varName, who=c("min","max")){
      if(equal(input$planningBrushingX, varName)){
        if("min" %in% who) updateTextInput(session, "planningBrushingMinX", value=input[[paste0("planningPropInputNumericInputMin",i)]])
        if("max" %in% who) updateTextInput(session, "planningBrushingMaxX", value=input[[paste0("planningPropInputNumericInputMax",i)]])
      }
      if(equal(input$planningBrushingY, varName)){
        if("min" %in% who) updateTextInput(session, "planningBrushingMinY", value=input[[paste0("planningPropInputNumericInputMin",i)]])
        if("max" %in% who) updateTextInput(session, "planningBrushingMaxY", value=input[[paste0("planningPropInputNumericInputMax",i)]])
      }
    }
    
    #Type
    if(!is.null(propTableObservers[[paste0("planningPropInputSelectInputType",i)]])) propTableObservers[[paste0("planningPropInputSelectInputType",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputSelectInputType",i)]] <<- observeEvent(input[[paste0("planningPropInputSelectInputType",i)]], {
      
    })
    
    #Fill and replace
    if(!is.null(propTableObservers[[paste0("planningPropInputFillBtn",i)]])) propTableObservers[[paste0("planningPropInputFillBtn",i)]]$destroy()
    propTableObservers[[paste0("planningPropInputFillBtn",i)]] <<- observeEvent(input[[paste0("planningPropInputFillBtn",i)]], ignoreInit=T,{
      showModal(
        modalDialog(
          size="l",
          footer = fluidRow(
            column(2,tags$div(
              style="width:max-content;",
              tags$div(
                modalButton("Cancel"),
                style="float:left;width:max-content;"),
              
              tags$div(
                style="width:max-content;float:left;padding-top:9px;margin-left:5px;",
                HTML("<b>&larr;</b> This will discard all submits."))
              
              )
            ),
            column(8,tags$div()),
            column(2,actionButton("planningFillReplaceData","Finish",class="btn btn-default action-button btn-primary shiny-bound-input"))
          ),
          
          fluidRow(
            column(3, style="padding:0px;",
                   tags$div(
                     class = "backgroundColor",
                     style="padding:10px;margin:5px;border-radius:4px;",
                     tags$label("Rows"),
                     textAreaInput("planningFillColumnRows", label=NULL, placeholder="1:10,21:30", resize="vertical"),
                     tags$div(class="wordWrappedInHorizLine", " or "),
                     tags$div(selectizeInput("planningFillColumnRowsDepend","Depending on",c("(Column)"="")),style="margin-bottom:-15px;"),
                     selectInput("planningFillColumnRowsDependValue","with value of ",c(),multiple=T),
                     tags$div(
                       class= "borderColor-regular",
                       style="border-top:1px solid; border-radius:4px;",
                       disabled(textAreaInput("planningFillColumnRowsFilled", label="Submitted rows", resize="vertica"))
                     ))
                   ),

            column(3, style="padding:0px;",
                   tags$div(
                     class = "backgroundColor",
                     style=";padding:10px;margin:5px;border-radius:4px;",
                     tags$label("Type"),
                     selectizeInput("planningFillColumnType", label=NULL,c("Distribution"="","Distribution","Replicates")),
                     uiOutput("planningFillColumnTypeUI"))
                   ),
            column(6, style="padding:0px;",
                   tags$div(
                     class = "backgroundColor",
                     style="padding:0px;margin:5px;border-radius:4px;",
                            uiOutput("planningFillColumnPlot"))
                   )
          ),
          fluidRow(
            class = "backgroundColor",
            style="margin-bottom:-20px; padding-top:5px;padding-bottom:5px;",
            column(offset=5,width=2, actionButton("planningFillColumnSubmit", "Submit", width="100%", 
                                                  class="btn btn-default action-button btn-primary shiny-bound-input")))
        )
      )
      
      #Remove old plot
      #Selected columns
      # isolate(selCol <- datasetProperties()[1,i])
      
      #set selected val
      selCol(datasetProperties()[1,i])
      
      output$planningFillColumnTypeUI <- renderUI(getUIOfVariableType(input$planningFillColumnType))
      output$planningFillColumnTypeUISub <- renderUI(getUIOfContiunousType(input,selCol()))
      addUIOfDistributionTypeObserver(session, input, output, selCol())
      warning("Auslagern...")
      addUIOFReplicatesTypeObserver(session, input, output)
      output$planningFillColumnPlot <- renderUI("")
      
    })
    
    
    
  })
  
  #Submit button
  observeEvent(input$planningFillColumnSubmit, ignoreInit=T, {
    
    selectedRows <- convertIndexStringToNumeric(input$planningFillColumnRows)
    
    #Distribution or replicates?
    newData <- NULL
    if(input$planningFillColumnType == "Distribution"){
      newData <- generatedDataForSubmitting()
    }else if(input$planningFillColumnType == "Replicates"){
      newData <- replicateData()
    }else{
      warning("Nothing here... should be?")
    }
    
    #Verify if generated data is empty, selectedRows is empty or both are not of equal length
    if(is.null(newData)){
      showNotification("Generate data before.", type="warning")
      return()
    }else if(is.null(selectedRows)){
      showNotification("Select rows for assignment before.", type="warning")
      return()
    }else if(length(newData) != length(selectedRows)){
      showNotification("Number of rows and number of generated points are unequal.", type="warning")
      return()
    }
    
    #update data of submittedData
    subData <- submittedData()
    subData <- subData[!subData$row %in% selectedRows,]
    subData <- rbind(subData,data.frame(row=selectedRows, value=newData,stringsAsFactors=F))
    submittedData(subData)
    
    #Update submitted rows
    submittedRows <- convertIndexStringToNumeric(input$planningFillColumnRowsFilled)
    index_string <- convertIndexNumericToString(c(selectedRows,submittedRows))
    updateTextAreaInput(session,"planningFillColumnRowsFilled",value=index_string)
  })
  
  #Finish button 
  if(!is.null(propTableObservers$planningFillReplaceData)) propTableObservers$planningFillReplaceData$destroy()
  propTableObservers$planningFillReplaceData <<- observeEvent(input$planningFillReplaceData, ignoreInit=T, {
    selCol <- selCol()
    u <- usedDS()
    sub <- submittedData()
    u[sub$row,selCol()] <- sub$value
    usedDS(u)
    removeModal()
    session$sendCustomMessage(type = "resetValue", message = "planningFillColumnType")
  })
  
  #Add button
  if(!is.null(propTableObservers$planningPropInputAddInPropCol)) propTableObservers$planningPropInputAddInPropCol$destroy()
  propTableObservers$planningPropInputAddInPropCol <<- observeEvent(input$planningPropInputAddInPropCol, ignoreInit=T, {
    df <- datasetProperties()
    cnames <- colnames(df)
    new_name <- ""
    i <- 1
    while(i > 0){
      if(!paste0("New_var",i) %in% cnames){
        new_name <- paste0("New_var",i)
        i <- 0
      }else{
        i <- i+1
      }
    }
    df <- cbind(df,c(new_name,
                     "0.01",
                     -Inf,
                     Inf,
                     "Continuous",
                     paste0("0 / ",dim(usedDS())[1])))
    colnames(df) <- c(cnames,new_name)
    # datasetProperties(df)
    
    tmp <- usedDS()
    l <- dim(tmp)[1]
    cnames <- colnames(tmp)
    tmp <- cbind(tmp, new_name = rep(NA,l))
    if(l == 0) tmp[1,] <- NA
    colnames(tmp) <- c(cnames,new_name)
    usedDS(tmp)
  })
  
}


getUIOfVariableType <- function(type){
  if(is.null(type))return()
  ret <- ""
  if(type=="Distribution"){
    ret <- tags$div(
      tags$div(style="margin-bottom:-10px;",
        selectizeInput("planningFillColumnTypeDist", "Distribution",choices=list(Normal="",Continuous=planningDistributionEnum("Continuous"),Discrete=planningDistributionEnum("Discrete")))),
      uiOutput("planningFillColumnTypeUISub"),
      actionButton("planningFillColumnTypeDistGenerate", "Generate", width="100%")
    )
  }else if(type=="Replicates"){
    ret <- tags$div(
      tags$div(style="margin-bottom:-10px;",
               textAreaInput("planningFillColumnTypeCate", "Values", resize="vertical", placeholder="1,2,3,... or male,female,..."),
               textAreaInput("planningFillColumnTypeCateOcc", "Occurrence", resize="vertical", placeholder="10 or 4,4,2,..."),
               tags$div(style="text-align:center;",
                        switchInput("planningFillColumnTypeCateSwitch",NULL, value=T, onLabel="Each", offLabel="Whole", onStatus="primary", offStatus="primary", inline=T))), 
      uiOutput("planningFillColumnTypeUISub")
    )
  }else if(type != ""){
    stop(paste0("Unknown type: ", type))
  }
  return(ret)
}

getUIOfContiunousType <- function(input, selCol){
  type <- input$planningFillColumnType 
  dist <- input$planningFillColumnTypeDist
  
  colNames <- colnames(datasetProperties())
  choices <- colNames[!colNames %in% selCol]
  
  ret <- ""
  if(!is.null(type) && type == "Distribution" && !is.null(dist) && dist != ""){
    auxPara <- getAuxParameterOfDistribution(dist)
    tList <- tagList()
    for(i in 1:length(auxPara)){
      tList[[i]] <- tags$div(
        tags$div(class="wordWrappedInHorizLine", paste0("  ", auxPara[[i]]$display_name,"  ")),
        tags$div(style="margin-bottom:-15px;",
                 textAreaInput(paste0("planningFillColumnAuxParameter_",auxPara[[i]]$name),NULL,height='35px',resize="vertical", 
                               placeholder=paste0(auxPara[[i]]$default_val," or ",paste(auxPara[[i]]$tmp_value,collapse=","),",..."))),
        selectizeInput(paste0("planningFillColumnAuxParameterSelect_",auxPara[[i]]$name),NULL,choices=c("or depending on ..." = "",choices))
      )
    }
    ret <- tags$div(
      tList
    )
    return(ret)
  }else{
    return(ret)
  }
}

addUIOfDistributionTypeObserver <- function(session, input, output, selCol){

  #Add column names to "depending on" selectInput
  colNames <- colnames(datasetProperties())
  choices <- colNames[!colNames %in% selCol]
  if(length(choices) >0){
    choices <- c("",choices)
    names(choices) <- c(choices[2],rep("",length(choices)-1))
    updateSelectizeInput(session, "planningFillColumnRowsDepend", choices=choices)
    inp <- input$planningFillColumnRowsDepend
    if(is.null(inp) || inp==""){
      choices <- character(0)
    }else{
      choices <- unique(removeHiddenIndex(filteredDS())[input$planningFillColumnRowsDepend])
    }
  }else{
    updateSelectizeInput(session, "planningFillColumnRowsDepend", choices=c("(Column)"=""))
  }
  
  if(length(choices) != 0){
    updateSelectInput(session, "planningFillColumnRowsDependValue", choices=choices[,1])
  }else{
    updateSelectInput(session, "planningFillColumnRowsDependValue", choices=c())
  }
  
  #Observe changes in "depending on" list
  observeEvent(input$planningFillColumnRowsDepend,{
    colNames <- colnames(datasetProperties())
    if(!input$planningFillColumnRowsDepend %in% colNames){
      updateSelectInput(session, "planningFillColumnRowsDependValue", choices=c())
    }else{
      choices <- unique(removeHiddenIndex(filteredDS())[input$planningFillColumnRowsDepend])
      choices[is.na(choices)] <- "(empty_cell)"
      if(length(choices) != 0){
        updateSelectInput(session, "planningFillColumnRowsDependValue", choices=choices[,1])
      }else{
        updateSelectInput(session, "planningFillColumnRowsDependValue", choices=c())
      }
    }
  })
  
  #Observe changes in value list of "depending on" column
  observeEvent(input$planningFillColumnRowsDependValue, {
    var <- input$planningFillColumnRowsDepend
    values <- input$planningFillColumnRowsDependValue
    values[values=="(empty_cell)"] <- NA
    col <- removeHiddenIndex(filteredDS())[var]
    index <- as.numeric(rownames(col[col[,1] %in% values,,drop=F]))
    
    index_string <- convertIndexNumericToString(index)
    updateTextAreaInput(session, "planningFillColumnRows", value=index_string)
  })
  
  #Observe auxpara selections that depends on the other columns
  observe({
    if(!is.null(input$planningFillColumnTypeDist) && input$planningFillColumnTypeDist != ""){
      auxPara <- getAuxParameterOfDistribution(input$planningFillColumnTypeDist)
      sapply(1:length(auxPara), function(i){
        observe({
          inp <- input[[paste0("planningFillColumnAuxParameterSelect_",auxPara[[i]]$name)]]
          if(is.null(inp) || inp == ""){
            updateTextAreaInput(session,paste0("planningFillColumnAuxParameter_",auxPara[[i]]$name),value="")
          }else{
            index <- input$planningFillColumnRows
            index_num <- convertIndexStringToNumeric(index)
            if(is.null(index_num))return()
            col <- removeHiddenIndex(filteredDS())[index_num,inp]
            updateTextAreaInput(session,paste0("planningFillColumnAuxParameter_",auxPara[[i]]$name),value=paste(col,collapse=","))
          }
        })
      })
    }
  })


  
  #Observe "Generate" button to generate data and also the plot
  observeEvent(input$planningFillColumnTypeDistGenerate, ignoreInit=T, {
    #Slide show
    dist <- input$planningFillColumnTypeDist
    generatedData <- ""
    
    if(is.null(dist) || dist == ""){
      generatedData <- "<p>Please select a distribution.</p>"
    }else{
      isolate(auxPara <- getAuxParameterOfDistribution(dist))
      
      for(i in auxPara){
        isolate(val_string <-input[[paste0("planningFillColumnAuxParameter_",i$name)]])
        valArray <- as.numeric(strsplit(val_string, ",")[[1]])
        if(any(is.na(valArray))){
          generatedData <- paste0(generatedData,"<p>Some values of <b>", i$name,"</b> could not be parsed. Please keep to the given format.</p>")
        }else if(length(valArray)==0){
          generatedData <- paste0(generatedData,"<p>Please provide at least a single value for <b>", i$name,"</b>.</p>")
        }else{
          i$value <- valArray
        }
      }
    }
    
    if(generatedData==""){
      #Row indexes
      isolate(index <- input$planningFillColumnRows)
      index_num <- convertIndexStringToNumeric(index)
      
      #TODO warnings if n is 0
      if(is.null(index_num) || length(index_num) == 0){
        generatedData <- "<p>Please select at least one row.</p>"
      }else{
        generatedData <- generateData(distEnum=dist, auxPara=auxPara, n=length(index_num))
        if(!is.character(generatedData)){
          prec <- as.numeric(datasetProperties()[2,selCol])
          roundedPlotDat <- round(generatedData/prec)*prec
          limitedPlotDat <- roundedPlotDat
          minLim <- as.numeric(datasetProperties()[3,selCol])
          maxLim <- as.numeric(datasetProperties()[4,selCol])
          limitedPlotDat[limitedPlotDat<minLim] <- minLim
          limitedPlotDat[limitedPlotDat>maxLim] <- maxLim
        }
      }
    }
      
    if(is.character(generatedData)){
      generatedDataForSubmitting(NULL)
      output$planningFillColumnPlot <- renderUI({
        return(
          tabsetPanel(
            tabPanel("Warnings",
                     tags$div(
                       style="padding-bottom:5px;", 
                       tags$div(
                         class = "uiChecklist-div-warning uiChecklist-font-warning",
                         style="margin:10px;padding:10px;border-top:solid 1px; border-radius:4px;",
                         HTML(generatedData))))
          )
        )
      })
    }else{
      generatedDataForSubmitting(limitedPlotDat)
      output$planningFillColumnPlot <- renderUI({
        return(
          tabsetPanel(
            #Raw points
            tabPanel("Points",tags$div(renderPlot(plotDistriubtion(limitedPlotDat, typePlot="points", selCol)))),
            #Density of points
            tabPanel("Density",tags$div(renderPlot(plotDistriubtion(limitedPlotDat, typePlot="dens", selCol))))
          )
        )
      })
    }

  })
}

addUIOFReplicatesTypeObserver <- function(session, input, output){

  observe({
    if(!is.null(input$planningFillColumnType) && input$planningFillColumnType == "Replicates"){
      replicateData(NULL)
      isolate(repData <- replicateData())
      values <- input$planningFillColumnTypeCate
      occu <- input$planningFillColumnTypeCateOcc
      repType <- input$planningFillColumnTypeCateSwitch #switchInput #T:Each, F:Whole
      index_num <- convertIndexStringToNumeric(input$planningFillColumnRows)
      warning <- F
      
      if(!(is.null(index_num) || index_num == "" ||
         is.null(values) || values == "" ||
         is.null(occu) || occu == "" )){

        #verify content
        values_val <- strsplit(values, ",")[[1]]
        occu_val <- as.numeric(strsplit(occu,",")[[1]])
        if(any(is.na(values_val))){
          repData <- "<p>Some <b>values</b> could not be parsed. Please keep to the given format.</p>"
        }
        if(any(is.na(occu_val))){
          repData <- paste0(repData,"<p>Some <b>Occurrence</b> could not be parsed. Please keep to the given format.</p>")
        }
        
        #No warnings so far
        if(is.null(repData)){
          l_index <- length(index_num)
          l_values <- length(values_val)
          l_occu <- length(occu_val)
          sum_occu <- sum(occu_val)
          if(l_occu > 1 && l_values > 1){
            if(l_values != l_occu ){
              repData <- "<p>If <b>Values</b> and <b>Occurrence</b> are not single elements, they have to be of equal length.</p>"
              warning <- T
            }else if(sum_occu != l_index){
              repData <- paste0("<p>The sum of <b>Occurrence</b>(", sum_occu,") and the number of selected <b>rows</b>(", l_index,
                                ") have to be equal.</p>")
              warning <- T
            }else{
              if(repType){
                repData <- rep(values_val, times=occu_val)
              }else{
                repData <- "<p>For the option <b>Whole</b> is just a single value for <b>Occurrence</b> allowed.</p>"
                warning <- T
              }
            }
          }else if(l_occu > l_values){ #l_values=1, l_occu > 1 --> not allowed
            repData <- "<p><b>Occurrence</b> should also be a single value.</p>"
            warning <- T
          }else if(l_values > l_occu){ # l_occu=1, l_values > 1
            if(l_values*occu_val == l_index){
              if(repType){
                repData <- rep(values_val, each=occu_val)
              }else{
                repData <- rep(values_val, times=occu_val)
              }
            }else{
              repData <- paste0("<p>The number of <b>rows</b> (",l_index,") is unequal to <b>Occurrence</b>(",occu_val,
                                ") times number elements of <b>Values</b>(",l_values,").</p><p>(",l_index," != ",occu_val*l_values,")</p>")
              warning <- T
            }
          }else{
            if(l_index == sum_occu){
              repData <- rep(values_val, each=sum_occu)
            }else{
              repData <- paste0("<p>The sum of <b>Occurrence</b>(", sum_occu,") and the number of selected <b>rows</b>(", l_index,
                                ") have to be equal.</p>")
              warning <- T
            }
          }
        }
        replicateData(repData)
      }else{
        replicateData(NULL)
      }
      
      if(isolate(!is.null(repData))){
        output$planningFillColumnPlot <- renderUI({
          if(warning){
            tags$div(style="padding:10px;",
                     tags$div(
                       class = "uiChecklist-div-warning uiChecklist-font-warning",
                       style="padding:10px;border-top:solid 1px;border-radius:4px;",
                              HTML(isolate(repData))))
          }else{
            tags$div(
              class = "uiChecklist-div-warning uiChecklist-font-warning",
              style="padding:10px;border-top:solid 1px;border-radius:4px;",
              textAreaInputWithoutContainer(
                "planningFillColumnReplicateResult",NULL,value=isolate(paste0(repData,collapse=" ")),
                style="width=100%;height=393px;resize:vertical;"))
          }
        })      
      }
    }
  })

}


###HELPER

convertIndexNumericToString <- function(index){
  index <- sort(unique(index))
  index_string <- ""
  start_index <- -1
  for(i in 1:length(index)){
    if(start_index == -1){
      start_index <- index[i]
      index_string <- paste0(index_string, start_index)
    } 
    if(i==length(index) || index[i] != (index[i+1]-1)){
      index_string <- paste0(index_string, ":",index[i],",")
      start_index <- -1
    }
  }
  index_string <- substring(index_string, 1,nchar(index_string)-1)
  return(index_string)
}

convertIndexStringToNumeric <- function(index){
  split <- strsplit(index,",")
  split2 <- strsplit(split[[1]],":")
  index_num <- c()
  for(i in split2){
    i <- as.numeric(i)
    if(any(is.na(i))) return(NULL)
    if(length(i)==1){
      index_num <- c(index_num,i[1])
    }else{
      index_num <- c(index_num,i[1]:i[2])
    }
  }
  index_num <- unique(index_num)
  index_num <- index_num[index_num>0]
  return(index_num)
}

#Remove hiddenIndex column
removeHiddenIndex <- function(x){
  x[,!(colnames(x) %in% c("hiddenIndex")), drop=F]
}

#Returns an index list used for showing just a fraction of datapoints
getFracIndex <- function(index, number){
  newIndex <- index[((1:(ceiling(length(index)/2)))*2-1)]
  if(length(newIndex) <= number){
    ret <- newIndex
    l <- length(ret)
    ret2 <- c()
    if(l < number){
      ret2 <- index[(1:(number-l))*2]
    }
    return(c(ret,ret2))
  } else if(number > 0){
    return(getFracIndex(newIndex, number))
  }
}

#Returns the data type of vector x
# Categorical if any of the elements are non numeric
# Discrete if evey element is numeric and discrete
# Continuous otherwise
getVarType <- function(x){
  x <- x[gsub(" ", "",x, fixed=T) != ""]
  suppressWarnings(nonNum <- is.na(as.numeric(x)) & !is.na(x))
  if(all(is.na(x))){
    return("Continuous")
  }else if(any(nonNum)){
    return("Categorical")
  }else if(all(round(as.numeric(x)) == as.numeric(x))){ 
    return("Discrete")
  }else{
    return("Continuous")
  }
}


dimQuasiEqual <- function(a,b){
  return(quasiEqual2(a[[1]],b[[1]]) &&
    quasiEqual2(a[[2]],b[[2]]) &&
    quasiEqual2(a[[3]],b[[3]]) &&
    quasiEqual2(a[[4]],b[[4]]))
}

# Returns the elements of str that are converted to factors within range (2 element vector of from - to)
# e.g. str=c("a","b","c"); range=c(1.8,3.5) -> return(c("b","c"))
# str=c("(Empty)","a","b","c"); range=c(1.8,2.1)
# str=c("a","b","c","(Empty)"); range=c(0.9,2.1)
# strInRange(str,range)
strInRange <- function(str, range, withEmpty=F){
  min <- max(1,ceiling(range[1]))
  max <- min(floor(range[2]),length(str))
  if(min > max) return(NULL)
  lev <- levels(as.factor(str))[min:max]
  if(withEmpty){
    lev[lev=="(Empty)"] <- NA
  }else{
    lev <- lev[!lev%in%"(Empty)"]
  }
  return(lev)
}
# str <- c("a","b","c", "(Empty)")
# points <- c(1)
# strInPoints(str, points)
strInPoints <- function(str, points){
  roundPoints <- round(points)
  roundPoints[roundPoints < 1 | roundPoints > length(str)] <- length(str)+1
  fac <- as.factor(str)
  return(as.character(fac[roundPoints]))
}
