init_upload_function <- function(input, output, session, dataModel){
  
  ns <- NS("uploadPage")
  
  
  uploadedUserFile <- reactiveVal(NULL)
  
  #Assign uploaded file to reactive uploadedUserFile
  observeEvent(input[[ns("userInputData")]], {
    file <- input[[ns("userInputData")]]
    
    list.append(file, 0, "num")
    fileOld <- uploadedUserFile()
    if(!is.empty(fileOld)){
      if(!is.null(file$num)){
        file$num <- fileOld$num+1
      }else{
        file$num <- 0
      }
    }
    uploadedUserFile(file)
  })
  
  observeEvent(input[[ns("userInputData2")]], {
    name <- input[[ns("userInputData2")]]$name
    updateTextInput(session, inputId=ns("userInputData-text"), value=name)
    
    file <- input[[ns("userInputData2")]]
    
    list.append(file, 0, "num")
    fileOld <- uploadedUserFile()
    if(!is.empty(fileOld)){
      if(!is.null(file$num)){
        file$num <- fileOld$num+1
      }else{
        file$num <- 0
      }
    }
    uploadedUserFile(file)
  })
  
  
  #Init raw data interface with an drag upload panel
  output[[ns("uiUserRawData")]] <- renderUI({
    upload_pageDragInput(ns)
  })

  sheetsOld <- reactiveVal(c())
  #input file
  observe({
    file <- uploadedUserFile()
    isolate({
  
      if (is.null(file) || is.null(file$datapath))
        return(NULL)

      if(!any(endsWith(file$datapath,c(".txt",".csv", ".xlsx")))){
        showNotification(ui = "Please upload a csv, txt or xlsx file!", duration = 10, type = "error")
        return()
      }
      
      if(any(endsWith(file$datapath,c(".xls", ".xlsx")))){
        sheets <- excel_sheets(file$datapath)
        sheetsOld(sheets)
        sheets <- as.list(sheets)
        names(sheets) <- sheets
        sheets <- list.insert(sheets, "", 1, names(sheets)[1])
        
        showModal(
          modalDialog(
            tags$div(
              style = "",
              selectInput(ns("chooseSheetOfXlsx"), label="Select sheet", choices = sheets,
                          width="100%")
            ),
            title = "Choose excel sheet",
            footer = tags$div(
              style = "",
              
              tags$div(
                tags$span(
                  style = "float: left;",
                  modalButton("Cancel")),
                actionButton(ns("chooseSheetOfXlsxConfirm"), label="Ok")
              )
            )
          )
        )

      }else{
        #Create new dataModelInputData object
        dMID <- dataModel$getDataModelInputData()
        dMID$setCurrentDataPath(file)
        
        tmpFile <- list(file = paste0(data_user_folder, "/", rlang::hash(file$datapath),"_" ,basename(file$name)),
                        type = "csv", sheet = 1)
        
        file.copy(file$datapath, tmpFile$file)
        dMID$setTmpDataPath(tmpFile)
        
        output[[ns("uiUserRawData")]]  <- renderUI({
          decSep <- dMID$getDecSep()
          cellSep <- dMID$getCellSep()
          upload_page_userData(ns, disable=F, selDecSep = decSep, selCellSep = cellSep)
        })
      }

    })
  })
  
  #Confirm sheet name if uploaded file is a xlsx
  observeEvent(input[[ns("chooseSheetOfXlsxConfirm")]], ignoreNULL = T, {

    if(input[[ns("chooseSheetOfXlsx")]] == ""){
      shinyFeedback::showFeedbackDanger(ns("chooseSheetOfXlsx"),
                                        text="Please choose an excel sheet", 
                                        color=BAYAS_COLORS$`--font-error`, icon=NULL)
    }else{
      removeModal()
    }
    
    if(!input[[ns("chooseSheetOfXlsx")]] %in% sheetsOld()) return()

    file <- uploadedUserFile()
    
    #Create new dataModelInputData object
    dMID <- dataModel$getDataModelInputData()
    dMID$setCurrentDataPath(file)
    
    dMID <- dataModel$getDataModelInputData()
    inpSheet <- input[[ns("chooseSheetOfXlsx")]]
    
    tmpFile <- list(file = paste0(data_user_folder, "/", rlang::hash(file$datapath), "_", basename(file$name)),
                    type = "xlsx", sheet = inpSheet)

    file.copy(file$datapath, tmpFile$file)
    dMID$setTmpDataPath(tmpFile)
    
    output[[ns("uiUserRawData")]]  <- renderUI({
      decSep <- dMID$getDecSep()
      cellSep <- dMID$getCellSep()
      upload_page_userData(ns, disable=F, selDecSep = decSep, selCellSep = cellSep)
    })
  })
  
  
  
  #use of example data
  observeEvent(input$exampleUserData, ignoreInit = T, {
    
    file <- list(name="BAYAS example data", datapath=paste0(data_folder, "/bayasExampleData.csv"),
                 num=0)
    fileOld <- uploadedUserFile()
    if(!is.empty(fileOld)){
      file$num <- fileOld$num+1
    }
    uploadedUserFile(file)
    updateTextInput(session, inputId=ns("userInputData-text"), value="BAYAS example data")
    session$sendCustomMessage("upload_txt", "BAYAS example data")
  })
  

  
  #Show example of long-format data
  observeEvent(input[[ns("showExampleOfLongFormat")]], {
    
    showModal(
      modalDialog(
        upload_page_longFormat(ns),
        title = "Example of 'long-format' data",
        footer = modalButton("Ok"),
        size = "l",
        easyClose = T
      )
    )
    
    exampleExcel <- fread(paste0(data_folder, "/1_fake_wrong_example_evaluation.csv"))
    exampleTable <- fread(paste0(data_folder, "/1_fake_correct_example_evaluation.csv"))
    
    exampleExcel[is.na(exampleExcel)] <- ""
    
    output[[ns("exampleExcelTable")]] <- renderExcel(
      excelTable(
        data=exampleExcel,
        minSpareRows=1, columnResize=F, rowResize=F, 
        comment.char="", colHeaders=rep("",dim(exampleExcel)[2]),
        editable = F,
        autoWidth = T,
        autoFill = T,
        mergeCells = list('A1'=c(4,1), 'A5'=c(4,1))
      )
    )
    
    output[[ns("exampleDataTable")]] <- renderDT(
      datatable(
        data = exampleTable,
        rownames = T,
        selection = "none",
        options = list(searching=F, paging=F, escape=F, info=F,
                       columnDefs = list(list(className = 'dt-right', targets = colnames(exampleTable)))),
        fillContainer = T
      ) %>%
        formatStyle(0:(dim(exampleTable)[2]), lineHeight="70%")
    )
  })
  
  #Change in separator (decimal and cell)
  observeEvent(input[[ns("userInputDecimalSeparator")]], {
    dMID <- dataModel$getDataModelInputData()
    inp <- input[[ns("userInputDecimalSeparator")]]
    if(!is.empty(inp) &&
       inp %in% c("point","comma")){
      dMID$setDecSep(inp)
    }
  })
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("decSep")
    updateBayasGroupedButtons(session, ns("userInputDecimalSeparator"), selected=dMID$getDecSep())
  })
  
  observeEvent(input[[ns("userInputCellSeparator")]], {
    dMID <- dataModel$getDataModelInputData()
    inp <- input[[ns("userInputCellSeparator")]]
    if(!is.empty(inp) &&
       inp %in% c("comma","semicolon","tab","empty")){
      dMID$setCellSep(inp)
    }
  })
  observe({   
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("cellSep")
    updateBayasGroupedButtons(session, ns("userInputCellSeparator"), selected=dMID$getCellSep())
  })
  
  
  #Update of excel table due to changes in decimal/cell separator or new user file
  update_excel_table <- reactive({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("decSep")
    dMID$dependReactiveValue("cellSep")
    dMID$dependReactiveValue("tmpDataPath")
    list(dMID$getDecSep(), dMID$getCellSep(), dMID$getTmpDataPath())
  })
  update_excel_table_d <- update_excel_table %>% debounce(30)
  
  observe({
    update_excel_table_d()
    
    isolate({
      dMID <- dataModel$getDataModelInputData()
      
      decSep <- dMID$getDecSep()
      cellSep <- dMID$getCellSep()
      file <- dMID$getTmpDataPath()
      
      if(is.null(file) || is.null(decSep) || is.null(cellSep)){
        # dnd panel
        output[[ns("uiUserRawData")]] <- renderUI({
          upload_pageDragInput(ns)
        })
        return(NULL)
      }
      
      decSep <- ifelse(decSep=="point",".",",")
      if(cellSep == "comma"){
        cellSep <- ","
      }else if(cellSep == "tab"){
        cellSep <- "\t"
      }else if(cellSep == "semicolon"){
        cellSep <- ";"
      }else{
        cellSep <- " "
      }
      
      data <- NULL
      data <- tryCatch({
        data <- NULL
        if(file$type == "csv"){
          data <- fread(input=file$file, sep=cellSep, dec=decSep, header=F)
          enable(ns("userInputDecimalSeparator"))
          enable(ns("userInputCellSeparator"))
        }else{
          data <- read_excel(file$file, sheet=file$sheet, col_names=F, col_types="text")
          data <- as.data.frame(data)
          disable(ns("userInputDecimalSeparator"))
          disable(ns("userInputCellSeparator"))
        }
        list(data=data, msg=NULL)
      },
      error = function(e){
        errormsg <- "Could not read data, please choose the correct separators."
        list(data=NULL, msg=errormsg)
      },
      warning = function(e){
        errormsg <- "Could not read data, please choose the correct separators."
        list(data=NULL, msg=errormsg)
      })
      
      dMID <- dataModel$getDataModelInputData()
      dMID$setExcelTable(data$data)
      
      if(!is.null(data$data)){
        show(ns("rawInputExcel"))
        hide(ns("rawInputExcelErrors"))
      }else{
        hide(ns("rawInputExcel"))
        show(ns("rawInputExcelErrors"))
        output[[ns("rawInputExcelErrors")]] <- renderUI(
          tags$div(
            style = "width: 100%; text-align: center;",
            data$msg
          )
        )
      }
      

      #change parse to long format button to btn-primary
      addClass(ns("rawDataToTable"),"btn-primary")
      
      #Information of getting to 'long format' (if no long-format is present)
      if(is.null(dMID$getLongFormat())){
        show(ns("tableUserDataInfo"))
        hide(ns("tableInputPropertiesInfo"))
        output[[ns("tableInputProperties")]] <- renderDT(NULL)
        
        dMID$setInputProperties(NULL)
        
        hide(ns("tableUserData"))
      }

    })
  })

  #Observe changes in excel data
  observeEvent(input[[ns("rawInputExcel")]], {
    dMID <- dataModel$getDataModelInputData()
    inp <- input[[ns("rawInputExcel")]]
    selInp <- excel_to_R(inp)
    index_log <- sapply(seq_len(dim(selInp)[2]), function(i){all(selInp[[i]] == "")})
    maxNeg <- max(seq_len(dim(selInp)[2])[!index_log])
    selInp <- selInp[, c(1:maxNeg)]
    
    selInp <- removeLastEmptyRows(selInp)
    
    dMID$setExcelTable(selInp, silent=T)
  })
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("excelTable")
    
    isolate({
      data <- dMID$getExcelTable()
    
      if(!is.null(data)){
        
        if(is.null(dMID$getLongFormat())){
          show(ns("tableUserDataInfo"))
          hide(ns("tableInputPropertiesInfo"))
          output[[ns("tableInputProperties")]] <- renderDT(NULL)
          dMID$setInputProperties(NULL)
        }
          
        
        show(ns("rawInputExcel"))
        hide(ns("rawInputExcelErrors"))
        
        file <- dMID$getTmpDataPath()
        disable <- !is.null(file) && file$type == "xlsx"
        
        output[[ns("uiUserRawData")]]  <- renderUI({
          decSep <- dMID$getDecSep()
          cellSep <- dMID$getCellSep()
          upload_page_userData(ns, disable=disable, selDecSep = decSep, selCellSep = cellSep)
        })

        output[[ns("rawInputExcel")]] <- renderExcel(
          excelTable(
            data=data, tableHeight="100%", showToolbar=TRUE,
            minSpareRows=10, minSpareCols=20,columnDrag=T, rowDrag=T,
            columnResize=T, rowResize=T, loadingSpinner=T,
            comment.char="", colHeaders=rep("",dim(data)[2]),
            getSelectedData = TRUE,
            autoFill =T)
        )
        
      }else{
        hide(ns("tableUserDataInfo"))
        output[[ns("rawInputExcel")]] <- renderExcel(NULL)
      }

    })
  })

  
  #Button ">>"
  toLongFormat <- reactiveVal(0)
  observeEvent(input[[ns("rawDataToTable")]], {
    toLongFormat(toLongFormat()+1)
  })
  observeEvent(input[[ns("rawDataToTable2")]], {
    toLongFormat(toLongFormat()+1)
  })
  observe({
    val <- toLongFormat()
    if(val == 0) return()
 
    isolate({
      inp <- input[[ns("rawInputExcel")]]
      sel <- get_selected_data(inp)
      
      if(is.empty(inp)){
        showNotification("Please upload a file first.", type="error")
        return()
      }else if(is.empty(sel)){
        showNotification("Select an area or columns in the spreadsheet that contain all of your data.", type="error")
        return()
      }
      ret <- verifyData(sel)
      
      if(!ret$valid){
          showNotification(ret$msg, type="error")
          return()
      }

      sel <- ret$data
      
      hide(ns("tableUserDataInfo"))
      show(ns("tableUserData"))
      
      dMID <- dataModel$getDataModelInputData()
      dMID$setLongFormat(sel)
      dMID$setGuessedInputProperties()
    })
  })
  
  
  #Button >>+
  observeEvent(input[[ns("rawDataToTableAdd")]], {
    

    inp <- input[[ns("rawInputExcel")]]
    sel <- get_selected_data(inp)
    
    if(is.empty(inp)){
      showNotification("Please upload a file first.", type="error")
      return()
    }else if(is.empty(sel)){
      showNotification("Select an area or columns in the spreadsheet that contain all of your data.", type="error")
      return()
    }

    
    dmid <- DataModelImportData$new()
    obj <- dmid$importData(sel)
    
    #confirm (disabled?)
    lastStep=F
    confirmButton <- actionButton(ns("confirmImportDataModal"),"Confirm")
    if(lastStep){
      confirmButton <- actionButton(ns("confirmImportDataModal"),"Confirm", class="btn-primary")
    }else{
      confirmButton <- disabled(confirmButton)
    }
    
    showModal(modalDialog(
      size="l",
      easyClose=T,
      footer = tags$div(tags$span(
        actionButton(ns("cancelImportDataModal"), "Cancel"),
        style = "float: left;"),
        confirmButton),
      tags$div(
        uiOutput(ns("importDataModal"))
      )
    ))
    output[[ns("importDataModal")]] <- renderUI({
      upload_import(ns, 1, obj)
    })
    
  })
  
  
  # Button "trash" for removing the datatable
  observeEvent(input[[ns("rawDataToTableRemove")]], {
    show(ns("tableUserDataInfo"))
    hide(ns("tableInputPropertiesInfo"))
    hide(ns("tableUserData"))
    output[[ns("tableUserData")]] <- renderDT(NULL)
    session$sendCustomMessage(type = "unbinding_table_elements", ns("tableInputProperties"))
    output[[ns("tableInputProperties")]] <- renderDT(NULL)
    dMID <- dataModel$getDataModelInputData()
    dMID$setInputProperties(NULL)
  })
  
  

  
  
  #Observe changes in long-format data
  observeEvent(input[[ns("tableUserData")]], {
    dMID <- dataModel$getDataModelInputData()
    dMID$setLongFormat(input[[ns("tableUserData")]], silent=T)
  })
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("longFormat")
    
    isolate({
      
      sel <- dMID$getLongFormat()
      
      if(!is.empty(sel)){
        
        #Show long-format data
        output[[ns("tableUserData")]] <- renderDT(
          datatable(
            data = sel,
            rownames = T,
            selection = "none",
            options = list(searching=F, paging=F, escape=F, info=F,
                           columnDefs = list(list(className = 'dt-right', targets = colnames(sel)))),
            fillContainer = T
          ) %>%
            formatStyle(0:(dim(sel)[2]), lineHeight="70%")
        )

        hide(ns("tableUserDataInfo"))
        show(ns("tableUserData"))
        
      }else{
        output[[ns("tableUserData")]] <- renderDT(NULL)
      }
      
    })
  })
  
  dtInputProperties <- reactiveVal(NULL)
  proxyInputProperties <- DT::dataTableProxy(ns("tableInputProperties"))
  #Observe changes in input table
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("inputProperties")
 
    isolate({
      session$sendCustomMessage(type = "unbinding_table_elements", ns("tableInputProperties"))
      
      newTable <- inputPropertiesTable(dMID, onlyData=T)
      if(!is.null(dtInputProperties())){
        c1 <- colnames(dtInputProperties())
        c2 <- colnames(newTable)
        if(all(c1 %in% c2) && all(c2 %in% c1)){
          DT::replaceData(proxyInputProperties, newTable)
        }else{
          output[[ns("tableInputProperties")]] <- renderDT(
            inputPropertiesTable(dMID)
          )
        }
      }else{
        output[[ns("tableInputProperties")]] <- renderDT(
          inputPropertiesTable(dMID)
        )
      }
      
      if(!is.null(newTable)){
        show(ns("tableInputPropertiesInfo"))
      }else{
        hide(ns("tableInputPropertiesInfo"))
      }

      dtInputProperties(newTable)

      inputPropertiesTableServer(input, output, session, dMID)
    })
  })
 

  
  ## Next Button that guides to next page
  observeEvent(input$btnUploadPageNext, {
    updateNavbarPage(session, "navbar", selected = "Data visualization")
  })  
  
}
 

observers <- list()
inputPropertiesTableServer <- function(input, output, session, dMID) {
  data <- dMID$getLongFormat()
  
  for(o in observers){
    o$destroy()
  }
  observers <<- list()
  
  seq <- seq_len(length(colnames(data)))

  sapply(seq, function(i) {
    
    #Type
    observers[[length(observers)+1]] <<- observeEvent(input[[paste0("row_select_a_", i)]], ignoreInit = T, {
      dMID$changeInputProperties(index = i, type = "type", value=input[[paste0("row_select_a_", i)]])
      
      
      #Enable / disable limits if 'Categorical' is choosen
      if(!is.null(input[[paste0("row_select_a_", i)]]) && 
         input[[paste0("row_select_a_", i)]] == "Categorical"){
        disable(paste0("row_select_b_", i))
        disable(paste0("row_select_c_", i))
        updateSelectInput(session, paste0("row_select_b_", i), selected = "0")
        updateSelectInput(session, paste0("row_select_c_", i), selected = "INF")
      }else{
        enable(paste0("row_select_b_", i))
        enable(paste0("row_select_c_", i))
      }
    })
    
    #Lower limit
    observers[[length(observers)+1]] <<- observeEvent(input[[paste0("row_select_b_", i)]], ignoreInit = T, {
      inp <- input[[paste0("row_select_b_", i)]]
      dMID$changeInputProperties(index = i, type = "lower", value=inp)
      if(isLimitUseless(data[[i]], lowerLimit=inp)){
        #Show warning
        showNotification(ui = HTML(
          paste0("Are you sure that \"", inp, "\" is a good limit for ", colnames(data)[i], "?")), 
          duration = 10, type = "error")
      }
    })
    
    #Upper limit
    observers[[length(observers)+1]] <<- observeEvent(input[[paste0("row_select_c_", i)]], ignoreInit = T, {
      inp <- input[[paste0("row_select_c_", i)]]
      dMID$changeInputProperties(index = i, type = "upper", value=inp)
      if(isLimitUseless(data[[i]], upperLimit=inp)){
        #Show warning
        showNotification(ui = HTML(
          paste0("Are you sure that \"", inp, "\" is a good limit for ", colnames(data)[i], "?")), 
          duration = 10, type = "error")
      }
    })
    
    #Make checkboxes select only one at a time
    observers[[length(observers)+1]] <<- observeEvent(input[[paste0("row_select_d_", i)]], ignoreInit = T, {
      if(input[[paste0("row_select_d_", i)]]){
        for(j in seq_len(length(colnames(data)))){
          if(j != i) updateCheckboxInput(session, paste0("row_select_d_", j), value=F)
        }
      }
      dMID$changeInputProperties(index = i, type = "response", value=input[[paste0("row_select_d_", i)]])
    })
  })

}


#Verify if users limits are useful. E.g. Lower limit > min(data)
isLimitUseless <- function(y, lowerLimit = NULL, upperLimit = NULL){
  if(is.factor(y)) return(F)
  y <- y[str_trim(y)!=""]
  y <- y[!is.na(y)]
  flag <- F
  if(is.null(upperLimit)){
    #Lower Limit: -INF, 0, >0
    if(lowerLimit == "0" & any(y < 0)) flag <- T
    if(lowerLimit == ">0" & any(y <= 0)) flag <- T
  }else{
    #Upper Limit: 1, INF
    if(upperLimit == "1" & any(y > 1)) flag <- T
  }
  return(flag)
}

  
verifyData <- function(data){

  #remove empty columns
  data <- removeEmptyCol(data)

  header <- unlist(data[1,])
  
  ret <- sapply(seq_along(header), function(i) could.numeric(header[i]))
  if(all(ret)){
    header <- paste0("V_", seq_along(header))
  }else if(any(ret)){
    header[ret] <- paste0("V_", header[ret])
    data <- data[-1,]
  }else{
    data <- data[-1,]
  }

  
  #Are there duplicates within header names?
  dup <- header[header != ""]
  if(length(dup) != length(unique(dup))){
    showNotification("Header names must be unique!", type="error")
    return(list(valid=F, msg="Header names must be unique!", data=NULL))
  }
  
  c <- 0
  for(hh_id in seq_along(header)){
    hh <- header[[hh_id]]
    if(hh == ""){
      flag <- T
      while(flag){
        new_header <- paste0("V",c)
        if(!new_header %in% header){
          header[[hh_id]] <- new_header
          flag <- F
        }
        c <- c+1
      }
    }
  }

  names(data) <- header
  
  rownames(data) <- seq_len(dim(data)[1])
  
  data <- data[rowSums(is.null(data) | is.na(data) | data == "") != ncol(data), ,F]
  
  return(list(valid=T, msg="", data=data))
}

removeLastEmptyRows <- function(data){
  data[data == ""] <- NA
  if(is.null(dim(data)) || dim(data)[2] == 1){
    index_log <- is.na(data)
    maxNeg <- max(seq_along(data)[!index_log])
    data <- data[c(1:maxNeg)]
  }else{
    index_log <- rowSums(is.na(data)) == ncol(data)
    maxNeg <- max(seq_len(dim(data)[1])[!index_log])
    data <- data[c(1:maxNeg),]
  }
  return(data)
}

removeEmptyCol <- function(data){
  data[, !sapply(data, is_empty_column)]
}
is_empty_column <- function(col) {
  all(is.na(col) | col == "")
}
