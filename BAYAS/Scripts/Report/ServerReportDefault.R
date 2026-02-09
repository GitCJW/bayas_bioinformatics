init_reportDefault_function <- function(input, output, session, dataModel, 
                                        mCDList, global_reportProgressModel){

  ns <- NS("reportTool")
  nsE <- NS("reportToolElements")
  tNum <- reportTypeEnum()
  
  global.reportProgressObs <<- list()
  

  #Single selected item from the sortable list at the top
  global.selectedSortableElement <- reactiveVal(NULL)
  
  #Bound elements. Named list, with names equal id of binding element, value of bound element id and binding direction
  global.bindingElements <- reactiveVal(c())
  
  changeInBlanks <- reactiveVal(T)

  
  #1: toggle if any button is clicked
  #2: id of clicked button
  clickedSortableElement <- reactiveVal(list(T,-1))
  
  #So that the pdf will be refreshed (for the same pdf file)
  togglePDF <- reactiveVal(T)
  
  #Stores the created pdfs to save calcu time
  #list of lists(id, reported_elements, fontSize)
  storedPDFs <- reactiveVal(list())
  pdfCount <- reactiveVal(0)
  

  observe({
    global_reportProgressModel$dependReactiveValue("items")
    global_reportProgressModel$dependReactiveValue("recommendedItems")
    
    isolate({
      choicesDM <- list("Evaluation: Data name"="")
      choicesFit <- list("Evaluation: Fit"="")
      
      items <- global_reportProgressModel$getItems()
      for(item in items){
        if(item$getModuleType() == "planning") next
        dMid <- item$getDataModel_id()
        pIDMname <- item$getpDIM_name()
        
        if(!is.null(dMid) && dMid != "")
          choicesDM <- list.append(choicesDM, dMid, basename(dMid), extendBySameName=T)
        if(!is.null(pIDMname) && pIDMname != "")
          choicesFit <- list.append(choicesFit, pIDMname, pIDMname)
      }
      items <- global_reportProgressModel$getItems(recommended=T)
      for(item in items){
        if(item$getModuleType() == "planning") next
        dMid <- item$getDataModel_id()
        pIDMname <- item$getpDIM_name()
        
        if(!is.null(dMid) && dMid != "")
          choicesDM <- list.append(choicesDM, dMid, basename(dMid),  extendBySameName=T)
        if(!is.null(pIDMname) && pIDMname != "")
          choicesFit <- list.append(choicesFit, pIDMname, pIDMname)
      }
      filteredData <- input[[ns("filterByData")]]
      updateSelectizeInput(session=session, ns("filterByData"), choices=choicesDM,
                           selected=filteredData)
      
      filteredFit <- input[[ns("filterByFits")]]
      updateSelectizeInput(session=session, ns("filterByFits"), choices=choicesFit,
                           selected=filteredFit)
      
    })
  })

  # Observes changes in the filtering (input data and fits)
  observe({
    filteredData <- input[[ns("filterByData")]]
    filteredFits <- input[[ns("filterByFits")]]
    filteredModule <- input[[ns("filterByBAYASModule")]]
    
    isolate({
      output[[ns("reportElements")]] <- renderUI({
        report_elements(dataModel=dataModel,
                        rrModel=global_reportProgressModel, 
                        filterByModule=filteredModule,
                        filterByData=filteredData,
                        filterByFits=filteredFits)})
      
      #Recommended items only for selected fits
      output[[ns("reportRecommendedElements")]] <- renderUI(
        report_elements(dataModel=dataModel,
                        rrModel=global_reportProgressModel, 
                        recommended=T,
                        filterByModule=filteredModule,
                        filterByData=filteredData,
                        filterByFits=filteredFits))
    })
  })
  
  
  triggerRPM <- reactive({
    model <- global_reportProgressModel
    
    model$dependReactiveValue("nextId")
    model$dependReactiveValue("items")
    model$dependReactiveValue("recommendedItems")
    model$dependReactiveValue("updateProgressPanel")
    model$dependReactiveValue("changeInItems")
  })
  triggerRPM_d <- debounce(triggerRPM, 100)
  
  #Observes changes in the report progress model
  observe({
    model <- global_reportProgressModel
    
    triggerRPM_d()

    isolate({
      filteredModule <- input[[ns("filterByBAYASModule")]]
      filteredData <- input[[ns("filterByData")]]
      filteredFits <- input[[ns("filterByFits")]]
    })
    output[[ns("reportElements")]] <- renderUI(
      report_elements(dataModel=dataModel,
                      rrModel=model, 
                      recommended=F,
                      filterByModule=filteredModule,
                      filterByData=filteredData,
                      filterByFits=filteredFits))
    
    output[[ns("reportRecommendedElements")]] <- renderUI(
      report_elements(dataModel=dataModel,
                      rrModel=model, 
                      recommended=T,
                      filterByModule=filteredModule,
                      filterByData=filteredData,
                      filterByFits=filteredFits))
    
    
    sapply(1:2, function(ls){
      items <- model$getItems(recommended=(ls==2))
      if(length(items) > 0){
        
        sapply(1:length(items), function(i){
        
          id <- items[[i]]$getId()
          id_n <- nsE(paste0("progressElementClick_",id))
          if(!is.null(global.reportProgressObs[[id_n]])){
            global.reportProgressObs[[id_n]]$destroy()
          }
          global.reportProgressObs[[id_n]] <<- observeEvent(input[[id_n]], ignoreInit=T, {
            items[[i]]$toggleClicked()
            clicked <- items[[i]]$getClicked()
            
            #If an element is selected from one list
            #select also the same element, if exists, in the other list
            mappingId <- model$mapToOtherList(items[[i]])
            if(!is.null(mappingId)){
              mapItem <- model$getItem(mappingId)
              if(is.null(mapItem)){
                showNotification("Internal error", type="error")
              }else{
                mapItem$setClicked(clicked)
                if(clicked){
                  shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                                    class="progress-element-active")
                  shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                                       class="progress-element-inactive")
                }else{
                  shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                                       class="progress-element-active")
                  shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                                    class="progress-element-inactive")
                }
              }
            }
            
            if(clicked){
              shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                                class="progress-element-active")
              shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                                   class="progress-element-inactive")
            }else{
              shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                                   class="progress-element-active")
              shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                                class="progress-element-inactive")
            }
          })
          
          id_r <- nsE(paste0("progressElementRemove_",id))
          if(!is.null(global.reportProgressObs[[id_r]])){
            global.reportProgressObs[[id_r]]$destroy()
          }
          global.reportProgressObs[[id_r]] <<- observeEvent(input[[id_r]], ignoreInit=T, {
            removeItem(id)
          })

        })
      }
    })
    
  })
  
  #(De)select all reported items
  observeEvent(input[[ns("selectAllReportedItems")]], {
    model <- global_reportProgressModel
    items <- getFilteredItems()
    if(!is.null(items)){
      for(item in items){
        id <- item$getId()
        shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                          class="progress-element-active")
        shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                             class="progress-element-inactive")
        item$setClicked(T)
        
        #If an element is selected from one list
        #select also the same element, if exists, in the other list
        mappingId <- model$mapToOtherList(item)
        if(!is.null(mappingId)){
          mapItem <- model$getItem(mappingId)
          if(is.null(mapItem)){
            showNotification("Internal error", type="error")
          }else{
            mapItem$setClicked(T)
            shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                              class="progress-element-active")
            shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                                 class="progress-element-inactive")
          }
        }
      }
    }
  })
  observeEvent(input[[ns("deselectAllReportedItems")]], {
    model <- global_reportProgressModel
    # items <- getFilteredItems()
    items <- global_reportProgressModel$getItems()
    if(!is.null(items)){
      for(item in items){
        id <- item$getId()
        shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                             class="progress-element-active")
        shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                          class="progress-element-inactive")
        item$setClicked(F)
        
        #If an element is selected from one list
        #select also the same element, if exists, in the other list
        mappingId <- model$mapToOtherList(item)
        if(!is.null(mappingId)){
          mapItem <- model$getItem(mappingId)
          if(is.null(mapItem)){
            showNotification("Internal error", type="error")
          }else{
            mapItem$setClicked(F)
            shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                              class="progress-element-active")
            shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                                 class="progress-element-inactive")
          }
        }
      }
    }
  })
  
  
  #(De)select all recommended items
  observeEvent(input[[ns("selectAllRecommendedItems")]], {
    model <- global_reportProgressModel
    items <- getFilteredItems(recommended=T)
    if(!is.null(items)){
      for(item in items){
        id <- item$getId()
        shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                          class="progress-element-active")
        shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                             class="progress-element-inactive")
        item$setClicked(T)
        
        #If an element is selected from one list
        #select also the same element, if exists, in the other list
        mappingId <- model$mapToOtherList(item)
        if(!is.null(mappingId)){
          mapItem <- model$getItem(mappingId)
          if(is.null(mapItem)){
            showNotification("Internal error", type="error")
          }else{
            mapItem$setClicked(T)
            shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                              class="progress-element-active")
            shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                                 class="progress-element-inactive")
          }
        }
      }
    }
  })
  observeEvent(input[[ns("deselectAllRecommendedItems")]], {
    model <- global_reportProgressModel
    
    # items <- getFilteredItems(recommended=T)
    items <- global_reportProgressModel$getItems(recommended=T)
    if(!is.null(items)){
      for(item in items){
        id <- item$getId()
        shinyjs::removeClass(id=nsE(paste0("progressElement_",id)), 
                             class="progress-element-active")
        shinyjs::addClass(id=nsE(paste0("progressElement_",id)), 
                          class="progress-element-inactive")
        item$setClicked(F)
        
        #If an element is selected from one list
        #select also the same element, if exists, in the other list
        mappingId <- model$mapToOtherList(item)
        if(!is.null(mappingId)){
          mapItem <- model$getItem(mappingId)
          if(is.null(mapItem)){
            showNotification("Internal error", type="error")
          }else{
            mapItem$setClicked(F)
            shinyjs::removeClass(id=nsE(paste0("progressElement_",mappingId)), 
                              class="progress-element-active")
            shinyjs::addClass(id=nsE(paste0("progressElement_",mappingId)), 
                                 class="progress-element-inactive")
          }
        }
      }
    }
  })

  
  blankCounter <- reactiveVal(0)
  #Add a blank custom text item
  observeEvent(input[[ns("addBlankItem")]], {
    
    
    tEnum <- reportTypeEnum()
    name <- paste0("blankItem",blankCounter())
    
    c <- blankCounter()
    div <- tags$div(
      textInput(ns(paste0("blankItemHeader",c)), label="Header", value="", 
                placeholder="If no header is defined, the text is appended to the previous section"), #Additional information

      
      tags$div(style="width:150px;",
               bayasGroupedButtons(ns(paste0("blankItemTextOrLatex",c)),
                                   c("text","latex"), selected="text",
                                   columns=2, btnStyle="padding:2px; width:100%;")),
      

      textAreaInput(ns(paste0("blankItemTextArea",c)), label=NULL),
      
      labelWithInfo(ns(paste0("blankItemGroupBtnsLabel",c)),"Binding to element in queue",
                    "Binding",paste0("<p>If \"previous\" is selected, the content may append to the previous section. ",
                                     "For \"Next\", the contect will lead the next section. </p>",
                                     "<p>When using the sorting to recommended order, this element will stick to the previous/next element in the queue. ",
                                     "If the order is changed by drag and drop the binding may changed, too.</p>"),
                    placement="left"),
      bayasGroupedButtons(ns(paste0("blankItemGroupBtns",c)),
                          c("Previous","No","Next"), selected="No",
                          outerStyle="margin-bottom:20px;",
                          btnStyle = "width:100%;")
    )
    reportDiv <- reportType(div=list(ui=div))
    reportLatex <- ""
    
    #Add custom formula element to report progress
    itemId <- addItem(moduleType = "blank",
                      # dataModel_id=-1, pDIM_id=-1, pDIM_name = NULL,
                      imgFile = paste0("Images/Report/blank.png"),
                      type=tEnum$blank, object=list(div=reportDiv, latex=reportLatex, model_latex=""), 
                      show=F, singleton=F, global_reportProgressModel=global_reportProgressModel)

    #Update blank item fields whenever new inputs are made
    observe({
      header <- input[[ns(paste0("blankItemHeader",c))]]
      text <- input[[ns(paste0("blankItemTextArea",c))]]
      latex <- input[[ns(paste0("blankItemTextOrLatex",c))]]
      binding <- input[[ns(paste0("blankItemGroupBtns",c))]]
      
      if(is.null(latex)) latex <- "text"
      
      item <- global_reportProgressModel$getItem(itemId)
      
      div <- tags$div(
        textInput(ns(paste0("blankItemHeader",c)), label="Header", value=header, 
                  placeholder="If no header is defined, the text is appended to the previous section."), #Additional information
        

        tags$div(style="width:150px;",
                 bayasGroupedButtons(ns(paste0("blankItemTextOrLatex",c)),
                                     c("text","latex"), selected=latex,
                                     columns=2, btnStyle="padding:2px; width:100%;")),
        
        
        textAreaInput(ns(paste0("blankItemTextArea",c)), 
                      label=NULL,
                      value=text),
        
        labelWithInfo(ns(paste0("blankItemGroupBtnsLabel",c)),"Binding to element in queue",
                         "Binding",paste0("<p>If \"previous\" is selected, the content may append to the previous section. ",
                                          "For \"Next\", the contect will lead the next section. </p>",
                                          "<p>When using the sorting to recommended order, this element will stick to the previous/next element in the queue. ",
                                          "If the order is changed by drag and drop the binding may changed, too.</p>"),
                      placement="left"),
        bayasGroupedButtons(ns(paste0("blankItemGroupBtns",c)),
                            c("Previous","No","Next"), selected=ifelse(!is.null(binding),binding,"No"),
                            outerStyle="margin-bottom:20px;",
                            btnStyle = "width:100%;")
      )
      reportDiv <- reportType(div=list(ui=div))
      reportLatex <- text
      
      if(is.null(binding)){
      }else if(binding=="Previous"){
        binding <- "prev"
      }else if(binding=="Next"){
        binding <- "next"
      }
      
      #As latex or simple text?
      if(!is.null(latex) && latex=="text") text <- paste0("\\begin{spverbatim}\n", text, "\n\\end{spverbatim}")
      
      item$setObject(list(div=reportDiv, latex=reportLatex, model_latex=""))
      item$setBlankData(list(header=header, text=text, binding=binding))
      isolate(changeInBlanks(!changeInBlanks()))
    })
    
    blankCounter(blankCounter()+1)
  })
  
  
  
  #Returns only the filtered (data input, fits) reported items
  getFilteredItems <- function(recommended=F){
    
    filterByModule <- input[[ns("filterByBAYASModule")]]
    filterByData <- input[[ns("filterByData")]]
    filterByFits <- input[[ns("filterByFits")]]
    
    items <- global_reportProgressModel$getItems(recommended=recommended)
    filter_items <- items
    
    if(!is.null(filterByModule) ||
       !is.null(filterByData) ||
       !is.null(filterByFits)){
      filter_items <- list()
      for(item in items){
        if(is.null(filterByModule) || item$getModuleType() %in% filterByModule){
          if(item$getModuleType() == "planning"){
            filter_items <- list.append(filter_items, item)
          }else{
            if((is.null(filterByData) || item$getDataModel_id() %in% filterByData) &&
               (is.null(filterByFits) || is.null(item$getpDIM_name()) || item$getpDIM_name() %in% filterByFits)){
              filter_items <- list.append(filter_items, item)
            }
          }
        }
      }
    }
    items <- filter_items
    return(filter_items)
  }
  
  
  reativeChangesInItem <- reactive({
    reA <- global_reportProgressModel$dependReactiveValue("changeInItems")
    reB <- global_reportProgressModel$dependReactiveValue("items")
    reC <- global_reportProgressModel$dependReactiveValue("recommendedItems")
    list(reA,reB,reC)
  })
  reativeChangesInItem_d <- throttle(reativeChangesInItem, 50)

  
  #Changes in the reported items 
  #including e.g. click events and removing/adding of items
  observe({
    reativeChangesInItem_d()

    isolate({
      items <- global_reportProgressModel$getItems(recommended=F)
      itemsRecommended <- global_reportProgressModel$getItems(recommended=T)
      ids <- global_reportProgressModel$getSelectedList()
      
      allItemsIds <- c()
      for(item in items){
        itemId <- item$getId()
        allItemsIds <- c(allItemsIds, itemId)
        if(!item$getClicked() && itemId %in% ids){
          ids <- ids[-match(itemId,ids)]
        }else if(item$getClicked() && !itemId %in% ids){
          ids <- c(ids, itemId)
        }
      }
      for(item in itemsRecommended){
        itemId <- item$getId()
        itemOtherListId <- global_reportProgressModel$mapToOtherList(item)
        if(is.null(itemOtherListId) || !itemOtherListId %in% allItemsIds){
          allItemsIds <- c(allItemsIds, itemId)
          if(!item$getClicked() && itemId %in% ids){
            ids <- ids[-match(itemId,ids)]
          }else if(item$getClicked() && !itemId %in% ids){
            ids <- c(ids, itemId)
          }
        }
      }
      ids <- ids[ids %in% allItemsIds]

      global_reportProgressModel$setSelectedList(ids)
    })
  })

  #Holds the sortable elements (tags$div) with the unique id as the name
  sortableElements <- reactiveVal(list())
  
  # Observe the reported elements that are finally selected
  observe({
    global_reportProgressModel$dependReactiveValue("selectedList")
    ids <- global_reportProgressModel$getSelectedList()

    activeChecklist <- c()
    inactiveChecklist <- c()
    
    changeInBlanks()
  
    isolate({
      sortableListIds <- c()
      if(length(input[[ns("selecetedReportedElements")]]) !=0){
        sortableListIds <- input[[ns("selecetedReportedElements")]]
      }
      sortableEl <- sortableElements()
      if(length(sortableEl) != length(sortableListIds)) return()
      
      sortableEl <- sortableEl[sortableListIds]
      
      sortableListIds <- as.numeric(sortableListIds)
      
      #Add items
      if(length(ids) > 0 &&
         (length(sortableListIds) == 0 ||
         any(!ids %in% sortableListIds))){
        #add element to sortable list
        for(id in setdiff(ids,sortableListIds)){
          item <- global_reportProgressModel$getItem(id)
          if(is.null(item)) print0("oops, item is null: ",id)
          imgFile <- item$getImgFile()
          sortableEl[[paste0('',id)]] <- report_elements_sortable(ns(paste0("sortableElementButton_",id)), imgFile)
        }
      }
      #Remove items
      if(length(sortableListIds) > 0 &&
         (length(ids) == 0 ||
          any(!sortableListIds %in% ids))){
        for(id in setdiff(sortableListIds,ids)){
          sortableEl[[paste0('',id)]] <- NULL
          sortableListIds <- sortableListIds[!sortableListIds %in% id]
        }
      }
      #Order items?
      if(length(sortableListIds) == length(ids) &&
         !all(sortableListIds==ids)){
        sortableEl <- list()
        for(id in ids){
          imgFile <- global_reportProgressModel$getItem(id)$getImgFile()
          sortableEl[[paste0('',id)]] <- report_elements_sortable(ns(paste0("sortableElementButton_",id)), imgFile)
        }
      }

      sortableElements(sortableEl)
      
      
      #Adapt the bindings of blank items to previous/next (or none)
      bE <- list()
      names <- c()
      for(i in seq_along(sortableListIds)){
        id <- sortableListIds[i]
        element <- global_reportProgressModel$getItem(id)
        bD <- NULL
        if(!is.null(element))
          bD <- element$getBlankData()
        
        if(!is.null(bD)){

          if(is.null(bD$binding)){
          }else if(bD$binding == "prev"){
            names <- c(names, id)
            if(i==1){
              bE <- list.append(bE, c("first","prev"))
            }else{
              bE <- list.append(bE, c(sortableListIds[i-1],"prev"))
            }
          }else if(bD$binding == "next"){
            names <- c(names, id)
            if(i==length(sortableListIds)){
              bE <- list.append(bE, c("last", "next"))
            }else{
              bE <- list.append(bE, c(sortableListIds[i+1], "next"))
            }
          }
        }
      }
      names(bE) <- names
      global.bindingElements(bE)
      
    })
    
    #### report checklist ####
    
    #Each entry is for a single perIerationDataModel
    numberChecklistElements <- 11

    #Items that relates to planning module
    reportedItemsPlanning <- as.list(rep(0, length(mCDList$getMCDList())))
    names(reportedItemsPlanning) <- names(mCDList$getMCDList())
    
    #Items that relates to data model e.g. preplots
    reportedItemsIdsDataModel <- list()
    #Items - pIDM mapping
    reportedItemsIds <- list()
    #DataModel - pIDM mapping
    # dataModelMapping <- list()
    #All pIDM ids of reported elements
    # used_ids <- c()
    # used_id_name_mapping <- list()
    
 
    for(i in ids){
      item <- global_reportProgressModel$getItem(i)
      if(!is.null(item)){
        dataModel_id <- item$getDataModel_id()
        pIDM_id <- item$getpDIM_id()
        pIDM_name <- item$getpDIM_name()
        repItemId <- item$checklistMapping()
        repItemType <- item$getType()

        if(repItemType == "planningExperiment"){
          pName <- item$getPlanningName()
          reportedItemsPlanning[[pName]] <- 1
        }else{
          
          if(repItemId==-1) next
          
          if(is.null(reportedItemsIdsDataModel[[dataModel_id]])){
            reportedItemsIdsDataModel[[dataModel_id]] <- rep(0,numberChecklistElements)
          }
          
          if(pIDM_id == -1){
            reportedItemsIdsDataModel[[dataModel_id]][repItemId] <- 1
          }else{
            if(is.null(reportedItemsIds[[as.character(pIDM_name)]])){
              reportedItemsIds[[as.character(pIDM_name)]] <- rep(0,numberChecklistElements)
            }
            reportedItemsIds[[as.character(pIDM_name)]][repItemId] <- 1
            
            # if(is.null(dataModelMapping[[dataModel_id]])){
            #   dataModelMapping[[dataModel_id]] <- c(pIDM_id)
            # }else{
            #   dataModelMapping[[dataModel_id]] <- c(dataModelMapping[[dataModel_id]], pIDM_id)
            # }
          }
        }

      }
    }


    l.df <- as.data.frame(reportedItemsIds)
    names(l.df) <- names(reportedItemsIds)
    ldm.df <- as.data.frame(reportedItemsIdsDataModel)
    names(ldm.df) <- names(reportedItemsIdsDataModel)
    lp.df <- as.data.frame(reportedItemsPlanning)
    names(lp.df) <- names(reportedItemsPlanning)
    
    activeChecklist <- sapply(1:numberChecklistElements, function(i){
      if(i == 1){
        if(dim(lp.df)[1]==0){
          return(0)
        }else{
          if(all(lp.df[1,] == 1)){
            return(2)
          }else if(all(lp.df[1,] == 0)){
            return(0)
          }else{
            return(1)
          }
        }
      }else{
        if(dim(ldm.df)[1]==0){
          return(0)
        }else{
          if(0 %in% ldm.df[i,] && 1 %in% ldm.df[i,]){
            return(1)
          }else if(0 %in% ldm.df[i,]){
            if(dim(l.df)[1]!=0){
              if(0 %in% l.df[i,] && 1 %in% l.df[i,]){
                return(1)
              }else if(0 %in% l.df[i,]){
                return(0)
              }else{
                return(2)
              }
            }else{
              return(0)
            }
          }else{
            return(2)
          }
        }
      }

    })

    #Which fits (or data sets) are missing in checklist items
    #Some items relates to data sets, the majority to fits
    #returns checklist ids (1-11)
    data_vs_fit <- get_checklist_data_vs_fit()
    sapply(1:numberChecklistElements, function(i){
      ret <- ""
      if(i %in% data_vs_fit$fit){
        ret <- names(l.df)[l.df[i,]==0]
      }else if(i %in% data_vs_fit$data){
        ret <- names(ldm.df)[ldm.df[i,]==0]
        if(!is.empty(ret)) ret <- basename(ret)
      }else if(i %in% data_vs_fit$planning){
        ret <- names(lp.df)[lp.df[i,]==0] 
      }
      
      elementName <- "reportChecklistMissingFitsE"
      j <- i
      if(j==1){
        elementName <- "reportChecklistMissingFitsP"
      }else{
        j <- j -1
      }
      if(!is.empty(ret)) {
        output[[ns(paste0(elementName,j))]] <- renderUI({
          tags$div(
            tags$div(style="text-align:left; margin-top:20px;", 
                     tags$label("Item missing for: ")),
            paste0(ret, collapse=", ")
          )
        })
      }else{
        output[[ns(paste0(elementName,j))]] <- renderUI({
          tags$div()
        })
      }
    })
    
    
    updateChecklistProgress(session, checklistId="checklistReport", 
                            active=activeChecklist,
                            label= c("P1", paste0("E", 1:10)))
    isolate(changeOrderHighlightSelected(!changeOrderHighlightSelected()))
  })
  
  #Click on a checklist button
  lapply(1:11, function(i){
    observeEvent(input[[paste0("checklistReport-",i)]], {
      updateTabsetPanel(session, ns("reportSelectedItemViewTabset"), "guidelines")
      if(i <= 6){
        if(i==1){
          bslib::accordion_panel_set(session=session, id="reportChecklistOverview", values=paste0("reportChecklistOverviewItemP",i))
        }else{
          bslib::accordion_panel_set(session=session, id="reportChecklistOverview", values=paste0("reportChecklistOverviewItemE",i-1))
        }
      }else{
        bslib::accordion_panel_set(session=session, id="reportChecklistOverview2", values=paste0("reportChecklistOverviewItemE",i-1))
      }
    })
  })
  
  
  
  # if the sortable rank list changes, also the order in ReportProgressModel's selectedList
  # have to change.
  observeEvent(input[[ns("selecetedReportedElements")]],{
    sortIds <- input[[ns("selecetedReportedElements")]]
    if(any(is.na(as.numeric(sortIds)))){
      print("na -> return")
      return()
    }
    global_reportProgressModel$setSelectedList(as.numeric(sortIds))
  })

  
  # Show reported elements in the left panel
  output[[ns("sortableReportElements")]] <- renderUI({
   
    s <- sortableElements()
    
    isolate({
      sel <- global.selectedSortableElement()
      
      r <- rank_list(
        text = NULL, 
        labels = s,
        input_id = ns("selecetedReportedElements"),
        options=sortable_options(direction = "vertical"),
        class ='default-sortable custom-sortable',
        css_id = ns("selecetedReportedElementsRank")
      )
      
      #highlight selected item in top panel
      if(!is.null(sel) && sel != -1){
        
        r <- paste0(r)
        if(grepl(ns(paste0("sortableElementButton_",sel)), r)){
          
          r <- str_split(r, ns(paste0("sortableElementButton_",sel)))[[1]]
          r[1] <- stringi::stri_replace_last(r[1], 
                                             replacement="rank-list-item rank-list-selected",
                                             fixed="rank-list-item")
          r <- paste0(r[1], ns(paste0("sortableElementButton_",sel)), r[2])
        }
        r <- HTML(r)
      }
      
      r
    })

  })


  
  #Add observer of buttons from the sortable elements
  #Observe changes in the sortable list. Used e.g. to add/remove classes for 
  #preview and download button
  global.observerSortableElements <<- list()
  observe({
    sortEl <- sortableElements()
    
    if(length(sortEl)>0){
      addCssClass(id=ns("createPDFPreviewAll"), class="btn-outline-primary")
      addCssClass(id=ns("downloadAll"), class="btn-primary")
    }else{
      removeCssClass(id=ns("createPDFPreviewAll"), class="btn-outline-primary")
      removeCssClass(id=ns("downloadAll"), class="btn-primary")
    }

    isolate({
      n <- names(sortEl)
      sapply(1:length(n), function(i){
        if(is.null(global.observerSortableElements[[paste0(n[i],"")]])){
          global.observerSortableElements[[paste0(n[i],"")]] <<- observeEvent(input[[ns(paste0("sortableElementButton_",n[i]))]], {
            g <- clickedSortableElement()
            g[[1]] <- !g[[1]]
            g[[2]] <- n[i]
            clickedSortableElement(g)
          })
        }
      })
      
      if(clickedSortableElement()[[2]] > -1 && !clickedSortableElement()[[2]] %in% n){
        clickedSortableElement(list(T, -1))
      }
    })
    
  })
  
  #Observe the current clicked sortable element
  observe({
    clicked <- clickedSortableElement() #list(T,-1)
    isolate({
      sel <- global.selectedSortableElement()
      if(is.null(sel) || clicked[[2]]!=sel){
        global.selectedSortableElement(clicked[[2]])
      }else{
        global.selectedSortableElement(NULL)
      }
    })
  })
  
  
  #Observe the current selected sortable element
  changeOrderHighlightSelected <- reactiveVal(T)
  observe({
    sel <- global.selectedSortableElement()
    changeOrderHighlightSelected()
    isolate({
      sortEl <- sortableElements()
      n <- names(sortEl)
      
      #Highlight border of selected element
      for(id in n){
        if(id %in% sel){
          shinyjs::runjs(paste0("$('#",ns(paste0("sortableElementButton_",id)),"').parent().parent().addClass('rank-list-selected');"))
        }else{
          shinyjs::runjs(paste0("$('#",ns(paste0("sortableElementButton_",id)),"').parent().parent().removeClass('rank-list-selected');"))
        }
      }
    })
  })
  
  local_observer_reportExperiment <- c()
  #Show selected element from sortable list
  observe({
    ret <- tags$div()
    sel <- global.selectedSortableElement()
    
    tNum <- reportTypeEnum()
    
    isolate({
      if(!is.null(sel) && sel >= 0){
        model <- global_reportProgressModel
        selItem <- model$getItem(sel)
        
        if(!is.null(selItem)){
  
          objType <- selItem$getType()
          obj <- selItem$getObject()
          
          #replace ui if selItem is a "planningExperiment" objType
          if(objType == tNum$planningExperiment){
            indData <- selItem$getIndividualData()
            obj$div$ui <- planning_reportExperiment(ns=indData$ns, mcd=NULL, reportItem=T, item=selItem)
            if(!indData$ns("") %in% local_observer_reportExperiment){
              planning_reportExperimentServer(id=str_sub(indData$ns(""), end=-2), selItem)
              local_observer_reportExperiment <- c(local_observer_reportExperiment, indData$ns(""))
            }
          }
          
          ret <- transposeReportedElementToDiv(obj, objType)
          
          #remove first (the parent div, to remove everything)
          removeUI(paste0("#",ns("singleElementDivWrapper")), immediate=T)
          

          #Belonging input data name and model fit
          #or planning experiment name
          insertUI(paste0("#",ns("singleElementDiv")), ui=uiOutput(ns("singleElementDivWrapper")), immediate=T)
          if(objType!=tNum$blank){
            insertUI(paste0("#",ns("singleElementDivWrapper")), ui=uiOutput(ns("singleElementViewAffiliation")), immediate=T)
            output[[ns("singleElementViewAffiliation")]] <- renderUI({
              l <- tagList()
              if(selItem$getModuleType() == "planning"){
                l[[1]] <- tags$div("Experiment name: ", tags$b(selItem$getPlanningName()))
              }else if(selItem$getModuleType() == "evaluation"){
                l[[1]] <- tags$div("Input data: ", tags$b(selItem$getDataModel_id()))
                if(!is.null(selItem$getpDIM_name())) l[[2]] <- tags$div("Fit: ", tags$b(selItem$getpDIM_name()))
              }
              wellPanel(class="getActiveColor", style="padding:10px;",l)
            })
          }

          
          selItemId <- selItem$getId()

          #-1 ignore space attribute of ret
          sapply(1:(length(ret)-1), function(i){
            #ui, plot or table
            type <- names(ret)[i]
            
            showedElement <- ret[[i]]
            
            if(type=="ui"){
              insertUI(paste0("#",ns("singleElementDivWrapper")), ui=uiOutput(ns(paste0("singleElementView",selItemId,"_",i))), immediate=T)
              output[[ns(paste0("singleElementView",selItemId,"_",i))]] <- renderUI(showedElement)
            }else if(type=="plot"){
              insertUI(paste0("#",ns("singleElementDivWrapper")), ui=plotOutput(ns(paste0("singleElementViewPlot",selItemId,"_",i))), immediate=T)
              output[[ns(paste0("singleElementViewPlot",selItemId,"_",i))]] <- renderPlot(showedElement)
            }else if(type=="table"){
              insertUI(paste0("#",ns("singleElementDivWrapper")), ui=DTOutput(ns(paste0("singleElementViewDT",selItemId,"_",i))), immediate=T)
              output[[ns(paste0("singleElementViewDT",selItemId,"_",i))]] <- DT::renderDT(showedElement)
            }
            if(i <= 2){ 
              space <- 20
              if(!is.null(ret$space)) space <- ret$space[i]
              insertUI(paste0("#",ns("singleElementDivWrapper")), 
                       ui=tags$div(
                         style=paste0("margin-bottom:",space, "px;"),
                         id=paste0(ns("singleElementSpace"),i)), immediate=T)
            }
          })
         

          addCssClass(id=ns("createPDFPreviewSingle"), class="btn-outline-primary")
          addCssClass(id=ns("downloadSingle"), class="btn-primary")
          enable(id=ns("createPDFPreviewSingle"))
          enable(id=ns("downloadSingle"))
        }else{
          if(localUse) browser()
          showNotification("Something went wrong. The operator is notified.", type="error")
          malfunction_report(code=malfunctionCode()$emptySelectedItem, msg="emptySelectedItem",
                             type="error", askForReport=T)
        }
      }else{
        removeUI(paste0("#",ns("singleElementDivWrapper")), immediate=T)
        removeCssClass(id=ns("createPDFPreviewSingle"), class="btn-outline-primary")
        removeCssClass(id=ns("downloadSingle"), class="btn-primary")
        disable(id=ns("createPDFPreviewSingle"))
        disable(id=ns("downloadSingle"))
      }
    })
  })

  
  #Single elements preview 
  observeEvent(input[[ns("createPDFPreviewSingle")]], {
    sel <- global.selectedSortableElement()
    
    if(is.null(input[[ns("outputOptionsFontSize")]])){
      showNotification("Please choose a valid font size!", type="error")
      return()
    }
    
    if(!is.null(sel) && sel >= 0){
      model <- global_reportProgressModel
      selItem <- model$getItem(sel)
      
      if(!is.null(selItem)){
        pdfFileName <- writePDF(reportedItems=list(selItem), 
                                fontSize=input[[ns("outputOptionsFontSize")]],
                                convertTo = "pdf")
        
        if(is.null(pdfFileName)) {
          showNotification("Couldn't write the PDF file. The operator is notified.", type="error")
		  if(localUse) browser()
          malfunction_report(code=malfunctionCode()$creatingPDF, msg="single elements preview writePDF",
                             type="error", askForReport=T)
          return()
        }
        
        t <- ifelse(togglePDF(),""," ")
       
        output[[ns("PDFPreview")]] <- renderText({
          paste0('<iframe', t,' style="height:600px; width:100%" src="', "Report/PDF/",pdfFileName,'"></iframe>')
        })
        togglePDF(!togglePDF())
        
        #Switch to pdf viewer
        updateTabsetPanel(session=session, inputId=ns("reportSelectedItemViewTabset"), selected="pdfViewer")

      }else{
        showNotification("Something went wrong. The operator is notified.", type="error")
		if(localUse) browser()
        malfunction_report(code=malfunctionCode()$creatingPDF, msg="single elements preview",
                           type="error", askForReport=T)
        output[[ns("PDFPreview")]] <- renderText("Create a pdf preview first.")
      }
    }else{
      output[[ns("PDFPreview")]] <- renderText("Create a pdf preview first.")
      showNotification("Please select a reported item from above!", type="warning")
    }
  })

  ignoreRecommendedOrder <- reactiveVal(F)
  #Observe sortable list changes to set checkbox
  observeEvent(input[[ns("selecetedReportedElements")]], {
    order <- getRecommendedOrderOfItems()
    if(!is.null(order) && all(order==input[[ns("selecetedReportedElements")]])){
      if(!input[[ns("reorderItemsRecommended")]]){
        ignoreRecommendedOrder(T)
        updateCheckboxInput(session=session, inputId=ns("reorderItemsRecommended"), 
                            value=T)
      }
      ignoreRecommendedOrder(F)
    }else{
      updateCheckboxInput(session=session, inputId=ns("reorderItemsRecommended"), 
                          value=F)
    }
  })
  
  #Reorder items to recommended ordering
  observeEvent(input[[ns("reorderItemsRecommended")]], ignoreInit = T, {
   
    if(!ignoreRecommendedOrder() && input[[ns("reorderItemsRecommended")]]){
      
      if(is.null(input[[ns("selecetedReportedElements")]]) || 
         length(input[[ns("selecetedReportedElements")]]) == 0){
        showNotification("Nothing to order.")
        ignoreRecommendedOrder(F)
        return()
      }
      
      # global_reportProgressModel$getSelectedList()
      
      newOrder <- getRecommendedOrderOfItems()
      if(is.null(newOrder)){
        showNotification("Can't reorder items. The operator is notified.", 
                         type="error")
						 if(localUse) browser()
        malfunction_report(code=malfunctionCode()$reorderReportedItems, msg="reporder items to recommended order",
                           type="error", askForReport=T)
        ignoreRecommendedOrder(F)
        return()
      }
      global_reportProgressModel$setSelectedList(newOrder)
    }
    ignoreRecommendedOrder(F)
  })
  
  getRecommendedOrderOfItems <- function(){
    try({
      sortableListIds <- input[[ns("selecetedReportedElements")]]
      if(is.null(sortableListIds) || length(sortableListIds)==0){
        return(c())
      }
      model <- global_reportProgressModel
      itemModuleType <- c() #"evaluation", "planning", "blank"
      datainputList <- c()
      fitList <- c()
      typeList <- c()
      reportedIds <- c() #To sort elements e,g. different preplots in incoming order
      for(id in sortableListIds){
        item <- model$getItem(id)
        itemModuleType <- c(itemModuleType, item$getModuleType())
        if(item$getModuleType() == "planning"){
          datainputList <- c(datainputList, paste0(item$getPlanningName(), item$getId()))
          fitList <- c(fitList, "")
        }else if(item$getModuleType() == "evaluation"){
          datainputList <- c(datainputList, item$getDataModel_id())
          fitList <- c(fitList, item$getpDIM_id())
        }else{
          datainputList <- c(datainputList, "")
          fitList <- c(fitList, "")
        }

        typeList <- c(typeList, item$getType())
        reportedIds <- c(reportedIds, item$getId())
      }
      
      newOrder <- c()
      
      
      #Blank item
      newOrder <- c(newOrder, which(itemModuleType %in% "blank"))
      
      #planning items
      newOrder <- c(newOrder, which(itemModuleType %in%"planning"))
      
      #evaluation items
      dataInputsStartWithBlanks <- unique(datainputList[itemModuleType=="evaluation"])
      for(inputDataElement in dataInputsStartWithBlanks){
        index <- which(datainputList %in% inputDataElement)
        subFits <- unique(fitList[index])
        subFits <- subFits[order(subFits)]
        allTypesInOrder <- reportTypeEnum(T)
        for(t in allTypesInOrder){
          for(fit in subFits){
            indexType <- which(typeList %in% t)
            indexFit <- which(fitList %in% fit)
            #ids could be of length 0 or positive
            ids <- intersect(index,intersect(indexType, indexFit))
            if(length(ids) > 0){
              subReportedIds <- reportedIds[ids]
              ids <- ids[order(subReportedIds)]
              newOrder <- c(newOrder, ids)
            } 
          }
        }
      }
   
      # dataInputsStartWithBlanks <- unique(datainputList)
      # if("-1" %in% dataInputsStartWithBlanks) dataInputsStartWithBlanks <- c(-1, dataInputsStartWithBlanks[dataInputsStartWithBlanks != -1])
      # for(inputDataElement in dataInputsStartWithBlanks){
      #   index <- which(datainputList %in% inputDataElement)
      #   subFits <- unique(fitList[index])
      #   subFits <- subFits[order(subFits)]
      #   allTypesInOrder <- reportTypeEnum(T)
      #   for(t in allTypesInOrder){
      #     for(fit in subFits){
      #       indexType <- which(typeList %in% t)
      #       indexFit <- which(fitList %in% fit)
      #       #ids could be of length 0 or positive
      #       ids <- intersect(index,intersect(indexType, indexFit))
      #       if(length(ids) > 0){
      #         subReportedIds <- reportedIds[ids]
      #         ids <- ids[order(subReportedIds)]
      #         newOrder <- c(newOrder, ids)
      #       } 
      #     }
      #   }
      # }
      
      newSorted <- as.numeric(sortableListIds[newOrder])
      

      #Sort in bound blank items
      bE <- global.bindingElements()
      nBE <- names(bE)
      for(i in seq_along(bE)){
        id <- as.numeric(nBE[i])
        if(bE[[i]][2]=="prev"){
          if(bE[[i]][1]=="first"){
            newSorted <- c(id, newSorted[newSorted != id])
          }else{
            boundId <- as.numeric(bE[[i]][1])
            newSorted <- newSorted[newSorted != id]
            boundIdIndex <- match(boundId, newSorted)
            if((boundIdIndex+1) <= length(newSorted)){
              newSorted <- c(newSorted[1:boundIdIndex], id, newSorted[(boundIdIndex+1):length(newSorted)])
            }else{
              newSorted <- c(newSorted[1:boundIdIndex], id)
            }
          }
        }else if(bE[[i]][2]=="next"){
          if(bE[[i]][1]=="last"){
            newSorted <- c(newSorted[newSorted != id],id)
          }else{
            boundId <- as.numeric(bE[[i]][1])
            newSorted <- newSorted[newSorted != id]
            boundIdIndex <- match(boundId, newSorted)
            
            if((boundIdIndex-1) >= 1 ){
              newSorted <- c(newSorted[1:(boundIdIndex-1)], id, newSorted[(boundIdIndex):length(newSorted)])
            }else{
              newSorted <- c(id, newSorted[(boundIdIndex):length(newSorted)])
            }
            
          }
        }else{
          showNotification("Error in sorting elements.", type="error")
        }
      }
      
      return(newSorted)
    })
    return(NULL)
  }
  
  #All elements in given order preview 
  observeEvent(input[[ns("createPDFPreviewAll")]], {
    if(length(input[[ns("selecetedReportedElements")]]) == 0){
      showNotification("Please select items on the left.", type="warning")
      return()
    }
    if(is.null(input[[ns("outputOptionsFontSize")]])){
      showNotification("Please choose a valid font size!", type="error")
      return()
    }
    
    sortableListIds <- input[[ns("selecetedReportedElements")]]
    sortableEl <- sortableElements()
    sortableEl <- sortableEl[sortableListIds]

    model <- global_reportProgressModel
    
    selItems <- list()
    for(id in sortableListIds){
      selItems <- list.append(selItems, model$getItem(id))
    }
    
    pdfFileName <- writePDF(reportedItems=selItems,
                            fontSize=input[[ns("outputOptionsFontSize")]], 
                            optionalTitle=input[[ns("outputOptionsOptionalTitle")]], 
                            optionalName=input[[ns("outputOptionsOptionalName")]],
                            makeTitle=T,
                            convertTo = "pdf")

    if(is.null(pdfFileName)) {
      showNotification("Couldn't write the PDF file. The operator is notified.", type="error")
	  if(localUse) browser()
      malfunction_report(code=malfunctionCode()$creatingPDF, msg="all elements preview writePDF",
                         type="error", askForReport=T)
      return()
    }

    t <- ifelse(togglePDF(),""," ")

    output[[ns("PDFPreview")]] <- renderText({
      paste0('<iframe', t,' style="height:600px; width:100%" src="', "Report/PDF/",pdfFileName,'"></iframe>')
    })
    togglePDF(!togglePDF())

    #Switch to pdf viewer
    updateTabsetPanel(session=session, inputId=ns("reportSelectedItemViewTabset"), selected="pdfViewer")

  })
  
  
  outputDownloadFile <- reactiveVal(NULL)
  #Single elements download
  output[[ns("downloadSingle")]] <- downloadHandler(
    filename = function(){
      sel <- global.selectedSortableElement()
      
      if(is.null(input[[ns("outputOptionsFontSize")]])){
        showNotification("Please choose a valid font size!", type="error")
        return()
      }
      
      if(!is.null(sel) && sel >= 0){
        model <- global_reportProgressModel
        selItem <- model$getItem(sel)
        # convertTo <- input[[ns("outputOptionsFormat")]]
        convertTo <- "pdf"
        # DPI <- input[[ns("outputOptionsDPI")]]
        # 
        # if(convertTo != "pdf" && DPI[1] == "FALSE"){
        #   showNotification("Please provide a valid DPI input!", type="error")
        #   outputDownloadFile(NULL)
        #   return()
        # }
        
        if(!is.null(selItem)){
          
          outputFile <- writePDF(reportedItems=list(selItem), 
                                  fontSize=input[[ns("outputOptionsFontSize")]],
                                  convertTo=convertTo)
          

          if(is.null(outputFile)){
            outputDownloadFile("")
            showNotification("Couldn't write the PDF file. The operator is notified.", type="error")
			      if(localUse) browser()
            malfunction_report(code=malfunctionCode()$creatingPDF, msg="single element download writePDF",
                               type="error", askForReport=T)
            return(NULL)
          } 

          
          if(convertTo=="pdf"){
            outputFile <- paste0(report_folder, "/PDF/", outputFile)
          }else{
            outputFile <- paste0(report_folder, "/PIC/", outputFile)
          }
          outputDownloadFile(outputFile)
          return(basename(outputFile))
        }else{
          print0("Error, sel: ",sel)
          showNotification("Something went wrong. The operator is notified.", type="error")
          malfunction_report(code=malfunctionCode()$creatingPDF, msg="single elements download",
                             type="error", askForReport=T)
		  if(localUse) browser()
          output[[ns("PDFPreview")]] <- renderText("Create a pdf preview first.")
          outputDownloadFile("")
        }
      }
      showNotification("Please select a reported item from above!", type="warning")
      outputDownloadFile("")
    }, 
    content = function(file){
      copyFile <- outputDownloadFile()
      if(is.null(copyFile) || copyFile=="") return()
      file.exists(copyFile)
      if(is.null(copyFile)){
        file.copy(paste0(report_folder, "/PDF/Empty.pdf"), file)
      }else{
        file.copy(copyFile, file)
      }
    },
    # contentType = ifelse(input[[ns("outputOptionsFormat")]]=="pdf", "pdf", paste0("",input[[ns("outputOptionsFormat")]]))
    contentType = "pdf"
  )
  
  # All elements download
  output[[ns("downloadAll")]] <- downloadHandler(
    filename = function(){

      if(is.null(input[[ns("outputOptionsFontSize")]])){
        showNotification("Please choose a valid font size!", type="error")
        return()
      }
      sortableListIds <- input[[ns("selecetedReportedElements")]]
      
      if(!is.null(sortableListIds) && length(sortableListIds) > 0){
        model <- global_reportProgressModel
        selItems <- list() 
        for(id in sortableListIds){
          selItems <- list.append(selItems, model$getItem(id))
        }
        
        # convertTo <- input[[ns("outputOptionsFormat")]]
        convertTo <- "pdf"
        # DPI <- input[[ns("outputOptionsDPI")]]
        # 
        # if(convertTo != "pdf" && DPI[1] == "FALSE"){
        #   showNotification("Please provide a valid DPI input!", type="error")
        #   outputDownloadFile(NULL)
        #   return()
        # }
        
        if(length(selItems) == length(sortableListIds)){
          outputFile <- writePDF(reportedItems=selItems, 
                                 fontSize=input[[ns("outputOptionsFontSize")]], 
                                 optionalTitle=input[[ns("outputOptionsOptionalTitle")]], optionalName=input[[ns("outputOptionsOptionalName")]],
                                 makeTitle=T,
                                 convertTo=convertTo)
          
          if(is.null(outputFile)){
            outputDownloadFile("")
            showNotification("Couldn't write the PDF file. The operator is notified.", type="error")
            malfunction_report(code=malfunctionCode()$creatingPDF, msg="all elements download writePDF",
                               type="error", askForReport=T)
            return(NULL)
          } 
          
          if(convertTo=="pdf"){
            outputFile <- paste0(report_folder, "/PDF/", outputFile)
          }else{
            outputFile <- paste0(report_folder, "/PIC/", outputFile)
          }
          
          outputDownloadFile(outputFile)
          return(basename(outputFile))
        }else{
          if(localUse) browser()
          showNotification("Something went wrong. The operator is notified.", type="error")
		  if(localUse) browser()
          malfunction_report(code=malfunctionCode()$creatingPDF, msg="all elements download",
                             type="error", askForReport=T)
          output[[ns("PDFPreview")]] <- renderText("Create a pdf preview first.")
          return(NULL)
        }
      }
      showNotification("Please select items from the left panel!", type="warning")
      outputDownloadFile(NULL)
      return("NULL")
    }, 
    content = function(file){
      copyFile <- outputDownloadFile()
      if(is.null(copyFile)){
        file.copy(paste0(report_folder, "/PDF/Empty.pdf"), file)
      }else{
        file.copy(copyFile, file)
      }
    },
    # contentType = ifelse(input[[ns("outputOptionsFormat")]]=="pdf", "pdf", paste0("",input[[ns("outputOptionsFormat")]]))
    contentType = "pdf"
  )
  
  
  
  #returns the file (pdf, png, jpeg, tiff)
  #uses its own thread to compile the tex/pdf
  writePDF <- function(reportedItems, fontSize, optionalTitle=NULL, optionalName=NULL, 
                       makeTitle=F,
                       convertTo=NULL, DPI=NULL){
    tEnum <- reportTypeEnum()
    if(!is.null(reportedItems) && length(reportedItems) > 0){

      pdfFileName <- ""
      outputFile <- ""
      texId <- NULL
      
      # Is pdf file already present? 
      # lists(id, reported_elements, fontSize)
      sPDFs <- storedPDFs()
      flag <- F
      for(s in sPDFs){
        flag <- T
        if(s$fontSize==fontSize &&
           equal0(s$optionalTitle, optionalTitle) &&
           equal0(s$optionalName, optionalName) &&
           length(s$reported_elements)==length(reportedItems)){
          for(r_index in 1:length(reportedItems)){
            if(reportedItems[[r_index]]$getId() != s$reported_elements[[r_index]] ||
               reportedItems[[r_index]]$getType() == tEnum$blank ||
               reportedItems[[r_index]]$getType() == tEnum$planningExperiment){
              flag <- F
              break
            }
          }
          if(flag){
            outputFile <- paste0("BAYAS_report_",s$id, ".",convertTo) 
            texId <- s$id 
            pdfFileName <- paste0(report_folder, "/PDF/BAYAS_report_",texId, ".pdf")
            break
          }
        }else{
          flag <- F
        }
      }
      
      progressBar <- Progress$new(min=0,max=1)
      if(!flag){
        progressBar$set(value=0, message = 'Creating tex file ...', detail= "This may take a while")
        
        pdfCount(pdfCount()+1)
        nextCount <- pdfCount()
        

        consoleFile <- paste0(report_folder,"/console.txt")
        f <- future({
          sink(consoleFile)
          out <- tryCatch({
            #Create tex.Rnw file
            texId <- createTexFile(reportedItems, fontSize, optionalTitle, optionalName, nextCount, makeTitle)

            rawTexFile <- paste0(report_folder,"/Tex/rawTex_",texId ,".Rnw")
            pdfOutputFile <- paste0(report_folder,"/PDF/BAYAS_report_",texId, ".tex")
            
            #Copy literature
            file.copy(paste0(report_folder,"/GeneralTex/literatur.bib"),
                      paste0(report_folder,"/PDF/literatur.bib"))
            
            #knit2html()
            knit2pdf(rawTexFile, 
                     output = pdfOutputFile,
                     clean = TRUE, bib_engine = "bibtex")
            
            print("@FINISHED")
            
            texId
          },
          error=function(cond){
            print(cond)
            return(NULL)
          })
          sink()
          out
        })
        
        progressBar$set(value=0.1, message = 'Writing PDF file ...', detail= "This may take a while")
        
        flag <- T
        
        while(!resolved(f)){
          fileConn <- file(consoleFile)
          head <- readLines(fileConn)
          if(length(head) > 0 && grepl("@FINISHED",head[length(head)])) flag <- F
          close(fileConn)
        }

        
        progressBar$set(value=0.7, message = 'Writing PDF file ...', detail= "This may take a while")
        
        texId <- value(f) 
        
        outputFile <- paste0("BAYAS_report_",texId, ".",convertTo)
        pdfFileName <- paste0(report_folder, "/PDF/BAYAS_report_",texId, ".pdf")
      }

   
      progressBar$close()
      

      if(is.null(texId)){
        #Are blanks are used?
        for(r_index in seq_along(reportedItems)){
          if(reportedItems[[r_index]]$getType() == tEnum$blank){
            showNotification("Couldn't read latex. Please verify the content of your blank items.", type="error",
                             duration =10)
            break
          }
        }
        return(NULL)
      }
      
      # Add a new stored PDF
      repId <- c()
      for(i in reportedItems){
        repId <- c(repId, i$getId())
      }
      sPDFs <- list.append(sPDFs, list(id=texId, 
                                       reported_elements=repId, 
                                       fontSize=fontSize,
                                       optionalTitle=optionalTitle,
                                       optionalName=optionalName))
      storedPDFs(sPDFs)
      
      #returns the file (pdf, png, jpeg, tiff)
      return(outputFile)
    }else{
      showNotification("Nothing to report. If this is not true, contact the owner.", type="warning")
    }
  }
  
}

createTexFile <- function(reportedItems, fontSize, optionalTitle, optionalName, nextCount, makeTitle){

  if(!is.null(reportedItems) && length(reportedItems) > 0){
    
    tNum <- reportTypeEnum()
    
   
    #Check the order
    #Are reported elements grouped by:
    # model fit
    # type of reported element
    #If one of these two options are given, thats fine,
    #otherwise give a warning, 
    #but create a pdf in this order with duplicate (sub)sections
    #Check whether the items are sorted by
    # -data (and)
    # -fits (xor)
    # -type of item 
    #if no sorting is detected, give each item an individual header
    sortedByModuleType <- T
    sortedByData <- T
    sortedByFits <- T
    sortedByType <- T
    headers <- list()
    if(length(reportedItems) > 1){
      itemsModuleType <- c()
      itemsData <- c()
      itemsFits <- c()
      itemsTypes <- c()
      for(item in reportedItems){
        if(item$getType() == tNum$blank){
          # blankData <- item$getBlankData()
          itemsData <- c(itemsData, -1)
          itemsFits <- c(itemsFits, -1)
          itemsModuleType <- c(itemsModuleType, "b")
        }else if(item$getType() == "planningExperiment"){
          itemsData <- c(itemsData, -1)
          itemsFits <- c(itemsFits, -1)
          itemsModuleType <- c(itemsModuleType, "p")
        }else{
          itemsData <- c(itemsData, item$getDataModel_id())
          itemsFits <- c(itemsFits, item$getpDIM_id())
          itemsModuleType <- c(itemsModuleType, "e")
        }

        t <- item$getType()
        if(t==tNum$previewppc) t <- tNum$ppc
        itemsTypes <- c(itemsTypes, t)
      }

      if(!vectorEqual(itemsModuleType, rev(sort(itemsModuleType))) &&
         !vectorEqual(itemsModuleType, sort(itemsModuleType)))
        sortedByModuleType <- F
      
      #wo blanks
      itemsModuleTypeWoBlanks <- itemsModuleType[itemsTypes!=tNum$blank]
      itemsDataWoBlanks <- itemsData[itemsTypes!=tNum$blank]
      itemsFitsWoBlanks <- itemsFits[itemsTypes!=tNum$blank]
      itemsTypeWoBlanks <- itemsTypes[itemsTypes!=tNum$blank]
      
      for(i in 1:length(itemsDataWoBlanks)){
        targetModuleType <- itemsModuleTypeWoBlanks[i] 
        targetData <- itemsDataWoBlanks[i]
        targetFits <- itemsFitsWoBlanks[i]
        targetType <- itemsTypeWoBlanks[i]
        
        matches <- which(itemsModuleTypeWoBlanks %in% targetModuleType)
        matches <- matches - (min(matches)-1)
        if(max(matches) != length(matches)) sortedByModuleType <- F
        
        matches <- which(itemsDataWoBlanks %in% targetData)
        matches <- matches - (min(matches)-1)
        if(max(matches) != length(matches)) sortedByData <- F
        matches <- which(itemsFitsWoBlanks %in% targetFits)
        matches <- matches - (min(matches)-1)
        if(max(matches) != length(matches)) sortedByFits <- F
        matches <- which(itemsTypeWoBlanks %in% targetType)
        matches <- matches - (min(matches)-1)
        if(max(matches) != length(matches)) sortedByType <- F
      }
      
      
      #Check whether there is a sorting within a certain type
      #cast sortedByData and sortedByFits to vectors
      countBlanks <- 0
      if(sortedByType){
        sortedByData <- rep(F, length=length(itemsData))
        sortedByFits <- rep(F, length=length(itemsData))
        for(i in seq_along(itemsData)){
          targetData <- itemsData[i]
          targetFits <- itemsFits[i]
          targetType <- itemsTypes[i]
          
          if(targetType == tNum$blank){
            countBlanks <- countBlanks+1
            next;
          }
          
          typeMatches <- which(itemsTypeWoBlanks %in% targetType)

          dataMatches <- which(itemsDataWoBlanks[typeMatches] %in% targetData)
          minDataMatches <- min(dataMatches) + (min(typeMatches)-1)
          dataMatches <- dataMatches - (min(dataMatches)-1)
          uniItemsData <- unique(itemsData)
          uniItemsData <- uniItemsData[uniItemsData != -1]
          if(max(dataMatches) == length(dataMatches) && 
             minDataMatches == (i-countBlanks) &&
             length(uniItemsData)>1) sortedByData[i] <- T
          
          fitMatches <- which(itemsFitsWoBlanks[typeMatches] %in% targetFits)
          minFitMatches <- min(fitMatches) + (min(typeMatches)-1)
          fitMatches <- fitMatches - (min(fitMatches)-1)
          uniItemsFits <- unique(itemsFits)
          uniItemsFits <- uniItemsFits[uniItemsFits != -1]
          if(max(fitMatches) == length(fitMatches) &&
             minFitMatches == (i-countBlanks) &&
             targetFits != -1 &&
             length(uniItemsFits)>1) sortedByFits[i] <- T
        }
      }

      prevHeader <- ""
      sub <- ""
      sub2 <- ""
      planningHeaders <- list()
      for(i in seq_along(itemsTypes)){
        objType <- reportedItems[[i]]$getType()
        objTypePrint <- elementSectionMapping(objType)
        
        
        if(objType == reportTypeEnum()$planningExperiment){
          pName <- reportedItems[[i]]$getPlanningName()
          if(pName %in% names(planningHeaders)){
            planningHeaders[[pName]] <- planningHeaders[[pName]]+1
          }else{
            planningHeaders <- list.append(planningHeaders, 0, pName)
          }
          objTypePrint <- paste0(objTypePrint, " ``", wordToLatexConform(pName), "''(",planningHeaders[[pName]], ")")
        }
          
        
        headers[[i]] <- ""
        
        if(objType == reportTypeEnum()$blank){

          headers[[i]] <- paste0(headers[[i]], "\\section{", objTypePrint, "}\n",
                                 getItemDescription(objType), "\n\n")
          
        }
        else if(objType == reportTypeEnum()$planningExperiment){
          headers[[i]] <- paste0("\\section{",objTypePrint,"}\n",
                                 getItemDescription(objType), "\n\n")
          sub <- ""

          if(prevHeader == headers[[i]]){
            headers[[i]] <- "\n"
          }else{
            prevHeader <- headers[[i]]
          }
          
        }
        else if(sortedByType){
          targetType <- itemsTypes[i]
          matches <- which(itemsTypes %in% targetType)
          if(min(matches)==i){
            headers[[i]] <- paste0("\\section{",objTypePrint,"}\n",
                                   getItemDescription(objType), "\n\n")
            sub <- ""
          }
          
          if(sortedByData[i]){
            targetData <- itemsData[i]
            t <- basename(targetData)
            t <- wordToLatexConform(t)
            headers[[i]] <- paste0(headers[[i]],"\\subsection{",t,"}\n")
            sub <- "sub"
          }
          if(sortedByFits[i]){
            t <- reportedItems[[i]]$getpDIM_name()
            t <- wordToLatexConform(t)
            headers[[i]] <- paste0(headers[[i]],"\\",sub,"subsection{",t,"}\n")
          }
          
          if(prevHeader == headers[[i]]){
            headers[[i]] <- "\n"
          }else{
            prevHeader <- headers[[i]]
          }
          
        }
        else if(sortedByData || sortedByFits){
          if(sortedByData){
            targetData <- itemsData[i]
            matches <- which(itemsData %in% targetData)
            if(min(matches)==i){
              t <- basename(targetData)
              t <- wordToLatexConform(t)
              headers[[i]] <- paste0("\\section{Report of ",t,"}\n")
            }
            sub <- "sub"
            sub2 <- "sub"
          }
          
          targetFits <- itemsFits[i]
          matches <- which(itemsFits %in% targetFits)
          t <- reportedItems[[i]]$getpDIM_name()
          t <- wordToLatexConform(t)
          if(min(matches)==i && !is.null(t)){
            headers[[i]] <- paste0(headers[[i]], "\\", sub,"section{Report of fit '",t,"'}\n")
          }
          sub2 <- paste0(sub,"sub")

          headers[[i]] <- paste0(headers[[i]], "\\", sub2,"section{",objTypePrint,"}\n",
                                 getItemDescription(objType), "\n\n")
          if(prevHeader == headers[[i]]){
            headers[[i]] <- "\n"
          }else{
            prevHeader <- headers[[i]]
          }
        }
        else{
          headers[[i]] <- paste0(headers[[i]], "\\section{",objTypePrint, "}\n",
                                 getItemDescription(objType), "\n\n")
          sub <- ""
          
          uniItemsData <- unique(itemsData)
          uniItemsData <- uniItemsData[uniItemsData != -1]
          
          if(length(uniItemsData)>1){
            targetData <- itemsData[i]
            t <- basename(targetData)
            t <- wordToLatexConform(t)
            headers[[i]] <- paste0(headers[[i]], "\\subsection{",t,"}\n")
            sub <- "sub"
          }
          
          uniItemsFits <- unique(itemsFits)
          uniItemsFits <- uniItemsFits[uniItemsFits != -1]
          
          # if(length(unique(itemsData))>1){
          if(length(uniItemsFits)>1){
            t <- reportedItems[[i]]$getpDIM_name()
            if(!is.null(t)){
              t <- wordToLatexConform(t)
              if(str_trim(t)!=""){
                headers[[i]] <- paste0(headers[[i]], "\\", sub,"subsection{",t,"}\n")
              }
            }

          }
        }

      }
    }else{
      #Element section
      objType <- reportedItems[[1]]$getType()
      objTypePrint <- elementSectionMapping(objType)
      if(objType == reportTypeEnum()$planningExperiment)
        objTypePrint <- paste0(objTypePrint, " ``",  wordToLatexConform(reportedItems[[1]]$getPlanningName()) ,"''")
      headers[[1]] <- paste0("\\section{",objTypePrint,"}\n", 
                             getItemDescription(objType),"\n\n")
    }
    
    #Read head
    fileConn <- file(paste0(report_folder,"/GeneralTex/head.txt"))
    head <- readLines(fileConn)
    close(fileConn)
    head <- c(head,paste0("\\KOMAoptions{fontsize=",fontSize,"pt}"))
    head <- paste0(head,collapse ="\n")

    
    #write optional header date and optional name
    if(makeTitle==T){
      if(!is.null(optionalTitle)){
        head <- paste0(head, "\n", "\\subtitle{", optionalTitle, "}\n")
      }
      if(!is.null(optionalName)){
        head <- paste0(head, "\\author{", optionalName, "}\n")
      }
      head <- paste0(head, "\\maketitle\n")
    }      
    tNum <- reportTypeEnum()
    rawLatexTxt <- ""

   
    nextBlank <- ""
    
    for(selItemId in seq_along(reportedItems)){
      selItem <- reportedItems[[selItemId]]
      
      #adjust latex e.g. in planningExperiment after report item inputs (such as include ssd goals, etc.)
      # selItem$adjustLatex()
      obj <- selItem$getObject(adjust=T)
      objType <- selItem$getType()
      
      #if this element is the first of its kind and there is at least a second
      #item directly following of same type and same fit, print also the model_latex
      model_latex <- F
      if((selItemId==1 || reportedItems[[selItemId-1]]$getType() != objType) &&
         (length(reportedItems) == selItemId || reportedItems[[selItemId+1]]$getType() == objType)) 
        model_latex <- T
      
      
      #if selItem is a blank element
      if(objType==tNum$blank){
        
        blankData <- selItem$getBlankData()
        header <- blankData$header
        text <- blankData$text
        binding <- blankData$binding
        
 
        if(!is.null(header) && str_trim(header) != ""){
          
          headerBinding <- ""
          
          if(binding == "prev"){
            if(selItemId != 1)
              headerBinding <- headers[[selItemId-1]]
          }else if(blankData$binding == "next"){
            if(selItemId != length(itemsTypes))
              headerBinding <- headers[[selItemId+1]]
          }
          
          sub <- ""
          if(grepl("\\subsubsection{", headerBinding,  fixed = TRUE)){
            sub <- "subsub"
          }else if(grepl("\\subsection{", headerBinding,  fixed = TRUE)){
            sub <- "sub"
          }
          headers[[selItemId]] <- paste0("\\",sub,substr(headers[[selItemId]], 2, nchar(headers[[selItemId]])))
          
          rawLatexTxt <- paste(rawLatexTxt,
                               str_replace(headers[[selItemId]],"blank", header),
                               text, sep="\n")
        }else{
          if(!is.null(binding) && binding=="next"){
            nextBlank <- paste0(nextBlank, "\n", text)
          }else{
            rawLatexTxt <- paste(rawLatexTxt, text, sep="\n")
          }
        }
        
      }else{
        section <- headers[[selItemId]]
        content <- transposeReportedElementToPDF(obj, objType, model_latex)
        rawLatexTxt <- paste(rawLatexTxt,section,nextBlank,content, sep="\n")
        nextBlank <- ""
      }

    }
    
    #Read tail
    fileConn <- file(paste0(report_folder,"/GeneralTex/tail.txt"))
    tail <- readLines( fileConn)
    tail <- paste0(tail,collapse ="\n")
    close(fileConn)
    
    rawLatexTxt <- paste(head,rawLatexTxt,tail, sep="\n")
    
    fileConn <- file(paste0(report_folder,"/Tex/rawTex_",nextCount ,".Rnw"))
    writeLines(rawLatexTxt, fileConn)
    close(fileConn)
    
    # return tex file id
    return(nextCount)
  }else{
    showNotification("Nothing to report. If this is not true, contact the owner.", type="warning")
    return(NULL)
  }
}

