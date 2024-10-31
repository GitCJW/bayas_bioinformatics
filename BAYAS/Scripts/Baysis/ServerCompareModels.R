
init_compare_models_function <- function(input, output, session, dataModel,
                                         global_reportProgressModel){

  ## When run a fit, the selectInputs of fitted models are updated. 
  ## Response selectInput contains only a value if there are at least to fitted models.
  observe({
    dataModel$dependReactiveValue("perIterationDataModels")
    isolate({
      updateFittedCompareModels(input = input, output = output, session = session, dataModel = dataModel)
    })
  })
  
   
  ## Observe compare button for two models
  observeEvent(input$compareModelsButtonCompareTwo ,{
    
    if(input$compareModelsSelectModelFirst == ""){
      showNotification("Please fit at least two models of same response.", type = "message", duration = 10)
      return()
    } else if(input$compareModelsSelectModelFirst == input$compareModelsSelectModelSecond){
      showNotification( "Please select two different models!", type = "message", duration = 10)
      return()
    }
    
    dataModel_first <- dataModel$get.perIterationDataModel(input$compareModelsSelectModelFirst) 
    dataModel_second <- dataModel$get.perIterationDataModel(input$compareModelsSelectModelSecond) 

    resp1 <- dataModel_first$getDataModelInputData()$getResponseVariable(onlyName = T)
    resp2 <- dataModel_second$getDataModelInputData()$getResponseVariable(onlyName = T)
    dataResp1 <- dataModel_first$getDataModelInputData()$getLongFormatVariable(resp1)
    dataResp2 <- dataModel_second$getDataModelInputData()$getLongFormatVariable(resp2)
    
    if(rlang::hash(dataResp1) != rlang::hash(dataResp2)){
      showNotification("Please select two models of same response!", type = "warning", duration = 10)
      return()
    }
    
    #Check whether due to missing values the response differs
    vars1 <- dataModel_first$get.selected_BAYSIS_stan_model()$get_used_vars(extras=T, response=T)
    vars2 <- dataModel_second$get.selected_BAYSIS_stan_model()$get_used_vars(extras=T, response=T)
    dataResp1 <- dataModel_first$getDataModelInputData()$getLongFormatVariable(vars1, completeCases=T)[[resp1]]
    dataResp2 <- dataModel_second$getDataModelInputData()$getLongFormatVariable(vars2, completeCases=T)[[resp2]]
    
    if(rlang::hash(dataResp1) != rlang::hash(dataResp2)){
      showNotification("Can't compare these models because of missing values due to different columns used in your data.", 
                       type = "error", duration = 20)
      return()
    }
    
    dataModels <- list(dataModel_first,dataModel_second)
    compareModels(dataModels)
  })
  
  

  output[["compareModelsModelsOfSelectedResponse"]] <- renderUI({
    modelIds <- input$compareModelsSelectResponse
    
    divContent <- ""
    
    if(!is.null(modelIds) && length(modelIds) == 1 && modelIds == ""){
      
    }else{
      modelIds <- str_split(modelIds, ";")[[1]]
      modelNames <- c()
      for(id in modelIds){
        model <- dataModel$get.perIterationDataModel(id=id)
        modelNames <- c(modelNames, model$get.name())
      }
      lis <- lapply(modelNames, function(i) tags$li(i))
      
      divContent <- tags$div(
        style = "",
        tags$ul(
          lis
        )
      )
    }
    return(tags$div(divContent))
  })
  
  
  ## Compare Models by same response variable
  observeEvent(input$compareModelsButtonCompareResponse, ignoreInit = T, {

    # Verify if at least two fitted models of same response exists
    if(is.null(input$compareModelsSelectResponse) || input$compareModelsSelectResponse == "" ){
      showNotification(
          "Please select a response with at least two fitted models of same response.", 
        type = "message", duration = 10)
      return()
    }else{
      
      modelIds <- input$compareModelsSelectResponse
      modelIds <- str_split(modelIds, ";")[[1]]
      models <- list()
      model_length <- c()
      for(id in modelIds){
        model <- dataModel$get.perIterationDataModel(id=id)
        models <- list.append(models, model)
        vars <- model$get.selected_BAYSIS_stan_model()$get_used_vars(extras=T, response=T)
        data <- dataModel$getDataModelInputData()$getLongFormatVariable(vars, completeCases=T)
        ll <- dim(data)[1]
        model_length <- c(model_length, ll)
      }
      
      if(length(models) > 1){
        
        if(length(unique(model_length)) > 1){
          showNotification(
            "Can't compare these models because of missing values due to different columns used in your data.", 
            type = "error", duration = 20)
        }else{
          compareModels(models)  
        }
      }else{
        showNotification(
          "Please select a response with at least two fitted models of same response.", 
          type = "message", duration = 10)
      }

    }
  })
  
  
  currentLoo <- reactiveVal(NULL)
  
  ## Compare models generic function
  compareModels <- function(dataModels){
   
    # Disable compare buttons to prevent queueing  by clicking again compare buttons
    shinyjs::disable(id = "compareModelsButtonCompareTwo")
    shinyjs::disable(id = "compareModelsButtonCompareResponse")
    
    withProgress(message = "Comparing models", {
      
      # Fitted models 
      stan_models <- lapply(1:length(dataModels), function(i) dataModels[[i]]$get.calculated_stan_object())
      baysis_models <- lapply(1:length(dataModels), function(i) dataModels[[i]]$get.selected_BAYSIS_stan_model())
      
      loo <- list()
      for(i in 1:length(stan_models)){
        loo_i <- tryCatch({
          loo::loo(stan_models[[i]], cores=min(4,dataModels[[i]]$get.number_cores()))},
          error=function(cond){
            showNotification("This model uses interaction levels that are not supported by your data and therefore be dropped.
                            Unfortunately, it is not possible to show results yet.", duration = 20, type="error")
            malfunction_report(code=malfunctionCode()$compareStanModels, msg="unused interaction levels",
                               type="warning", askForReport=T)
            return(NULL)
          }
        )
        if(is.null(loo_i)) return()
        dMID <- dataModels[[i]]$getDataModelInputData()
        
        respData <- dMID$getLongFormatVariable(dMID$getResponseVariable()$variable)
        loo_i$pointwise <- baysis_models[[i]]$jacobianCorrection(x=loo_i$pointwise, response=respData[[1]])
        
        loo[[dataModels[[i]]$get.name()]] <- loo_i
        setProgress(0.8*i/length(stan_models))
      }
      
      names(loo) <- lapply(1:length(stan_models), function(i) dataModels[[i]]$get.name())
      
      # result_loo <- loo::loo_compare(x = loo)
      result_loo <- tryCatch({
        loo::loo_compare(x = loo)},
        error=function(cond){
          showNotification("Can't compare these models.", duration = 20, type="error")
          return(NULL)
        }
      )
      if(is.null(result_loo)){
        return()
      }
      setProgress(0.9)
      
      
      loo_df <- as.data.frame(row.names = NULL, stringsAsFactors = F, as.matrix(result_loo))
      
      length <- length(loo_df[,1])
      row_names <- rownames(loo_df)
      
    
      link_names <- lapply(1:length, function(i) {
        as.character(
          actionLink(inputId = paste0("goToModel", i), label = row_names[i], 
                     onclick = paste0('Shiny.setInputValue(\"goToModel',i,'\", this.id, {priority: \"event\"})' )))
      })
      
      
      loo_df$model_name <- unlist(link_names, use.names = F)
      rownames(loo_df) <- NULL

      
      loo_df <- loo_df[,c(9,1,2,3,4,5,6,7,8)]
      
      #round
      loo_df[,-1] <- round(loo_df[,-1], 2)
      

      #to DT
      dt <- datatable(loo_df[,c(1,2,3)], escape = F, selection = "none", 
                      filter="none", options = list(dom = 't'))
      
      
      simple_loo_df <- loo_df
      simple_loo_df$model_name <- row_names
      
      dataModel$set.loo_result(list(path=dataModels[[1]]$get.dataPath(), loo=loo, 
                                    result_loo=result_loo, simple_loo = simple_loo_df[,c(1,2,3)], 
                                    dt=dt))
      
      setProgress(1)
    })
    
    
    # Enable compare buttons 
    shinyjs::enable(id = "compareModelsButtonCompareTwo")
    shinyjs::enable(id = "compareModelsButtonCompareResponse")
  }
  
  
  #Changes in loo object of dataModel
  observe({
    
    dataModel$dependReactiveValue("loo_result")
    
    isolate({
      
      loo <- dataModel$get.loo_result()
      currentLoo(loo)
      if(!is.null(loo)){
        row_names <- loo$simple_loo$model_name 
        lapply(seq_along(row_names), function(i) {
          observeEvent(input[[paste0("goToModel", i)]], {
            updateNavbarPage(session, "navbar", selected = "Model fitting")
            dataModel$setSelectedPIDM(row_names[i])
          })
        })
      }
    })
  })
  
  
  #Update loo
  observe({
    
    cLoo <- currentLoo()
    
    isolate({
      if(is.null(cLoo)){
        
        output$compareModelsTableResult <- DT::renderDataTable(NULL)
        output$compareModelsTableResultDescription <- renderUI(tags$div())
        
      }else{
        
        verbalOutput <- 
          paste0("<p>The <b>best model</b> is by default at the top of the list with an elpd_diff of 0.</p>",
                 "<p>The greater elpd_diff the worse the other model in comparison to the top model. ",
                 "If se_diff is about equal or greater than elpd_diff, the ranking of the models is not clear.</p>")
        
        
        
        addClass("reportModelComparison", "btn-primary")
        
        
        # show results
        output$compareModelsTableResult <- DT::renderDataTable(cLoo$dt)
        
        
        verify_loo <- verify_loo(cLoo$loo)
        output$compareModelsParetoDiagnostic <- renderPlot({
          plot_verification_loo(verify_loo)
        })
        
        if(verify_loo$summary[[2]] > 0){
          ins <- "some"
          if(verify_loo$summary[[2]] == 1) ins <- "one"
          if(verify_loo$summary[[2]] == verify_loo$summary[[1]]) ins <- "all"
          
          verbalOutput <- paste0(verbalOutput, "<p>The Pareto diagnostics shows problems in ", 
                                 verify_loo$summary[[2]]," of ",verify_loo$summary[[1]]," models. ",
                                 "The model comparison may <b>not</b> be reliable if there are problems indicated by the Pareto diagnostics..</p>",
                                 "<p>If the majority of datapoints are in 'good' and the rest in 'ok' or if a tiny fraction of datapoints are in 'bad', the model comparison can be used cautiously. ",
                                 "If there are more datapoints in 'bad' or 'very bad' the model comparison should not be used.</p>")
        }else{
          verbalOutput <- paste0(verbalOutput, "<p>The Pareto diagnostics shows no problems.</p>")
        }
        
        output$compareModelsTableResultDescription <- renderUI(HTML(verbalOutput))
        
      }
      
    })
  })
  
  
  
  ## Report
  observeEvent(input$reportModelComparison, {
    
    loo_result <- dataModel$get.loo_result()
    
    if(is.null(loo_result)){
      showNotification("Compare fits before.", type="warning")
      return()
    }
    
    
    ui <- tags$h5(style="margin:0px;", "Model comparison")
    
    #create table
    table <- datatable(loo_result[[4]], escape = F, selection = "none", 
                       filter="none", options = list(dom = 't'))
    
    
    verify_loo <- verify_loo(loo_result[[2]])
    plot <- plot_verification_loo(verify_loo)
    
    
    #create loo tex
    looLatex <- modelComparisonItem(loo_result, verify_loo)
    
    tEnum <- reportTypeEnum()
    
    reportDiv <- reportType(div=list(ui=ui, table=table, plot=plot))
    
    fileName <- dataModel$getDataModelInputData()$getCurrentDataPath()
    
    #Add loo element to report progress
    addItem(moduleType = "evaluation",
            dataModel_id=fileName, pDIM_id=-1,  pDIM_name = NULL,
            imgFile = paste0("Images/Report/ModelComparison.jpg"),
            type=tEnum$modelComparison, object=list(div=reportDiv, latex=looLatex), 
            show=T, singleton=F, global_reportProgressModel=global_reportProgressModel)
    
  })
  
  
  
  
  observeEvent(input$btnModelComparisonBack ,{
    updateNavbarPage(session, "navbar", selected = "Model fitting")
  })
  
  observeEvent(input$btnModelComparisonNext, {
    updateNavbarPage(session, "navbar", selected = "Effects / Predictions")
  })
}


## Update selectInput of fitted models
# The models are grouped by the used response variable
updateFittedCompareModels <- function(input, output, session, dataModel){

  res <- dataModel$get.perIterationDataModelNamesWithHash()$res
  
  list_names <- names(res)
  list_names_contemplable <- list()
  for(i in seq_along(list_names)){
    modelIds <- c()
    for(model_name in res[[list_names[i]]]){
      model <- dataModel$get.perIterationDataModel(name=model_name)
      modelIds <- c(modelIds, model$get.id())
    }
    modelIds <- paste0(modelIds, collapse=";")
    list_names_contemplable <- list.append(list_names_contemplable, modelIds, list_names[[i]])
  }
 
  res1 <- list.insert(res, "", 1, name = "Select first model")
  res2 <- list.insert(res, "", 1, name = "Select second model")
  list_names_contemplable <- list.insert(list_names_contemplable, "", 1, name = "Select response")
  
  updateSelectInput(session = session, inputId = "compareModelsSelectModelFirst", choices = res1)
  updateSelectInput(session = session, inputId = "compareModelsSelectModelSecond", choices = res2)
  updateSelectInput(session = session, inputId = "compareModelsSelectResponse", choices = list_names_contemplable)
}


#x list of loo objects
verify_loo <- function(x){
  res <- list()
  summary <- list(total=length(x), problems=0)
  
  df <- data.frame(good=rep(0,length(x)), ok=rep(0,length(x)),
                   bad=rep(0,length(x)), veryBad=rep(0,length(x)),
                   name=rep("",length(x)))
  
  for(i in 1:length(x)){
    par <- x[[i]]$diagnostics$pareto_k
    veryBad <- length(par[par >= 1])
    bad <- length(par[par >= 0.7 & par < 1])
    ok <- length(par[par >= 0.5 & par < 0.7])
    good <- length(par[par < 0.5])
    df[i,] <- c(good=good,ok=ok,bad=bad,veryBad=veryBad, names(x)[i])
    if(length(par[par > 0.5]) > 0) summary$problems <- summary$problems+1
  }
  
  res$df <- df
  res$summary <- summary
  return(res)
}

plot_verification_loo <- function(verify_loo){
  df <- verify_loo$df

  # res <- verify_loo(l, c("fit1","fit2"))
  # df <- res$df
  # df[2,] <- c(60,35,4,1,df[2,5])
  # df[1,5] <- "Ein ganz langer model fit name"
  ggdata <- tidyr::gather(df, key="category", "counts", -name)
  ggdata$category[ggdata$category=="veryBad"] <- "very bad"
  ggdata$category <- factor(ggdata$category, levels=c("very bad","bad","ok","good"))
  ggdata$counts <- as.numeric(ggdata$counts)
  
  ggplot(ggdata) + geom_bar(aes(x=name, y=counts, fill=category), stat="identity") +
    scale_fill_manual("", values=c("good"=BAYAS_COLORS$`--plotVerificationLoo-good`, 
                                   ok=BAYAS_COLORS$`--plotVerificationLoo-ok`,
                                   bad=BAYAS_COLORS$`--plotVerificationLoo-bad`, 
                                   'very bad'=BAYAS_COLORS$`--plotVerificationLoo-veryBad`)) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust=1),
          legend.position="bottom") +
    ggtitle("Pareto diagnostic")
}


