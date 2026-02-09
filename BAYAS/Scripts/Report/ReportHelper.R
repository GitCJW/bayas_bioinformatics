# Enum for reported types of elements
reportTypeEnum <- function(inRecommendedOrder = T) {
  if(inRecommendedOrder){
    list(blank="blank", 
         planningExperiment = "planningExperiment",
         preplot="preplot", formula="formula", validation="validation", 
         previewppc="previewppc", ppc="ppc", mp="mp", mpSummary = "mpSummary", pairs="pairs", pvp="pvp", 
         priorpc="priorpc", effectMatrix = "effectMatrix", 
         singleEffect = "singleEffect", prediction = "prediction", 
         modelComparison="modelComparison")
  }else{
    list(blank="blank",
         planningExperiment = "planningExperiment",
         preplot="preplot", formula="formula", validation="validation", 
         previewppc="previewppc", ppc="ppc", mp="mp", mpSummary = "mpSummary", pairs="pairs", pvp="pvp", 
         priorpc="priorpc", effectMatrix = "effectMatrix", 
         singleEffect = "singleEffect", prediction = "prediction", 
         modelComparison="modelComparison")
  }
}

reportSubTypeEnum <- function() {
  list(formula_horseshoe="formula_horseshoe", 
       formula_horseshoe_normal_noise="formula_horseshoe_normal_noise")
}

#Either a div, plot or DT 
transposeReportedElementToDiv <- function(object, objectType){
  tNum <- reportTypeEnum()
  
  if(!objectType %in% tNum) return(NULL)
  
  return(object$div)
}


transposeReportedElementToPDF <- function(object, objectType, model_latex=F){
  tNum <- reportTypeEnum()
  if(!objectType %in% tNum) return(NULL)
  
  ret <- ""
  if(model_latex && !is.null(object$model_latex)){
    ret <- object$model_latex
  }
  
  return(paste0(ret,object$latex))
}

getItemDescription <- function(objectType){
  tNum <- reportTypeEnum()
  if(!objectType %in% tNum) return(NULL)
  if(objectType == tNum$formula){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Formula.txt")))
  }else if(objectType == tNum$ppc){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/PPC.txt")))
  }else if(objectType == tNum$pairs){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Pairs.txt")))
  }else if(objectType == tNum$previewppc){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/PPC.txt")))
  }else if(objectType == tNum$pvp){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/PVP.txt")))
  }else if(objectType == tNum$priorpc){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/PriorPC.txt")))
  }else if(objectType == tNum$mp){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/MP.txt")))
  }else if(objectType == tNum$mpSummary){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/MPSummary.txt")))
  }else if(objectType == tNum$modelComparison){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/ModelComparison.txt")))
  }else if(objectType == tNum$effectMatrix){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/EffectMatrix.txt")))
  }else if(objectType == tNum$singleEffect){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/SingleEffect.txt")))
  }else if(objectType == tNum$prediction){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Prediction.txt")))
  }else if(objectType == tNum$planningExperiment){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Planning/head.txt")))
  }else{
    return("")
  }
}

getSubItemDescription <- function(subType){
  tSubNum <- reportSubTypeEnum()
  if(!subType %in% tSubNum) return(NULL)
  if(subType == tSubNum$formula_horseshoe){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Formula_horseshoe_prior.txt")))
  }else if(subType == tSubNum$formula_horseshoe_normal_noise){
    return(readTxtFile(paste0(report_folder,"/GeneralTex/Formula_Horseshoe_prior_normal_noise.txt")))
  }
}

elementSectionMapping <- function(objType){
  tNum <- reportTypeEnum()
  if(objType == tNum$formula) objType <- "Formula"
  if(objType == tNum$preplot) objType <- "Visualization of input data"
  if(objType == tNum$ppc) objType <- "Posterior predictive check"
  if(objType == tNum$previewppc) objType <- "Posterior predictive check"
  if(objType == tNum$validation) objType <- "Model validation"
  if(objType == tNum$pairs) objType <- "Pairs plot"
  if(objType == tNum$pvp) objType <- "Prior vs posterior"
  if(objType == tNum$priorpc) objType <- "Prior predictive check"
  if(objType == tNum$mp) objType <- "Marginal posteriors"
  if(objType == tNum$mpSummary) objType <- "Summary of marginal posteriors"
  if(objType == tNum$modelComparison) objType <- "Model comparison"
  if(objType == tNum$effectMatrix) objType <- "Model effects"
  if(objType == tNum$singleEffect) objType <- "Model single effects"
  if(objType == tNum$prediction) objType <- "Model prediction"
  if(objType == tNum$planningExperiment) objType <- "Planning of experiment"
  
  return(objType)
}


#div: list of named elements that are shown in the report view
#valid names: "ui", "table", "plot". The order of elements is also used.
reportType <- function(div, space=NULL){
  if(any(!names(div) %in% c("table", "plot", "ui"))) stop("Invalid order elements")

  if(!is.null(space)){
    if(length(space)+1 != length(div)) stop("space must have length of order -1.")
    div$space <- space
  }else{
    div$space <- rep("20", length(div)-1)
  }
  return(div)
}




#Called directly when report button is clicked
transposeFormulaToPDF <- function(pIDM){
  
  latex <- ""
  try({

    latex <- "\\begin{equation}\n\t\\begin{split}\n"
    
    stanModel <- pIDM$get.selected_BAYSIS_stan_model()
    dMID <- pIDM$getDataModelInputData()
    responseName <- dMID$getResponseVariable()$variable
    responseName <- str_replace_all(responseName, "_","\\\\_")
    formula_elements <- stanModel$new_formula(responseName)
    modelParameters <- stanModel$used_parameter
    
    modelPred <- stanModel$used_predictor
    
    
    #Iterate through "FormulaElements" objects
    for(element in formula_elements){
      
      # & before =\~ for alignment
      math <- paste0(element$latex$left, " &",element$latex$center)
      if(element$predictorLine){
        index <- 1
        for(pred in stanModel$get_predictor_line()){
          para <- pred$modelParameter
          mathString <- ""
          #e.g. intercept that is displayed by just the parameter (b0)
          if(is.null(pred$display_name)){
            if(index > 1){
              mathString <- paste0(mathString, " + ")
            }
            mathString <- paste0(mathString, para$getFullDisplayNameLatex())
            
          }else{
            a <- pred$display_name
            a <- str_replace_all(a, "&sdot;", " \\\\cdot ")
            
            if(index > 1){
              #right side of = ~ ... with more than one term
              mathString <- paste0(" + ", para$getFullDisplayNameLatex(), " \\cdot ", a)
            }else{
              #right side of = ~ ... with just one term
              mathString <- paste0(para$getFullDisplayNameLatex(), " \\cdot ", a)
            }
          }
          math <- paste0(math, " ", mathString)
          index <- index+1
        }
      }else{
        math <- paste0(math, " " ,element$latex$right)
      }
      latex <- paste0(latex, "\t\t", math, "\\\\ \n")
    }
    
    #Iterate through parameters to get possible vectors
    latexVectors <- ""
    for(para in modelParameters){
      if(para$is.vector){
        userVar <- para$userVariable
        dists <- para$distributions
        dataX <- dists[[1]]$dataX
        paraName <- para$getFullDisplayNameLatex()
        
        #If the distributions are all horseshoes there is no need to write the parameterization for each parameter,
        #as they must all be the same
        distSingleParameterization <- sapply(dists, function(dist){dist$singleParameterization})
        if(all(distSingleParameterization)) next
        
        auxLatexName <- dists[[1]]$getAuxParametersLatex(para$id)
        values <- list()
        for(dist in dists){
          for(auxParaIndex in 1:length(dist$auxParameter)){
            auxPara <- dist$auxParameter[[auxParaIndex]]
            if(length(values) < auxParaIndex){
              values[[auxParaIndex]] <- auxPara$getValueFormatted()
            }else{
              values[[auxParaIndex]] <- c(values[[auxParaIndex]],auxPara$getValueFormatted())
            }
          }
        }
        if(length(values) != length(auxLatexName)) stop("Have to be of equal length")
        
        pred <- para$getParentPredictor()
        paraElements <- pIDM$get.selected_BAYSIS_stan_model()$getTermCombinationsOfPredictorList(pred)
        paraElementsString <- paste0(paraElements, collapse=" ,\\allowbreak ")
        latexVectors <- paste0(latexVectors, "$",paraName, " = ",
                               "( ", paraElementsString, " ) \\\\ $ \n")
        for(i in 1:length(auxLatexName)){
          vals <- values[[i]]
          #If a predictor had once more elements due to missing intercept, 
          #thus elements are still stored in the parameter and have to be ignored --> 1:length(paraElements)
          vals <- vals[1:length(paraElements)] 
          latexAux <- paste0(vals, collapse=" ,\\allowbreak" )
          latexVectors <- paste0(latexVectors, "$",auxLatexName[[i]], " = ",
                                 "( ", latexAux, " ) \\\\ $ \n")
        }
      }
    }
    
    latex <- paste0(latex, "\t\\end{split}\n\\end{equation}\n", latexVectors)
  })
  
  
  #Are there additional information e.g. definition of the horseshoe prior?
  latexHorseshoe <- horseshoePriorUse(pIDM)
  
  
  paste0(latex,latexHorseshoe)
}


horseshoePriorUse <- function(pIDM){

  usesHorseshoe <- F
  
  dEnum <- distributionEnum()
  
  stanModel <- pIDM$get.selected_BAYSIS_stan_model()
  modelParameters <- stanModel$used_parameter

  for(para in modelParameters){
    dists <- list(para$distribution)
    if(para$is.vector) dists <- para$distributions
    
    distNames <- sapply(dists, function(dist){dist$name})
    if(any(distNames==dEnum$Horseshoe)){
      usesHorseshoe <- T
      break
    }
  }
  
  if(usesHorseshoe){
    tSubNum <- reportSubTypeEnum()
    #Normal or log-normal model
    if(stanModel$id %in% c(1,8)){
      return(getSubItemDescription(tSubNum$formula_horseshoe_normal_noise))
    }else{
      return(getSubItemDescription(tSubNum$formula_horseshoe))
    }
  }
}


toLatex <- function(left, center, right){
  center <- str_replace(center, "~", "\\\\sim")
  return(list(left=left, center=center, right=right))
}

asFormulaLine <- function(left=NULL, center=NULL, right=NULL, list=NULL){
  if(!is.null(list)){
    left <- list$left
    center <- list$center
    right <- list$right
  }
  ret <- paste0(left, center, " &\\: ", right, " \\\\ ")
  return(ret)
}

plotToTex <- function(plotId, plot, caption=NULL, label=NULL, ignoreTex=F){
  plotFile <- paste0(report_folder, "/Images/",plotId,".pdf")
  
  grDevices::cairo_pdf(filename=plotFile, width=5.5, height=5.5, fallback_resolution=300)
  print(plot)
  dev.off()
  # ggsave(plotFile, plot, device="pdf", width=5.5, height=5.5, dpi=300)
  
  if(ignoreTex) return()
  
  tex <- paste0("\\begin{figure}[H] \n")
  tex <- paste0(tex, "\\centering \n")
  tex <- paste0(tex, "\\includegraphics{",plotFile,"}")
  if(!is.null(caption)) tex <- paste0(tex, "\\caption{", caption, "} \n")
  if(!is.null(label)) tex <- paste0(tex, "\\label{", label, "} \n")
  tex <- paste0(tex, "\\end{figure} \n")
  
  return(tex)
}

formulaParameterToLatex <- function(word){
  if(is.null(word) || is.empty(word)) return(word)
  
  if(length(word) > 1){
    ret <- sapply(word, formulaParameterToLatex)
    return(ret)
  }else{
 
    word_rep <- str_replace(word, "\\.\\.", ":")
    word_split <- str_split(word_rep, "_")[[1]]
    
    if(length(word_split) > 2 && localUse) browser()
    
    if(length(word_split) > 1){
      word_rep <- paste0(word_split[1], "_{")
      for(i in 2:length(word_split)){
        word_rep <- paste0(word_rep, word_split[i], " ")
      }
      word_rep <- paste0(word_rep, "}")
    }else{
      word_rep <- paste0("\\",word)
    }
    word_rep <- paste0("$",word_rep,"$")
    return(word_rep)
  }
}

wordToLatexConform <- function(word){
  word <- str_replace_all(word, "%", "\\\\%")
  word <- str_replace_all(word, "_", "\\\\_")
  word <- str_replace_all(word, "<b>", "")
  word <- str_replace_all(word, "</b>", "")
  return(word)
}

wordToLatexUnderline <- function(word){
  return(paste0("\\underline{", word, "}"))
}

wordToLatexItalic <- function(word, wordToLatexConform=F){
  if(wordToLatexConform) word <- wordToLatexConform(word)
  return(paste0("\\textit{", word, "}"))
}

wordToLatexBold <- function(word, wordToLatexConform=F){
  if(wordToLatexConform) word <- wordToLatexConform(word)
  return(paste0("\\textbf{", word, "}"))
}

dfToLatexTable <- function(df, caption=NULL, label=NULL){
  ce <- paste0(rep("c", dim(df)[2]), collapse = " ")
  latex <- paste0("\\begin{table}[htbp] \n")
  latex <- paste0(latex, " \\begin{center} \n  \\begin{tabular}{||", ce, "||}\n  \\hline \n")
  
  # header
  latex <- paste0(latex, "  ", paste0(wordToLatexConform(colnames(df)), collapse=" & "), "\\\\ \n")
  latex <- paste0(latex, "  \\hline \\hline \n")
  
  colFactors <- sapply(1:ncol(df), function(i) is.factor(df[[i]]))
  if(any(colFactors)) df[,colFactors] <- sapply(df[,colFactors], as.character)
  # data
  data <- sapply(seq_len(dim(df)[1]), function(i){
    paste0("  ",paste0(wordToLatexConform(df[i,]), collapse=" & "), "\\\\ \n  \\hline \n")
  })
  latex <- paste0(latex, paste0(data, collapse=""))
  
  
  latex <- paste0(latex, "  \\end{tabular} \n \\end{center}\n \\caption{", caption, "}\n")
  if(!is.null(label)) latex <- paste0(latex, " \\label{", label, "}\n")
  latex <- paste0(latex, "\\end{table}\n")
  return(latex)
}


modelValidationItem <- function(iterationDataModel, ppc_res, imageOutputFolder){
  
  #check
  #0: check
  #1: warning
  #2: error
  
  sampled_model <- iterationDataModel$get.calculated_stan_object()

  stanfit <- extract_stanfit(sampled_model)
  
  monitor <- invisible(monitor(stanfit))
  sampler_params <- get_sampler_params(stanfit, inc_warmup = F)
  div_trans <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  div_trans_max <- floor(iterationDataModel$get.number_chains()*iterationDataModel$get.number_iterations()/2)
  treedepth_max <- iterationDataModel$get.max_treedepth()
  treedepth <- sum(sapply(sampler_params, function(x) ifelse(x[, "treedepth__"] == treedepth_max,1,0)))
  seed <- iterationDataModel$get.seed()
 
  res_list <- list()
  
  
  ## Content for checklist
  # ESS
  n_eff_min <- iterationDataModel$get.number_chains()*100
  n_eff_bulk <- monitor$Bulk_ESS[monitor$Bulk_ESS < n_eff_min]
  n_eff_tail <- monitor$Tail_ESS[monitor$Tail_ESS < n_eff_min]
  
  check <- 0
  if(length(n_eff_bulk) > 0){
    check <- 2
  }else if(length(n_eff_tail) > 0){
    check <- 1
  }
  
  res_list$ess_bulk <- c(length(n_eff_bulk), length(monitor$Bulk_ESS))
  res_list$ess_tail <- c(length(n_eff_tail), length(monitor$Tail_ESS))
  res_list$checks <- check

  #Rhat
  rhat <- monitor$Rhat
  rhat_sub <- rhat[c(1:(length(rhat)-2))]
  rhat_sub_err <- rhat[rhat_sub>1.01]
  rhat_sub_err2 <- rhat[rhat_sub>1.05]
  
  res_list$rhat_sub_err <- c(length(rhat_sub_err), length(rhat))
  res_list$rhat_sub_err2 <- c(length(rhat_sub_err2), length(rhat))
  
  check <- 0
  if(length(rhat_sub_err2) > 0){ #greater than 1.05
    check <- 2
  }else if(length(rhat_sub_err) > 0){
    if(length(rhat_sub_err)/length(rhat_sub) >= 0.1){ # more then 10% from parameter
      check <- 2
    }else{
      check <- 1
    }
  }
  res_list$checks <- c(res_list$checks,check)
  
  
  
  #Divergent transitions
  check <- 0
  if(div_trans/div_trans_max >= 0.01){
    check <- 2
  }else if(div_trans/div_trans_max > 0){
    check <- 1
  }
  res_list$div_trans <- div_trans/div_trans_max
  res_list$checks <- c(res_list$checks,check)
  
  
  #Treedepth
  check <- 0
  if(treedepth/div_trans_max >= 0.05){
    check <- 2
  }else if(treedepth/div_trans_max > 0){
    contentMessage4 <- paste0(treedepth, " of ", div_trans_max , " exceeded the maximum tree depth of ", treedepth_max ,"!")
    check <- 1
  }
  res_list$treedepth <- c(treedepth,div_trans_max,treedepth_max)
  res_list$checks <- c(res_list$checks,check)
  
  
  #PPC
  check <- 0
  if(length(ppc_res[ppc_res>0]) > 20){
    check <- 2
  }else if(length(ppc_res[ppc_res>0]) > 10){
    check <- 1
  }
  res_list$checks <- c(res_list$checks,check)
  res_list$checks <- c(res_list$checks,0)

  
  tex <- paste0("Bayesian inference is done with \\href{https://www.mc-stan.org}{Stan} ",
                "running Hamiltonian Markov chain Monte Carlo on ", 
                iterationDataModel$get.number_chains() ,
                " chains, each with ",iterationDataModel$get.number_iterations(),
                " iterations. ")
  
  if(res_list$rhat_sub_err[1] == 0 &&
     res_list$ess_bulk[1] == 0 &&
     res_list$ess_tail[1] == 0 &&
     res_list$div_trans[1] == 0){
    
    tex <- paste0(tex,"The diagnostic values look good, i.e.\\ there are no obvious problems with these runs.\n\n")
  }
  
  
  tex <- paste0(tex,
                "The $\\hat{R}$ (R-hat) value compares compares the between- and within-chain variances for model parameters. ",
                "For reliable results we need R-hat values of $<1.05$ (or better $<1.01$). ")

  if(res_list$rhat_sub_err[1] == 0){
    tex <- paste0(tex, 
                  "In the current run, all R-hat values are $<1.01$. ")
  }else if(res_list$rhat_sub_err[1] - res_list$rhat_sub_err2[1] > 0){
    diff <- res_list$rhat_sub_err[1] - res_list$rhat_sub_err2[1]
    tex <- paste0(tex, 
                  diff, " of ", res_list$rhat_sub_err[2],
                  "parameters have a R-hat value greater $1.01$, but smaller $1.05$. ")
  }else{
    tex <- paste0(tex, 
                  res_list$rhat_sub_err2[1], " of ", res_list$rhat_sub_err[2],
                  "parameters have a R-hat value greater $1.05$, ",
                  "indicating that these parameters didn't converge and samples may not reliable. ")
  }
  
  
  tex <- paste0(tex,
                "To avoid misleading results due to (high) correlation within samples, 
                estimated Effective Sample Size (ESS) for bulk (mean and median) ",
                "and tail (for variance and quantile) should be at least 100 per ",
                "Markov chain. ")
  
  if(res_list$ess_bulk[1] > 0){
    tex <- paste0(tex,
                  res_list$ess_bulk[1], " of ", res_list$ess_bulk[2], " parameters " ,
                  "do not fulfill the criterion for bulk-ESS, and ")
  }else{
    tex <- paste0(tex,
                  "All parameters fulfill this criterion for bulk-ESS, and ")
  }
  
  if(res_list$ess_bulk[1] > 0){
    tex <- paste0(tex,
                  res_list$ess_bulk[1], " of ", res_list$ess_bulk[2], " parameters " ,
                  "do not fulfill the criterion for tail-ESS. ")
  }else{
    tex <- paste0(tex,
                  "all parameters fulfill this criterion for tail-ESS. ")
  }
  
  tex <- paste0(tex,
                "\nDivergent transitions occur, when the target distribution could not ",
                "be searched properly. A small number of divergent transition showing ", 
                "no pattern may be ignored when R-hat and ESS are good. ")
  
  if(res_list$div_trans > 0){
    tex <- paste0(tex,
                  res_list$div_trans[1], " of ", res_list$treedepth[2], " iterations " ,
                  "ended in divergence. ")
  }else{
    tex <- paste0(tex,
                  "In our run there were no divergent transitions. ")
  }
  
  if(iterationDataModel$get.seedByUser()){
    tex <- paste0(tex, "\n\n The rstanarm call used a setted seed by the user of ", seed, ".")
  }else{
    tex <- paste0(tex, "\n\n The rstanarm call used a random seed of ", seed, ".")
  }
  


  #Create thumbnail image
  png(imageOutputFolder, width=100, height=96)
  img_check <- readPNG(paste0(image_folder, "/Report/list_check_", GLOBAL_THEME,".png"))
  img_warning <- readPNG(paste0(image_folder, "/Report/list_warning_", GLOBAL_THEME,".png"))
  img_error <- readPNG(paste0(image_folder, "/Report/list_error_", GLOBAL_THEME,".png"))
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:6, ncol=1, byrow=TRUE))
  
  for(i in res_list$checks){
    plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",xaxs = 'i',yaxs='i')
    if(i==0){
      rasterImage(img_check,0,0,1,1)
    }else if(i==1){
      rasterImage(img_warning,0,0,1,1)
    }else if(i==2){
      rasterImage(img_error,0,0,1,1)
    }else{
     stop("error, wrong number") 
    }
  }
  dev.off()
  return(tex)
}

modelComparisonItem <- function(loo_result, verify_loo){
  
  tex <- "\\begin{center}\n\\begin{tabular}{ c | c | c }\n"
  tex <- paste0(tex, "Model name & $\\Delta$elpd & se\\textsubscript{$\\Delta$elpd} \\\\ \n \\hline \n")
  
  df <- loo_result[[4]]
  for(i in 1:dim(df)[1]){
    tex <- paste0(tex, wordToLatexConform(paste0(df[i,], collapse=" & ")), " \\\\ \n")
  }
  tex <- substr(tex, 1, nchar(tex)-1)
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  
  # Table for loo pareto diagnostics
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{ c | c | c | c | c}\n")
  tex <- paste0(tex, "Model name & good & ok & bad & very bad \\\\ \n \\hline \n")
  
  df <- verify_loo$df
  df <- df[,c(5,1,2,3,4)]
  for(i in 1:dim(df)[1]){
    tex <- paste0(tex, wordToLatexConform(paste0(df[i,], collapse=" & ")), " \\\\ \n")
  }
  tex <- substr(tex, 1, nchar(tex)-1)
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  return(tex)
}

modelEffectMatrix <- function(df, type, hdiType, hdiRange, headerStuff, numericVal){
  tex <- ""
  tableCaption <- paste0("The table shows the ", type, " value (on a ", hdiRange*100, "\\% ", toupper(hdiType), "). ")
  if(!is.null(numericVal) && length(numericVal) > 0){
    tableCaption <- paste0(tableCaption, "This effect depends on concrete values for ", 
                           paste0(names(numericVal),collapse=", "),
                           " with: ")
    for(i in 1:length(numericVal)){
      tableCaption <- paste0(tableCaption, names(numericVal)[i],": ", numericVal[[i]])
      if(i < length(numericVal)) tableCaption <- paste0(tableCaption,"; ")
    }
    tableCaption <- paste0(tableCaption,".")
  }

  tex <- paste0(tex,"\\begin{table}[ht] \n \\begin{center} \n")
  
  #print header
  diagonalHeader <- headerStuff$headerNames
  multicolumnHeader <- headerStuff$headers
  
  tableX <- dim(df)[2]
  

  tex <- paste0(tex, "\\begin{tabular}{", 
                paste0(rep("c",length(diagonalHeader)),collapse=""), "|",
                paste0(rep("c",tableX-length(diagonalHeader)),collapse=""),
                "}\n")
  
  #header
  posOfVLine <- c()
  for(i in 1:length(diagonalHeader)){
    tex <- paste0(tex, paste0(rep("&", (i-1)), collapse=""), 
                  " ", diagonalHeader[i], " ",
                  paste0(rep("&", (length(diagonalHeader)-i)), collapse=""))
    sum <- 0
    for(j in 1:length(multicolumnHeader[[i]])){
      headerName <- names(multicolumnHeader[[i]])[j]
      colmnWidth <- as.numeric(multicolumnHeader[[i]][j])
      if(i == 1 && j < length(multicolumnHeader[[i]])){
        posOfVLine <- c(posOfVLine, sum+colmnWidth)
      }
      sum <- sum+colmnWidth
      pipe <- ""
      if(sum %in% posOfVLine && length(diagonalHeader) > 1) pipe <- "|"
      tex <- paste0(tex," & \\multicolumn{",colmnWidth,"}{c", pipe,"}{",headerName,"}")
      
    }
    tex <- paste0(tex, " \\\\ \n")
  }
  tex <- paste0(tex, "\\hline \n")
  
  
  #rows containing values
  for(i in 1:dim(df)[1]){
    if(i %% 2 == 1) tex <- paste0(tex, "\\rowcolor{black!03} \n")
    tex <- paste0(tex, paste0(df[i,], collapse=" & "), " \\\\ \n")
    if(i < dim(df)[1]) tex <- paste0(tex, "\\arrayrulecolor{black!20} \\hline \\arrayrulecolor{black} \n")
  }
  
  
  tex <- paste0(tex, "\\end{tabular} \n \\caption{",tableCaption,"}\n", "\\end{center} \n \\end{table} \n")
  
  return(tex)
}

modelEffectItem <- function(summaryTable, varsHeader, hdiType, hdiRange, 
                            plotId, plot, numericVal){
  
  # The effect on the response weight of changing from predictor value male to predictor value female \{remark DaHo: CHECK!\}. 
  # the group elements male and female is shown using a 90\% highest density interval (HDI) 

  # This effect depends on the value of response (here: on the weight). 
  # In this case the weight is 19.04 (in same unit as response data).} 
  
  df <- summaryTable$x$data
  tex <- ""
  
  varsHeader <- wordToLatexConform(varsHeader)
  if(length(unique(varsHeader))==1) varsHeader <- varsHeader[1]
  
  #types: c("Group difference", "Slope", "Slope difference")
  type <- df$type

  
  #Plot caption
  plotCaption <- ""
  # plotCaption <- paste0("The effect on the response ", weight)
  
  if(type=="Slope difference"){
    plotCaption <- paste0(plotCaption, "The effect between the slopes ", paste0(varsHeader, collapse=" and "),
                  " is shown using ")
  }else if(type=="Slope"){
    plotCaption <- paste0(plotCaption, "The effect of the slope ", varsHeader, " is shown using ")
  }else if(type=="Group difference"){
    plotCaption <- paste0(plotCaption, "The effect between the group elements ", paste0(varsHeader, collapse=" and "),
                  " is shown using ")
  }else{ #Should never occur
    plotCaption <- paste0(plotCaption, "The effect between ", paste0(varsHeader, collapse=" and "),
                  " is shown using ")
  }
  
  plotCaption <- paste0(plotCaption, "a ", hdiRange*100, "\\%")
  
  if(hdiType == "hdi"){
    plotCaption <- paste0(plotCaption, " highest density interval (HDI) enclosed by the two outer bars.")
  }else if(hdiType == "eti"){
    plotCaption <- paste0(plotCaption, " equal-tailed interval (ETI) enclosed by the two outer bars. ")
  }
  plotCaption <- paste0(plotCaption, " The mid bar shows the median. ")
  
  if(!is.null(numericVal) && length(numericVal)>0){
    plotCaption <- paste0(plotCaption, "This effect depends on concrete values for ", 
                           paste0(names(numericVal),collapse=", "),
                           " with: ")
    for(i in 1:length(numericVal)){
      plotCaption <- paste0(plotCaption, names(numericVal)[i],": ", numericVal[[i]])
      if(i < length(numericVal)) plotCaption <- paste0(plotCaption,"; ")
    }
    plotCaption <- paste0(plotCaption,".")
  }
  
  #Plot
  tex <- paste0(tex,"\n", plotToTex(plotId, plot, caption=plotCaption),"\n")
  

  #Table
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{ c | c | c | c | c | c }\n")
  tex <- paste0(tex, "Type & CI\\_low & median & mean & CI\\_high & pi \\\\ \n \\hline \n")
  
  tex <- paste0(tex, paste0(paste0(df[1,], collapse=" & ")), "\\\\ \n" )
  
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  return(tex)
}

modelPredictionItem <- function(summaryTable, valueTable, color, plotType,
                                postType, hdiType, hdiRange, plotId, plot){

  
  plotCaption <- ""
  
  meanCaption <- ""
  if(postType=="mean"){
    meanCaption <- "of the expectation value "
  }
  
  if(plotType=="Violin"){
    plotCaption <- paste0("Violins are distributions ",meanCaption,
                          "of predictions (y-axis) for given predictor values (x-axis). ",
                          "Dots are medians of predictions, intervals (shaded) are ", 
                          hdiRange*100 , "\\% ", toupper(hdiType), 
                          " of prediction distributions. \n")
  }else{
    plotCaption <- paste0("Shown are distributions ",meanCaption,
                          "of predictions  for given predictor values (color). ",
                          "Outer bars enclose ", 
                          hdiRange*100 , "\\% ", toupper(hdiType), 
                          " interval and mid bar shows the median. \n")
  }

  tex <- ""

  #Define colors
  for(i in seq_len(length(color))){
    c <- color[i]
    c <- substr(c,2,nchar(c))
    tex <- paste0(tex, "\\definecolor{BAYASColor",i,"}{HTML}{", c,"}\n" )
  }
  
  
  #Value table
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{", paste0(rep("c",dim(valueTable)[2]-1), collapse=" | ") ,"}\n")
  cnames <- colnames(valueTable)
  cnames <- cnames[1:(length(cnames)-1)]
  cnames <- wordToLatexConform(cnames)
  tex <- paste0(tex, paste0(cnames, collapse = " & "))
  tex <- paste0(tex, " \\\\ \n \\hline \n")
  
  for(i in seq_len(dim(valueTable)[1])){
    tex <- paste0(tex, paste0(paste0(valueTable[i,1:(dim(valueTable)[2]-2)], collapse=" & ")))
    tex <- paste0(tex, " & \\begin{tikzpicture} \n \\fill[BAYASColor", i,"] (0,0) rectangle (.2,.2); \n \\end{tikzpicture} \n")
    tex <- paste0(tex, "\\\\ \n" )
  }
  
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  
  
  #plot
  tex <- paste0(tex,"\n", plotToTex(plotId, plot, plotCaption),"\n")
  
  
  
  #summary table
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{", paste0(rep("c",dim(summaryTable)[2]), collapse=" | ") ,"}\n")
  cnames <- colnames(summaryTable)
  cnames <- cnames[1:(length(cnames))]
  cnames <- wordToLatexConform(cnames)
  tex <- paste0(tex, paste0(cnames, collapse = " & "))
  tex <- paste0(tex, " \\\\ \n \\hline \n")
  
  for(i in seq_len(dim(summaryTable)[1])){
    tex <- paste0(tex, paste0(paste0(summaryTable[i,1:(dim(summaryTable)[2]-1)], collapse=" & ")))
    tex <- paste0(tex, " & \\begin{tikzpicture} \n \\fill[BAYASColor", i,"] (0,0) rectangle (.2,.2); \n \\end{tikzpicture} \n")
    tex <- paste0(tex, "\\\\ \n" )
  }
  
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  
  return(tex)
}

modelPredictionDifferenceItem <- function(summaryTable, valueTable, color,
                                postType, hdiType, hdiRange, plotId, plot){
  
  
  
  plotCaption <- ""
  if(postType=="mean"){
    plotCaption <- paste0(plotCaption, "Distribution of the difference using a ",
                          100*hdiRange, "\\% ", toupper(hdiType), ".\n",
                          "Difference of: ", valueTable[2,1]," - ", valueTable[1,1])
  }

  
  tex <- ""
  
  #Value table
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{", paste0(rep("c",dim(valueTable)[2]), collapse=" | ") ,"}\n")
  cnames <- colnames(valueTable)
  cnames <- wordToLatexConform(cnames)
  tex <- paste0(tex, paste0(cnames, collapse = " & "))
  tex <- paste0(tex, " \\\\ \n \\hline \n")
  
  for(i in seq_len(dim(valueTable)[1])){
    tex <- paste0(tex, paste0(paste0(valueTable[i,1:dim(valueTable)[2]], collapse=" & ")))
    tex <- paste0(tex, "\\\\ \n" )
  }
  
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  
  
  #Plot
  tex <- paste0(tex,"\n", plotToTex(plotId, plot, plotCaption),"\n")
  
  
  #Summary table
  tex <- paste0(tex, "\\begin{center}\n\\begin{tabular}{", paste0(rep("c",dim(summaryTable)[2]), collapse=" | ") ,"}\n")
  cnames <- colnames(summaryTable)
  cnames <- wordToLatexConform(cnames)
  tex <- paste0(tex, paste0(cnames, collapse = " & "))
  tex <- paste0(tex, " \\\\ \n \\hline \n")
  
  for(i in seq_len(dim(summaryTable)[1])){
    tex <- paste0(tex, paste0(paste0(summaryTable[i,1:dim(summaryTable)[2]], collapse=" & ")))
    tex <- paste0(tex, "\\\\ \n" )
  }
  
  tex <- paste0(tex, "\\hline \n")
  tex <- paste0(tex, "\\end{tabular}\n\\end{center}\n")
  

  return(tex)
}


