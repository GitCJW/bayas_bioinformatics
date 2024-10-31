malfunctionCode <- function(){
  list(incorrectBAYASFile="incorrectBAYASFile",
       compareStanModels="compareStanModels",
       missingReportItem="missingReportItem",
       pairsplot="pairsplot",
       pvpPlot="pvpPlot",
       makeStanPredictions="makeStanPredictions",
       distributions="distributions",
       planningFormula="planningFormula",
       emptyResonseSteps="emptyResonseSteps",
       emptyOVSteps="emptyOVSteps",
       emptyPredictorSteps="emptyPredictorSteps",
       parameterModal="parameterModal",
       ssdSampling="ssdSampling",
       ggplot="ggplot",
       emptySelectedItem="emptySelectedItem",
       creatingPDF="creatingPDF",
       reorderReportedItems="reorderReportedItems")
}

#askForReport: not yet implemented
malfunction_report <- function(code, msg=NULL, type = c("error","warning","info"), 
                               writeMail=T, askForReport=F){

  if(writeMail && !localUse){
    from <- "bayas@uni-due.de"
    subject <- paste0("BAYAS_error (", type, "): ",code)
    body <- paste0(msg)
    
    user <- readLines(file(paste0(pw_folder, "/email_auth.txt")))
    pwd <- readLines(file(paste0(pw_folder, "/email_auth_pw.txt")))
    
    mailControl <- list(host.name="mailout.uni-duisburg-essen.de",
                        port=587,
                        user.name=user, 
                        passwd=pwd,
                        ssl=T)
    send.mail(
      from=from, to=from, subject=subject, body=body,
      smtp=mailControl, authenticate =T
    )
  }
}

