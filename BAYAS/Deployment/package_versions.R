cat(paste(
  sprintf(
    "'%s'='%s'",
    names(sessionInfo()$otherPkgs),
    sapply(sessionInfo()$otherPkgs, `[[`, "Version")),
  collapse = ", \\ \n"))
