posterior_vs_prior.brmsfit <- function(object,
                                       pars = NULL,
                                       regex_pars = NULL,
                                       prob = 0.9,
                                       color_by = c("parameter", "vs", "none"),
                                       group_by_parameter = FALSE,
                                       facet_args = list(),
                                       ...) {

  
  stopifnot(isTRUE(prob > 0 && prob < 1))
  
  # stuff needed for ggplot
  color_by <- switch(
    match.arg(color_by),
    parameter = "parameter",
    vs = "model",
    none = NA
  )
  if (group_by_parameter) {
    group_by <- "parameter"
    xvar <- "model"
  } else {
    group_by <- "model"
    xvar <- "parameter"
  }
  aes_args <-
    list(
      x = xvar,
      y = "estimate",
      ymin = "lb",
      ymax = "ub"
    )
  if (!is.na(color_by))
    aes_args$color <- color_by
  if (!length(facet_args)) {
    facet_args <- list(facets = group_by)
  } else {
    facet_args$facets <- group_by
  }
  
  # draw from prior distribution and prepare plot data
  message("\nDrawing from prior...")
  capture.output(
    Prior <- suppressWarnings(update(
      object,
      sample_prior = "only"
    ))
  )
  objects <- nlist(Prior, Posterior = object)
  plot_data <-
    stack_estimates.brms(objects,
                    prob = prob,
                    pars = pars,
                    regex_pars = regex_pars)
  
  graph <-
    ggplot(plot_data, mapping = do.call("aes_string", aes_args)) +
    geom_pointrange(...) +
    do.call("facet_wrap", facet_args) +
    theme_default() +
    xaxis_title(FALSE) +
    yaxis_title(FALSE) +
    xaxis_ticks() +
    xaxis_text(angle = -30, hjust = 0) + 
    grid_lines(color = "gray", size = 0.1)
  
  if (group_by == "parameter")
    return(graph)
  
  # clean up x-axis labels a bit if tick labels are parameter names
  # (user can override this after plot is created if need be,
  # but this makes the default a bit nicer if many parameters)
  abbrevs <- abbreviate(plot_data$parameter, 12, method = "both.sides", dot = TRUE)
  graph + scale_x_discrete(name = "Parameter", labels = abbrevs)
}


# internal ----------------------------------------------------------------
stack_estimates.brms <-
  function(models = list(),
           pars = NULL,
           regex_pars = NULL,
           prob = NULL) {
    mnames <- names(models)
    if (is.null(mnames)) {
      mnames <- paste0("model_", seq_along(models))
    } else {
      has_name <- nzchar(mnames)
      if (!all(has_name))
        stop("Either all or none of the elements in 'models' should be named.")
    }
    
    alpha <- (1 - prob) / 2
    probs <- sort(c(0.5, alpha, 1 - alpha))
    labs <- c(paste0(100 * probs, "%"))
    ests <- lapply(models, function(x) {
      s <- NULL
      if(is.null(pars)){
        s <- summary(x$fit,
                              regex_pars = regex_pars,
                              probs = probs)$summary
      }else{
        s <- summary(x$fit,
                              pars = pars,
                              regex_pars = regex_pars,
                              probs = probs)$summary
      }

      if (is.null(pars))
        s <- s[!rownames(s) %in% c("lprior", "lp__"),]
      s[, labs, drop = FALSE]
    })
    est_column <- function(list_of_matrices, col) {
      x <- sapply(list_of_matrices, function(x) x[, col])
      if (is.list(x))
        unlist(x)
      else
        as.vector(x)
    }
    data.frame(
      model = rep(mnames, times = sapply(ests, nrow)),
      parameter = unlist(lapply(ests, rownames)),
      estimate = est_column(ests, labs[2]),
      lb = est_column(ests, labs[1]),
      ub = est_column(ests, labs[3])
    )
  }
