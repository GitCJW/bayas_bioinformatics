# plot 
#data is a list
plotSeveralAreas <- function(data, countData=F, prop=0.9, x_axis, method="eti", colors){
  
  minX <- Inf
  maxX <- -Inf
  for(i in 1:length(data)){
    subData <- data[[i]]
    minX <- min(minX, subData$x)
    maxX <- max(maxX, subData$x)
  }
  plot <- ggplot()
  if(!countData) plot <- plot + xlim(minX,maxX)
  for(i in 1:length(data)){
    subData <- data[[i]]
    
    approx <- approxfun(x=subData$x,y=subData$density, yleft=0, yright=0)
    approx_y <- approx(subData$x)
    
    #Could be NULL
    interval <- ciOfDens(subData, prop, method)
    
    crp.rg <- colorRampPalette(c("grey",colors[i]))
    subColor <- crp.rg(7)[c(3,4,6,7)]
    
    if(!countData){

      gData <- data.frame(x=subData$x, y=approx_y)
      
      plot <- plot +
        geom_line(data=gData, mapping=aes(x=x,y=y), color=subColor[4]) +
        geom_area(data=gData, mapping=aes(x=x,y=y), alpha=0.5, fill=subColor[1]) +
        yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE)
      
      plot +
        geom_line(data=gData, mapping=aes(x=x,y=y), color=subColor[4]) +
        # geom_area(data=gData, mapping=aes(x=x,y=y), alpha=0.5, fill=subColor[1]) +
        # yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE) +
        scale_x_continuous(trans="log")
      
      if(!is.null(interval)){
        plot <- plot +
          geom_segment(data=interval,aes(x=center,y=0,xend=center,yend=centerPointDens), color=subColor[3], lineWidth=1) +
          geom_segment(data=interval,aes(x=lower,y=0,xend=lower,yend=lowerDens), color=subColor[3], linewidth=1) +
          geom_segment(data=interval,aes(x=upper,y=0,xend=upper,yend=upperDens), color=subColor[3], linewidth=1) 
      }
      
    }else{
      
       plot <- plot +
          geom_bar(subData, mapping=aes(x=x, y=density), stat="identity", fill=subColor[1], alpha=0.5) +
          yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE)
       
       if(!is.null(interval)){
         intervals <- data.frame(x=c(floor(interval$lower), round(interval$center), ceiling(interval$upper)),
                                 y=c(interval$lowerDens,interval$centerPointDens, interval$upperDens))
         
         intervalData <- subData
         
         l_ab <- abs(intervalData$x-intervals$x[1])
         l_ab_index <- match(min(l_ab), l_ab)
         m_ab <- abs(intervalData$x-intervals$x[2])
         m_ab_index <- match(min(m_ab), m_ab)
         u_ab <- abs(intervalData$x-intervals$x[3])
         u_ab_index <- match(min(u_ab), u_ab)
         
         intervals$x <- c(intervalData$x[l_ab_index], 
                          intervalData$x[m_ab_index], 
                          intervalData$x[u_ab_index])
         intervalData$density[!intervalData$x %in% intervals$x] <- 0
         
         plot <- plot +
           geom_bar(intervalData, mapping=aes(x=x, y=density), stat="identity", fill=subColor[4], alpha=0.5)
         
       }
       
    }
  }
  plot <- plot + xlab(x_axis)
  
  return(plot)
}


  
plotSeveralViolins <- function(data, prop=0.9, x_axis, y_axis, method="eti", colors){
  minX <- Inf
  maxX <- -Inf
  for(i in 1:length(data)){
    subData <- data[[i]]
    minX <- min(minX, subData$x)
    maxX <- max(maxX, subData$x)
  }
  plot <- ggplot() + ylim(minX,maxX)
  for(i in 1:length(data)){
    subData <- data[[i]]
    
    interval <- ciOfDens(subData, prop, method)
    
    crp.rg <- colorRampPalette(c("grey",colors[i]))
    subColor <- crp.rg(7)[c(3,4,6,7)]
    
    gData <- data.frame(x = x_axis[i],y=subData$x, dens = subData$density, 
                        width=subData$density/max(subData$density))
    

    suppressWarnings({
      plot <- plot + 
        geom_violin(data=gData, mapping=aes(x=x,y=y,violinwidth=width), 
                    stat="identity", fill=subColor[1], alpha=0, col=subColor[4]) 
    })
    
    if(!is.null(interval)){
      gDataHDI <- gData[gData$y >= interval$lower & gData$y <= interval$upper,]
      gDataCenter <- gData
      gDataCenter$sort <- abs(gDataCenter$dens - interval$centerPointDens)
      gDataCenter <- gDataCenter[order(gDataCenter$sort),]
      gDataCenter <- gDataCenter[1,]
      barData <- data.frame(x=rep(gDataHDI$x[1],2), 
                            y=range(gDataHDI$y))
      
      suppressWarnings({
        plot <- plot + 
          geom_violin(data=gDataHDI, mapping=aes(x=x,y=y,violinwidth=width), 
                      stat="identity", fill=subColor[1],alpha=0.5, col="transparent") +
          geom_line(data=barData, mapping=aes(x=x,y=y), col=subColor[4]) +
          geom_point(data=gDataCenter, mapping=aes(x=x,y=y), col=subColor[4], size=3)
      })
    }
    
  }
  plot <- plot + xlab("index") + 
    ylab(y_axis) 

  return(plot)
}



## data a un(named) list of numeric data (from e.g. MPs)
plotMP <- function(data, prop_a=0.9, prop_b=0.9, pi =T, x_axis, method="eti", colors, centerPoint=0){
  stop("Not in use?")
  plot <- ggplot()
  for(i in 1:length(data)){
    subData <- data[[i]]
    approx <- approxfun(density(subData))
    median <- median(subData)
    medianDens <- approx(median)
    
    if(method == "eti"){
      interval_a <- bayestestR::eti(subData, prop_a)
      interval_b <- bayestestR::eti(subData, prop_b)
    }else if(method=="hdi"){
      interval_a <- bayestestR::hdi(subData, prop_a)
      interval_b <- bayestestR::hdi(subData, prop_b)
    }else{
      stop(paste0("Unknown method: ", method))
    }
    crp.rg <- colorRampPalette(c("grey",colors[i]))
    subColor <- crp.rg(7)[c(3,4,6,7)]
    
    lowDens <- approx(interval_b$CI_low)
    highDens <- approx(interval_b$CI_high)
    
    gData <- data.frame(x=subData)
    
    if(pi){
      min <- min(c(centerPoint,subData))
      max <- max(c(centerPoint,subData))
      fillColorLower <- subColor[4]
      fillColorUpper <- subColor[4]
      if(length(subData[subData < centerPoint]) < length(subData[subData > centerPoint])){
        fillColorLower <- "darkred"
      }else{
        fillColorUpper <- "darkred"
      }
      if(min < centerPoint) plot <- plot + stat_function(data=gData, fun = approx, geom="area", 
                                               fill=fillColorLower,alpha=0.25, xlim =c(min,min(max(subData),centerPoint)))
      if(max > centerPoint) plot <- plot + stat_function(data=gData, fun = approx, geom="area", 
                                               fill=fillColorUpper,alpha=0.25, xlim =c(max(min(subData),centerPoint),max))
      plot <- plot + 
        stat_function(data=gData, fun=approx , geom ="line",color=subColor[4], xlim=c(min(subData),max(subData))) +
        geom_segment(data=gData, x=interval_b$CI_low,y=0,xend=interval_b$CI_low,yend=lowDens, color=subColor[3], lineWidth=1) +
        geom_segment(data=gData, x=median,y=0,xend=median,yend=medianDens, color=subColor[3], lineWidth=1) +
        geom_segment(data=gData, x=interval_b$CI_high,y=0,xend=interval_b$CI_high,yend=highDens, color=subColor[3], lineWidth=1) +
        yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE)
      if(centerPoint > min(subData) && centerPoint < max(subData)) plot <- plot + geom_vline(xintercept=centerPoint, linetype="dashed")
    }else{
      plot <- plot + 
        stat_function(data=gData, fun = approx, geom="area", fill=subColor[1],alpha=0.5, xlim =c(interval_a$CI_low,interval_a$CI_high)) +
        stat_function(data=gData, fun = approx, geom="area", fill=subColor[2],alpha=0.5, xlim =c(interval_b$CI_low,interval_b$CI_high)) +
        geom_segment(data=gData, x=median,y=0,xend=median,yend=medianDens, color=subColor[3], lineWidth=1) +
        stat_function(data=gData, fun=approx , geom ="line",color=subColor[4], xlim=c(min(gData),max(gData))) + 
        yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE)
      if(centerPoint > min(subData) && centerPoint < max(subData)) plot <- plot + geom_vline(xintercept=centerPoint, linetype="dashed")
    }
  }
  plot <- plot + xlab(x_axis) 
  return(plot)
}

##INTERSTING!
# set.seed(3)
# d <- c(rnorm(1e3),rnorm(1e3,3,1))
# plot(density(d))
# dens <- approxfun(density(d))
# x <- seq(-2,6,length=1e6)
# y <- dens(x)
# x[y==max(y)]
# 
# ia <- bayestestR::hdi(d, 0.38)
# ib <- bayestestR::hdi(d, 0.39)
# integrate(dens, ia$CI_low, ia$CI_high)
# integrate(dens, ib$CI_low, ib$CI_high)
# 
# plotDiff(data=d,x_axis=paste0("\u0394", "y"), prop=0.337, method="hdi")
# plotDiff(data=d,x_axis=paste0("\u0394", "y"), prop=0.38, method="hdi")
# plotDiff(data=d,x_axis=paste0("\u0394", "y"), prop=0.38, method="eti")

plotDiff <- function(data, prop=0.9, x_axis, method="hdi", centerPoint=0){
  plot <- ggplot() + xlab(x_axis)
  
  approx <- approxfun(density(data))
  median <- median(data)
  medianDens <- approx(median)
  
  if(method == "eti"){
    interval <- bayestestR::eti(data, prop)
  }else if(method=="hdi"){
    interval <- bayestestR::hdi(data, prop)
  }else{
    stop(paste0("Unknown method: ", method))
  }
  crp.rg <- colorRampPalette(c(BAYAS_COLORS$`--plotDiff_range_a`, BAYAS_COLORS$`--plotDiff_range_b`))
  subColor <- crp.rg(7)[c(3,4,6,7)]
  
  lowDens <- approx(interval$CI_low)
  highDens <- approx(interval$CI_high)
  
  gData <- data.frame(x=data)

  min <- min(data)
  max <- max(data)
  fillColorLower <- subColor[4]
  fillColorUpper <- subColor[4]
  if(length(data[data < centerPoint]) < length(data[data > centerPoint])){
    fillColorLower <- "darkred"
  }else{
    fillColorUpper <- "darkred"
  }
  if(min < centerPoint) plot <- plot + stat_function(data=gData, fun = approx, geom="area", n=1024, 
                                                     fill=fillColorLower,alpha=0.25, xlim =c(min,min(centerPoint,max)))
  if(max > centerPoint) plot <- plot + stat_function(data=gData, fun = approx, geom="area", n=1024, 
                                                     fill=fillColorUpper,alpha=0.25, xlim =c(max(min,centerPoint),max))
  plot <- plot + 
    stat_function(data=gData, fun=approx , geom ="line", n=1024, color=subColor[4], xlim=c(min(data),max(data))) +
    geom_segment(data=gData, x=median,y=0,xend=median,yend=medianDens, color=subColor[3], linewidth=1) +
    geom_segment(data=gData, x=interval$CI_low,y=0,xend=interval$CI_low,yend=lowDens, color=subColor[3], linewidth=1) +
    geom_segment(data=gData, x=interval$CI_high,y=0,xend=interval$CI_high,yend=highDens, color=subColor[3], linewidth=1) +
    yaxis_text(FALSE) + yaxis_ticks(FALSE) + yaxis_title(FALSE)
  if(centerPoint > min(data) && centerPoint < max(data)) plot <- plot + geom_vline(xintercept=centerPoint, linetype="dashed")

  return(plot)
}

# pp <- rnorm(1e3,1,1)
# 
# data <- pp
# prop <- 0.2
# x_axis <- "x"
# method <- "eti"
# centerPoint <- 0
# plotDiff(data, prop, x_axis, method, centerPoint)
