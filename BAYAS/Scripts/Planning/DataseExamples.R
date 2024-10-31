
getExampleData = function(i){
  if(i==1){
    return(empty())
  }else if(i==2){
    return(twoCol1())
  }else if(i==3){
    return(simpleGroup1())
  }else if(i==4){
    return(responseEmptyX())
  }else if(i==5){
    return(threeCol1())
  }else if(i==6){
    return(threeCol2())
  }
}
round <- 10

#Both vectors of same length
own.rep <- function(elements, n){
  if(length(elements) != length(n)) stop("Not same length")
  res <- c()
  for(i in 1:length(n)){
    res <- c(res, rep(elements[i], n[i]))
  }
  return(res)
}



#(1) Empty
empty = function(){
  return(data.frame(stringsAsFactors=F))
}

#(2) Two columns, response and a continuous
twoCol1 = function(){
  set.seed(123)
  x <- round(rnorm(50,0,1),round)
  y <- round(rnorm(50,5+x,3),round)
  return(data.frame(response = y, X=x, stringsAsFactors=F))
}

#(3) Two columns, response and a continuous
simpleGroup1 = function(){
  set.seed(123)
  x <- own.rep(c("a","b","c"),c(20,20,10))
  x_num <- own.rep(c(1,5,9),c(20,20,10))
  y <- round(rnorm(50,5+x_num,3),round)
  return(data.frame(response = y, X=x,stringsAsFactors=F))
}

#(4) Two columns, response and a empty continuous
responseEmptyX = function(){
  set.seed(123)
  y <- round(rnorm(50,5,3),round)
  return(data.frame(response = y, X=rep(NA,50), stringsAsFactors=F))
}

#(5) Three columns, response and two continuous predictors
threeCol1 = function(){
  set.seed(123)
  x1 <- round(rnorm(50,0,1),round)
  x2 <- round(rnorm(50,5,2),round)
  y <- round(rnorm(50,5+x1+x2,3),round)
  return(data.frame(response = y, X1=x1, X2=x2, stringsAsFactors=F))
}

#(6) Three columns, response and two categorical predictors
threeCol2 = function(){
  set.seed(123)
  x1 <- rep(c(0,2),each=25)
  x2 <- rep(c(5,9),25)
  y <- round(rnorm(50,x1+x2,3),round)
  return(data.frame(response = y, sex=rep(c("m","f"),each=25), type=rep(c("wt","mutant"),25), stringsAsFactors=F))
}
