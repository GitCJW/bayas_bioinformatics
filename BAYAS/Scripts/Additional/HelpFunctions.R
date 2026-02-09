trigger <- function(bool, status = "toggle"){
  if(is.null(bool)) return(T)
  if(status == "toggle"){
    return(!bool)
  }
}

#Sets the value of the given ui element to NULL
resetUIElement <- function(session, id){
  session$sendCustomMessage(type = "resetValue", message = session$ns(id))
}



# Enum for variables characteristic
characteristicEnum <- function() {
  list(Discrete = "Discrete", Continuous = "Continuous", Categorical = "Categorical")
}



## Check input data (vector of values) of its characteristic and lower/upper limit
## Returns a named list with its attributes
analyseInputdata <- function(vector){
  cEnum <- characteristicEnum()
  
  # Integer, Continuous, Categorical
  characteristic <- cEnum$Categorical

  # Convert to numeric
  vector_numeric <- suppressWarnings(as.numeric(vector))
  vector_numeric <- suppressWarnings(vector_numeric[!is.na(vector_numeric)])
  
  
  # Check if (at least 50%) 100% of the members are numbers
  # T: Check if characteristic is whether int or continuous
  # F: Accept the data as categor
  if(length(vector_numeric) > 0  & (length(vector_numeric) / length(vector)) ==1){
    characteristic <- cEnum$Discrete
    if(!all(vector_numeric %% 1 == 0 )) characteristic <- cEnum$Continuous
  }
  
  
  if(characteristic == cEnum$Categorical){
    lowerLimit <- 0
    upperLimit <- "INF"
  }else{
    if(characteristic == cEnum$Discrete){
      lowerLimit <- 0
    }else{
      lowerLimit <- ">0"
    }
    min <- min(vector_numeric)
    if(min < 0) lowerLimit <- "-INF"
    if(min == 0) lowerLimit <- "0"
    
    # upper limit
    upperLimit <- "INF"
    max <- max(vector_numeric)

    if(max < 1 && min >= 0){
      if(characteristic == cEnum$Discrete){
        upperLimit <- 1
      }else{
        upperLimit <- "<1"
      }
    } 
    if(min < 0) upperLimit <- "INF"
  }
  
  result <- list(
    characteristic = characteristic,
    lowerLimit= lowerLimit,
    upperLimit = upperLimit
  )
  
  return(result)
}


## Verify the input data of its consistency. It should be a NxM matrix.
## Returns a list of errors/warnings with an unique id
# 1: column contains values and strings 
# 2: missing values
# 3: missing header
verifyInputData <- function(x){
  col_names <- colnames(x)
  res <- list(id = 0, message = "")
  index <- 1
  tC <- tryCatch({
    
    # check warning 1
    for(col in col_names){
      subset <- x[[col]]
      subset_numeric <- suppressWarnings(as.numeric(subset))
      subset_logical <- suppressWarnings(is.na(subset_numeric))
      sum <- sum(is.na(subset))
      val <- (sum(subset_logical) - sum) / length(subset)
      if(val > 0 && val < 1){
        res$id[[index]] <- 1
        res$message[[index]] <- "<p><b>Numbers and text!</b></p><p>At least one column contains numbers and text. If this is desired, the values of this column will be interpreted as categoricals.<p>"
        index <- index+1
        break;
      }
    }
    
    # check warning 2
    if(any(is.na(x))){
      res$id[[index]] <- 2
      res$message[[index]] <- "<p><b>Missing value detected!</b></p><p>The complete row of a missing value will be removed!</p>"
      index <- index+1
    }
    
    # check warning 3
    if("V1" %in% col_names){
      res$id[[index]] <- 3
      res$message[[index]] <- "<p><b>Probably missing header(s) detected!</b></p><p>They will be added by \"V\" plus index!</p>"
      index <- index+1
    }
    
    # check warning 4 
    for(col in col_names){
      if(nchar(col) > 10){
        res$id[[index]] <- 4
        res$message[[index]] <- "<p><b>Long header(s) found.</b></p><p>Please shorten your column headers for better clarity!</p>"
      }
    }
    T
  },
  error=function(cond){
    return(F)
  })
  if(!tC) return(NULL)
  return(res)
}


## 
cleanInputData <- function(x, allowOnlyComplete = F){
  colnames(x) <- gsub("[[:space:]]", "_", colnames(x))
  
  #remove columns with just one value
  if(allowOnlyComplete){
    x <- x[complete.cases(x),]
    
    remove_c <- c()
    for(i in 1:length(x[1,])){
      if(length(unique(x[,i]))==1) remove_c <- c(remove_c,i)
    }
    if(length(remove_c)>0){
      x <- x[,-remove_c, drop=F]
    }
  }
 
  #if every row is parsable to numeric --> as.numeric
  for(i in 1:dim(x)[2]){
    if(all(!is.na(as.numeric(x[,i])))) x[,i] <- as.numeric(x[,i])
  }
  
  return(x)
}

##
readPNG_as_list <- function(file, contentType, width_scale = 1, height_scale = 1, alt = "", rel = T){
  img <- suppressWarnings(png::readPNG(file, native = FALSE, info = FALSE))
  d <- dim(img)
  
  if(rel){
    image_list <- list(src = file,
                       contentType = contentType,
                       width = d[2] * width_scale,
                       height = d[1] * height_scale,
                       alt = alt)
  }else{
    image_list <- list(src = file,
                       contentType = contentType,
                       width = width_scale,
                       height = height_scale,
                       alt = alt)
  }

  
  return(image_list)
}

# readJPG_as_list <- function(file, contentType, width_scale = 1, height_scale = 1, alt = ""){
#   img <- jpeg::readJPEG(file, native = FALSE)
#   d <- dim(img)
#   
#   image_list <- list(src = file,
#                      contentType = contentType,
#                      width = d[2] * width_scale,
#                      height = d[1] * height_scale,
#                      alt = alt)
#   
#   return(image_list)
# }

# Check if the vector are categorical
# Set convert, if the categorical vector should be converted to numerics
checkCategorical <- function(vector, convert = F){
  
  vector_numeric <- suppressWarnings(sapply(vector,as.numeric))

  cat <- any(is.na(vector_numeric))
  if(!convert) return(cat)
  
  if(is.factor(vector)){
    return(as.numeric(vector))
  }
  
  if(cat){
    return(as.numeric(as.factor(vector)))
  }else{
    return(vector)
  }
  
}

# Returns by a given formula and variable names all possible interaction terms
# E.g. y~ x1+x2+x1:x2 and vars=c("x1","x2) ; x1 and x2 categorial var with "A","B"
# Return: x1A:x2A, x1B:x2B (and depending on data also: ) x1A:x2B, x1B:x2A
formula_combination <- function(ft, dat, vars){
  elements <- colnames(as.data.frame(model.matrix(ft, dat)) %>% select_if(~ !is.numeric(.) || (length(unique(.)) != 1 || sum(.)!= 0)))
  
  c1 <- attr(ft,"variables")
  c1_var <- trimws(str_split(toString(c1),",")[[1]][-c(1:2)])
  c1_var <- c1_var[c1_var %in% vars]
  
  res <- c()
  for(i in elements){
    if(i != "(Intercept)"){
      el_split <- strsplit(i, ":")[[1]]
      if(length(el_split) == length(c1_var)){
        found <- T
        #Starts with the same var?
        for(i_el in 1:length(el_split)){
          if(!startsWith(el_split[i_el], c1_var[i_el])) found <- F
        }
        if(found) res <- c(res, i)
      }
    }
  }
  return(res)
}


#extract the stanfit class of rstanarm or brms objects
extract_stanfit <- function(object){
  if("brmsfit" %in% class(object)){
    return(object$fit)
  }else if("stanreg" %in% class(object)){
    return(object$stanfit)
  }else{
    stop(paste0("Unknown 'stan' object: ", class(object)))
  }
}

#substring of length nchar
#if nchar is negative, number of discarded nchars from end
sub_str <- function(str, nchar){
  if(nchar < 0){
    if((-1*nchar) > nchar(str)) return("")
    return(substr(str,1,nchar(str)+nchar))
  }else{
    if(nchar(str) < nchar){
      return(str)
    }else{
      return(substr(str,1,nchar))
    }
  }
}


print0 <- function(...,sep=""){
  print(paste(...,sep=sep))
}
print1 <- function(...){
  print("-----------------------")
  for(e in list(...))print(e)
  print("-----------------------")
}
print2 <- function(...){
  l <- list(...)
  print0("-----------",l[[1]],"------------")
  for(e in l[c(2:length(l))])print(e)
  print("")
}


getCurrentTimeInMilliSec <- function(){
  as.numeric(Sys.time())*1000
}

# a <- c("1","3","4")
# b <- c("3","2","1")
# equal0(a,b)
vectorEqual <- function(a,b){
  if(!is.null(class(a)) && "integer" %in% class(a)) a <- as.numeric(a)
  if(!is.null(class(b)) && "integer" %in% class(b)) b <- as.numeric(b)
  if(identical(a,b)){
    # if(length(a) <= 1 && length(b) <= 2)
    if(any(is.na(a)) || any(is.na(b))){
      if(sum(is.na(a)) == sum(is.na(b))){
        a <- a[!is.na(a)]
        b <- b[!is.na(b)]
      }
    }
    if(any(a=="") || any(b=="")){
      if(sum(a=="") == sum(b=="")){
        a <- a[a!=""]
        b <- b[b!=""]
      }
    }
    # ta <- table(a)
    # tb <- table(b)
    # for(el_a in names(ta)){
    #   if(ta[el_a] != tb[el_a]) return(F)
    # }
  }else{
    return(F)
  }
  return(T)
}

#compares two numerics  for equality with a given accuracy
quasiEqual <- function(a,b,acc=1e-10){
  if(!is.numeric(a) || !is.numeric(b) || is.na(a) || is.na(b) || is.null(a) || is.null(b)) return(F)
  return(abs(a-b) < acc)
}

#compares two list of different types of entries
#numerics uses quasiEqual
#string returns true if: 
# single elements equal. vectors contains same elements
# NULL = NULL
# NA = NA
quasiEqual2 <- function(a,b,acc=1e-10){
  suppressWarnings({
    if(is.null(a) && is.null(b)){
      return(T)
    }else if(is.null(a) || is.null(b)){
      return(F)
    }else if(is.na(a) && is.na(b)){
      return(T)
    }else if(is.na(a) || is.na(b)){
      return(F)
    }else if(!is.na(as.numeric(a)) && !is.na(as.numeric(b))){
      return(quasiEqual(a,b,acc))
    }else if(all(a %in% b) && all(b %in% a)){
      return(T)
    }else{
      return(F)
    }
  })
}

# Compares whatever of equality 
# If both are NULL, NA or NaN returns false with a warning
equal <- function(a,b){
  if((is.null(a) || is.na(a) || is.nan(a)) && 
     (is.null(b) || is.na(b) || is.nan(b))){
    warning("Both objects are undefined. Returning FALSE")
    return(F)
  }else if(!(is.null(a) || is.na(a) || is.nan(a)) && 
           !(is.null(b) || is.na(b) || is.nan(b))){
    return(a==b)
  }else{
    return(F)
  }
}



# Compares whatever of equality 
# If both are NULL/NA/NAN return TRUE
equal0 <- function(a,b){
  if(is.null(a) && is.null(b)) return(T)
  if(is.null(a) || is.null(b)) return(F)
  
  if(identical(a, character(0)) && identical(b, character(0))) return(T)
  if(identical(a, character(0)) || identical(b, character(0))) return(F)
  
  if(identical(a, numeric(0)) && identical(b, numeric(0))) return(T)
  if(identical(a, numeric(0)) || identical(b, numeric(0))) return(F)
  
  if(is.null(a) && is.null(b)) return(T)
  if(is.null(a) || is.null(b)) return(F)
  
  if((is.null(a) && is.null(b)) || 
     (is.na(a) && is.na(b)) ||
     (is.nan(a) && is.nan(b))){
    return(T)
  }else if(is.null(a) || is.na(a) || is.nan(a) ||
           is.null(b) || is.na(b) || is.nan(b)){
    return(F)
  }else{
    return(a==b)
  }
}

equal1 <- function(a,b){
  if(is.null(a) && is.null(b)) return(T)
  if(is.null(a) || is.null(b)) return(F)
  
  if(identical(a, character(0)) && identical(b, character(0))) return(T)
  if(identical(a, character(0)) || identical(b, character(0))) return(F)
  
  if(identical(a, numeric(0)) && identical(b, numeric(0))) return(T)
  if(identical(a, numeric(0)) || identical(b, numeric(0))) return(F)
  
  if(is.null(a) && is.null(b)) return(T)
  if(is.null(a) || is.null(b)) return(F)
  
  if(length(a) != length(b)) return(F)
  
  return(all(a==b))
}

# Compares two vars (or arrays of same length, or a=array, b=single number) 
# a <= b --> TRUE
# a > b --> FaLSE
less <- function(a,b){
  a <- as.numeric(a)
  b <- as.numeric(b)
  if(length(b) > 1){
    if(length(a) != length(b)) stop("Have to be same length")
    sapply(1:length(a), function(i){
      if(!is.na(a[i]) && !is.na(b[i])){
        if(a[i] <= b[i]) T else F
      }else{
        F
      }
    })
  }else{
    if(length(b) == 0 || is.na(b)) return(rep(F,length(a)))
    sapply(1:length(a), function(i){
      if(!is.na(a[i])){
        if(a[i] <= b) T else F
      }else{
        F
      } 
    })
  }
}

# Compares two vars (or arrays of same length, or a=array, b=single number) 
# a >= b --> TRUE
# a < b --> FaLSE
greater <- function(a,b){
  a <- as.numeric(a)
  b <- as.numeric(b)
  if(length(b) > 1){
    if(length(a) != length(b)) stop("Have to be same length")
    sapply(1:length(a), function(i){
      if(!is.na(a[i]) && !is.na(b[i])){
        if(a[i] >= b[i]) T else F
      }else{
        F
      }
    })
  }else{
    if(length(b) == 0 || is.na(b)) return(rep(F,length(a)))
    sapply(1:length(a), function(i){
      if(!is.na(a[i])){
        if(a[i] >= b) T else F
      }else{
        F
      } 
    })
  }
}

list.append <- function(list, element, name=NULL, extendBySameName=F){
  size <- length(list)
  if(extendBySameName){
    list[[size+1]] <- element
    n <- names(list)
    n[size+1] <- name
    names(list) <- n
  }else{
    if(is.null(name)){
      list[[size+1]] <- element
    }else{
      list[[name]] <- element
    }
  }
  return(list)
}
list.append.vector <- function(list, element, name=NULL, extendBySameName=F){
  size <- length(list)
  if(!is.null(name) && length(element) != length(name)) stop("element and name must be the same length.")
  for(el_id in seq_along(element)){
    if(extendBySameName){
      list[[size+1]] <- element[[el_id]]
      n <- names(list)
      if(!is.null(name))
        n[size+el_id] <- name[el_id]
      names(list) <- n
    }else{
      if(is.null(name)){
        list[[size+el_id]] <- element[[el_id]]
      }else{
        list[[name[el_id]]] <- element[[el_id]]
      }
    }
  }
  return(list)
}

#Insert an element into a list at position 'position' and shift all
#following elements by 1 position. 
list.insert <- function(list, element, position, name=NULL){
  size <- length(list)
  names <- names(list)
  if(!is.numeric(position) || position < 1) 
    stop(paste0("position must be numeric and greater 0: ", position))
  
  
  newlist <- list()
  newnames <- c()
  
  if(size >= position){
    for(i in seq_len(position-1)){
      newlist[[i]] <- list[[i]]
      newnames <- c(newnames, names[i])
    }
    newlist <- list.append(newlist, element, name)
    if(is.null(name)) name <- NA
    newnames <- c(newnames, name)
    for(i in position:size){
      newlist[[i+1]] <- list[[i]]
      newnames <- c(newnames, names[i])
    }
  }else{
    newlist <- list
    newlist[[position]] <- element
    if(is.null(name)) name <- NA
    newnames <- c(names, rep(NA, position-size-1), name)
  }
  
  names(newlist) <- newnames
  return(newlist)
}

list.contains.vec = function(list, vec){
  for(l in list){
    if(all(l %in% vec) && all(vec %in% l)) return(T)
  }
  return(F)
}


# l <- list('sex@f'=c(4,5,6),
#           'sex@m'=c(1,2,3),
#           'cat@c'=c(3,6),
#           'cat@b'=c(2,5))
# vec <- c(1,4)
# vecName <- c("cat@a")
# list.contains.conc.vec(l, vec, vecName)
# 
# l <- list('sex@f'=c(4,5,6),
#           'sex@m'=c(1,2,3),
#           'cat@c'=c(3,6),
#           'cat@b'=c(2,5),
#           'sex:cat@f:c'=c(6),
#           'sex:cat@f:b'=c(5))
# vec <- c(1)
# vecName <- c("sex:cat@m:b")
# list.contains.conc.vec(l, vec, vecName)

#Only used for redundant predictors/parameters
list.contains.conc.vec = function(list, vec){ #, vecName
  if(is.empty(list)) return(F)
  
  newList <- list()
  for(l_i in seq_len(length(list))){
    l <- list[[l_i]]
    for(nL_i in seq_len(length(newList))){
      nL <- newList[[nL_i]]
      new <- unique(c(nL,l))
      if(!list.contains.vec(newList,new))
        newList <- list.append(
          newList, unique(c(nL,l)), 
          paste0(names(newList)[nL_i],"@@",names(list)[l_i]))
    }
    newList <- list.append(newList, l, names(list)[l_i])
  }
  
  if(length(newList) < 2) return(list(flag=F))

  tmp <- list()
  for(l1 in 1:(length(newList)-1)){
    for(l2 in (l1+1):length(newList)){
      new <- unique(c(newList[[l2]],vec))
      if(vectorEqual(sort(newList[[l1]]),sort(new))){
        na <- names(newList)
        if(any(str_split(na[l1], "@@")[[1]] %in% str_split(na[l2], "@@")[[1]])){
          tmp <- list(flag=T, red=na[l1], comb=na[l2])
        }else{
          return(list(flag=T, red=na[l1], comb=na[l2]))
        }
      }
    }
  }
  if(!is.empty(tmp)) return(tmp)
  return(list(flag=F))
}


#Test if all elements of an array are numeric
is.num <- function(l){
  for(i in l){
    if(!is.numeric(i)) return(F)
  }
  return(T)
}

could.numeric <- function(x){
  if(is.null(x) || is.na(x) || x == "NA") return(F)
  suppressWarnings(x_num <- as.numeric(x))
  return(!(is.na(x_num) || is.null(x_num)))
}

compareLimits <- function(single, vector){
  ret <- rep(NA,length(vector))
  if(length(vector) == 0) return(F)
  for(i in 1:length(vector)){
    ret[i] <- T
    if(vector[i]!="VAR" && single!=vector[i]) ret[i] <- F
  }
  return(ret)
}

Lcm.vec = function(x){
  if(is.null(x) || length(x) == 0) return(1)
  lcm <- 1
  for(el in x){
    lcm <- Lcm(lcm, el)
  }
  return(lcm)
}

Lcm.permut = function(x,y){
  if(is.null(x) || length(x) == 0) return(1)
  if(is.null(y) || length(y) == 0) return(1)
  lcm <- c()
  for(ex in x){
    for(ey in y){
      lcm <- c(lcm, Lcm(ex, ey))
    }
  }
  return(lcm)
}

limitsToNumeric <- function(limits){
  #lower: -INF, 0, >0
  #upper: <1, 1, INF
  min <- 0
  max <- 0
  if(limits[1]=="-INF" || limits[1]==-Inf){
    min <- -Inf
  }else if(limits[1]=="0" || limits[1]==0){ 
    min <- 0
  }else if(limits[1]==">0"){ 
    min <- .Machine$double.xmin
  }
  
  if(limits[2]=="<1" ){
    max <- .Machine$double.xmax
  }else if(limits[2]=="1" || limits[2]==1){ 
    max <- 1
  }else if(limits[2]=="INF" || limits[2]==Inf){ 
    max <- Inf
  }
  return(c(min, max))
}

isValueInLimits <- function(values, lower_limit, upper_limit, discrete){
  #lower: -INF, 0, >0
  #upper: <1, 1, INF 
  if(is.null(values) || is.na(values) || values == "" || !is.numeric(values)) return(F)
  
  if(discrete){
    if(any(values != round(values))) return(F)
  }
  
  #check lower
  if(lower_limit=="-INF"){
    
  }else if(lower_limit=="0" || lower_limit==0){
    if(min(values) < 0) return(F)
  }else if(lower_limit==">0"){
    if(min(values) <= 0) return(F)
  }else{
    stop("Unknown lower limit!")
  }
  
  #check upper
  if(upper_limit=="<1"){
    if(max(values) >= 1) return(F)
  }else if(upper_limit=="1" || upper_limit==1){
    if(max(values) > 1) return(F)
  }else if(upper_limit=="INF"){
    
  }else{
    stop("Unknown upper limit!")
  }
  return(T)
}

#returns a list where the first element is boolean and second a verbal output
isValueInLimitsVerbal <- function(values, lower_limit, upper_limit, discrete){
  if(length(values)==1){
    if(!isValueInLimits(values, lower_limit, upper_limit, discrete)){
      if(discrete){
        return(list(F,paste0("Your value has to be discrete and in a range of (",lower_limit,",",upper_limit,").")))
      }else{
        return(list(F,paste0("Your value has to be in a range of (",lower_limit,",",upper_limit,").")))
      }
    }
  }else{
    if(!isValueInLimits(values, lower_limit, upper_limit, discrete)){
      if(discrete){
        return(list(F,paste0("Your values have to be discrete and in a range of (",lower_limit,",",upper_limit,").")))
      }else{
        return(list(F,paste0("Your values have to be in a range of (",lower_limit,",",upper_limit,").")))
      }
    }
  }
  return(list(T,""))
}


#Creates an empty data.frame with given column names
empty.data.frame <- function(colnames=NULL, ncol=NULL, nrow=NULL, val=0){
  if(is.null(colnames) && is.null(ncol)){
    warning("Either colnames or ncol must be not null.")
    return()
  }
  if(is.null(ncol)) ncol <- length(colnames)
  
  if(!is.null(nrow) && is.numeric(nrow) && nrow >0){
    ret <- data.frame(matrix(val,nrow, ncol))
  }else{
    ret <- data.frame()
    for(i in 1:ncol){
      ret[[i]] <- numeric(0)
    }
  }
  if(!is.null(colnames)) colnames(ret) <- colnames
  return(ret)
}

#reads a txt file
readTxtFile <- function(stringFile, linesep = "\n"){
  fileConn <- file(stringFile)
  txt <- readLines(fileConn)
  txt <- paste0(txt,collapse =linesep)
  close(fileConn)
  return(txt)
}

drawRandomSeed <- function(n=1, min=-1e6, max=1e6){
  tt <- round(as.numeric(Sys.time())*1000)
  tt <- as.character(tt)
  tt <- as.numeric(substr(tt,nchar(tt)-8, nchar(tt)))
  withr::with_seed(tt, {
    round(runif(n,min,max))
  })
}

randomSeedValid <- function(seed){
  !is.null(seed) &&
  is.numeric(seed) &&
    seed == round(seed) &&
    seed >= -.Machine$integer.max &&
    seed <= .Machine$integer.max
}

removeUnnecessaryEnvInPlot <- function(gg){

  gg <- tryCatch({
    #Remove large objects for saving with rds
    gg <- ggplotGrob(gg)
    gg <- ggpubr::as_ggplot(gg)
    gg
  },
  error = function(e){
    print(e)
    return(gg)
  })

  return(gg)
}

