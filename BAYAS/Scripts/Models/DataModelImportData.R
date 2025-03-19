
#Data model only for the import of input data 
DataModelImportData <- R6Class(
  classname = "DataModelImportData", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    data = list(),
    dataIndex = 0
  ),
  
  
  public = list(

    setData = function(data, silent=F){
      private$data <- private$data[seq_len(private$dataIndex)]
      private$dataIndex <- private$dataIndex + 1
      private$data <- list.append(private$data, data)
      if(!silent){
        self$triggerReactiveValue("data")
        self$triggerReactiveValue("dataIndex")
      }
    },
    
    addData = function(data, silent=F){
      
    },
    
    getData = function(){
      if(length(private$data) == 0 || private$dataIndex == 0) return(list())
      return(private$data[[private$dataIndex]])
    },
    
    undo = function(silent=F){
      if(private$dataIndex == 0) return(F)
      
      private$dataIndex <- private$dataIndex - 1
      
      if(!silent) self$triggerReactiveValue("dataIndex")
    },
    
    redo = function(silent=F){
      if(private$dataIndex == length(private$data)) return(F)
      
      private$dataIndex <- private$dataIndex + 1
      if(!silent) self$triggerReactiveValue("dataIndex")
    },
    
    importData = function(data, header=c("row", "col"), combineHeaders=c(T,F),
                          mergeColumns=c(T,F), mergeColNames = c()){
      importData(data, header=header, combineHeaders=combineHeaders,
                            mergeColumns=mergeColumns, mergeColNames = mergeColNames)
    },
    
    
    getInstance = function(){

    },
    
    setInstance = function(instance){

    },
    
    getState = function(uuid){
      
    },
    
    setState = function(state){

    },
    
    resetState = function(){

    }

  )
  
)


#Import functions

# Either a data.frame (first touch), or a object returned by this function
# Possible status: c("hasMult", "nonMult", "invalidHeaderTypeDefined", "transposed", "dupHeaders", "uniqueHeaders", "invalidCombineHeadersTypeDefined", "failed", "invalidMergeColumnsTypeDefined")
importData = function(data, header=c("row", "col"), combineHeaders=c(T,F),
                      mergeColumns=c(T,F), mergeColNames = c()){
  obj <- list(data=data, header=c(), status="init", requires = list())
  if(!"DataModelImportDataObject" %in% class(data)){
    if(class(data) == "data.frame"){
      class(obj) <- "DataModelImportDataObject"
    }else{
      warning("Couldn't process the data (no data.frame)")
      return(data)
    }
  }else{
    obj <- data
  }
  
  
  if(obj$status == "init"){
    obj <- importData_init(obj)
  }else if(obj$status == "hasMult"){
    warning("Not possible, more than one cluster.")
    obj <- importData_init(obj)
  }else if(obj$status == "nonMult"){
    if(!(length(header) == 1 && header %in% c("row", "col"))){
      obj$status <- "invalidHeaderTypeDefined"
      warning("Parameter 'header' has to be either 'row' or 'col'.")
    }else{
      obj$required$header <- header
      if(header=="row"){
        obj <- importData_transpose(obj)
      }else{
        data <- obj$data
        header <- data[1,]
        if(any(is.na(header))){
          obj$status <- "failed"
        }else{
          obj$header <- header
          data <- data[-1,,F]
          colnames(data) <- header
          obj$data <- data
          obj$requires <- NULL
        }
      }
      if(obj$status != "failed"){
        obj$requires <- NULL
        obj$status <- "transposed"
      }
    }
  }else if(obj$status == "transposed"){
    dupHead <- hasDuplicateHeaders(header=obj$header)
    if(dupHead$hasMultiple){
      obj$status <- "dupHeaders"
      obj$requires <- list(combineHeaders=c(T,F))
    }else{
      obj$status <- "uniqueHeaders"
      obj$requires <- list(mergeColumns=c(T,F))
    }
  }else if(obj$status == "dupHeaders"){
    obj$required$combineHeaders <- combineHeaders
    if(!(length(combineHeaders) == 1 && combineHeaders %in% c(T,F))){
      obj$status <- "invalidCombineHeadersTypeDefined"
      warning("Parameter 'combineHeaders' has to be either TRUE or FALSE.")
    }else{
      objNew <- NULL
      if(combineHeaders) objNew <- importData_combineHeaders(obj)
      if(is.null(objNew)){
        obj$status <- "failed"
      }else{
        obj <- objNew
        obj$status <- "uniqueHeaders"
        obj$requires <- list(mergeColumns=c(T,F), mergeColNames=c())
      }
    }
  }else if(obj$status == "uniqueHeaders"){
    if(!(length(mergeColumns) == 1 && mergeColumns %in% c(T,F))){
      obj$status <- "invalidMergeColumnsTypeDefined"
      warning("Parameter 'mergeColumns' has to be either TRUE or FALSE.")
    }else{
      if(mergeColumns) objNew <- importData_mergeColumns(obj, mergeColNames)
      if(is.null(objNew)){
        obj$status <- "failed"
      }else{
        obj <- objNew
        obj$status <- "uniqueHeaders"
        obj$requires <- list(mergeColumns=c(T,F), mergeColNames=c())
      }
    }
  }

  return(obj)
}


importData_init =  function(obj){
  data <- obj$data
  data_trimmed <- removeLastEmptyRows(data)
  data_trimmed <- trim_cells(data_trimmed)
  data_trimmed <- removeLastEmptyRows(data_trimmed)
  obj$header <- as.vector(t(data[1,]))
  
  hasMult <- hasMultipleSnippets(data_trimmed)
  if(is.null(hasMult)){
    warning("Couldn't process the data (hasMultipleSnippets)")
    obj$status <- "failed"
    return(obj)
  }
  if(hasMult){
    warning("Not possible, more than one cluster.")
    obj$status <- "hasMult"
    return(obj)
  }else{
    obj$status <- "nonMult"
    obj$requires <- list(header=c("row","column"))
    return(obj)
  }
}

#Assumes that headers are in first column and complete
importData_transpose = function(obj){
  data <- obj$data
  header <- data[[1]]
  if(any(is.na(header))){
    obj$status <- "failed"
  }else{
    data_transposed <- transpose(data)
    obj$header <- header
    rownames(data_transposed) <- NULL
    data_transposed <- data_transposed[-1,,F]
    colnames(data_transposed) <- header
    obj$data <- data_transposed 
    obj$requires <- NULL
  }
  
  return(obj)
}


importData_combineHeaders = function(obj){
  
  obj <- tryCatch({

    data <- obj$data
    header <- obj$header
    header <- unlist(header)
    if(dim(data)[2] != length(header)){
      stop("data and header must be the same size")
    }
    
    header_table <- table(header)
    header_indexes <- match(unique(header), header)

    new_data <- list()
    header_indexes_t <- match(header, unique(header))
    for(i in seq_along(header_indexes_t)){
      if(length(new_data) < header_indexes_t[i]){
        new_data[[header_indexes_t[i]]] <- data[[i]]
      }else{
        new_data[[header_indexes_t[i]]] <- c(new_data[[header_indexes_t[i]]], data[[i]])
      }
    }
    
    max_length <- max(sapply(new_data, length))
    
    my_list_filled <- lapply(new_data, function(x) {
      length(x) <- max_length
      return(x)
    })
    
    df <- as.data.frame(my_list_filled)
    colnames(df) <- unique(header)
    obj$data <- df
    obj$header <- unique(header)
    
    obj
  },
  
  error = function(e){
    NULL
  })
  
  return(obj)
}


importData_mergeColumns = function(obj, mergeColNames){

  obj <- tryCatch({
    
    data <- obj$data
    
    new_data <- data %>% pivot_longer(all_of(mergeColNames))
    
    obj$data <- new_data
    obj$header <- colnames(new_data)
    obj
  },
  
  error = function(e){
    NULL
  })
  
  return(obj)
}


#Are there any "clusters" (snippets) within a data.frame?
hasMultipleSnippets = function(data){
  
  flag <- tryCatch({
    
    suppressWarnings(non_empty_matrix <- !is_empty(data))
    
    clusters  <- find_clusters(non_empty_matrix)
    
    #More than 1 cluster
    if(clusters$num_clusters > 1){
      return(T)
    }else{
      return(F)
    }
    
  },
  
  error = function(e){
    NULL
  })
  
  return(flag)
}

#Finds and returns (number of) clusters within a data.frame
find_clusters <- function(mat) {
  cluster_matrix <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  cluster_id <- 0
  
  # Function to recursively label the clusters
  label_cluster <- function(i, j) {
    if (i < 1 || i > nrow(mat) || j < 1 || j > ncol(mat)) return()
    if (!mat[i, j] || cluster_matrix[i, j] != 0) return()
    
    cluster_matrix[i, j] <<- cluster_id
    # Explore neighbors (4-directional)
    label_cluster(i-1, j)
    label_cluster(i+1, j)
    label_cluster(i, j-1)
    label_cluster(i, j+1)
  }
  
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (mat[i, j] && cluster_matrix[i, j] == 0) {
        cluster_id <- cluster_id + 1
        label_cluster(i, j)
      }
    }
  }
  
  return(list(cluster_matrix = cluster_matrix, num_clusters = cluster_id))
}


hasDuplicateHeaders <- function(x, headers=NULL){
  if(is.null(headers)) headers <- colnames(x)
  headers <- unlist(headers)
  headers_table <- table(headers)
  ret <- list(hasMultiple=length(unique(headers)) != length(headers),
              count=length(headers_table[headers_table > 1]))
}


################################################################################
#################################### Helpers ###################################
################################################################################

trim_cells <- function(x){
  x %>%
    mutate(across(everything(), str_trim))
}

is_empty <- function(x) {
  is.na(x) | str_trim(x) == ""
}

removeLastEmptyRows <- function(x){
  
  x <- tryCatch({
    
    x_m <- as.matrix(x)
    
    x_m[x_m == ""] <- NA
    if(is.null(dim(x_m)) || dim(x_m)[2] == 1){
      index_log <- is.na(x_m)
      maxNeg <- max(seq_along(x_m)[!index_log])
      x_m <- x_m[c(1:maxNeg)]
    }else{
      index_log <- rowSums(is.na(x_m)) == ncol(x_m)
      maxNeg <- max(seq_len(dim(x_m)[1])[!index_log])
      x_m <- x_m[c(1:maxNeg),]
    }
    
    x <- as.data.frame(x_m)
  },
  error = function(e){
    NULL
  })
  
  return(x)
}
