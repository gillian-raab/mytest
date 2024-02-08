###-----merge levels of factors.syn-------------------------------------------------------
# merge factor levels with fewer than minsize values

mergelevels.syn <- function(data, vars = NULL, newlabel = FALSE, addNA = FALSE,
                            print.flag = FALSE, minsize = 10, merge.byhand = FALSE,
                            merge.details = NULL)
{
  if (!is.data.frame(data)) stop("Data must be a data.frame.\n", call. = FALSE)
  varnames <- names(data)
  
  print(ls())
  if (merge.byhand == FALSE) {
    # checks on levels
    if (!is.null(vars)) {
      if (is.numeric(vars)) {
        if (!all(vars %in% 1:length(data))) stop("Column indices must be between 1 and ", 
                                                 length(data), ".", sep = "", call. = FALSE)  
        varnos <- vars
        vars <- names(data)[varnos]
      } else {
        if (!all(vars %in% varnames)) stop("Variable(s) ", 
                                           paste(vars[!vars %in% varnames], collapse = ", "),
                                           " not in data.\n", sep = "", call. = FALSE)
        varnos <- match(vars,varnames)
      }
      
      vclass <- sapply(data[, varnos, drop = FALSE], is.factor)
      if (!all(vclass)) stop("Variable(s) (", 
                             paste(vars[!vclass], collapse = ", "), 
                             ") in mergelevels.syn not factor(s).\n", sep = "", call. = FALSE)
    } else { 
      ## if NULL use all factor variables
      varnos   <- (1:length(data))[sapply(data, is.factor)]
      vars <- names(data)[varnos]
    }
    if (addNA) for (i in 1:length(varnos)) data[,varnos[i]] <- addNA(data[,varnos[i]])
    # find factors that need fixing
    tofix <- rep(FALSE, length(varnos))
    for (i in 1:length(varnos)) {
      tb <- table(data[,varnos[i]])
      
      if (min(tb) < minsize ){ 
        tofix[i] <- TRUE
        if (print.flag) cat("Categories merged for ",vars[i],"\n")
        if (print.flag) {cat("Original data\n") ;print(tb)}
        data[,varnos[i]] <- fct_lump_min(data[,varnos[i]],min = minsize)
        lenlev <- length(levels(data[,varnos[i]]))
        if ( table(data[,varnos[i]])[lenlev] < minsize ) {
          ########### agregate with next lowest
          levnxtlow <- levels(data[,varnos[i]])[lenlev -1 ]
          data[,varnos[i]] <- fct_lump_n(data[,varnos[i]],n = lenlev - 2)
          if (newlabel == TRUE) levels(data[,varnos[i]])[lenlev -1 ] <- paste(levnxtlow,names(tb)[tb < minsize], collapse = " ")
        } else {
          ######################## relabel combined category
          if (newlabel  == TRUE) levels(data[,varnos[i]])[lenlev] <- paste(names(tb)[tb < minsize], collapse = " ")
        }
        if (print.flag) {cat("Merged categories data\n") ;print(table(data[,varnos[i]]))}
      }
    }
    if (sum(tofix) >0) cat("The following" ,sum(tofix),"variables have categories merged:\n",vars[tofix],"\n")
    else cat("No variables had categories with fewer than ", minsize,"counts\n")
  } else {
    if ( is.null(merge.details) ) stop("Need to supply merge.details if merge,byhand is TRUE\n", call. = FALSE)
    if (!all(names(merge.details) %in% names(data))) stop("Some names of the list merge.details is not a variable in data:\n",
                                                          names(merge.details)[!(names(merge.details) %in% names(data))],"\n", call. = FALSE)
    vars <-  names(merge.details)
    varnos   <- (1:length(data))[match(names(merge.details),names(data))] 
    if (addNA) for (i in 1:length(varnos)) data[,varnos[i]] <- addNA(data[,varnos[i]])
    
    for ( i in 1:length(varnos)) { 
      if (print.flag) {
        cat("Original data\n")
        print(table(data[,varnos[i]], useNA = "ifany"))
      }
      templevs <- levels(data[,varnos[i]] )
      
      if (addNA & any(is.na(templevs)))   templevs[is.na(templevs)] <- "NA"
      if (  !all( merge.details[[i]][-1] %in% templevs ) )   stop("Levels in merge.details for ", 
                                                                  vars[i], " are not in its levels problem entries shown here:\n",
                                                                  merge.details[[i]][-1][!(merge.details[[i]][-1] %in% replace)],"\n", call. = FALSE)
      levels(data[,varnos[i]])[match(merge.details[[i]][-1],templevs)] <-
        merge.details[[i]][1]
      if (print.flag) {
        cat("Merged cateories data\n")
        print(table(data[,varnos[i]], useNA = "ifany"))
      }
    }
  }
  return(data)
}