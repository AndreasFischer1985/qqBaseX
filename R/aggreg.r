#' Function aggreg
#' 
#'Splits a numeric vector or matrix into subsets, computes summary statistics for each, and returns the result in a convenient form.
#' @param x Numeric vector or matrix.
#' @param y Vector or matrix specifying the subsets to compute summary statistics for.
#' @param fun Function specifying the summary statistic to compute. If NULL (default), mean will be calculated.
#' @param verbose Logical value specifying the verbocity of output.
#' @details Splits a numeric vector or matrix into subsets, computes summary statistics for each, and returns the result in a convenient form.
#' @keywords modeling
#' @export
#' @examples
#' aggreg(x=1:10,y=c(rep(1,5),rep(2,5)))

aggreg <- function (x, y = NULL, fun = NULL, verbose = T, y.is.dummy=F) 
{
    if (is.null(fun)) 
        fun = function(x) mean(x, na.rm = T)
    x = cbind(x)
    if (is.null(y)) 
        y = rep(1, dim(x)[1])
    y = cbind(y)
    x=x[,colSums(is.na(x))<dim(x)[1]]  
    y=y[,colSums(is.na(y))<dim(y)[1]]    
    z=(rowSums(is.na(x))<dim(x)[2])&(rowSums(is.na(y))<dim(y)[2])
    if(sum(z)<2)stop("less than two valid cases")
	x=cbind(x[z,])
	y=cbind(y[z,])
    
    if (dim(x)[2] == 1 & is.character(x)) {
        if (suppressWarnings(sum(is.na(as.numeric(x))) != sum(is.na(x)))) {
            if (verbose == T) 
                print("x is treated as a factor and translated to a dummy-matrix")
            x = sapply(unique(x), function(y) x == y)
        }
    }
    dummy = function(x, g = NULL, na.rm = TRUE) {
        n <- length(x)
        t <- table(x)
        l <- length(t)
        if (is.null(g)) {
            u <- matrix(0, nrow = n, ncol = l)
            if (na.rm) {
                u[is.na(x), ] <- NA
            }
            xlev <- as.factor(x)
            for (i in 1:n) {
                u[i, xlev[i]] <- 1
            }
            colnames(u) <- names(t)
        }
        else {
            u <- rep(0, n)
            xl <- as.factor(x)
            if (na.rm) {
                u[is.na(x)] <- NA
            }
            for (i in 1:n) {
                u[i] <- xl[i] %in% g
            }
        }
        return(u)
    }
    if(y.is.dummy==F)                 
    if (dim(y)[2] == 1) 
        y = dummy(y)
    else {
        if (is.null(colnames(y))) 
            colnames(y) = 1:dim(y)[2]
        n = colnames(y)
        y = lapply(1:dim(y)[2], function(z) dummy(y[, z]))
        y = lapply(1:length(n), function(i) {
            colnames(y[[i]]) = paste(n[[i]], colnames(y[[i]]), 
                sep = "_")
            y[[i]]
        })
        y = do.call(cbind, y)
    }
    typecast = function(x) {
        if (is.character(x)) {
            if (suppressWarnings(sum(is.na(as.numeric(x))) == 
                sum(is.na(x)))) {
                return(as.numeric(x))
            }
            else {
                return(as.numeric(as.factor(x)))
            }
        }
        else {
            return(as.numeric(x))
        }
    }
    x = apply(x, 2, typecast)
    y = apply(y, 2, typecast)
    if (dim(y)[2] == 1) {
        if (verbose == T) {
            print(x)
            print(y)
        }
        y = y[, 1]
        x = t(sapply(unique(y), function(subset) fun(rbind(x[y == 
            subset, ]))))
    }
    else {
        if (verbose == T) {
            print(x)
            print(y)
        }
        x = sapply(colnames(y), function(subset) apply(rbind(x[y[, 
            subset] == 1, ]), 2, fun))
    }
    return(x)
}
