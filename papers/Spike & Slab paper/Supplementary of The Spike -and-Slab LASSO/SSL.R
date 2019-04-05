SSL <- function(X, y, penalty=c("separable","adaptive"),
                   lambda1, theta=0.5, lambda0, a=1, b=p, eps=.001, max.iter=500, counter=10, approximate=FALSE,
                   warn=FALSE) {
  # Coersion
  penalty <- match.arg(penalty)
  if (approximate){approximate=1}else{approximate=0}
  if (missing(approximate)){approximate=0}
  
  nlambda <- length(lambda0)
  if (class(X) != "matrix") {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") storage.mode(X) <- "double"
  if (class(y) != "numeric") {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("y must numeric or able to be coerced to numeric")
  }

 
  # Lambda0 should be an increasing sequence

  monotone<-sum((lambda0[-1]-lambda0[-nlambda])>0)
  if (monotone!=nlambda-1){stop("lambda0 must be a monotone increasing sequence")}



  # Error checking
  standardize <- TRUE

  if (lambda1 > min(lambda0) ) stop("lambda1 must be smaller than lambda0")
  if (any(is.na(y)) | any(is.na(X))) stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to ncvreg")

 
  ## Set up XX, yy, lambda
  if (standardize) {
    std <- .Call("standardize", X)
    XX <- std[[1]]
    center <- std[[2]]
    scale <- std[[3]]
    nz <- which(scale > 1e-6)
    if (length(nz) != ncol(XX)) XX <- XX[ ,nz, drop=FALSE]    
  } else {
    XX <- X
  }
  p <- ncol(XX)

  yy <- y - mean(y)

  n <- length(yy)
 

  ## Fit
  if (standardize==TRUE) {
    res <- .Call("SSL_gaussian", XX, yy, penalty, lambda0, eps, as.integer(max.iter), as.double(lambda1), as.double(theta), as.integer(counter),
                 as.integer(approximate), as.double(a), as.double(b))
    bb <- matrix(res[[1]], p, nlambda)
    iter <- res[[3]]
    thetas<-res[[4]]
  }

  ## Warning
  if (warn & any(iter==max.iter)) {

      warning("Algorithm did not converge for the ABOVE MENTIONED values of lambda0")

      print(lambda0[iter==max.iter])
                               }


  ## Unstandardize
  if (standardize) {
    beta <- matrix(0, nrow=(ncol(X)), ncol=length(lambda0))
    bbb <- bb/scale[nz]
    beta <- bbb
  } else {
    beta <-  bb 
  }

  ## Names
  varnames <- if (is.null(colnames(X))) paste("V",1:ncol(X),sep="") else colnames(X)
  dimnames(beta) <- list(varnames, round(lambda0,digits=4))

  ## Select

  select<-apply(beta,2,function(x){as.numeric(x!=0)})

  ## Model

  model<-(1:p)[select[,nlambda]==1]

  ## Output
  val <- structure(list(beta = beta,
                        iter = iter,
                        lambda0 = lambda0,
                        penalty = penalty,
                        lambda1 = lambda1,
                        thetas=thetas,
                        select=select,
                        model=model,
                        n = n),
                   class = "SSL")
 
  val
}
