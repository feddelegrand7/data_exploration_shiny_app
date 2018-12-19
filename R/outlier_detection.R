library("carData")

library("doMC")
registerDoMC()
options(cores=8)



detect.mv.outliers <- function(X){

  p <- ncol(X)
  n <- nrow(X)
  ind.cont <- rep(FALSE, p)
  for(j in 1:p)
    ind.cont[j] <- is.numericish(X[,j])

  pval <- rep(0,n)
  v2 <- rep(0,n)
  for(i in 1:n){
    ind.nna.i <- !is.na(X[i,])
    ind.cont.i <-  ind.nna.i & ind.cont
    ind.cat.i <-  ind.nna.i & !ind.cont
    X.cont.i <- X[,ind.cont.i, drop=FALSE]
    X.cat.i <- X[,ind.cat.i, drop=FALSE]
    p.cont.i <- ncol(X.cont.i)
    p.cat.i <- ncol(X.cat.i)
    ind.obs.i <- apply(cbind(is.na(X.cat.i), is.na(X.cont.i)),1,sum)==0
    X.cat.i <- X.cat.i[ind.obs.i,,drop=FALSE]
    X.cont.i <- X.cont.i[ind.obs.i,,drop=FALSE]
    if(p.cont.i==0){
      pval[i] <- NA
      v2[i] <- NA
    }
    else{
      eps.i <- X.cont.i
      for(j in 1:p.cont.i){
        if(p.cat.i==0)
          x.ij <- matrix(1,nrow(X.cont.i),1)
	else
	  x.ij <- model.matrix(~ . , data=X.cat.i)

        fit.ij <- lm.fit(x.ij, X.cont.i[,j])
	eps.i[,j] <- X.cont.i[,j]- x.ij%*%fit.ij$coef
      }
      SX <- cov(eps.i, use="complete.obs")
      if(any(is.na(SX))){
        SX <- cov(eps.i, use="pairwise.complete.obs")
      }
      foo <- try(chol(SX), silent = TRUE)
      while(is.character(foo[1])){
     ##print("blah")
        dSX <- diag(SX)
        SX <- .95*SX
        diag(SX) <- dSX
        foo <- try(chol(SX), silent = TRUE)
      }
      Sinv <- chol2inv(foo)
      ii <- which(i==which(ind.obs.i))
      res.i <- as.numeric(eps.i[ii,])
      v2[i] <- t(res.i)%*%Sinv%*%res.i
      pval[i] <- pchisq(v2[i], p.cont.i, lower.tail=FALSE)
    }
  }
  return(pval)
}




detect.mv.outliers.par <- function(X){
  X <- as.data.frame(X)
  p <- ncol(X)
  n <- nrow(X)
  ind.cont <- rep(FALSE, p)
  for(j in 1:p)
    ind.cont[j] <- is.numericish(X[,j])

  pval <- foreach(i=1:n, .combine=c)%dopar%{
    ind.nna.i <- !is.na(X[i,])
    ind.cont.i <-  ind.nna.i & ind.cont
    ind.cat.i <-  ind.nna.i & !ind.cont
    X.cont.i <- X[,ind.cont.i, drop=FALSE]
    X.cat.i <- X[,ind.cat.i, drop=FALSE]
    p.cont.i <- ncol(X.cont.i)
    p.cat.i <- ncol(X.cat.i)
    ind.obs.i <- apply(cbind(is.na(X.cat.i), is.na(X.cont.i)),1,sum)==0
    X.cat.i <- X.cat.i[ind.obs.i,,drop=FALSE]
    X.cont.i <- X.cont.i[ind.obs.i,,drop=FALSE]
    if(p.cont.i==0){
      pval[i] <- NA
      v2[i] <- NA
    }
    else{
      eps.i <- X.cont.i
      for(j in 1:p.cont.i){
        if(p.cat.i==0)
          x.ij <- matrix(1,nrow(X.cont.i),1)
	else
	  x.ij <- model.matrix(~ . , data=X.cat.i)

        fit.ij <- lm.fit(x.ij, X.cont.i[,j])
	eps.i[,j] <- X.cont.i[,j]- x.ij%*%fit.ij$coef
      }
      SX <- cov(eps.i, use="complete.obs")
      if(any(is.na(SX))){
        SX <- cov(eps.i, use="pairwise.complete.obs")
      }
      foo <- try(chol(SX), silent = TRUE)
      while(is.character(foo[1])){
     ##print("blah")
        dSX <- diag(SX)
        SX <- .95*SX
        diag(SX) <- dSX
        foo <- try(chol(SX), silent = TRUE)
      }
      Sinv <- chol2inv(foo)
      ii <- which(i==which(ind.obs.i))
      res.i <- as.numeric(eps.i[ii,])
      v2.i <- t(res.i)%*%Sinv%*%res.i
      pval.i <- pchisq(v2.i, p.cont.i, lower.tail=FALSE)
    }
    pval.i
  }
  return(pval)
}


#
# set.seed(22)
# head(Hartnagel)
#
# XX <- X <- Hartnagel[,-1]
# XX$mtheft.cat <- cut(Hartnagel$mtheft, breaks=seq(150,300,by=50))
# detect.mv.outliers(X)
# detect.mv.outliers(XX)
#
# detect.mv.outliers.par(X)
# detect.mv.outliers.par(XX)
#






























































detect.mv.outliers.old <- function(X, min.unique=10){

  p <- ncol(X)
  n <- nrow(X)
  ind.cont <- rep(FALSE, p)
  for(j in 1:p)
    ind.cont[j] <- !(is.factor(X[,j]) || length(unique(X[,j])) < min.unique)

  X.cont <- X[,ind.cont]
  p.cont <- ncol(X.cont)
  muX <- apply(X.cont,2,mean, na.rm=TRUE)
  SX <- cov(X.cont, use="complete.obs")
  if(any(is.na(SX))){
    SX <- cov(X.cont, use="pairwise.complete.obs")
  }
  foo <- try(chol(SX), silent = TRUE)
  while(is.character(foo[1])){
  print("blah")
    dSX <- diag(SX)
    SX <- .95*SX
    diag(SX) <- dSX
    foo <- try(chol(SX), silent = TRUE)
  }
  Sinv <- chol2inv(foo)
  muX.mat <- matrix(muX, n, p.cont, byrow=TRUE)
  eps <- as.matrix(X.cont - muX.mat)
  v2 <- rep(0,n)
  for(i in 1:n)
    v2[i] <- as.numeric(t(eps[i,])%*%Sinv%*%eps[i,])
  df <- rep(p.cont,n)

  ind.na <- which(is.na(v2))
  for(i in ind.na){
    ind.obs.i <- which(!is.na(X.cont[i,]))
    if(length(ind.obs.i)==0){
      v2[i] <- NA
      df[i] <- 0
      next
    }
    Xobs.i <- X.cont[i,ind.obs.i]
    SX.i <- SX[ind.obs.i, ind.obs.i]
    Sinv.i <- chol2inv(chol(SX.i))
    muX.i <- muX[ind.obs.i]
    eps.i <- as.numeric(Xobs.i - muX.i)
    v2[i] <- as.numeric(t(eps.i)%*%Sinv.i%*%eps.i)
    df[i] <- length(ind.obs.i)
  }
  pval <- pchisq(v2, df, lower.tail=FALSE)
  return(pval)
}


