# library("carData")
library("doMC")

registerDoMC()
options(cores=8)



trend.break <- function(x, buffer=5, max.ind=100){

  x <- x[!is.na(x)]
  n <- length(x)
  if(n-2*buffer < max.ind)
    b.ind <- seq(buffer, n-buffer, by=1)
  else
    b.ind <- round(seq(buffer+1, n-buffer, length=100))
  n.ind <- length(b.ind)
  Fmax <- 0

  if(!is.numericish(x)){
    Fmax <- 0
    for(j in 1:n.ind){
      y <- c(rep(0, b.ind[j]), rep(1,n-b.ind[j]))
      tab <- table(y,x)
      n <- sum(tab)
      sr <- rowSums(tab)
      sc <- colSums(tab)
      E <- outer(sr, sc, "*")/n
      Fj <- sum(sort((tab - E)^2/E, decreasing = TRUE))
      if(Fj > Fmax){
        Fmax <- Fj
        ind.max <- b.ind[j]+1
      }
    }
  }
  else{
    xbar <- mean(x)
    SSE0 <- sum((x-mean(x))^2)
    df0 <- n-1
    for(j in 1:n.ind){
      x1 <- x[1:b.ind[j]]
      x2 <- x[(b.ind[j]+1):n]
      foo1 <- lm.fit(cbind(1,1:b.ind[j]), x1)
      foo2 <- lm.fit(cbind(1,(b.ind[j]+1):n), x2)
      xhat1 <- c(foo1$fitted, foo2$fitted)
      SSE1 <- sum((x-xhat1)^2)
      df1 <- n-4
      Fj <- ((SSE0-SSE1)/(df0-df1))/(SSE1/df1)
      if(Fj > Fmax){
        Fmax <- Fj
        ind.max <- b.ind[j]+1
      }
    }
  }
  return(list(Fmax=Fmax, ind.max=ind.max))
}





trend.perm.test <- function(x, nperm=1000, buffer=5, max.ind=100){

  ans <- trend.break(x, buffer, max.ind)
  F.vec <- rep(0,nperm)
  for(k in 1:nperm){
    x.k <- sample(x)
    F.vec[k] <- trend.break(x.k, buffer, max.ind)$Fmax
  }
  pval <- (2*sum(F.vec > ans$Fmax)+1)/(2*nperm+1)
  return(list(pval=pval, Fmax=ans$Fmax, ind.max=ans$ind.max))
}


trend.perm.test.par <- function(x, nperm=1000, buffer=5, max.ind=100){
  ans <- trend.break(x, buffer, max.ind)
  F.vec <- rep(0,nperm)
  F.vec <- foreach(k=1:nperm, .combine=c)%dopar%{
    x.k <- sample(x)
    trend.break(x.k, buffer, max.ind)$Fmax
  }
  pval <- (sum(F.vec > ans$Fmax)+1)/(nperm)
  return(list(pval=pval, Fmax=ans$Fmax, ind.max=ans$ind.max))
}


trend.test <- function(dat, nperm = 10, buffer = 5, max.ind = 100)
{
  tmp <- purrr::map(dat, trend.perm.test.par, nperm = nperm, buffer = buffer, max.ind = max.ind)
  pvals <- purrr::map_dbl(tmp, "pval")
  tmp <- tmp[order(pvals)]
  arsenal::set_attr(paste0(
    names(tmp), " (Observation=",
    purrr::map_dbl(tmp, "ind.max"), ", p-value=",
    formatC(purrr::map_dbl(tmp, "pval"), digits = 3, format = "f"), ")"
  ), "results", tmp)
}


# set.seed(22)
# head(Hartnagel)
#
#
#
# x1 <- Hartnagel$fconvict
# x2 <- cut(x1, breaks=seq(40,160,by=10))
#
# system.time(trend.perm.test(x1, nperm=1000, buffer=5, max.ind=100))
# system.time(ans1 <- trend.perm.test.par(x1, nperm=1000, buffer=5, max.ind=100))
# ans1
#
# ans2 <- trend.perm.test.par(x2, nperm=1000, buffer=5, max.ind=100)
# ans2
#

trend_plot <- function(var, dat, results)
{
  validate(
    need(var != " ", "Please select a variable.")
  )
  stopifnot(var %in% names(dat))
  stopifnot(var %in% names(results))

  brk <- results[[var]]$ind.max
  dat2 <- data.frame(y = dat[[var]], obs = seq_len(nrow(dat)), gp = c(rep("Before break", brk-1), rep("After break", nrow(dat) - brk + 1)))
  if(is.numericish(dat2$y))
  {
    ggplot(dat2, aes(x = obs, y = y, group = gp)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      xlab("Observation Number") + ylab(var)
  } else
  {
    dat2 <- as.data.frame(table(dat2[c("gp", "y")]))
    dat2$gp <- factor(dat2$gp, levels = c("Before break", "After break"))
    ggplot(dat2, aes(x = gp, y = Freq, fill = y)) +
      geom_bar(position = "fill", stat = "identity") +
      xlab("") + ylab("Proportion") +
      scale_fill_discrete(name = var)
  }
}




