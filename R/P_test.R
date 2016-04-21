# P-test on coefficients of QR-fits
#
#' Function computing significance of a tau-quantile fit (in 9999 MC permutation test)
#'
#' @import data.table
#'
#' @export
p_test <- function(data,tau,N=9999, freq=FALSE, method="fn", STN=TRUE,plot=TRUE){
  if(freq==FALSE){
  reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(max2d) ~ d2, tau=tau,method=method))[2]),digits=7))),by=STN]
  obs <- data[, list(round(as.numeric(coef(rq(max2d ~ d2, tau=tau, method=method))[2]),digits=7)),by=STN]
  setkey(reps,STN)
  setkey(obs,STN)
  tmp <- merge(reps,obs)
  setnames(tmp, c("STN","reps","obs"))
  p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
  }
  else if(freq==TRUE & STN==TRUE){
    N <- 58
    reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau,method="br"))[2]),digits=7))),by=STN]
    obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau,method="br"))[2]),digits=7)),by=STN]
    setkey(reps,STN)
    setkey(obs,STN)
    tmp <- merge(reps,obs)
    setnames(tmp, c("STN","reps","obs"))
    p <- tmp[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
  }
  else{
    N <- 58
    tmp <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau,method="br"))[2]),digits=7)))]
    tmp$obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau,method="br"))[2]),digits=7))]
    setnames(tmp, c("reps","obs"))
    p <- unique(tmp[, list((1+sum(obs > reps))/(N+1))])
  }
  return(p)
}
