# P-test on coefficients of QR-fits
#
#' Function computing significance of a tau-quantile (f.e. tau) fit (in 9999 MC permutation test)
#'
#' @import data.table
#'
#' @export
p_test <- function(data,tau,N=9999, freq=FALSE, STN=TRUE,plot=TRUE){
  if(freq==FALSE){
  reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(max2d) ~ d2, tau=tau))[2]),digits=7))),by=STN]
  obs <- data[, list(round(as.numeric(coef(rq(max2d ~ d2, tau=tau))[2]),digits=7)),by=STN]
  p <- merge(reps, obs)
  setnames(p, c("STN","reps","obs"))
  p <- p[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
  }
  else if(freq==TRUE & STN==TRUE){
    reps <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau))[2]),digits=7))),by=STN]
    obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau))[2]),digits=7)),by=STN]
    p <- merge(reps, obs)
    setnames(p, c("STN","reps","obs"))
    p <- p[, list(unique((1+sum(obs > reps))/(N+1))),by=STN]
  }
  else{
    tmp <- data[, list(replicate(N, round(as.numeric(coef(rq(sample(f) ~ Year, tau=tau))[2]),digits=7)))]
    tmp$obs <- data[, list(round(as.numeric(coef(rq(f ~ Year, tau=tau))[2]),digits=7))]
    setnames(tmp, c("reps","obs"))
    p <- unique(tmp[, list((1+sum(obs > reps))/(N+1))])
  }
  return(p)
}
