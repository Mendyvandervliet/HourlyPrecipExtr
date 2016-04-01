# MC- permutation on coefficients of QR-fits
#
#' Function computing coefficients of 9999 random permuted tau-quantile (f.e. 99%) fits
#' To make histogram of distribution of coefficients and eventuelly test the significance of a fit
#' @import plyr
#'
#' @export
MC_coefperm <- function(x, y, tau, N=9999, plot=TRUE){
  reps <- replicate(N, round(as.numeric(coef(rq(sample(y) ~ x, tau=tau))[2]),digits=7))
  obs <- round(as.numeric(coef(rq(y ~ x, tau=tau))[2]),digits=7)
  p <- (1+sum(obs > reps))/(N+1)   #p <- mean(reps > obs)
  mean <- mean(reps)
  sd <- sd(reps)
  area <- pnorm(max(reps), mean, sd) - pnorm(0, mean, sd)
  tmp <- data.table(reps,obs,tau,p,area)
  #return(list(obs=obs,reps=reps,p=p))
  return(tmp)
}
