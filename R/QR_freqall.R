# Quantile regression on frequency of wet events for multiple stations
#
#' Function to obtain regression data of frequency data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_freqall <- function(data,STN=TRUE,Year=TRUE){
  tmp <- data
  tmp[, ":="(f25= -1,f50 = -1, f75 = -1, f90 = -1)]
  if((STN==TRUE)&(Year==TRUE)){
  setkey(tmp,STN)
  tmp[, f25 := as.numeric(fitted.values(rq(f ~ Year, tau =0.25))),by=STN]
  tmp[, f50 := as.numeric(fitted.values(rq(f ~ Year, tau =0.5))),by=STN]
  tmp[, f75 := as.numeric(fitted.values(rq(f ~ Year, tau =0.75))),by=STN]
  tmp[, f90 := as.numeric(fitted.values(rq(f ~ Year, tau =0.90))),by=STN]
  }
  else if((STN==FALSE)&(Year==TRUE)){
  tmp[, f25 := as.numeric(fitted.values(rq(f ~ Year, tau =0.25)))]
  tmp[, f50 := as.numeric(fitted.values(rq(f ~ Year, tau =0.5)))]
  tmp[, f75 := as.numeric(fitted.values(rq(f ~ Year, tau =0.75)))]
  tmp[, f90 := as.numeric(fitted.values(rq(f ~ Year, tau =0.90)))]
  }
  else{
  #setkey(tmp,STN)
  tmp[, f25 := as.numeric(fitted.values(rq(f ~ d2, tau =0.25))),by=STN]
  tmp[, f50 := as.numeric(fitted.values(rq(f ~ d2, tau =0.5))),by=STN]
  tmp[, f75 := as.numeric(fitted.values(rq(f ~ d2, tau =0.75))),by=STN]
  tmp[, f90 := as.numeric(fitted.values(rq(f ~ d2, tau =0.90))),by=STN]
  tmp[, f95 := as.numeric(fitted.values(rq(f ~ d2, tau =0.95))),by=STN]
  tmp[, f99 := as.numeric(fitted.values(rq(f ~ d2, tau =0.99))),by=STN]
  tmp[, f999 := as.numeric(fitted.values(rq(f ~ d2, tau =0.999))),by=STN]
  }
  return(tmp)
}
