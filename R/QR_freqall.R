# Quantile regression on frequency of wet events for multiple stations
#
#' Function to obtain regression data of frequency data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_freqall <- function(data){
  tmp <- data
  tmp[, ":="(f50 = -1, f75 = -1, f90 = -1)]
  setkey(tmp,STN)
  tmp[, f25 := as.numeric(fitted.values(rq(f ~ Year, tau =0.25))),by=STN]
  tmp[, f50 := as.numeric(fitted.values(rq(f ~ Year, tau =0.5))),by=STN]
  tmp[, f75 := as.numeric(fitted.values(rq(f ~ Year, tau =0.75))),by=STN]
  tmp[, f90 := as.numeric(fitted.values(rq(f ~ Year, tau =0.90))),by=STN]
  return(tmp)
}
