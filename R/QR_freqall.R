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
  # reduce computational cost, mean GR fitted values per year
  tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
  tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=list(Year,STN)]
  return(tmp)
}
