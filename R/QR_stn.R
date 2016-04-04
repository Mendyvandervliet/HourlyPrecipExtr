# Quantile regression for multiple stations
#
#' Function to obtain regression data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_stn <- function(data=hrKNMI,STN1=235,STN2=260,STN3=280,STN4=310,STN5=380){
  tmp <- data
  tmp[, ":="(f50 = -1, f75 = -1, f90 = -1, f95 = -1, f99 = -1, f999 = -1)]
  setkey(tmp,STN)
  tmp[STN == STN1] <- tmp[STN==STN1][, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
  tmp[STN == STN1] <- tmp[STN==STN1][, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
  tmp[STN == STN1] <- tmp[STN==STN1][, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
  tmp[STN == STN1] <- tmp[STN==STN1][, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
  tmp[STN == STN1] <- tmp[STN==STN1][, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
  tmp[STN == STN1] <- tmp[STN==STN1][, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
  tmp[STN == STN2] <- tmp[STN==STN2][, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
  tmp[STN == STN3] <- tmp[STN==STN3][, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
  tmp[STN == STN4] <- tmp[STN==STN4][, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
  tmp[STN == STN5] <- tmp[STN==STN5][, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
  # reduce computational cost, mean GR fitted values per year
  tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
  tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=list(Year,STN)]
  return(tmp)
}
