# Quantile regression on multiple variables for multiple stations
#
#' Function to obtain regression data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_alld2 <- function(data,allstn=TRUE){
  tmp <- data
  tmp[, ":="(f50 = -1, f75 = -1, f90 = -1, f95 = -1, f99 = -1, f999 = -1)]
  if(allstn==TRUE){
    tmp[, f50 := as.numeric(fitted.values(rq(max ~ d2, tau =0.5,method="fn")))]
    tmp[, f75 := as.numeric(fitted.values(rq(max ~ d2, tau =0.75,method="fn")))]
    tmp[, f90 := as.numeric(fitted.values(rq(max ~ d2, tau =0.90,method="fn")))]
    tmp[, f95 := as.numeric(fitted.values(rq(max ~ d2, tau =0.95,method="fn")))]
    tmp[, f99 := as.numeric(fitted.values(rq(max ~ d2, tau =0.99,method="fn")))]
    tmp[, f999 := as.numeric(fitted.values(rq(max ~ d2, tau =0.999,method="fn")))]
    # reduce computational cost, mean GR fitted values per year
    tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
    tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=Year]
  }
  else{
    setkey(tmp,STN)
    tmp[, f50 := as.numeric(fitted.values(rq(max ~ d2, tau =0.5,method="fn"))),by=STN]
    tmp[, f75 := as.numeric(fitted.values(rq(max ~ d2, tau =0.75,method="fn"))),by=STN]
    tmp[, f90 := as.numeric(fitted.values(rq(max ~ d2, tau =0.90,method="fn"))),by=STN]
    tmp[, f95 := as.numeric(fitted.values(rq(max ~ d2, tau =0.95,method="fn"))),by=STN]
    tmp[, f99 := as.numeric(fitted.values(rq(max ~ d2, tau =0.99,method="fn"))),by=STN]
    tmp[, f999 := as.numeric(fitted.values(rq(max ~ d2, tau =0.999,method="fn"))),by=STN]
    # reduce computational cost, mean GR fitted values per year
    tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
    tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=list(Year,STN)]
  }
  return(tmp)
}
