# Quantile regression for multiple stations
#
#' Function to obtain regression data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_all <- function(data,all=FALSE){
  if(all==FALSE){
   tmp <- data
   tmp[, ":="(f50 = -1, f75 = -1, f90 = -1, f95 = -1, f99 = -1, f999 = -1)]
   setkey(tmp,STN)
   tmp[, f50 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.5,method="fn"))),by=STN]
   tmp[, f75 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.75,method="fn"))),by=STN]
   tmp[, f90 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.90,method="fn"))),by=STN]
   tmp[, f95 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.95,method="fn"))),by=STN]
   tmp[, f99 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.99,method="fn"))),by=STN]
   tmp[, f999 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.999,method="fn"))),by=STN]
   # reduce computational cost, mean GR fitted values per year
   tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
   tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=list(Year,STN)]
  }
  # else{
  #
  # }
   return(tmp)
}

# Q <- data.table(fitted.values(rq(allmax2d$Pmax2d ~ allmax2d$d2,tau=taus,method="fn")))
# setnames(Q, c("Q50","Q75","Q90","Q95","Q99","Q999"))
#
#
# taus = c(0.50,0.75,0.90,0.95,0.99,0.999)
# QRallmax2d <- data.table(quantiles=taus)
# QRallmax2d[, f := list(2:10),by=quantiles]
# QRallmax2d[, f := as.numeric(fitted.values(rq(allmax2d$Pmax2d ~ allmax2d$d2,tau=quantiles,method="fn"))),by=quantiles]


# if(var=="RH"){
#   tmp[, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5))),by=STN]
#   tmp[, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75))),by=STN]
#   tmp[, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90))),by=STN]
#   tmp[, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95))),by=STN]
#   tmp[, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99))),by=STN]
#   tmp[, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999))),by=STN]
# }
