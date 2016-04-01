# Quantile regression for multiple stations
#
#' Function to obtain regression data

#' @import data.table
#' @import quantreg
#'
#' @export
QR_all <- function(data,var,time,STN1=235,STN2=260,STN3=280,STN4=310,STN5=380){
   tmp <- data
   tmp[, ":="(f50 = -1, f75 = -1, f90 = -1, f95 = -1, f99 = -1, f999 = -1)]
   if(var=="RH"){
     tmp[, f50 := as.numeric(fitted.values(rq(RH ~ date, tau =0.5)))]
     tmp[, f75 := as.numeric(fitted.values(rq(RH ~ date, tau =0.75)))]
     tmp[, f90 := as.numeric(fitted.values(rq(RH ~ date, tau =0.90)))]
     tmp[, f95 := as.numeric(fitted.values(rq(RH ~ date, tau =0.95)))]
     tmp[, f99 := as.numeric(fitted.values(rq(RH ~ date, tau =0.99)))]
     tmp[, f999 := as.numeric(fitted.values(rq(RH ~ date, tau =0.999)))]
   }
   if(var=="max2d"){
     tmp[, f50 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.5)))]
     tmp[, f75 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.75)))]
     tmp[, f90 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.90)))]
     tmp[, f95 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.95)))]
     tmp[, f99 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.99)))]
     tmp[, f999 := as.numeric(fitted.values(rq(max2d ~ d2, tau =0.999)))]
   }
      # reduce computational cost, mean GR fitted values per year
   tmp[, ":="(m50 = -1, m75 = -1, m90 = -1, m95 = -1, m99 = -1, m999 = -1)]
   tmp[, ":="(m50=mean(f50),m75=mean(f75),m90=mean(f90),m95=mean(f95),m99=mean(f99),m999=mean(f999)), by=list(Year,STN)]
   return(tmp)
}


