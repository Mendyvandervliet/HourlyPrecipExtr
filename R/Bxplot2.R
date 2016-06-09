# Boxplot data, factor=month
#
# Function to subset a data file containing only boxplot data
# As data is lognormal, we use logaritmic plotting. Therefore all data which value = 0 is omitted.

#' Function to obtain boxplot data

#' @import data.table
#'
#' @export

bxdata2 <- function(file) {
  bx <- file[P>0]
  bx<- within(bx,
              month <- factor(month))
  bx[, ':='(Qm = mean(P)), by=month]
  bx[, ':='(Q50 = quantile(P, probs=c(0.50))), by=month]
  bx[, ':='(Q99 = quantile(P, probs=c(0.99)), Q99.9 = quantile(P, probs=c(0.999))), by=month]
  bx <- subset(bx, select=c("STN", "P", "Qm", "Q99", "Q99.9"))
  return(bx)
}
