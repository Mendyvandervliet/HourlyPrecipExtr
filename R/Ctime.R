# Making a data set in which all hours of metadata belong to the latest CAPE value measured, so numbering
# all hours after a CAPE measurement with the same number in variable Ctime.
#
#' Function to obtain data set with Ctime

#' @import data.table
#'
#' @export
Ctime <- function(tmp){
  tmp$Ctime <- 0
  tmp$Ctime[5] <- 1
  n <- 1
  len <- length(tmp$Ctime)
  for(i in 6:len){
    if(!is.na(tmp$sbCAPE[i])){
      tmp$Ctime[i] <- n
      n <- n+1
    } else{
      tmp$Ctime[i] <- n
    }
    #print(i/len*100)
  }
  return(tmp)
}
