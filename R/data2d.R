# Making a data set divided in units of 2 days, which a corresponding (independent) max RH values
#
#' Function to obtain data set with resolution of 2 days

#' @import data.table
#'
#' @export
data2d <- function(data,STN1=235,STN2=260,STN3=280,STN4=310,STN5=380,SW=FALSE){
  tmp <- data
  tmp$d2 <- 0
  tmp$d2[1] <- 1
  tmp$d2[2] <- 1
  n <- 0
  if(STN1!=0){
  for(i in 1:(length(tmp[STN == STN1]$d2)/48)){
    n <- n+1
    tmp[STN == STN1]$d2[(((i-1)*48)+1):(i*48)] <- n
    print(as.numeric(i/length(tmp[STN == STN1]$d2)*100))
  }
  tmp[STN == STN2]$d2 <- tmp[STN == STN1]$d2
  tmp[STN == STN3]$d2 <- tmp[STN == STN1]$d2
  tmp[STN == STN4]$d2 <- tmp[STN == STN1]$d2
  tmp[STN == STN5]$d2 <- tmp[STN == STN1]$d2
  }
  else if((STN1!=0)&(SW==TRUE)){
    for(j in min(tmp$Year):max(tmp$Year)){
      for(i in 1:(length(tmp[STN == STN1][Year==j]$d2)/48)){
        n <- n+1
        tmp[STN == STN1][Year==j]$d2[(((i-1)*48)+1):(i*48)] <- n
      }
      print(j)
    }
    tmp[STN == STN2]$d2 <- tmp[STN == STN1]$d2
    tmp[STN == STN3]$d2 <- tmp[STN == STN1]$d2
    tmp[STN == STN4]$d2 <- tmp[STN == STN1]$d2
    tmp[STN == STN5]$d2 <- tmp[STN == STN1]$d2
  }
  else{
  for(i in 1:(length(tmp$d2)/48)){
    n <- n+1
    tmp$d2[(((i-1)*48)+1):(i*48)] <- n
    print(as.numeric(i/length(tmp$d2)*100))
    }
  }
  return(tmp)
}
#
# data=hrKNMI
# STN1=235
# STN2=260
# STN3=280
# STN4=310
# STN5=380
#
# # TEST LAST PART OF FUNCTION
# tmp$YM <- format(tmp$Date,format="%Y-%m")
# tmp2 <- tmp[,.(Year=Year,YM=YM, month=round(mean(month),digits=0),day=mean(day), max2d=max(RH)),by=.(d2,STN)]
# tmp2 <- tmp2[, list(max2d=unique(max2d), Year=unique(Year),YM=unique(YM),month=unique(month)),by= list(d2,STN)]
