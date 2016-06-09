# Data loading


#' Function to load data table with correct units
#' @import data.table

#' @export
KNMI_loading <- function(file){
  tmp <- fread(file,skip=22L)
  setnames(tmp,c("STN", "YYYYMMDD", "HH", "DD","FH", "T", "TD", "DR", "P", "RH", "WW"))
  #tmp <- data.table(tmp)
  #Date <- as.PCICt(strptime(tmp$YYYYMMDD,"%Y%m%d"),cal="gregorian")
  tmp[, Date := as.Date(as.POSIXlt(as.PCICt(strptime(YYYYMMDD,"%Y%m%d"),cal="gregorian")))]
  tmp[, Date := as.Date(as.POSIXlt(Date))]
  tmp[, date := as.POSIXct(paste(Date,as.character(HH)), format="%Y-%m- %d %H")]
  tmp[, ':='(Year= year(Date), Month= month(Date), Day= mday(Date))]
  # Correct units
  tmp[, ':='(FH=FH * 0.1,RH = RH * 0.1, T = T * 0.1, TD = TD * 0.1, DR = DR * 0.1)] # RH in mm, T and Td in degrees Celsius, DR in hrs
  tmp[RH == -0.1] <- tmp[RH == -0.1][,RH := 0] # All negative values(meaning RH<0.05) become 0
  # Generate a column indicating season
  tmp[,Season := 0]
  tmp <- within(tmp, Season[(Month == 12) | (Month == 1)| (Month == 2)] <- "Winter")
  tmp <- within(tmp, Season[(Month == 3) | (Month == 4)| (Month == 5)] <- "Spring")
  tmp <- within(tmp, Season[(Month == 6) | (Month == 7)| (Month == 8)] <- "Summer")
  tmp <- within(tmp, Season[(Month == 9) | (Month == 10)| (Month == 11)] <- "Autumn")
  # Generate a column Intensity
  tmp[, I := (RH/DR), by=date]
  tmp <- within(tmp, I[DR==0] <- 0)
  # code all dry and dry hours with 0 and 1's.
  tmp[, code := 0]
  tmp <- within(tmp, code[RH>0] <- 1)
  return(tmp)
}



