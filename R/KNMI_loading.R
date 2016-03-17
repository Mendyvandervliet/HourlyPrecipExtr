# Data loading


# Function to load data table with correct units
loaddata<- function(file){
  tmp <- fread("./inst/extData/KNMI_rainhr.txt", sep=",",col.names=c("STN", "YYYYMMDD", "HH", "DD", "T", "TD", "DR", "RH", "U", "WW"))
  tmp[, Date := as.Date(as.POSIXlt(as.PCICt(strptime(YYYYMMDD,"%Y%m%d"),cal="gregorian")))]
  tmp[, date := as.POSIXct(paste(Date,as.character(HH)), format="%Y-%m- %d %H")]
  tmp[, ':='(Year= year(Date), Month= month(Date), Day= mday(Date))]
  tmp[, ':='(RH = RH * 0.1, T = T * 0.1, TD = TD * 0.1, DR = DR * 0.1)] # RH in mm, T and Td in degrees Celsius, DR in hrs
  tmp[RH == -0.1] <- tmp[RH == -0.1][,RH := 0]                          # All negative values(RH<0.05) become 0
}

