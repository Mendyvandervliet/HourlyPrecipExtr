# Data loading


getdata_uur <- read.table("http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi", header=TRUE, quote="\"", sep=",",
                          stns=c(235,260,280,320,380),vars=c("STN", "YYYYMMDD", "HH", "DD", "FH", "FX", "T", "TD", "DR", "RH", "U"),
                          byear=1958,bmonth=1,bday=1,eyear=2015,emonth=12,eday=31)


# Create correct data format and units
loaddata<- function(url,stns,vars,datetime){
  tmp <- read.table(url, header=TRUE, quote="\"",
                    stns=stns,vars=vars, col.names=vars,
                    byear=1958,bmonth=1,bday=1,eyear=2015,emonth=12,eday=31)
  a_B<-as.PCICt(strptime(tmp$YYYYMMDD,"%Y%m%d"),cal="gregorian")
  Date <- as.Date(as.POSIXlt(a_B))
  tmp$date <- as.POSIXct(paste(Date,as.character(tmp$HH)), format="%Y-%m- %d %H")
  tmp$Day <- format(tmp$Date,format="%d")
  tmp$month <- tmp$date$mon+1
  tmp$Year <- as.integer(format(tmp$Date,format="%Y"))

  tmp$RH <- tmp$RH * 0.1            #Convert unit= 0.1 m back to unit=1 mm
  tmp$T <- tmp$T * 0.1
  tmp$RH[tmp$RH == -0.1] <- 0.0     #All negative values(RH<0.05) become 0
}


loaddata(url="http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi",
         stns=c(235,260,280,320,380), vars= c("STN", "YYYYMMDD", "HH", "DD", "FH", "FX", "T", "TD", "DR", "RH", "U"),)



