#' Function to load CAPE data into a data table with correct units
#' @import data.table
#' @import PCICt
#'
#' @export
CAPEloading <- function(file){
  # Read text file with delimiter white space
  # sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns
  tmp <- read.table("./inst/extData/cape.txt", sep="")

  # Give names to columns
  setnames(tmp,c("date","LCL(hPa)","LCL(m)", "LFC(hPa)","LFC(m)","EL(hPa)", "EL(m)", "sbCAPE", "Conv.Inhib.", "NormsbCAPE", "Boyden"))
  #sbCAPE, Convect. Inhib, normsbCAPE --> unit J/kg
  tmp <- as.data.table(tmp)

  # Set date units
  tmp[, nr := 1:length(tmp$date)] # Make an index column
  dates <- tmp[,as.POSIXlt(strptime(date,"%Y%m%d%H%M")),by=nr] # creates for every date a column year, month day etc..
  tmp <- merge(tmp,dates,by="nr") # Merge by index column
  tmp[, ":="(Date = as.Date(as.POSIXlt(strptime(date,"%Y%m%d%H%M"),cal="gregorian"))),by=nr] # make date object
  tmp[, ":="(Dat = format(strptime(date,"%Y%m%d%H%M"),"%Y-%m-%d %H")),by=nr]
  tmp[, ':='(Year= year(Date), Month=month(Date))] # create year as YYYY, and mon from 1 to 12 instead of 0 to 11.
  return(tmp)
}
#tmp[, ":="(Dat = format(as.POSIXlt(strptime(date,"%Y%m%d%H%M")),"%y-%m-%d %H:%M")),by=nr]

