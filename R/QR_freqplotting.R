# Plotting quantile regression fits of frequency data for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR_freqplotting <- function(data,title,ymin,ymax,Year=TRUE){
  if(Year==TRUE){
    ggplot(data, aes(x = Year)) +        # data
      geom_point(data=data,aes(y=f),color="gray",size=0.3)+
      geom_line(aes(y=f25), color="purple") +
      geom_line(aes(y=f50), color="blue") +
      geom_line(aes(y=f75), color="green") +
      geom_smooth(aes(y=f),method=lm, color="black")+
      ggtitle(title) + # title
      ylim(c(ymin,ymax)) +
      annotate("text",label=paste(paste("Coef=",round(coef(lm(f ~ Year,data))[[2]],digits=2)),"nr/yr"),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time (yr)") + ylab("F (counts)")+ # x and y-axis label
      theme_bw()
  }
  else{
    ggplot(data, aes(x = d2)) +        # data
      #geom_point(data=data,aes(y=f),color="gray",size=0.3)+
      geom_smooth(aes(y=yr_mean), method=lm, color="black") +
      geom_line(aes(y=f50), color="purple") +
      geom_line(aes(y=f75), color="blue") +
      geom_line(aes(y=f90), color="green") +
      geom_line(aes(y=f95), color="orange") +
      geom_line(aes(y=f99), color="red") +
      #geom_smooth(aes(y=f), method=lm, color="black")+
      ggtitle(title) + # title
      ylim(c(ymin,ymax)) +
      scale_x_continuous(breaks=c(365.2,4017.7,7670.1,10409.4),
                         labels=c("1960","1980","2000","2015")) +
      # scale_x_continuous(breaks=c(365.2,1278.3,2191.4,3104.5,4017.7,4930.8,5843.9,6757.0,7670.1,8583.2,9496.3,10409.4),
      #                    labels=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015")) +
      #annotate("text",label=paste(paste("Coef=",round(coef(lm(f ~ yr,data))[[2]],digits=4)),"nr/yr"),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time (2 days)") + ylab("F (counts)")+ # x and y-axis label
      theme_bw()
  }
}
