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
      annotate("text",label=paste("Coef=",round(coef(lm(f ~ Year,data))[[2]],digits=2)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time") + ylab("F (counts/yr)")+ # x and y-axis label
      theme_bw()
  }
  else{
    ggplot(data, aes(x = d2)) +        # data
      #geom_point(data=data,aes(y=f),color="gray",size=0.3)+
      geom_line(aes(y=f25), color="purple") +
      geom_line(aes(y=f50), color="blue") +
      geom_line(aes(y=f75), color="green") +
      geom_smooth(aes(y=f), method=lm, color="black")+
      ggtitle(title) + # title
      ylim(c(ymin,ymax)) +
      annotate("text",label=paste("Coef=",round(coef(lm(f ~ d2,data))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time") + ylab("F (counts/2days)")+ # x and y-axis label
      theme_bw()
  }
}
