# Plotting quantile regression fits of Psum data for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
Psum_plotting <- function(data,title,ymin,ymax,Year=FALSE){
  if(Year==FALSE){ # 2 day Psums
    ggplot(data, aes(x = d2)) +        # data
      #geom_point(aes(y=Psum),color="gray",size=0.3)+
      geom_line(aes(y=P50), color="purple") +
      geom_line(aes(y=P75), color="blue") +
      geom_line(aes(y=P90), color="green") +
      geom_line(aes(y=P95), color="orange") +
      geom_line(aes(y=P99), color="red") +
      #geom_smooth(aes(y=Psum), method=lm, color="black") +
      ggtitle(title) + # title
      ylim(c(ymin,ymax)) +
      scale_x_continuous(breaks=c(365.2,4017.7,7670.1,10409.4),
                         labels=c("1960","1980","2000","2015")) +
      #annotate("text",label=paste("Coef=",round(coef(lm(Psum ~ d2,data=max2d[STN==260]))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time (2 days)") + ylab("Psum (mm/2days)")+ # x and y-axis label
      theme_bw()
  }
  else{            # yearly Psums
    ggplot(data, aes(x = Year)) +        # data
      #geom_point(aes(y=Psum),color="gray",size=0.3)+
      geom_line(aes(y=P25), color="purple") +
      geom_line(aes(y=P50), color="blue") +
      geom_line(aes(y=P75), color="green") +
      #geom_line(aes(y=P99), color="red") +
      geom_smooth(aes(y=Psumyr), method=lm, color="black") +
      ggtitle(title) + # title
      ylim(c(ymin,ymax)) +
      annotate("text",label=paste("Coef=",round(coef(lm(Psumyr ~ Year,data))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
      xlab("Time (yr)") + ylab("Psum (mm/yr)")+ # x and y-axis label
      theme_bw()
  }
}
