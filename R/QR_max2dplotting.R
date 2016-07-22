# Plotting quantile regression fits on max 2day of multiple variables, for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR_max2dplotting<- function(data=data, title=title,ytitle=ytitle,LM=TRUE){
  if(LM==TRUE){
    ggplot(data, aes(x = d2)) +        # data
      #geom_point(data=data[max2d > quantile(max2d,probs=0.999)],aes(y=max2d),color="gray",size=0.3) +
      geom_smooth(aes(y=max),method="lm",color="black") +
      geom_line(aes(y=f50), color="purple") +
      geom_line(aes(y=f75), color="blue") +
      geom_line(aes(y=f90), color="green") +
      geom_line(aes(y=f95), color="orange") +
      geom_line(aes(y=f99), color="red") +
      ggtitle(title) + # title
      #ylim(0,ymax) +
      scale_x_continuous(breaks=c(365.2,4017.7,7670.1,10409.4),
                         labels=c("1960","1980","2000","2015")) +
      xlab("Time (2 days)") + ylab(ytitle)+ # x and y-axis label
      theme_bw()
  } else{
    ggplot(data, aes(x = d2)) +        # data
      #geom_point(data=data[max2d > quantile(max2d,probs=0.999)],aes(y=max2d),color="gray",size=0.3) +
      #geom_line(aes(y=yr_mean), color="black") +
      geom_smooth(aes(y=yr_mean),method="lm",color="black") +
      geom_line(aes(y=f50), color="purple") +
      geom_line(aes(y=f75), color="blue") +
      geom_line(aes(y=f90), color="green") +
      geom_line(aes(y=f95), color="orange") +
      geom_line(aes(y=f99), color="red") +
      ggtitle(title) + # title
      #ylim(0,ymax) +
      scale_x_continuous(breaks=c(365.25,1278.375,2191.5,3104.625,4017.75),
                         labels=c("1995","2000","2005","2010","2015")) +
      xlab("Time (2 days)") + ylab(ytitle)+ # x and y-axis label
      theme_bw()
  }
}
