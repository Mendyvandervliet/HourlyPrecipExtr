# Plotting quantile regression fits on max 2day prec, for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR2d_plotting<- function(data=data, title=title, ymax=ymax, pointplot=FALSE){
  #data <- data
  L <- data[,length(d2)]*0.01
  if(L>=100){
  L=TRUE
  } else{
  L=FALSE
  }
  if((pointplot==FALSE)&(L==FALSE)){
    ggplot(data, aes(x = Year)) +        # data
      #geom_point(data=data[max2d > quantile(max2d,probs=0.999)],aes(y=max2d),color="gray",size=0.3) +
      #geom_line(aes(y=yr_mean), color="black") +
      geom_smooth(aes(y=yr_mean),method=lm, color="black") +
      geom_line(aes(y=m75), color="purple") +
      geom_line(aes(y=m90), color="blue") +
      geom_line(aes(y=m95), color="green") +
      geom_line(aes(y=m99), color="orange") +
      ggtitle(title) + # title
      ylim(0,ymax) +
      xlab("Time") + ylab("I (mm/hr)")+ # x and y-axis label
      theme_bw()
  } else if((pointplot==FALSE)&(L==TRUE)){
    ggplot(data, aes(x = Year))+        # data
      #geom_line(aes(y=yr_mean), color="black") +
      geom_smooth(aes(y=yr_mean),method=lm, color="black") +
      geom_line(aes(y=m75), color="purple") +
      geom_line(aes(y=m90), color="blue") +
      geom_line(aes(y=m95), color="green") +
      geom_line(aes(y=m99), color="orange") +
      geom_line(aes(y=m999), color="red") +
      ggtitle(title) + # title
      ylim(0,ymax) +
      xlab("Time (2 days)") + ylab("I (mm/hr)")+ # x and y-axis label
      theme_bw()
  } else if((pointplot==TRUE)&(L=TRUE)){
    ggplot(data, aes(x = Year))+        # data
      geom_point(data=data[max2d > quantile(max2d,probs=0.999)],aes(y=max2d),color="gray",size=0.3) +
      #geom_line(aes(y=yr_mean), color="black") +
      geom_smooth(aes(y=yr_mean),method=lm, color="black")+
      geom_line(aes(y=m75), color="purple") +
      geom_line(aes(y=m90), color="blue") +
      geom_line(aes(y=m95), color="green") +
      geom_line(aes(y=m99), color="orange") +
      geom_line(aes(y=m999), color="red") +
      ggtitle(title) + # title
      ylim(0,ymax) +
      xlab("Time") + ylab("I (mm/hr)")+ # x and y-axis label
      theme_bw()
  } else{
    ggplot(data, aes(x = Year))+        # data
      geom_point(data=data[max2d > quantile(max2d,probs=0.999)],aes(y=max2d),color="gray",size=0.3) +
      #geom_line(aes(y=yr_mean), color="black") +
      geom_smooth(aes(y=yr_mean),method=lm, color="black")+
      geom_line(aes(y=m75), color="purple") +
      geom_line(aes(y=m90), color="blue") +
      geom_line(aes(y=m95), color="green") +
      geom_line(aes(y=m99), color="orange") +
      ggtitle(title) + # title
      ylim(0,ymax) +
      xlab("Time") + ylab("I (mm/hr)")+ # x and y-axis label
      theme_bw()
  }
}


