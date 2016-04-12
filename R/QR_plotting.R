# Plotting quantile regression fits for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR_plotting <- function(data,title,individual=FALSE){
  if(individual==FALSE){
  ggplot(data, aes(x = Year)) +        # data
    #geom_point(data=data[RH > quantile(RH,probs=0.999)],aes(y=RH),color="gray",size=0.3)+
    #geom_line(aes(y=m50), color="black") +
    #geom_line(aes(y=m75), color="purple") +
    geom_line(aes(y=m90,colour="blue")) +
    geom_line(aes(y=m95, colour="green")) +
    geom_line(aes(y=m99, colour="orange")) +
    geom_line(aes(y=m999, colour="red")) +
    ggtitle(title) + # title
    #ylim(0,ymax) +
    xlab("Time") + ylab("I (mm/hr)")+ # x and y-axis label
    theme_bw()
  } else{
    ggplot(data, aes(x = Year)) +        # data
    geom_line(aes(y=m90,colour="blue")) +
    geom_line(aes(y=m95, colour="green")) +
    geom_line(aes(y=m99, colour="orange")) +
    geom_line(aes(y=m999, colour="red")) +
    scale_color_manual(name = 'Quantiles',values=c("blue","green","orange","red" ),
                       labels = c('90%','95%','99%','99.9%')) +
    ggtitle(title) + # title
    xlab("Time") + ylab("I (mm/hr)")+ # x and y-axis label
    theme_bw()
  }

}
