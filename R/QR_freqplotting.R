# Plotting quantile regression fits of frequency data for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR_freqplotting <- function(data,title){
  ggplot(data, aes(x = Year)) +        # data
    geom_point(data=data,aes(y=f),color="gray",size=0.3)+
    geom_smooth(aes(y=f),method=lm, color="black")
    geom_line(aes(y=f25), color="black") +
    geom_line(aes(y=f50), color="blue") +
    geom_line(aes(y=f75), color="green") +
    geom_line(aes(y=f90), color="orange") +
    ggtitle(title) + # title
    ylim(0,ymax) +
    xlab("Time") + ylab("F (counts/yr)")+ # x and y-axis label
    theme_bw()
}
