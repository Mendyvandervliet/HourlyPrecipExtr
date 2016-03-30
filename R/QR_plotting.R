# Plotting quantile regression fits for multiple stations and quantiles
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
QR_plotting <- function(data,title,ymax){
  ggplot(data[RH > quantile(RH,probs=c(0.50))], aes(x = Year)) +        # data
    geom_point(aes(y=RH),color="gray",size=0.3)+
    geom_line(aes(y=m50), color="black") +
    geom_line(aes(y=m75), color="purple") +
    geom_line(aes(y=m90), color="blue") +
    geom_line(aes(y=m95), color="green") +
    geom_line(aes(y=m99), color="orange") +
    geom_line(aes(y=m99.9), color="red") +
    #color="Points", scale_color_manual(values=c("Points"="gray","50%"="black", "75%"="purple", "90%"="blue" ,"95%"="green","99%"="orange","99.9%"="red" )) +
    ggtitle(title) + # title
    ylim(0,ymax) +
    xlab("Time") + ylab("Intensity (mm/hr)")+ # x and y-axis label
    theme_bw()
}
