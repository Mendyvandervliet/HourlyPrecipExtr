# Trend plotting
#
#' Function to obtain complex ggplots

#' @import ggplot2
#'
#' @export
Trend_plotting <- function(data,title,ymin,ymax,Year=FALSE,var){
  if(var==T){
    if(Year==FALSE){
      ggplot(data, aes(x = d2,y=Tmax2d)) +        # data
        #geom_point(data=data,aes(y=f),color="gray",size=0.3)+
        geom_quantile(formula= y~x,quantiles=0.25, aes(colour="25%"), color="purple")+
        geom_quantile(formula= y~x,quantiles=0.50, aes(colour="50%"), color="blue")+
        geom_quantile(formula= y~x,quantiles=0.75, aes(colour="75%"), color="green")+
        geom_smooth(method=lm, color="black")+
        ggtitle(title) + # title
        #ylim(c(ymin,ymax)) +
        annotate("text",label=paste("Coef=",round(coef(lm(Tmax2d ~ d2,data))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
        xlab("Time") + ylab("2d max T (Celsius")+ # x and y-axis label
        theme_bw()
    }
    # if(Year==TRUE){
    #   ggplot(data, aes(x = Year)) +        # data
    #     geom_point(data=data,aes(y=f),color="gray",size=0.3)+
    #     geom_line(aes(y=f25), color="purple") +
    #     geom_line(aes(y=f50), color="blue") +
    #     geom_line(aes(y=f75), color="green") +
    #     geom_smooth(aes(y=f),method=lm, color="black")+
    #     ggtitle(title) + # title
    #     ylim(c(ymin,ymax)) +
    #     annotate("text",label=paste("Coef=",round(coef(lm(f ~ Year,data))[[2]],digits=2)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
    #     xlab("Time") + ylab("F (counts/yr)")+ # x and y-axis label
    #     theme_bw()
  #}
  }
  if(var==TD){
    if(Year==FALSE){
      ggplot(data, aes(x = d2,y=TDmax2d)) +        # data
        #geom_point(data=data,aes(y=f),color="gray",size=0.3)+
        geom_quantile(formula= y~x,quantiles=0.25, aes(colour="25%"), color="purple")+
        geom_quantile(formula= y~x,quantiles=0.50, aes(colour="50%"), color="blue")+
        geom_quantile(formula= y~x,quantiles=0.75, aes(colour="75%"), color="green")+
        geom_smooth(method=lm, color="black")+
        ggtitle(title) + # title
        #ylim(c(ymin,ymax)) +
        annotate("text",label=paste("Coef=",round(coef(lm(TDmax2d ~ d2,data))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
        xlab("Time") + ylab("2d max T (Celsius")+ # x and y-axis label
        theme_bw()
    }
  }
  # if(var==){
  #   if(Year==FALSE){
  #     ggplot(data, aes(x = d2,y=TDmax2d)) +        # data
  #       #geom_point(data=data,aes(y=f),color="gray",size=0.3)+
  #       geom_quantile(formula= y~x,quantiles=0.25, aes(colour="25%"), color="purple")+
  #       geom_quantile(formula= y~x,quantiles=0.50, aes(colour="50%"), color="blue")+
  #       geom_quantile(formula= y~x,quantiles=0.75, aes(colour="75%"), color="green")+
  #       geom_smooth(method=lm, color="black")+
  #       ggtitle(title) + # title
  #       #ylim(c(ymin,ymax)) +
  #       annotate("text",label=paste("Coef=",round(coef(lm(TDmax2d ~ d2,data))[[2]],digits=5)),x=Inf,y=Inf, hjust=1.25,vjust=2) +
  #       xlab("Time") + ylab("2d max T (Celsius")+ # x and y-axis label
  #       theme_bw()
  #   }
  # }
}
