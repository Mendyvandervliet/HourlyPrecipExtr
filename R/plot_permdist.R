# MC- permutation distribution plots

#
#' Function computing coefficients of 9999 random permuted tau-quantile (f.e. 99%) fits
#' To make histogram of distribution of coefficients and eventuelly test the significance of a fit
#'
#' @export
plot_permdist <- function(data){
plot.new()
#Confidence levels
p05 <- quantile(data$reps, probs=c(0.05))
p95 <- quantile(data$reps, probs=c(0.95))
p99 <- quantile(data$reps, probs=c(0.99))
result <- paste("P(B >",0,") =",
                signif(unique(data$area), digits=3))
d <- density(data$reps)
plot(d, type='l', col='blue', lwd=3, main=NA,          # main="#Distribution of permuted slopes(99QR fit)"
     xlab="Slope (increase per 2days)", ylab="Density")
i <- d$x >= p05 & d$x <= p95
lines(d$x, d$y)

polygon(c(p05,d$x[i],p95), c(0,d$y[i],0), col="red")
abline(v=p95,col="purple")
abline(v=data$obs, col="black")
abline(v=p99,col="darkgray")
if(data$obs[1] > p99){
text(x=p95, y=max(d$y),"95%",col="purple", font=2)
text(x=p99, y=max(d$y), "99%", col="darkgray", font=2)
text(x=data$obs[1], y=max(d$y), "Observed",pos=4,col="black")
}
else{
text(x=p95, y=max(d$y),"95%",col="purple", font=2)
text(x=p99, y=max(d$y), "99%", col="darkgray", font=2)
text(x=data$obs[1]-(0.75*p95), y=max(d$y), "Observed",pos=4,col="black")
}
mtext(result,3)
}
