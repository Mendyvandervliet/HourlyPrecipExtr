#Chapter 3 Mechanisms

library(reshape2)
library(ggplot2)
library(data.table)
library(quantreg)
library(HourlyPrecipExtr)
library(GGally)
# Correlation between extreme precipitation data and plausible explanatory variables.
#
# Combined for all stations!
#
# Intensity
# Correlation between max2d, and T, Td, Wd and Cape
# (99,99.9) --> only those with significant changes
# Correlation between max2d[S], and T, Td, Wd and Cape
# (50,75,90,95,99,99.9)
# Correlation between max2d[W], and T, Td, Wd and Cape
# (50,75,90,95,99,99.9)

load("./inst/tussenStap/data2d.rda")
M_max2d <- data2d[, list(max2d=unique(max2d), Year=mean(Year),month=mean(month),T = mean(T), TD = mean(TD), WD = mean(DD)),by= list(d2,STN)]
save(M_max2d, file="./inst/tussenStap/M_max2d.rda")

cor(M_max2d$max2d,M_max2d$T, method=c("pearson", "kendall", "spearman"))

# Correlation matrix
cor(M_max2d,method=c("pearson"))
#cor(M_max2d,method=c("kendall"))
cor(M_max2d,method=c("spearman"))

!!! watch out with mean T , maxima hours have specific corresponding temperature
# Visualize correlation matrix by

#1. Representing the correlations as proportionally sized circles.
load("./inst/tussenStap/M_max2d.rda")
M <- M_max2d[, list(max2d, T, TD, WD)]

cor(M)
ggcorr(M, geom = "circle", nbreaks = 10, midpoint = 0.5)


## Heavy precipitation
#95 < max2d
ggcorr(M[max2d > quantile(max2d,0.95) & (max2d < quantile(max2d,0.99))], geom = "circle", nbreaks = 10, midpoint = 0.5)


## Not significant
#75 < max2d < 90
ggcorr(M[(max2d > quantile(max2d,0.75)) & (max2d < quantile(max2d,0.90))], geom = "circle", nbreaks = 10, midpoint = 0.5)
#90 < max2d < 95
ggcorr(M[(max2d > quantile(max2d,0.90)) & (max2d < quantile(max2d,0.95))], geom = "circle", nbreaks = 10, midpoint = 0.5)
#99 < max2d
ggcorr(M[max2d > quantile(max2d,0.99)], geom = "circle", nbreaks = 10, midpoint = 0.5)
#99.9 < max2d
ggcorr(M[max2d > quantile(max2d,0.999)], geom = "circle", nbreaks = 10, midpoint = 0.5)




#2. Correlation heatmap
M <- M_max2d[, list(max2d, T, TD, WD)]
M95 <- M[max2d > quantile(max2d,0.95)]

# Impacting events
# > 10 mm/hr, corresponding quantile of 0.99, 528 points
M99 <- M[max2d > quantile(max2d,0.99)]
# >20 mm/hr (21.7)
M_20mmhr <- M[max2d > quantile(max2d,0.9988)]
M_30mmhr <- M[max2d > quantile(max2d,0.9998)]

# Helper function to reorder the correlation matrix :
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Heatmaps
# All 2day max
heatmap(M)
# 5% most intensive 2day max, mean RH of 8.2mm/hr (from 5-51.4 mm/hr)
##mean(M95$max2d)
heatmap(M95)
# --> Very low (negative) correlation of variables with WD; local systems??
heatmap(M99)

# Highest extremes, > 21.7mm/hr
heatmap(M_20mmhr)
# Highest extremes, > 30.6mm/hr
heatmap(M_30mmhr)


# Summer, mean of (mean(Ms$max2d)) 1.83 mm/hr
Ms <- M_max2d[month==6| month==7 | month==8][, list(max2d, T, TD, WD)]
# Winter, mean of (mean(Mw$max2d)) 1.03 mm/hr
Mw <- M_max2d[month==12| month==1| month==2][, list(max2d, T, TD, WD)]

heatmap(Ms)
heatmap(Mw)



>95
S
W
Per Station




Taylor diagram
(Time of year differentiating
Intensity 1 x:   dots representing all hours, only Summer, only Winter
Frequency 1 x:   dots representing all hours, only Summer, only Winter)

Station differentiating
Intensity 1 x:   dots representing stations (all hours)
Frequency 1 x:   dots representing all hours, only Summer, only Winter
