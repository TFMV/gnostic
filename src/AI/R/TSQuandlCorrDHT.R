setwd("~/R/Dom Class2/AdvClass/BigDataCode/8_TSClusterIntro")

# Install the necessary packages
library(devtools)
#install_github('R-package','quandl')
library(Quandl)
library(ggplot2)
library(gridExtra)
library(ggdendro)
library(zoo)

options(warn=-1) # Turn Off warnings - if you do, do this after the application has been tested!

# Get some data from QUANDL
# The zoo time series format handles irregularly spaced time series - like daily stock prices. 
# Returning data in this format allows for the easy calculation of things that require consideration of 
# the date, like a 200 day moving average or volatility.

amazon <- Quandl('GOOG/NASDAQ_AMZN', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
apple <- Quandl('GOOG/NASDAQ_AAPL', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
google <- Quandl('GOOG/NASDAQ_GOOG', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
qlikview <- Quandl('GOOG/NASDAQ_QLIK', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
sap <- Quandl('GOOG/NYSE_SAP', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
walmart <- Quandl('GOOG/NYSE_WMT', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
sears <- Quandl('GOOG/NASDAQ_SHLD', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')
jcpenny <- Quandl('GOOG/NYSE_JCP', start_date="2014-05-01", end_date='2015-05-01', collapse='weekly', type='zoo')

# Plot the time series
plot(amazon)
plot(apple)
plot(google)

# Merge and plot the time series (just the closing price)
joined_ts <- cbind(amazon[,4], apple[,4], google[,4], qlikview[,4], sap[,4], walmart[,4], sears[,4], jcpenny[,4])
names(joined_ts) <- c('amazon', 'apple', 'google', 'qlikview', 'sap', 'walmart', 'sears', 'jcpenny')
joined_ts
plot(joined_ts)

# Call TSFreqDHT (see other R script)
# Analize the time-series data 
# Do this for Amazon
source('TSFreqDHT.R')
dummyVector = as.vector(t(amazon[,4])) # Get only the closing price!
tsdataDHT <- ts(dummyVector, start = c(2014, 5), end = c(2015, 5), frequency = 52)
tsdataDHT
str(tsdataDHT)
summary(tsdataDHT)
# autocorrelation - a value close to 1 implies that the data is very similar to the date in the next epoch
#
# skewness and kurtosis are higher-order statistical attributes of a time series. 
# Skewness indicates the symmetry of the probability density function (PDF) of the amplitude of a time series. 
# A time series with an equal number of large and small amplitude values has a skewness of zero.
#
# The Hurst exponent is referred to as the "index of dependence" or "index of long-range dependence". 
# It quantifies the relative tendency of a time series either to regress strongly to the mean or to cluster 
# in a direction.A value H in the range 0.5-1 indicates a time series with long-term positive autocorrelation, 
# meaning both that a high value in the series will probably be followed by another high value and that the values 
# a long time into the future will also tend to be high. A value in the range 0 - 0.5 indicates a time series with 
# long-term switching between high and low values in adjacent pairs, meaning that a single high value will probably 
# be followed by a low value and that the value after that will tend to be high, with this tendency to switch 
# between high and low values lasting a long time into the future. A value of H=0.5 can indicate a completely u
# ncorrelated series, but in fact it is the value applicable to series for which the autocorrelations at small 
# time lags can be positive or negative but where the absolute values of the autocorrelations decay exponentially 
# quickly to zero.
#
# Lyapunov - < 0 stable osciliating, = 0 steady state, > 0 chaotic (Brownian motion)
#
# dc = decomposition. Classical time series decomposition separates a time series into five components: 
# mean, long-range trend, seasonality, cycle, and randomness. 
measures(tsdataDHT)

# Find Outliers
# Code:
# Additive Outlier (AO)
# Innovation Outlier (IO)
# Level Shift (LS)
# Temporary change (TC)
# Seasonal Level Shift (SLS)
library(tsoutliers)
data.ts.outliers <- tso(tsdataDHT) # may be long running ....
data.ts.outliers
plot(data.ts.outliers)

# Scale the time series and plot
maxs <- apply(joined_ts, 2, max)
mins <- apply(joined_ts, 2, min)
joined_ts_scales <- scale(joined_ts, center = mins, scale = maxs - mins)
plot(joined_ts_scales)

# Sparklines & Dendograms
hc <- hclust(dist(t(joined_ts_scales)), "ave")
colours_hc <- cutree(hc, h=2) # colour the tree at different levels by changing the h value

### Plot
hcdata <- dendro_data(hc)
names_order <- hcdata$labels$label
# A dendrogram (from Greek dendro "tree" and gramma "drawing") is a tree diagram frequently used to 
# illustrate the arrangement of the clusters produced by hierarchical clustering.
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking
# Look at the distance in the plot! The smaller the distance the greater the correlation
hcdata$labels$label <- ''
p1 <- ggdendrogram(hcdata, rotate=TRUE, leaf_labels=FALSE)

new_data <- joined_ts_scales[,rev(as.character(names_order))]

p2 <- autoplot(new_data, facets = Series ~ . ) + 
  aes(colour=as.character(rep(colours_hc,each=53)), linetype = NULL) +
  xlab('') + ylab('') + theme(legend.position="none")

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 

grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))
