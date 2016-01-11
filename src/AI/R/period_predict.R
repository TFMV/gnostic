# required libraries used

require(ggplot2)
require(grid)
require(gridBase)
require(gridExtra)
require(nnet)
require(iterators)
#options(warn=-1) # Turn Off warnings - if you do, do this after the application has been tested!

# assign our working directory to location of training data
setwd("D:/Dropbox/gnostic/data")
# Load the training data and assign to variable
load("period_data.RData")
class(orderts)
#pdf("training.pdf")
# examine 5 observations in the training data
head(orderts, 5)
orderts
# we will compare the observations of metrics for each of the lowest level interval grouped by the highest order periods in the space of observations
print(qplot(week, orders, data = orderts, colour = as.factor(year), geom = "line"))

#dev.off()
# problematic extra week in Q1 of 2009 as seen in weekly level plot
qplot(week, data=orderts, colour=as.factor(year), binwidth=0.5) + facet_wrap(~ quarter)
# get rid of the extra week
orderts2 <- cbind(orderts[-13,], weekinq=c(1:117))
prev <- orderts2[1,]
runvar <- 1
for(i in 2:nrow(orderts2)){
  current <- orderts2[i,]
  orderts2[i,"weekinq"] <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  runvar <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  prev <- current
}
rm(prev, current, runvar, i)
# Examine corrected dataset
qplot(weekinq, orders, data = orderts2, colour = as.factor(year), geom = "line") + facet_wrap(~quarter)
# Note the spike at the end of the quarters
# We are going to use our two full years of observations for training data and the partial (most recent) year for model validation

# Use Fourier Fast Transform to decompose the complex time function
# and compute the magnitude of the frequencies
# The following code adapted from:
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html
# And
# http://www.ams.org/journals/mcom/1965-19-090/S0025-5718-1965-0178586-1/home.html
f <- data.frame(coef = fft(orderts2[1:104, "orders"]), freqindex = c(1:104))
# Plot the modulus of the frequency
qplot(freqindex, Mod(coef), data = f[2:53,], geom = "line")
# there are exacly eight peak differences
f[Mod(f$coef) > 3 & f$freqindex < 53, "freqindex"] - 1
peaks <- Mod(f$coef) > 3
ffilt <- f
ffilt[!peaks, "coef"] <- 0
ffilt <- data.frame(index=ffilt$freqindex, value=Re(fft(ffilt$coef, inverse=TRUE))/104, type=rep("filtered", times=104))
ffilt <- rbind(ffilt, data.frame(index=seq(1:104), value=orderts2[1:104,"orders"], type=rep("original", times=104)))
# Start to decompose the training data
peakind <- f[abs(f$coef) > 3 & f$freqindex > 1 & f$freqindex < midindex,]
midindex <- ceiling((length(f$coef)-1)/ 2) + 1
lindex <- length(f$coef)

lowerind <- 1

subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){
  upperind <- x
  fsub <- f
  notnullind <- ((fsub$freqindex >= lowerind
                  & fsub$freqindex < upperind)
                 |
                   (fsub$freqindex >  (lindex - upperind + 2)
                    & fsub$freqindex <= (lindex - lowerind + 2)))
  fsub[!notnullind,"coef"] <- 0
  lowerind <<- upperind
  Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))
})
# compute and display the decomposed series of sin and cos functions
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

psig <- function(x, y, z){
  h <- data.frame(index = c(1:length(subsignals[[x]])),
                  orders = subsignals[[x]])
  lab <- paste("Subseries ", as.character(x), sep="")
  print(qplot(index, orders, data = h, geom = "line", main=lab), vp = vplayout(y,z))
  TRUE
}

psig(1,1,1); psig(2,1,2); psig(3,2,1); psig(4,2,2); psig(5,3,1); psig(6,3,2); psig(7,4,1)

# Begin Artifical Neural Network training
# Set the hidden neurons count
nn.sizes <- c(4,2,3,3,3,2,2,2)
# determine the size of the time window
numofsubs <- length(subsignals)
twindow <- 4

odf <- lapply(1:numofsubs, function(x){
  singleoffsets <- lapply(0:(twindow-1), function(y){
    subsignals[[x]][(twindow-y):(length(subsignals[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")})
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})
# compute the sample size
sample.number <- length(odf[[1]][,1])
# initiate training of the ANN
# See the following link for information about training an ANN
# http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
# We will train each decomposed function and then display them, iteratively
nns <- lapply(1:length(odf), function(i)
{
  nn <- nnet(odf[[i]][1:(sample.number),],
             subsignals[[i]][(twindow+1):(length(subsignals[[i]]))],
             #corresponding to the training samples
             size=nn.sizes[i], #number of neurons
             maxit = 5000,
             linout = TRUE)
  # graph the results of the trained ANN
  plot(subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], type="l")
  lines(nn$fitted.values,type="l",col="red")
  nn
})
# use our remaining (partial year) data for validation
number.of.predict <- 10
long.predictions <- lapply(1:length(odf), function(i)
{
  prediction <- vector(length=number.of.predict, mode="numeric")
  input <- odf[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
  {
    prediction[j] <- predict(nns[[i]], input)
    input <- c(prediction[j],input[1:(length(input)-1)])
    print (i)
    print(prediction[j])
  }
  pdf("plots.pdf")
  # display our predictions for the series
  # The predicted signal values are displayed in blue
  print(plot(c(nns[[i]]$fitted.values,prediction), type="l",col="blue"))
  lines(subsignals[[i]][(twindow+1):length(subsignals[[i]])])
  prediction
})
dev.off()
system('taskkill /f /im AcroRd32.exe')
graphics.off()
