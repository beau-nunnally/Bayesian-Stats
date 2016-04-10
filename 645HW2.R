#Problem 3.5
#Initialize all variables (and simple calculations)
y <- c(10,10,12,11,9) 
n <- length(y) 
meany <- mean(y) 
sd2 <- sum((y-mean(y))^2)/(n-1) 

#Functions to calculate densities
aposterior <- function(mu,sd,y){ 
  logdens <- 0 
  for (i in 1:length(y)) logdens <- logdens + 
      log(dnorm(y[i],mu,sd)) 
  logdens} 
bposterior <- function(mu,sd,y){ 
  logdens <- 0 
  for (i in 1:length(y)) logdens <- logdens + 
      log(pnorm(y[i]+0.5,mu,sd) - pnorm(y[i]-0.5,mu,sd))
  logdens} 

#Initialize random sample draw, log sigma and mu grids
nsim <- 1000 
mugrid <- seq(4,17,length=200) 
logsdgrid <- seq(-1.25,2.5,length=200) 
contours <- seq(.0001,.9999,.01)
#Function calls for density from part a
logdens <- outer (mugrid, exp(logsdgrid), aposterior, y) 
dens <- exp(logdens - max(logdens)) 
#Plot the function
contour (mugrid, logsdgrid, dens, levels=contours, xlab="Mu", ylab="log Sigma", cex=2) 
mtext ("Density Without Rounding", 3) 
#Calculate SD and Mu from the density function
sd <- sqrt((n-1)*sd2/rchisq(nsim,4)) 
mu <- rnorm(nsim,meany,sd/sqrt(n)) 
#Summary Statistics for Mu
mean(mu)
sqrt(var(mu))
#Summary Statistics for SD
mean(sd)
sqrt(var(sd))
#Function call for part b
logdens <- outer (mugrid, exp(logsdgrid), bposterior, y) 
dens <- exp(logdens - max(logdens)) 
#Plot it
contour (mugrid, logsdgrid, dens, levels=contours, xlab="Mu", ylab="log Sigma", cex=2) 
mtext ("Density With Rounding", 3) 
#Calculate SD and Mu from the density function
dens.mu <- apply(dens,1,sum) 
muindex <- sample (1:length(mugrid), nsim, replace=T, prob=dens.mu) 
mu <- mugrid[muindex] 
sd <- rep (NA,nsim) 
for (i in (1:nsim)) sd[i] <- exp (sample (logsdgrid, 1, prob=dens[muindex[i],])) 
#Summary Statistics for Mu
mean(mu)
sqrt(var(mu))
#Summary Statistics for SD
mean(sd)
sqrt(var(sd))
#Calculations for part d
z <- matrix (0, nsim, length(y))
for (i in 1:length(y)){ 
  z[,i] <- qnorm (pnorm (y[i]-.5, mu, sd)  + runif(nsim)*(pnorm (y[i]+.5, mu, sd) -pnorm (y[i]-.5, mu, sd) ), mu, sd)} 
mean ((z[,1]-z[,2])^2)
