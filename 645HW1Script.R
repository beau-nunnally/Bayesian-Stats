#Homework1 STAT 645
library(foreign)

#Problem 1
y <- seq(-6,11,.1) 
dens <- 0.5*dnorm(y,1,2)+0.5*dnorm(y,2,2) 
plot(y, dens, ylim=c(0,1.1*max(dens)), type="l", xlab="y", ylab="", xaxs="i", yaxs="i", yaxt="n",bty="n",cex=2) 

dnorm(1,1,2)/(dnorm(1,1,2)+dnorm(1,2,2))

sigma <-seq(0,100,0.1)
postprop <- dnorm(1,1,sigma)/(dnorm(1,1,sigma)+dnorm(1,2,sigma))
plot(sigma,postprop, xlab="Sigma",ylab="Posterior Probability of Theta")

#Problem 2 required no code

#Call for Problem 1.9 (ensure Working Directory is set)
source("doc_sim.R")
doc_sims(1)
doc_sims(100)

#Problem 2.1:

theta <- seq(0,1,.02) 
dens <- theta^3*(1-theta)^13 + 10*theta^4*(1-theta)^12 + 45*theta^5*(1-theta)^11 
plot (theta, dens, ylim=c(0,max(dens)), type="l", xlab="theta", ylab="", xaxs="i", yaxs="i", yaxt="n", bty="n", cex=2)

#Problem 2.11:
y <- c(43, 44, 45, 46.5, 47.5)
step <- .01 
theta <- seq(0, 100, step)
dens.nonnorm <- NULL
for (i in 1:length(theta)) {
  dens.nonnorm <- c(dens.nonnorm, prod (dcauchy (y, theta[i], 1))) }
plot (theta, dens.nonnorm, ylim=c(0,1.1*max(dens.nonnorm)), type="l", xlab="theta", ylab="non-normalized density", xaxs="i", yaxs="i", cex=2)
dens.norm <- dens.nonnorm/(step*sum(dens.nonnorm)) 
plot (theta, dens.norm, ylim=c(0,1.1*max(dens.norm)), type="l", xlab="theta", ylab="normalized density", xaxs="i", yaxs="i", cex=2)



thetas <- sample (theta, 1000, step*dens.norm, replace=TRUE)
hist (thetas, xlab="theta", yaxt="n", cex=2)

y6 <- rcauchy (length(thetas), thetas, 1) 
hist (y6, xlab="new observation", yaxt="n", nclass=100, cex=2)

#2.21
pew_data <- read.dta("pew_research_center_june_elect_wknd_data.dta")
election_res <- read.csv("2008ElectionResult.csv")

state_data=table(pew_data$ideo,pew_data$state)
prop_state_data=prop.table(table(pew_data$ideo,pew_data$state),2)
prop_libs=prop_state_data[5,]*100+prop_state_data[6,]*100
prop_libs[2]=0
vote_obama=election_res[,3]
new.state.abb=c(state.abb[1:8],"DC",state.abb[9:50])
plot(prop_libs,vote_obama, main="",xlab="Proportion of Liberals", ylab="Obama Voting Percentage",pch=19,cex=1,lty="solid",xlim=c(0,40),ylim=c(30,100),lwd=2)
text(prop_libs,vote_obama,labels=new.state.abb,cex=0.5,pos=3)


mean(prop_libs)
var(prop_libs)

resp=(state_data[1,]+state_data[2,]+state_data[3,]+state_data[4,]+state_data[5,]+state_data[6,]+state_data[7,])

beta=mean(prop_libs)/(var(prop_libs)-mean(prop_libs))
alpha=beta*mean(prop_libs)



prop_libs2=(prop_libs+alpha)/(beta+1)

plot(prop_libs2,vote_obama, main="",xlab="Proportion of Liberals", ylab="Obama Voting Percentage",pch=19,cex=1,lty="solid",xlim=c(0,40),ylim=c(30,100),lwd=2)
text(prop_libs2,vote_obama,labels=new.state.abb,cex=0.5,pos=3)

resp=(state_data[1,]+state_data[2,]+state_data[3,]+state_data[4,]+state_data[5,]+state_data[6,]+state_data[7,])

plot(resp,prop_libs, main="",xlab="Respondants", ylab="Liberals",pch=19,cex=1,lty="solid",xlim=c(0,2000),ylim=c(0,40),lwd=2)
text(resp,prop_libs,labels=new.state.abb,cex=0.5,pos=3)

plot(resp,prop_libs2, main="",xlab="Respondants", ylab="Liberals",pch=19,cex=1,lty="solid",xlim=c(0,2000),ylim=c(0,40),lwd=2)
text(resp,prop_libs2,labels=new.state.abb,cex=0.5,pos=3)


