#Inputs for 5.3
school=c("A","B","C","D","E","F","G","H")
estimate = c(28,8,-3,7,-1,1,18,12)
sd=c(15,10,16,11,9,11,10,18)
schools=data.frame(school,estimate,sd)
#Run STAN for 53
J=nrow(schools)
y=schools$estimate
sigma=schools$sd
schools_fit = stan(file='schools.stan',data=c("J","y","sigma"),iter=1000,chains=4)
print(schools_fit)
schools_sim=extract(schools_fit)
means=matrix(NA,nrow=8,ncol=8)
for (i in 1:8){
  for (j in 1:8){
    if(j==i){
    }
      else{
        means[i,j]=mean( schools_sim$theta[,i] > schools_sim$theta[,j])
      
    }
  }
}
temp_max=matrix(NA,nrow=2000,ncol=1)
best_school=matrix(NA,nrow=8,ncol=1)
for (i in 1:2000){
temp_max[i]=which.max(schools_sim$theta[i,])
}

for (i in 1:8){
  best_school[i]=dim(as.matrix(temp_max[temp_max==i]))[1]/dim(temp_max)[1]
}
#Calculations for independent normals on 53b
runs=2000
IndependentSim=matrix(NA,nrow=runs,ncol=8)
for (i in 1:8){
  IndependentSim[,i]=rnorm(runs,schools$estimate[i],schools$sd[i])
}
meansi=matrix(NA,nrow=8,ncol=8)
for (i in 1:8){
  for (j in 1:8){
    if(j==i){
    }
    else{
      meansi[i,j]=mean( IndependentSim[,i] > IndependentSim[,j])
      
    }
  }
}
temp_maxi=matrix(NA,nrow=runs,ncol=1)
best_schooli=matrix(NA,nrow=8,ncol=1)
for (i in 1:runs){
  temp_maxi[i]=which.max(IndependentSim[i,])
}
for (i in 1:8){
  best_schooli[i]=dim(as.matrix(temp_maxi[temp_maxi==i]))[1]/dim(temp_maxi)[1]
}
#Sim and Plots for 5.8
theta <- seq(-2,4,.001) 
prior <- c (0.9, 0.1) 
dens <- prior[1]*dnorm(theta,1,0.5) + prior[2]*dnorm(theta,-1,0.5) 
plot (theta, dens, ylim=c(0,1.1*max(dens)), type="l", xlab="theta", ylab="", xaxs="i", yaxs="i", yaxt="n", bty="n", cex=2) 
mtext ("Prior Density", cex=2, 3)
marg <- dnorm(-.25,c(1,-1),sqrt(c(0.5,0.5)^2+1/10)) 
posterior <- prior*marg/sum(prior*marg)
dens <- posterior[1]*dnorm(theta,1.5/14,sqrt(1/14)) + posterior[2]*dnorm(theta,-6.5/14,sqrt(1/14)) 
plot (theta, dens, ylim=c(0,1.1*max(dens)), type="l", xlab="theta", ylab="", xaxs="i", yaxs="i", yaxt="n", bty="n", cex=2) 
mtext ("Posterior Density", cex=2, 3)
#Sim and Plot for 6.6
teststatistic=NULL
draws=1000
for (i in 1:draws){
  theta=rbeta(1,8,14)
  y_rep=rbinom(1,1,theta)
  while(sum(y_rep==0)<13)
    y_rep=c(y_rep,rbinom(1,1,theta))
  n_rep=length(y_rep)
  teststatistic=c(teststatistic,sum(y_rep[2:n_rep] !=y_rep[1:(n_rep-1)]))
}
hist(teststatistic,xlab="T (Y_rep}",main=NULL, yaxt="n",breaks=seq(-.5, max(teststatistic)+.5),cex=2, col="grey")
#Sim and plot for 6.7
teststatistic2=NULL
draws2=1000
for (i in 1:draws2){
  theta2=rnorm(1,5.1,1/sqrt(100))
  y_rep2=rnorm(100,theta2,1)
  teststatistic2=c(teststatistic2,max(abs(y_rep2)))}
par(mar=c(5,5,4,1)+.1)
hist(teststatistic2,xlab="T (Y_rep}",main=NULL, yaxt="n",nclass=20,cex=2, col="grey")
lines (rep(8.1,2),c(0,1000),lwd=5)
print(mean(teststatistic2>8.1))



