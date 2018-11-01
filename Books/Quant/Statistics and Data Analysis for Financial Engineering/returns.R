###########  R script for Chapter 2   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

################################################################
########## Code for figure 2.1  ################################
################################################################

pdf("logxplusone.pdf",width=6,height=5) 
x= seq(-.25,.25,length=200)
plot(x,log(x+1),type="l",lwd=2)
lines(x,x,lty=2,lwd=2,col="red")
legend("bottomright",c("log(x+1)","x"),lty=c(1,2),lwd=2,col=c("black","red"))
graphics.off()

################################################################
########## Code for figure 2.2  ################################
################################################################
pdf("RandWalk.pdf",width=6,height=5)
x=seq(0,10,length=300)
plot(x,x/2,type="l",ylim=c(-1,9),lwd=2,ylab="",xlab="time")
lines(x,x/2+sqrt(x),lty=2,lwd=2)
lines(x,x/2-sqrt(x),lty=2,lwd=2)
legend("topleft",c("mean","mean + SD",
                   expression( paste("mean ",-phantom()," SD") )),
       lwd=2,lty=c(1,2,2))
graphics.off()

################################################################
########## Code for the R lab   ################################
################################################################
dat = read.csv("Stock_bond.csv",header=TRUE)
names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC,type="l")
plot(F_AC)

n = dim(dat)[1]
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1

pdf("ReturnsLab01.pdf",width=6,height=5)
par(mfrow=c(1,1))
plot(GMReturn,FReturn)
graphics.off()

#########  problem 3  ##########
MSFTReturn = MSFT_AC[-1]/MSFT_AC[-n] - 1
MRKReturn = MRK_AC[-1]/MRK_AC[-n] - 1
plot(MSFTReturn,MRKReturn)
round(cor(MSFTReturn,MRKReturn),3)

################################################################
########## Code for simulations   ##############################
################################################################
rm(list=ls(all=TRUE))
niter = 1e5
below = rep(0,niter)
set.seed(2009)
options(warn=-1)
for (i in 1:niter)
{
  r = rnorm(100,mean=.05/253,sd=.23/sqrt(253))
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice)
  below[i] = as.numeric(minlogP < log(950000))
}
mean(below)


################################################################
########## Code for Simulating a geometric random walk   #######
################################################################

set.seed(2012)
n = 253
pdf("FigA1.pdf",width = 6, height=6)  ########## Fig A.1
par(mfrow=c(3,3))
for (i in (1:9))
{
  logr = rnorm(n,0.05/253,0.2/sqrt(253))
  price = c(120,120*exp(cumsum(logr)))
  plot(price,type="b")
}
graphics.off()



  
  