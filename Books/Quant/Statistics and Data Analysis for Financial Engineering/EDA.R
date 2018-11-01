###########  R script for Chapter 4   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

################################################
#####  Code for figures 4.1 - 4.4 ##############
################################################

data(SP500,package="Ecdat")
SPreturn = SP500$r500

library(evir)  # for emplot
data(Garch,package="Ecdat")
attach(Garch)

data(Capm,package="Ecdat")
diffrf=diff(Capm$rf)

diffdm = diff(dm)  #  Deutsch mark
diffbp = diff(bp)  #  British pound
diffcd = diff(cd)  #  Canadian dollar
diffdy = diff(dy)  #  Japanese yen

n = length(SPreturn)
n2 = length(diffdm)
n3 = length(diffrf)
Reldiffrf = diffrf/(Capm$rf[2:(n3+1)])

year_SP = 1981 + (1:n)* (1991.25-1981)/n
year_dm = 1980 + (1:n2)* (1987.5-1980)/n2
year_rf = 1960 + (1:n3) * (2003 - 1960)/n3

pdf("SP_plot.pdf",width=6,height=4)   # Figure 4.1
plot(year_SP,SPreturn,main="S&P 500 daily returns",xlab="year",type="l",
     ylab="log return")
graphics.off()

pdf("DM_plot.pdf",width=6,height=4)    #  Figure 4.2
plot(year_dm,diffdm,xlab="year",ylab="change in rate",main=
       "changes inDM/dollar exchange rate",type="l")
graphics.off()

pdf("rf_plot.pdf",width=6,height=4)  #  Figure 4.3
plot(year_rf,diffrf,main="changes in risk-free interest return",xlab="year",
     ylab="change in rate",type="l")
graphics.off()

pdf("SP_hist.pdf",width=7,height=6)  #  Figure 4.4
par(mfrow=c(2,2))
hist(SPreturn,breaks=30,xlab="return",
     main="(a) 30 cells, full range")
hist(SPreturn,breaks=30,main="(b) 30 cells, central range",
     xlim=c(-.04,.04),xlab="return") #,cex.main=1.5)
hist(SPreturn,breaks=20,main="(c) 20 cells, central range",
     xlim=c(-.04,.04),xlab="return",)
hist(SPreturn,breaks=50,main="(d) 50 cells, central range",
     xlim=c(-.04,.04), xlab="return")
graphics.off()



################################################
#####  Code for figure 4.5 #####################
################################################
pdf("constructingKDE.pdf",width = 6,height = 8)  #  Figure 4.5
set.seed("992291")
x = rnorm(6)
l = matrix(1,200,6)
l2 = l
l3 =l
xgrid = seq(from=-3,to=1.5,length=200)
for (i in 1:6)
{
  l[,i] = dnorm(xgrid,x[i],.4)/6
  l2[,i] = dnorm(xgrid,x[i],.2)/6
  l3[,i] = dnorm(xgrid,x[i],.8)/6
  
}
par(mfrow=c(2,1))
kde = l%*%matrix(1,6,1)
kde2 = l2%*%matrix(1,6,1)
kde3 = l2%*%matrix(1,6,1)
plot(xgrid,kde,type="l",lwd=3,xlab="x",
     font.lab=1,
     ylab="KDE",main="b = 0.4")
for (i in 1:6)
{
  lines(xgrid,l[,i],lwd=2,lty=2,col=i)
}
rug(x,side=1,lwd=2)

plot(xgrid,kde2,type="l",lwd=3,xlab="x",ylab="KDE",main="b = 0.2")
for (i in 1:6)
{
  lines(xgrid,l2[,i],lwd=2,lty=2,col=i)
}
rug(x,side=1,lwd=2)
graphics.off()

################################################
#####  Code for figure 4.6 #####################
################################################
pdf("SP_density.pdf",width=7,height=5)  ##  Figure 4.6
par(mfrow=c(1,1))
data(SP500,package="Ecdat")
SPreturn = SP500$r500
plot(density(SPreturn,kernel="gaussian",adjust=1),xlim=c(-.04,.04),
     main="S&P 500 daily returns",lwd=2,
     ylim=c(0,60),xlab="return")

fit_d = density(SPreturn,kernel="gaussian",adjust=1/3)
lines(fit_d$x,fit_d$y,lty=2,lwd=2,col="red")
fit_d = density(SPreturn,kernel="gaussian",adjust=3)
lines(fit_d$x,fit_d$y,lty=4,lwd=2,col="blue")
legend(-.04,45,c('adjust=1','adjust=1/3','adjust=3'),lty=c(1,2,4),
       lwd=c(2,2,2),box.lty=0,cex=1.3,col=c("black","red","blue"))
graphics.off()


################################################
#####  Code for figure 4.7 #####################
################################################
pdf("SP_density_norm.pdf",width=10,height=7)  ##  Figure 4.7
par(mfrow=c(1,2))
data(SP500,package="Ecdat")
SPreturn = SP500$r500
plot(density(SPreturn,kernel="gaussian"),xlim=c(-.06,.06),
     main="(a) standard estimates",lwd=2,cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,63),cex.main=1.5,lty=1)
z = seq(from=-.06,to=.06,by=.001)
lines(z,dnorm(z,mean=median(SPreturn),sd=sd(SPreturn)),
      lty=2,lwd=3,cex.lab=1.5,cex.axis=1.5,col="red")
legend(-.065,63,c("estimated density","normal density"),lty=c(1,2),cex=1.5,
       box.lty=0,lwd=2,col=c("black","red"))
plot(density(SPreturn,kernel="gaussian"),xlim=c(-.06,.06),
     main="(b) robust estimates",lwd=2,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,
     ylim=c(0,63),lty=1)
lines(z,dnorm(z,mean=median(SPreturn),sd=mad(SPreturn)),lty=2,lwd=3,col="red")
legend(-.065,63,c("estimated density","normal density"),lty=c(1,2),cex=1.5,
       box.lty=0,lwd=2,col=c("black","red"))
graphics.off()

################################################
#####  Code for figure 4.8 #####################
################################################
set.seed("991155")  
edf_norm= ecdf(rnorm(150))
pdf("normalcdfplot.pdf",width=6,height=5)  ##  Figure 4.8
par(mfrow=c(1,1))
plot(edf_norm, verticals= TRUE, do.p = FALSE,main="EDF and CDF")
tt=seq(from=-3,to=3,by=.01)
lines(tt,pnorm(tt),lty=2,lwd=2,col="red")
legend(1.5,.2,c("EDF","CDF"),lty=c(1,2),lwd=c(1.5,2),col=c("black","red"))
graphics.off()

################################################
#####  Code for figure 4.9 #####################
################################################
pdf("convex_concave.pdf",width=6,height=5)  ## Figure 4.9
x = (1:299)/300
f1  = exp(3*x)
f2 = log(x+.01)
f3 = qnorm(x)
f4 = pnorm(6*x -3)
par(mfrow=c(2,2))
plot(x,f1,type="l",main="Convex", xlab="sample quantile", ylab="normal quantile")
plot(x,f2,type="l",main="Concave", xlab="sample quantile", ylab="normal quantile")
plot(x,f4,type="l",main="Convex-concave", xlab="sample quantile", ylab="normal quantile")
plot(x,f3,type="l",main="Concave-convex", xlab="sample quantile", ylab="normal quantile")
graphics.off()

################################################
#####  Code for figure 4.10 ####################
################################################
pdf("normal_plots_normal.pdf")  ##  Fig 4.10
par(mfrow=(c(3,2)))
set.seed("543")
x1=rnorm(20)
qqnorm(x1,datax=T,main="n = 20")
qqline(x1,datax=T)
x1=rnorm(20)
qqnorm(x1,datax=T,main="n = 20")
qqline(x1,datax=T)
x1=rnorm(150)
qqnorm(x1,datax=T,main="n = 150")
qqline(x1,datax=T)
x1=rnorm(150)
qqnorm(x1,datax=T,main="n = 150")
qqline(x1,datax=T)
x1=rnorm(1000)
qqnorm(x1,datax=T,main="n = 1000")
qqline(x1,datax=T)
x1=rnorm(1000)
qqnorm(x1,datax=T,main="n = 1000")
qqline(x1,datax=T)
graphics.off()

################################################
#####  Code for figure 4.11 ####################
################################################
pdf("normal_plots_lognormal.pdf")  ##  Figure 4.11
par(mfrow=(c(3,2)))
set.seed("864")
x1=rlnorm(150,sd=1)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1"))) 
qqline(x1,datax=T)
x1=rlnorm(150,sd=1/2)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1/2"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1/2)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1/2"))) 
qqline(x1,datax=T)
x1=rlnorm(150,sd=1/5)
qqnorm(x1,datax=T,main=expression(paste("n=150, ", sigma, " = 1/5"))) 
qqline(x1,datax=T)
x1=rlnorm(1000,sd=1/5)
qqnorm(x1,datax=T,main=expression(paste("n=1000, ", sigma, " = 1/5"))) 
qqline(x1,datax=T)
graphics.off()

################################################
#####  Code for figure 4.12 ####################
################################################
pdf("normal_plots_t.pdf")  ##  Figure 4.12
par(mfrow=(c(3,2)))
set.seed("7290")
x1=rt(150,df=4)
qqnorm(x1,datax=T,main="n=150, df=4") 
qqline(x1,datax=T)
x1=rt(1000,df=4)
qqnorm(x1,datax=T,main="n=150, df=4") 
qqline(x1,datax=T)
x1=rt(150,df=10)
qqnorm(x1,datax=T,main="n=150, df=10") 
qqline(x1,datax=T)
x1=rt(1000,df=10)
qqnorm(x1,datax=T,main="n=150, df=10") 
qqline(x1,datax=T)
x1=rt(150,df=30)
qqnorm(x1,datax=T,main="n=150, df=30") 
qqline(x1,datax=T)
x1=rt(1000,df=30)
qqnorm(x1,datax=T,main="n=150, df=30") 
qqline(x1,datax=T)
graphics.off()

################################################
#####  Code for figure 4.13 ####################
################################################
pdf("Bimodal_QQ.pdf",width=7,height=4)   ##  Figure 4.13
set.seed(997733)
x = c(rnorm(250,mean=6),rnorm(150,mean=1),rnorm(250,mean = 12))
par(mfrow=c(1,2))
plot(density(x))
qqnorm(x,datax=T)
qqline(x,datax=T)
graphics.off()

data(SP500,package="Ecdat")
SPreturn = SP500$r500

library(evir)  # for emplot
library(faraway)
data(Garch,package="Ecdat")
attach(Garch)


################################################
#####  Code for figure 4.14 ####################
################################################
data(Capm,package="Ecdat")
diffrf=diff(Capm$rf)
diffdm = diff(dm)  #  Deutsch mark
diffbp = diff(bp)  #  British pound
diffcd = diff(cd)  #  Canadian dollar
diffdy = diff(dy)  #  Japanese yen
library(faraway)
pdf("dm_halfnormal.pdf",width=7,height=6)  # Figure 4.14
par(mfrow=c(1,1))
halfnorm(abs(diffdm), main="changes in DM/dollar exchange rate",
  ylab="Sorted data")
graphics.off()

################################################
#####  Code for figure 4.15 ####################
################################################
pdf("SP_qqnorm.pdf",width=5,height=7)  ##  Figure 4.15
par(mfrow=c(3,2))
qqnorm(SPreturn,datax=TRUE,ylab="Data",xlab="normal quantiles",
       main="(a) Normal probability plot")
qqline(SPreturn,datax=TRUE)
s_SPreturn = sort(SPreturn)
grid = (1:n)/(n+1)
qqplot(s_SPreturn, qt(grid,df=1),main="(b) t-probability plot, 
       df = 1",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 1) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=2),main="(c) t-probability plot, 
       df = 2",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 2) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=4),main="(d) t-probability plot, 
       df = 4",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 4) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

qqplot(s_SPreturn, qt(grid,df=8),main="(e) t-probability plot, 
       df = 8",xlab="Data",ylab="t-quantiles")
lmfit = lm( qt(c(.25,.75),df = 8) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)


qqplot(s_SPreturn, qt(grid,df=15),main="(f) t-probability plot, 
       df = 15",xlab="Data",ylab="t-quantiles")
lines(quantile(s_SPreturn,c(.25,.75)),qt(c(.25,.75),df=15))
lmfit = lm( qt(c(.25,.75),df = 15) ~ quantile(s_SPreturn,c(.25,.75)) )
abline(lmfit)

graphics.off()



################################################
#####  Code for figure 4.16 ####################
################################################
pdf("sampleQQplots.pdf",width=6,height=6)  ##  Figure 4.16
par(mfrow=c(2,2))
qqplot(SPreturn,diffdm,
       xlab="S&P return",ylab="change in DM/dollar rate",main="(a)")
xx= quantile(SPreturn,c(.25,.75))
yy = quantile(diffdm,c(.25,.75))
slope = (yy[2]-yy[1])/(xx[2]-xx[1])
inter = yy[1]-slope*xx[1]
abline(inter,slope,
       lwd=2 )
qqplot(SPreturn,diffrf,
       xlab="S&P return",ylab="change in risk-free return",main="(b)")
xx= quantile(SPreturn,c(.25,.75))
yy = quantile(diffrf,c(.25,.75))
slope = (yy[2]-yy[1])/(xx[2]-xx[1])
inter = yy[1]-slope*xx[1]
abline(inter,slope,
       lwd=2 )
qqplot(diffrf,diffdm,
       ylab="change in DM/dollar rate",xlab="change in risk-free return",main="(c)")
xx= quantile(diffrf,c(.25,.75))
yy = quantile(diffdm,c(.25,.75))
slope = (yy[2]-yy[1])/(xx[2]-xx[1])
inter = yy[1]-slope*xx[1]
abline(inter,slope,
       lwd=2 )
graphics.off()

################################################
#####  Code for figure 4.17 ####################
################################################
pdf("boxplots.pdf")         ##  Figure 4.17
par(mfrow=c(1,1))
boxplot(list(SPreturn,diffdm,diffrf),boxwex=.5,
        names=list("S&P 500","DM/dollar","risk-free") )
graphics.off()

################################################
#####  Code for figure 4.18 ####################
################################################
pdf("boxplots2.pdf")        ##  Figure 4.18
x1 = SPreturn
x1s = (x1-median(x1))/mad(x1)
x2 = diffdm
x2s = (x2-median(x2))/mad(x2)
x3 = diffrf
x3s = (x3-median(x3))/mad(x3)
boxplot(list(x1s,x2s,x3s),boxwex=.5,
        names=list("S&P 500","DM/dollar","risk-free") )
graphics.off()



dat=read.csv("FlowData.csv")
dat = dat/10000
x1=dat$Flow1


################################################
#####  Code for figure 4.19 ####################
################################################
pdf("risk_free_w_w0_log.pdf",width=6,height=6)  # Figure 4.19
par(mfrow=c(2,2))
plot(Capm$rf[-1],diff(Capm$rf),xlab="lagged rate",ylab="change in rate",
     main="(a)")
plot(year_rf,diff(Capm$rf),xlab="year",ylab="change in rate",
     main="(b)")
plot(Capm$rf[-1],diff(log(Capm$rf)),xlab="lagged rate",ylab="change in log rate",
     main="(c)")
plot(diff(log(Capm$rf)),xlab="year",ylab="change in log rate",
     main="(d)")
graphics.off()

################################################
#####  Code for figure 4.20 ####################
################################################
pdf("FlowsHist.pdf")  # Figure 4.20
dat=read.csv("FlowData.csv")
dat = dat/10000
par(mfrow=c(2,2))
hist(dat$Flow1)
hist(dat$Flow2)
hist(dat$Flow3)
graphics.off()

################################################
#####  Code for figure 4.21 ####################
################################################
pdf("GasFlowsTransHist.pdf",width=6,height=4.5)  #  Figure 4.21
x1=dat$Flow1
par(mfrow=c(2,3))
plot(density(x1),main=expression(alpha == 1) )
plot(density((x1^2-1)/2),main=expression(alpha == 2) )
plot(density((x1^3-1)/3),main=expression(alpha == 3) )
plot(density((x1^4-1)/4),main=expression(alpha == 4) )
plot(density((x1^5-1)/5),main=expression(alpha == 5) )
plot(density((x1^6-1)/6),main=expression(alpha == 6) )
graphics.off()


################################################
#####  Code for figure 4.22 ####################
################################################


pdf("ttest_lognormal.pdf",width=7,height=4)  # Figure 4.22
set.seed("119933")
x1=rlnorm(20,meanlog=1,sdlog=2)
hist(x1,n=15)
x2=rlnorm(20,meanlog=3,sdlog=2)
hist(x2,n=15)
par(mfrow=c(1,2))
boxplot(list(x1,x2),main="(a) no transformation")
boxplot(list(log(x1),log(x2)),main="(b) log transformation")
t.test(x1,x2,equal.var=F)
t.test(log(x1),log(x2))
graphics.off()


######    Figures 4.23 and 4.24 were produced by Matlab          #####
######    for the book "Statistics and Finance: An Introduction  #####
######    No code is available                                   #####



################################################
#####  Code for figure 4.25 ####################
################################################
lambda = seq(from=-1,to=2,by=.01)
pdf("DerRatio.pdf",width=6,height=5)  # Figure 4.25
par(mfrow=c(1,1))
plot(lambda,2^(lambda-1),type="l",lwd=2,xlab=expression(alpha),
     ylab="Derivative ratio",main="Example: b/a = 2")
abline(h=1,lty=2)
abline(v=1,lty=2)
text(1.5,.3,"convex")
text(.5,.3,"concave")
graphics.off()

################################################
#####  Code for figure 4.26 ####################
################################################
pdf("risk_free_var_stabilize.pdf",width=6,height=5)  # Figure 4.26
library("car")  ##  for bcPower
library("Ecdat")   ##  for Capm
data(Capm)
alpha = seq(-.1,1,by =.01)
result_sq = 0*alpha
result_abs = result_sq
n_rf = length(Capm$rf)
for (i in 1:length(alpha))
{
  result_sq[i] =cor(   Capm$rf[-n_rf],abs(diff(bcPower(Capm$rf,alpha[i])))^2   )
  result_abs[i] =cor(   Capm$rf[-n_rf],abs(diff(bcPower(Capm$rf,alpha[i])))  )
}
par(mfrow=c(1,1))
plot(alpha,result_abs,lty=1,type="l",xlab=expression(alpha),
     ylab="correlation",
     lwd=2)
lines(alpha,result_sq,lty=5,lwd=2,col="red")
abline(h=0)
abline(v=spline(result_sq,alpha,xout=0)$y,lty=5,col="red")
abline(v=spline(result_abs,alpha,xout=0)$y)
legend(.3,-.02,c("absolute changes","squared changes"),lwd=2,lty=c(1,5),col=c("black","red"))
graphics.off()


################################################
#####  Code for figure 4.27 ####################
################################################

pdf("Earning_TKDE.pdf",width=6,height=5)  #  Figure 4.27
library("Ecdat")   ##  for Earnings
data(Earnings)
attach(Earnings)
ind = (Earnings$age=="g1")
x = Earnings$y[ind]/1000
f = density(x)
froot = density(sqrt(x))
ind2 = (froot$x > sqrt(min(x)))
par(mfrow = c(1,1))
plot(f$x[ind2],f$y[ind2],type="l",ylim=c(0,.035),xlim=c(0,100),
     ylab="Density(y)",xlab="y=income (in $1000)",lwd=2)
abline(h=0)
f2 = .5*froot$y / froot$x
lines(froot$x[ind2]^2, f2[ind2],type="l",
      ylim=c(0,.035),xlim=c(0,100),ylab="Density(y)",xlab="y=income (in $1000)",  
      main="TKDE",lty=2,lwd=3,col="red")
abline(h=0)
legend(60,.03,c("KDE","TKDE"),lty=c(1,2),lwd=c(2,3),col=c("black","red"))
graphics.off()


pdf("Earning_hist.pdf",width=7,height=4)  #  Figure 4.28
par(mfrow=c(1,2))
y=x
hist(y)
hist(sqrt(y))
graphics.off()

################################################
#####  Begin R lab          ####################
################################################

data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)

pdf("EuStocks.pdf",width=6,height=5)
plot(EuStockMarkets)
graphics.off()

logR = diff(log(EuStockMarkets))
plot(logR)
plot(as.data.frame(logR))

par(mfrow=c(2,2))
for(i in colnames(logR))
{
  qqnorm(logR[,i],datax=T,main=i)
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

n=dim(logR)[1]
q_grid = (1:n)/(n+1)
df_grid=c(1,4,6,10,20,30)
index.names = dimnames(logR)[[2]]
for(i in 1:4)
{
  # windows()
  # dev.new()
  par(mfrow=c(3,2))
  for(df in df_grid)
  {
    qqplot(logR[,i], qt(q_grid,df),
           main=paste(index.names[i], ", df=", df) )
    abline(lm(qt(c(.25,.75),df=df)~quantile(logR[,i],c(.25,.75))))
  }
}



library("fGarch")
x=seq(-.1,.1,by=.001)
par(mfrow=c(1,1))
df = 3.5
plot(density(logR[,1]),lwd=2,ylim=c(0,60))
lines(x,dstd(x,mean=mean(logR[,1]),sd=sd(logR[,1]),nu=df),
      lty=5,lwd=2,col="red")
lines(x,dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),
      lty=3,lwd=4,col="blue")
legend("topleft",c("KDE",paste("t: df=",df),"normal"),lwd=c(2,2,4),
       lty=c(1,5,3),col=c("black","red","blue"))


#############  McDonald's  ##########

data = read.csv('MCD_PriceDaily.csv')
head(data)
adjPrice = data[ ,7]
plot(adjPrice, type = "l", lwd = 2)

LogRet = diff(log(adjPrice))
hist(LogRet, 80, freq = FALSE)



################################################
#####  Begin exercises      ####################
################################################


###  Exercise 1

ford = read.csv("ford.csv")
FORD = ford[,3]
options(digits=3)
mean(FORD)
median(FORD)
sd(FORD)

pdf("fordNormal.pdf",width=6,height=5)
qqnorm(FORD,datax=T)
graphics.off()

shapiro.test(FORD)
n=length(FORD)
q.grid = (1:n)/(n+1)
df=c(1,2,3,4,5,6,7,8,10)


pdf("fordNormalPlots.pdf",width=6,height=6)
par(mfrow=c(3,3))
for(j in 1:length(df))
{
  qqplot(FORD, qt(q.grid,df=df[j]),
         main=paste("df=", df[j]) )
  abline(lm(qt(c(.25,.75),df=df[j])~quantile(FORD,c(.25,.75))))
}
graphics.off()


options(digits=4)
den=density(FORD)
n =  length(FORD)
sqrt(.25/n)/spline(den$x,den$y,xout=median(FORD))$y
sd(FORD)/sqrt(n)


# exercise 5



pdf("EDAproblems05A.pdf",width=6,height=4)
par(mfrow=c(3,2))
data(Garch,package="Ecdat")
diffbp = diff(Garch$bp)

p=.25
qqnorm(diffbp,datax=TRUE,main=paste("p = ",p))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.1
qqnorm(diffbp,datax=TRUE,main=paste("p = ",p))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.05
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.01
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.0025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)
graphics.off()




pdf("EDAproblems05B.pdf",width=6,height=4)
par(mfrow=c(3,2))
diffbp = rnorm(length(diffbp))

p=.25
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.1
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.05
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.01
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.0025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)
graphics.off()





pdf("EDAproblems05C.pdf",width=6,height=4)
par(mfrow=c(3,2))
diffbp = diff(log(Garch$bp))

p=.25
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.1
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.05
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.01
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

p=.0025
qqnorm(diffbp,datax=TRUE,main=paste("p = ",as.character(p)))
lmfit = lm( qnorm(c(p,1-p)) ~ quantile(diffbp,c(p,1-p)) )
abline(lmfit,lwd=2)

graphics.off()





