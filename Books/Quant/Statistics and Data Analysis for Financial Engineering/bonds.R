###########  R script for Chapter 3   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################


################################################################
#####  Code for section 3.4 and figure 3.1  ####################
################################################################
bondvalue = function(c,T,r,par) 
{
  #
  #   Computes bv = bond values (current prices) corresponding
  #       to all values of yield to maturity in the
  #       input vector r
  #
  #       INPUT
  #        c = coupon payment (semi-annual)
  #        T = time to maturity (in years)
  #        r = vector of yields to maturity (semi-annual rates)
  #        par = par value
  #
  bv = c/r + (par - c/r) * (1+r)^(-2*T)
  bv
}
################################

#   Computes the yield to maturity of a bond paying semi-annual
#   coupon payments
#
#   price, coupon payment, and time to maturity (in years)
#   are set below
#
#   Uses the function "bondvalue"
#
price = 1200    #   current price of the bond
C = 40          #   coupon payment
T= 30           #   time to maturity
par = 1000      #   par value of the bond

r = seq(.02,.05,length=300)
value = bondvalue(C,T,r,par) 
yield2M = spline(value,r,xout=price) 

################################################
#####  Code for figure 3.1  ####################
################################################
pdf("yield2m.pdf",width=6,height=5)
plot(r,value,xlab='yield to maturity',ylab='price of bond',type="l",
     main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200,col="red")
abline(v=yield2M,col="red")
graphics.off()

################

uniroot(function(r) bondvalue(C,T,r,par) - price, c(0.001,.1))

################################################
#####  Code for figure 3.2  ####################
################################################
rates = as.matrix(read.table("WeeklyInterest.txt",header=F))
ff = rates[,4]/100
tb03 = rates[,5]/100
cm10 = rates[,6]/100
cm30 = rates[,7]/100
n=length(ff)
year = 1900 + rates[1:n,1:3] %*% c(1/12, 1/365 , 1) -1/12
pdf("threerates.pdf",width=6,height=5) # Fig 3.2
plot(year,tb03,type="l",xlim=c(1978,1994),lwd=3,ylab="rate",lty=2)
lines(year[cm30>0],cm10[cm30>0],lty=1,lwd=2,col="red")
lines(year[cm30>0],cm30[cm30>0],lty=3,lwd=4,col="blue")
legend("topright",c("3-month","10-year","30-year"),lty=c(2,1,3),lwd=c(3,2,4),
       col=c("black","red","blue"))
graphics.off()


##################################################################
#####  Code for the R lab  #######################################
##################################################################


##########  Section 3.10.1  ##########

#   Computes the yield to maturity of a bond paying semi-annual
#   coupon payments
#
#   price, coupon payment, and time to maturity (in years)
#   are set below
#
#   Uses the function "bondvalue"
#
price = 1200    #   current price of the bond
C = 40          #   coupon payment
T= 30           #   time to maturity
par = 1000      #   par value of the bond

r = seq(.02,.05,length=300)
value = bondvalue(C,T,r,par) 
yield2M = spline(value,r,xout=price) 
round(yield2M$y,4)

pdf("yield2m.pdf",width=6,height=5)
plot(r,value,xlab='yield to maturity',ylab='price of bond',type="l",
     main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M$y)
graphics.off()

##########  Section 3.10.2  ##########

mk.maturity = read.csv("mk.maturity.csv",header=T)
mk.zero2 = read.csv("mk.zero2.csv",header=T)
plot(mk.maturity[,1],mk.zero2[5,2:56],type="l",
     xlab="maturity",ylab="yield")
lines(mk.maturity[,1],mk.zero2[6,2:56],lty=2,type="l")
lines(mk.maturity[,1],mk.zero2[7,2:56],lty=3,type="l")
lines(mk.maturity[,1],mk.zero2[8,2:56],lty=4,type="l")
legend("bottomright",c("1985-12-01", "1986-01-01",
                       "1986-02-01", "1986-03-01"),lty=1:4)

intForward = mk.maturity[,1]*mk.zero2[6,2:56]
xout=seq(0,20,length=200)
z1=spline(mk.maturity[,1],intForward,xout=xout)
forward = diff(z1$y)/diff(z1$x)
T_grid = (xout[-1]+xout[-200])/2
plot(T_grid,forward,type="l",lwd=2,ylim=c(0.06,0.11))

##########  Problem 9  ##########

intForward = mk.maturity[,1]*mk.zero2[6,2:56]
xout=seq(0,20,length=200)
z1=spline(mk.maturity[,1],intForward,xout=xout)
forward = diff(z1$y)/diff(z1$x)
T_grid = (xout[-1]+xout[-200])/2
plot(T_grid,forward,type="l",lwd=2,ylim=c(0.06,0.11))

intForward = mk.maturity[,1]*mk.zero2[7,2:56]
z1=spline(mk.maturity[,1],intForward,xout=xout)
forward = diff(z1$y)/diff(z1$x)
lines(T_grid,forward,type="l",lwd=2,lty=2)

intForward = mk.maturity[,1]*mk.zero2[8,2:56]
z1=spline(mk.maturity[,1],intForward,xout=xout)
forward = diff(z1$y)/diff(z1$x)
lines(T_grid,forward,type="l",lwd=2,lty=3)

intForward = mk.maturity[,1]*mk.zero2[9,2:56]
z1=spline(mk.maturity[,1],intForward,xout=xout)
forward = diff(z1$y)/diff(z1$x)
lines(T_grid,forward,type="l",lwd=2,lty=4)

legend("bottomright",c("1985-12-01", "1986-01-01",
                       "1986-02-01", "1986-03-01"),lty=1:4,lwd=2)


##################################################################
#####  Code for the exercises  ###################################
##################################################################

##########  Exercise 2  ##########

forward = function(t){0.04 + 0.0002 * t - 0.00003*t^2}
yield = function(t){0.04 + 0.0001 * t - 0.00001*t^2}
# (a)
yield(8)
# (b)
1000*exp(-5*yield(5))
#  (c)
tgrid = seq(0,20,by=.02)
plot(tgrid,forward(tgrid),type="l",lwd=2,ylab="interest rate")
lines(tgrid,yield(tgrid),lty=2,lwd=2)
round(exp(-9*yield(9)+10*yield(10)) - 1,4)


##########  Exercise 5  ##########
yields = c(.025,.028,.032,.033)
T = seq(.5,2,by=.5)
cashflows = c(35,35,35,1035)
prices = cashflows * exp(-T*yields)
print(sum(prices),digits=6)

##########  Exercise 11  ##########

int=(0.033)*(15) + (0.0012)*(15^2)/2
100*exp(-int)


###########  Exercise 12  ##########

p1 = exp(-8*(.04+.001*8/2))
p2 = exp(-7.5*(.03+.0013*7.5/2))
options(digits=3)
return = p2/p1-1
return
2*return ### annualized return


###########  Exercise 21  ##########

par = 1000
T = 4
C = 25
price = 1015
yieldToPrice = function(C,par,yield,T){sum( C/(1+yield)^(1:(2*T))) + par/(1+yield)^(2*T)}
yield = uniroot(function(yield){
  yieldToPrice(C,par,yield,T) - price}, lower=.0001,upper = .1 )
round(yield$root,4)



