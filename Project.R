dat1<-read.table(file="C:\\Users\\niazs\\Desktop\\Applied Regression Analysis\\data_Niaz.txt",header = T)
attach(dat1)
summary(dat1)
X6<-as.factor(dat1$X6)
X7<-as.factor(dat1$X7)
X11<-as.factor(dat1$X11)
is.factor(X6)
Z7<-as.numeric(X7==3)
counts <- table(X6)
barplot(counts, main="Bar Diagram for Carburetor", 
        xlab="Carburetor")

counts <- table(X7)
barplot(counts, main="Bar Diagram for Number of Transmission Speeds", 
        xlab="No. of Transmission Speeds")
counts <- table(X11)
barplot(counts, main="Bar Diagram for Type of Transmission", 
        xlab="Tyoes of Transmission")

hist(dat1$Y,main = "Miles per gallon")
hist(dat1$X1,main = "Displacement")
hist(dat1$X2,main = "Horsepower")
hist(dat1$X3,main = "Torque")
hist(dat1$X4,main = "Compression Ratio")
hist(dat1$X5,main = "Rear axle ratio")
hist(dat1$X8,main = "Oveall length")
hist(dat1$X9,main = "Width")
hist(dat1$X10,main = "Weight")


confint(mean(dat1$X1),level=0.95)

t.test(X1)

sapply(dat1, function(x) length(unique(x)))
t.test(Z7)

t.test(Y[X11==1],Y[X11==0])
t.test(Y[Z7==1],Y[Z7==0])
contrasts(X11)

an6<-aov(Y~X6)
res6<-residuals(an6)

an7<-aov(Y~X7)
res7<-residuals(an7)
an11<-aov(Y~X11)
res11<-residuals(an11)
anova(lm(Y~X7))

bartlett.test(Y~X6)
library(car)
leveneTest(Y~X7)

shapiro.test(res11)
library(nortest)
ad.test(resid(res7))
cvm.test(res7)
lillie.test(res7)
library(MASS)

out<-boxCox(Y~X6)
out$x[which.max(out$y)]
Yst<-((Y^-0.7474747)-1)/-0.7474747

an6<-aov(Yst~X6)
res6<-residuals(an6)
shapiro.test(res6)
cvm.test(res6)
lillie.test(res6)


bartlett.test(Yst~X7)
levene.test(Yst~X7)

## multi comp test for X6 and X7
TukeyHSD(aov(Yst~X6))
pairwise.t.test(Yst,X6,p.adjust.method = "fdr",pool.sd = F)

## multi comp test for X6
posthoc.kruskal.dunn.test(Y~X6,p.adjust="bonf")

X<-subset(dat1,select=c(1,2,3,4,5,6,9,10,11))
names(X)
cor(X)
##test of independence
chisq.test(cbind(X6,Z7,X11))
source("procfreq.R")
procfreq(cbind(X6,X7,X11))
lrtest(cbind(X6,X7,X11))

Z7<-as.numeric(X7==3)
lreg<-lm(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)
summary(lreg)
##normality of errors
resreg<-residuals(lreg)
shapiro.test(resreg)

##partial residual plot to detect the error variability

lreg<-lm(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)
beta<-coef(lreg)
resid1<-residuals(lreg)
plot(X11,resid1+X11*beta[12])

##plot of residuals vs fitted y to detect heteroscedasticity
yhat<-fitted(lreg)
t<-rstudent(lreg)##rstudent 
plot(yhat,t) 
abline(0,0)

e<-residuals(lreg)
plot(yhat,e)
abline(0,0)


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lreg)

## box-cx for resloving heteroscedasticity by transfroming Y
out1<-boxcox(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)
out1$x[which.max(out1$y)]
Yst<-((Y^-0.7070707)-1)/-0.7070707
lreg1<-lm(Yst~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)

par(mfrow=c(2,2)) # init 4 charts in 1 panel for testing hetero.
plot(lreg1)
library(lmtest)
bptest(lreg)


lreg2<-lm(Yst1~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)

##autocorrelation checking
library(lmtest)
dwtest(lreg,alternative = "two.sided")
library(car)
vif(lreg)
vif(lreg2) 

##ridge regression
library(MASS)
fit=lm.ridge(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11,
               lambda=seq(0,100,0.05))
plot(fit$lambda, fit$GCV)
select(fit)

fit2=lm.ridge(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11,lambda=32.8)

##partial regression plot##no need
resid.y1<-residuals(lm(Y~X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11))
resid.x1<-residuals(lm(X1~X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11))
plot(resid.x1,resid.y1)

# ques 13
qt(0.05/(2*30),30-12-1,lower.tail = F)
myResid<-residuals(lreg)
MSE<-summary(lreg)$sigma^2
StanResid<-myResid/sqrt(MSE)
h<-hatvalues(lreg)
StudResid<-rstandard(lreg)
RStudent<-rstudent(lreg)
cbind(myResid,StanResid,StudResid,RStudent)

plot(myResid)
plot(StanResid)
plot(StudResid)
plot(RStudent)


##leverage or influential point
2*12/30 # h_ii

2/sqrt(30) # DFBETAS

2*sqrt(12/25) # DFFITS
1+3*12/30 # upper bound for COVRATIO
1-3*12/30 # lower bound for COVRATIO
lreg<-lm(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)
influence.measures(lreg) # dfbeta, dffit, covratio, cook.d, hat

##(15) choosing best model using Adj R^2
library(leaps)
ndat<-dat1[,-c(1,8)]
X<-cbind(ndat,Z7)
y<-dat1[,1]

myr2=leaps(X,y,method="r2")
myadjr2=leaps(X,y,method="adjr2")
cbind(myr2$which,R2=myr2$r2,
        myadjr2$which,AdjR2=myadjr2$adjr2)[which.max(myadjr2$adjr2),]
lreg<-lm(Y~X5+Z7+X9)

##stepwise selection
lreg<-lm(Y~X1+X2+X3+X4+X5+X6+Z7+X8+X9+X10+X11)
full.model<-lreg
stepwise=step(full.model, direction='both')
summary(stepwise)

lregnew<-lm(Y~X5+X8+X10)
resregnew<-residuals(lregnew)
shapiro.test(resregnew)
library(nortest)
ad.test(resregnew)

##partial residual plot to detect the linear relationship bet Y and X's
betanew<-coef(lregnew)

plot(X10,resregnew+X10*betanew[4])


##plot of residuals vs fitted y to detect heteroscedasticity
yhatnew<-fitted(lregnew)
t<-rstudent(lregnew)##rstudent 
plot(yhatnew,t) 
abline(0,0)

e<-residuals(lregnew)
plot(yhatnew,e)
abline(0,0)


##autocorrelation checking
library(lmtest)
dwtest(lregnew,alternative = "two.sided")

## multicollinearity checking
library(car)
vif(lregnew)

library(MASS)
fitnew=lm.ridge(Y~X5+X8+X10,lambda=seq(0,100,0.05))
plot(fitnew$lambda, fitnew$GCV)
select(fitnew)
fitnew1=lm.ridge(Y~X5+X8+X10,lambda=0.1)

