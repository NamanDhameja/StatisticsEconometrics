library(wooldridge)
# Dataset in wooldridge 
data("ceosal1")
View(ceosal1)
# Declare variable from the ceosal1 dataset
x=ceosal1$roe
y=ceosal1$salary

cov(x,y)  # Covariance 
var(x)  #Variance
b1=cov(x,y)/var(x)  #Beta1
b1
b0=mean(y)-b1*mean(x) #Beta0
b0

CEOSAL<-lm(x~y) # Tild
CEOSAL
CEOreg<-lm(ceosal1$salary~ ceosal1$roe)
names(CEOreg)

fit_hat<-CEOreg$fitted.values
fit_hat
u_hat<-CEOreg$residuals
u_hat
View(cbind(ceosal1$roe,ceosal1$salary,fit_hat,u_hat))

plot(ceosal1$roe,u_hat,ylim = c(0,3500))
abline(CEOreg)

plot(x,y,ylab="salary",xlab="ROE",ylim=c(0,3500))
abline(CEOSAL)

# Calculate R square value
summary(CEOreg)


###################### --------------------- ##################

data("wage1")

View(wage1)

y=wage1$wage
x=wage1$educ

length(x)
length(y)
cov(x,y)  # Covariance 
var(x)  #Variance

b1=cov(x,y)/var(x)  #Beta1
b1
b0=mean(y)-b1*mean(x) #Beta0
b0

WAGE<-lm(x~y) # Tild
WAGE

plot(x,y)
abline(WAGE)

######## -----------------  ###################
data("vote1")
View(vote1)


x=vote1$shareA
y=vote1$voteA

length(x)
length(y)
cov(x,y)  # Covariance 
var(x)  #Variance

b1=cov(x,y)/var(x)  #Beta1
b1
b0=mean(y)-b1*mean(x) #Beta0
b0

VOTE<-lm(x~y) # Tild
VOTE

plot(x,y)
abline(VOTE)