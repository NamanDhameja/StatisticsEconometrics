D_87<-c(10,1,6,0.45,1.25,1.3,1.06,3,8.18,1.67,0.98,1,0.45,5.03,8,9,18,.28,7,3.97)
D_88<-c(3,1,5,0.5,1.54,1.5,0.8,2,0.67,1.17,0.51,0.5,0.61,6.7,4,7,19,0.2,5,3.83)

change<-D_88-D_87

length(D_88)
length(D_87)
avgch<-mean(change)
N<-length(change)
# Standard Change
sdch<- sd(change)
sdch
# Standard Error
se<- sdch/sqrt(N)
se
# to calculate confidence level
c<-qt(0.975,19)
c
c(avgch-c*se,avgch+c*se)


########################################################

#Install package for Audit dataset
install.packages("wooldridge")
library(wooldridge)
data<-audit
View(data)
change=mean(data$y)
change
n<-length(data$w)
n
sdy<-sd(data$y)
sdy # Standard deviation
sey<-sdy/sqrt(n)
sey  # Standard error
c<-(qt(0.975,240)) # OR c<-qnorm(0.975)
c(change-c*sey,change+c*sey)

c<-qnorm(0.995) # OR c<-qnorm(0.975)
c(change-c*sey,change+c*sey)


c<-qnorm(0.98) # OR c<-qnorm(0.975)
c(change-c*sey,change+c*sey)

###################
# Degree of freedom =n-1
df<-19

alpha.one.tailed=c(0.1,0.05,0.025,0.01,0.005,0.001)
alpha.two.tailed=alpha.one.tailed*2
# Critical values and table
cv<-qt(1- alpha.one.tailed,df )
cbind(alpha.one.tailed,alpha.two.tailed,cv)

