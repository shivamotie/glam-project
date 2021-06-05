data0 <- read.csv("pollution.csv", h=T)
data <- na.omit(data0)
sapply(data, class)
library(gee)
require(mgcv)
attach(data)
#data$weekday <-factor(data$weekday, levels = unique(data$weekday))
#data$year <- factor(data$year, levels = unique(data$year))
#data$month <- factor(data$month, levels = unique(data$month))

boxplot(resp , main = "resp" , col = "gray")
hist(resp)

pairs(data[1:7])
cor(rsp,no2) #0.7869
cor(so2,no2) #0.4542
boxplot(resp~weekday, main="each weekday", xlab="weekdays",ylab="resp")
boxplot(resp~month, main="each month", xlab="month", ylab="resp")
boxplot(resp~day, main="each days", xlab="day",ylab="resp")
boxplot(resp~year,main="each year",xlab="year",ylab="resp")

plot(resp)
plot(no2)
plot(so2)
plot(rsp)
plot(o3)
plot(temp)
plot(hum)


m1<- gam(resp~ s(no2) + s(so2) + s(o3, k=8) +s(rsp)+ s(temp, bs="cc") + s(hum,k=15,bs="cc") + factor(year)+ 
           s(day)+ factor(month)+factor(weekday), family = poisson)

summary(m1)
extractAIC(m1)


m2<- gam(resp~ s(no2) + s(so2) + s(o3, k=8) + s(temp, bs="cc") + s(hum,k=15,bs="cc") + factor(year)+ 
           s(day)+ factor(month)+factor(weekday), family = poisson)
summary(m2)
gam.check(m2)
extractAIC(m2)


m3<- gam(resp~ s(no2) + s(so2) + s(o3) + s(rsp) +s(temp, bs="cc") + s(hum,k=15, bs="cc") + factor(year)+ 
           s(day)+ factor(month)+factor(weekday)+ factor(year)*factor(month), family = poisson)
summary(m3)
gam.check(m3)
extractAIC(m3)


#without day
m4<- gam(resp~ s(no2) + s(so2) + s(rsp) + s(o3) + s(temp, bs="cc") + s(hum, k=15, bs="cc") + 
           factor(year)+ factor(month)+factor(weekday), family = poisson)
summary(m4)
gam.check(m4)
extractAIC(m4)


extractAIC(m1)
extractAIC(m2)  #lowest
extractAIC(m3)
extractAIC(m4)


        #====================================================#



#change the levels of month, to have "seasons

data$month_bis[data$month==12] <- 1
data$month_bis[data$month==1] <- 1
data$month_bis[data$month==2] <- 1

data$month_bis[data$month==3] <- 2
data$month_bis[data$month==4] <- 2
data$month_bis[data$month==5] <- 2

data$month_bis[data$month==6] <- 3
data$month_bis[data$month==7] <- 3
data$month_bis[data$month==8] <- 3

data$month_bis[data$month==9] <- 4
data$month_bis[data$month==10] <- 4
data$month_bis[data$month==11] <- 4

unique(data$month_bis)
attach(data)
m5<- gam(resp~ s(no2) + s(so2) + s(o3, k=8) + s(temp, bs="cc") + s(hum,k=15, bs="cc") + factor(year)+ 
            factor(month_bis)+factor(weekday), family = poisson)
summary(m5)
gam.check(m5)
extractAIC(m5)
#plot(m2)
#plot(m2$residuals~m2$fitted.values, main="residuals vs. fitted")


m6<- gam(resp~ s(no2) + s(so2) + s(o3) + s(temp,bs="cc") + s(hum,k=15, bs="cc") + factor(year)+ 
           s(day)+ factor(month_bis) +factor(weekday)+  factor(year)*factor(month_bis)*factor(weekday), family = poisson)
summary(m6)
gam.check(m6) #best in deviance explained so far
extractAIC(m6)


m7<- gam(resp~ s(no2) + s(so2) + s(o3) + s(temp, bs="cc") + s(hum,k=15, bs="cc") + factor(year)+ 
           s(day)+ factor(month_bis)+factor(weekday)+  factor(year)*factor(month_bis), family = poisson)
summary(m7)
gam.check(m7) #lowest UBRE
extractAIC(m7)


m8<- gam(resp~ s(no2) + s(so2) + s(o3) + s(temp, bs="cc") + s(hum,k=15, bs="cc") + factor(year)+ 
           s(day)+ factor(month_bis)+factor(weekday)+ factor(month_bis)*factor(weekday)+  factor(year)*factor(month), family = poisson)
summary(m8)
gam.check(m8)
extractAIC(m8)



#without day
m9<- gam(resp~ s(no2) + s(so2) + s(rsp) + s(o3) + s(temp, bs="cc") + s(hum, k=15, bs="cc") + 
           factor(year)+ factor(month_bis)+factor(weekday), family = poisson)
summary(m9)
gam.check(m9)
extractAIC(m9)


m10<- gam(resp~ s(no2) + s(so2) + s(o3) + s(temp, bs= "cc") + s(hum,k=15, bs= "cc") + factor(year)+
           factor(month_bis)+factor(weekday)+  factor(year)*factor(month_bis)*factor(weekday), family = poisson)
summary(m10)
gam.check(m10)
extractAIC(m10)




#final model
summary(m5)
par(mfrow=c(2,2))
plot(m5)

par(mfrow=c(1,1))
plot(m5$residuals~m5$fitted.values, main="residuals vs. fitted")
abline(h=0, col="red", lwd=2)


par(mfrow=c(1,1))
qqnorm(m5$residuals)
qqline(m5$residuals, col="red")
