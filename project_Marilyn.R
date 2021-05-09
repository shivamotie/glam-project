data0 <- read.csv("pollution.csv", h=T)
data <- na.omit(data0)
sapply(data, class)
library(gee)
# poisson with no transform : Shiva
# autocorr matrix gee : Olivier

#summaryBy(resp~weekday, data=data,
#          FUN = function(x) { c(Number_of_units=length(x),  Mean= mean(x), standard_deviation = sd(x)) })

#summaryBy(resp~month, data=data,
#          FUN = function(x) { c(Number_of_units=length(x),  Mean= mean(x), standard_deviation = sd(x)) })


#Shiva's models
data$weekday = relevel(data$weekday, ref="mon")

pois_1 =glm(resp ~ no2 + so2 + rsp + o3 + temp + hum + factor(day) + factor(month) + factor(weekday) + factor(year) , data = data , family = poisson)
summary(pois_1)
par(mfrow=c(2,2))
plot(pois_1)

pois_2 =glm(resp ~ no2 + so2 + rsp + o3 + temp + hum  + factor(month) + factor(weekday) + factor(year) , data = data, family = poisson)
summary(pois_2)
par(mfrow=c(2,2))
plot(pois_2)

pois_3 =glm(resp ~ no2 + so2 + o3 + hum  + factor(month) + factor(weekday) + factor(year) , data = data , family = poisson)
summary(pois_3)
par(mfrow=c(2,2))
plot(pois_3)


pois_4 =glm(resp ~ no2 + o3 + hum  + factor(month) + factor(weekday) + factor(year) , data =data , family = poisson)
summary(pois_4)
par(mfrow=c(2,2))
plot.glm(pois_4)

extractAIC(pois_1)
extractAIC(pois_2)
extractAIC(pois_3) #smallest AIC
extractAIC(pois_4)

library(statmod)
par(mfrow=c(2, 2))
for (i in 1:4){
  rqresid.fit <- qresid(pois_1)
  qqnorm(rqresid.fit)
  abline(a = 0, b = 1, col = "red")
}

for (i in 1:4){
  rqresid.fit <- qresid(pois_2)
  qqnorm(rqresid.fit)
  abline(a = 0, b = 1, col = "red")
}

for (i in 1:4){
  rqresid.fit <- qresid(pois_3)
  qqnorm(rqresid.fit)
  abline(a = 0, b = 1, col = "red")
}

for (i in 1:4){
  rqresid.fit <- qresid(pois_4)
  qqnorm(rqresid.fit)
  abline(a = 0, b = 1, col = "red")
}

  

#Olivier's models
data$month <- as.factor(data$month)
data$year <- as.factor(data$year)
data$weekday <- as.factor(data$weekday)
str(data)


pol.gee2 <- gee(
  resp ~ no2 + so2 + rsp + o3 + temp + hum + factor(day) +
    month + year,
  corstr = "exchangeable",
  id = resp,
  family = poisson,
  data = data)
pol.gee2


pol.gee1 <- gee(
  resp ~ no2 + so2 + rsp + o3 + temp + hum + 
    month + year,
  corstr = "AR-M", Mv = 2,
  id = resp,
  family = poisson,
  data = data)
pol.gee1


res.pol.gee1 <- (data$resp - pol.gee1$fitted) / sqrt(pol.gee1$fitted)
bwplot(data$resp ~ res.pol.gee1,
       xlab = 'Pearson residuals',
       ylab = 'resp',       
       panel = function(...) {
         panel.abline(v = c(-1.96, 0, 1.96), lty = 2)
         panel.bwplot(...)
       }
)


extractAIC(pol.gee2)
