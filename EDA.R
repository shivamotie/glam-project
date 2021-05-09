




# 1.Reading datasets


setwd("C:/Users/shiva/Desktop/2-spring 2021/GLAM/Project 2-20210421/glam-project")

pollu = read.csv("pollution.csv")

source("pairslab.R")






## 2.1.scatterplots, barpolts and boxplots 

#Univariate descriptive variables



boxplot(pollu$resp , main = "resp" , col = "gray")
hist(pollu$resp)



#Multivariate descriptive analysis

pollu_2 = subset(pollu, select = -c(day:weekday) ) 

pairs(pollu_2)





boxplot(resp~weekday,
        data=pollu,
        main="each weekday",
        xlab="weekdays",
        ylab="resp",
        col="orange",
        border="brown")


boxplot(resp~month,
        data=pollu,
        main="each month",
        xlab="month",
        ylab="resp",
        col="orange",
        border="brown")



boxplot(resp~day,
        data=pollu,
        main="each days",
        xlab="day",
        ylab="resp",
        col="orange",
        border="brown")



boxplot(resp~year,
        data=pollu,
        main="each year",
        xlab="year",
        ylab="resp",
        col="orange",
        border="brown")




plot(pollu$resp)
plot(pollu$no2)
plot(pollu$so2)
plot(pollu$rsp)
plot(pollu$o3)
plot(pollu$temp)
plot(pollu$hum)



pollu_na = na.omit(pollu)

dim(pollu_na)

dim(pollu)



# changing reference level


pollu_na$weekday = relevel(pollu_na$weekday, ref="mon")




pois_1 =glm(resp ~ no2 + so2 + rsp + o3 + temp + hum + factor(day) + factor(month) + factor(weekday) + factor(year) , data = pollu_na , family = poisson)

summary(pois_1)




pois_2 =glm(resp ~ no2 + so2 + rsp + o3 + temp + hum  + factor(month) + factor(weekday) + factor(year) , data = pollu_na , family = poisson)

summary(pois_2)




pois_3 =glm(resp ~ no2 + so2 + o3 + hum  + factor(month) + factor(weekday) + factor(year) , data = pollu_na , family = poisson)

summary(pois_3)




pois_4 =glm(resp ~ no2 + o3 + hum  + factor(month) + factor(weekday) + factor(year) , data = pollu_na , family = poisson)

summary(pois_4)





