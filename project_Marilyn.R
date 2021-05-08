data0 <- read.csv("pollution.csv", h=T)
data <- na.omit(data0)
sapply(data, class)
# poisson with no transform : Shiva
# autocorr matrix gee : Olivier

data$weekday <-  as.factor(data$weekday)
data$month <-  as.factor(data$month)
data$year <-  as.factor(data$year)
sapply(data, class)



summaryBy(resp~weekday, data=pollu_na,
          FUN = function(x) { c(Number_of_units=length(x),  Mean= mean(x), standard_deviation = sd(x)) })

summaryBy(resp~month, data=pollu_na,
          FUN = function(x) { c(Number_of_units=length(x),  Mean= mean(x), standard_deviation = sd(x)) })
