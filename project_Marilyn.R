data0 <- read.csv("pollution.csv", h=T)
data <- drop_na(data0) #or na.omit()
sapply(data, class)

# poisson with no transform : Shiva
# autosorr matrix gee : Olivier
jhdgdkjc