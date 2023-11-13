library(ggplot2)

data <- read.csv("./datatests/Iris.csv")
#print(data[2])
print(data$SepalLengthCm)
#hist(data[2])
hist(data$SepalLengthCm)
summary(data)

a <- data[2] # [2]=SepalLengthCm
b <- data$SepalLengthCm
#c <- as.numeric(data[2])
#print(as.numeric(as.character(data[2])))
c <- colnames(data)

print(data$c[2])
d <- as.numeric(data[c[2]])
colname <- c[2]

#------------------------------
print(data[colname])

hist(data[colname])

dat <- data.frame()
dat <- data
      