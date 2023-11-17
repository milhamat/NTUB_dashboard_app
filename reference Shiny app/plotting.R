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

print(typeof(data)) # list
print(typeof(data$SepalLengthCm))

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_violin()

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_boxplot()

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_point()

ggplot(data, aes(x=SepalLengthCm))+geom_density()

ggplot(data, aes(x=SepalLengthCm))+geom_area(stat="bin")

ggplot(data, aes(x=SepalLengthCm))+geom_dotplot()

ggplot(data, aes(sample=SepalLengthCm))+stat_qq()

ggplot(data, aes(x=SepalLengthCm))+geom_freqpoly()

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_hex()

# geom_label
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm, label=rownames(data)))+geom_label()
# geom_quantile
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_quantile()
# geom_rug
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_rug(sides = "bl")
# smooth
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_smooth()
# geom_text
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm, label=rownames(data)))+geom_text()

####################
ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_crossbar(ymin=data$PetalLengthCm, ymax=data$PetalWidthCm,fatten = 1)

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_errorbar()

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_linerange()

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm))+geom_pointrange()

