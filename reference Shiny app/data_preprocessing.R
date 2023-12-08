data <- read.csv("./datatests/algae.csv")

mean(data$"a4") # pointing data column using string
data[,"a4"]
data$a4
mean(data$a4)
mean(data[,"a4"])
typeof(data$a4)


str(data)

data <- na.omit(data)

str(data)

############### AVG
df1<-c(2,2,NA,10, 20, NA,3)

idx<-which(is.na(df1))

df1[idx] <- (df1[idx-1] + df1[idx+1])/2

df1
###############
data <- read.csv("./datatests/algae.csv")
data
sum(is.na(data))
valcnt <- sum(is.na(data))
print(paste0("total missing value: ", valcnt))
data$mxPH[is.na(data$mxPH)] <- mean(data$mxPH, na.rm = TRUE)

#for (col in colnames(data)) {
#  print(col)
#  data$col[is.na(data$col)] <- mean(data$col, na.rm=T) 
#}

library(dplyr)

## standardize all variables
rmchar <- data[, !sapply(data, is.character)]
df2 <- rmchar %>% mutate_all(~(scale(.) %>% as.vector))
df <- data[, sapply(data, is.character)]
typeof(df)
typeof(df2)
typeof(data)
df2[names(df)] <- df
df2
data <- df2
## remove with zero
data[is.na(data)] <- 0
data

#################
data <- read.csv("./datatests/algae.csv")
data2 <- data
rmchar <- data[, !sapply(data, is.character)]
scl <- scale(rmchar)
scl <- data.frame(scl)
#colMeans(rmchar)
colnm <- colnames(scl)
data2[colnm] <- scl[colnm]

#######################
#normalize 0 to 1
library(caret)
data <- read.csv("./datatests/algae.csv")
data2 <- data
rmchar <- data[, !sapply(data, is.character)]
process <- preProcess(as.data.frame(rmchar), method=c("range"))
norm_scale <- predict(process, as.data.frame(rmchar))
colnm <- colnames(norm_scale)
data2[colnm] <- norm_scale[colnm]
