data <- read.csv("./datatests/algae.csv")

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

data$mxPH[is.na(data$mxPH)] <- mean(data$mxPH, na.rm = TRUE)

for (col in colnames(data)) {
  #print(col)
  data$col[is.na(data$col)] <- mean(data$col, na.rm=T) 
}

library(dplyr)

## standardize all variables
rmchar <- data[, !sapply(data, is.character)]
df2 <- rmchar %>% mutate_all(~(scale(.) %>% as.vector))
