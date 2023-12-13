library(dplyr)
library(ISLR)

data <- read.csv("./datatests/algae.csv")

### Filter
colnames(data)
data %>% filter(mxPH == 8)
data %>% filter(mxPH != 8)
data %>% filter(mxPH > 8)
data %>% filter(mxPH < 8)

### Select
data$mxPH

### Arrange
colnames(data)
data
data %>% arrange(mxPH)

### Mutate
mpg
auto <- as.tibble(mpg)

auto_specs_new <- mutate(auto, hp_to_weight = year / cyl)
auto_specs_new

auto_ <- mutate(mpg, hp_to_weight = year / cyl)
auto_

auto2 <- mutate(mpg, 'hp_to_weight' = 'year' / 'cyl')
auto2

MPG = "mpg"
YEAR = "year"
CYL = "cyl"

tt = paste0(MPG, "%>% mutate(tf =", YEAR, "/", CYL, ")")

eval(parse(text = tt))

dd <- eval(parse(text = tt))

### Summarise
data <- na.omit(data)
data %>% summarise_at('mxPH', mean)
data %>% summarise_at('mxPH', median)
data %>% summarise_at('mxPH', min)
data %>% summarise_at('mxPH', max)
data %>% summarise_at('mxPH', sd)
data %>% summarise_at('mxPH', IQR)
summary(data$mxPH)
### Group-by
data %>% group_by(data[,"mxPH"])
data
### Rename
colnames(data)
rename(data, "SIZE" = "size")
