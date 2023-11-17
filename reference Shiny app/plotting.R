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

### TWO VARIABLE
## both_conti_pick
# geom_point
# if (input$both_conti_pick=="geom_point"){
#   ggplot(dat, aes_string(x=input$xcol))+geom_point(colour='darkblue')
#   }
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

## one_dist_one_pick
# geom_boxplot
# if (input$one_dist_one_pick=="geom_boxplot"){
#   ggplot(dat, aes_string(x=input$xcol))+geom_boxplot(colour='darkblue')
#   }
# geom_violin
# else if (input$one_dist_one_pick=="geom_violin"){
#   ggplot(dat, aes_string(x=input$xcol))+geom_violin(colour='darkblue')
#   }
# geom_col
# geom_dotplot

## both_dist_pick
# geom_count
# geom_jitter

## conti_bivar_dist_pick
# geom_bin2d
# geom_desity_2d
# geom_hex

## conti_func_pick
# geom_area_two
# geom_line
# geom_step

## maps_pick
# geom_map

### THREE VARIABLE
## three_var_pick
# geom_contour
# geom_contour_filled
# geom_raster
# geom_tile

