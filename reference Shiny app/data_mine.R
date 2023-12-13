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

### Summarise

### Group-by

### Rename
