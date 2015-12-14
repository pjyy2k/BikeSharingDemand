install.packages("dplyr")
library(dplyr)

data <- read.table(file="./DataSource/train.csv",header = TRUE, sep = ",")
data
