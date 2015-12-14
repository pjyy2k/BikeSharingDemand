install.packages("dplyr")
library(dplyr)

bikedata <- read.table(file="./DataSource/train.csv",header = TRUE, sep = ",")
bikedata
#자료 Summary 관찰.
str(bikedata) #자료 형태 관찰
head(bikedata) #자료 미리보기
summary(bikedata) #자료 요약값 제공

cor(bikedata)

# REF, 데이터 시각화 하는 방법
library(corrplot)
mcor <- cor(mtcars)
round(mcor, 2) 
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)