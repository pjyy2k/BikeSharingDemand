#[overview3]

install.packages("dplyr")
library(dplyr)

bikedata <- read.table(file="./DataSource/train.csv",header = TRUE, sep = ",")
bikedata

#자료 Overview
  which(is.na(bikedata)==T)#결측치 확인
  str(bikedata) #자료 형태 관찰
  head(bikedata) #자료 미리보기
  summary(bikedata) #자료 요약값 제공
  plot(bikedata) # Scotter plot

#분석 편의를 위하여 Attach
  attach(bikedata)

#데이터 전처리 수행
  #Datatime 연도별, 월별, 일별, 요일별, 시간별로 바꿀 수 있을 듯, 시간대도 가능하면 투입. 오전, 오후, 저녁, 심야, 새벽 등으로 구분
  #(Finish)season 같은 경우 순서형 변수가 아니므로 명목형태로 바꾸어 줘야 할듯.
    bikedata$season <- factor(bikedata$season)
    bikedata$holiday <- factor(bikedata$holiday)
    bikedata$workingday <- factor(bikedata$workingday)
    str(bikedata)

#변수들 관계 overview    
    boxplot(count~season)
    boxplot(registered~season)
    boxplot(casual~season)  #등록유무에 따른 모형의 형태가 다른지 확인(모형의 형태만 볼것, 숫자의 크기는 서로 분산이 다름)
    
    boxplot(count~holiday)
    boxplot(registered~holiday)
    boxplot(casual~holiday)

    boxplot(count~weather)
    boxplot(registered~weather)
    boxplot(casual~weather)
    
    boxplot(count~temp)
    boxplot(registered~temp)
    boxplot(casual~temp)
  #temp와 atemp는 비슷한 양상을 보이나 같은 것은 아니므로, 어느것이 더 적합이 잘되는지는 테스트 후에 결정해야 할듯
    
    boxplot(count~humidity) #선형이 아님, 변수변환 고려
    boxplot(registered~humidity)
    boxplot(casual~humidity)
    plot(count,humidity)
    
    boxplot(count~windspeed) #선형 아님, 극단값의 영향을 받음. 로그 변환이나 이분화 하는 것도 좋을 듯
    boxplot(registered~windspeed)
    boxplot(casual~windspeed)
    plot(count,windspeed)
    plot(registered,windspeed)
    plot(casual,windspeed)
    
    qqplot(count,humidity)


bikemodel.1<-lm(count~season+weather+temp) # 변경예정


#weather는 Ranking을 가진다고 볼 수 있을 것 같음 클수록 악천후


summary(bikemodel.1)
attributes(bikemodel.1)
plot(count,fitted.values(bikemodel.1))
# REF, 데이터 시각화 하는 방법
library(corrplot)
mcor <- cor(mtcars)
round(mcor, 2) 
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)
