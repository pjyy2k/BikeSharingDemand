df <- read.table(file="./DataSource/train.csv",header = TRUE, sep = ",")

#자료 Overview
which(is.na(df)==T)#결측치 확인
str(df) #자료 형태 관찰
head(df) #자료 미리보기
summary(df) #자료 요약값 제공
plot(df) # Scotter plot


#데이터 전처리
df$season <- factor(df$season)
df$holiday <- factor(df$holiday)
df$workingday <- factor(df$workingday)
df$months <- months(as.Date(df$datetime))
df$weekday <- weekdays(as.Date(df$datetime))
df$hour <- as.numeric(substring(df$datetime,12,13))
#weather의 경우 1에서 4로 갈수록 악천후의 성격을 띄고 있어서 순서형 범주로 보고 int 형태로 유지.

#문자형태 변수 숫자로 변환
df$months[df$months=="1월"]<-1
df$months[df$months=="2월"]<-2
df$months[df$months=="3월"]<-3
df$months[df$months=="4월"]<-4
df$months[df$months=="5월"]<-5
df$months[df$months=="6월"]<-6
df$months[df$months=="7월"]<-7
df$months[df$months=="8월"]<-8
df$months[df$months=="9월"]<-9
df$months[df$months=="10월"]<-10
df$months[df$months=="11월"]<-11
df$months[df$months=="12월"]<-12

df$weekday[df$weekday=="일요일"]<-1
df$weekday[df$weekday=="월요일"]<-2
df$weekday[df$weekday=="화요일"]<-3
df$weekday[df$weekday=="수요일"]<-4
df$weekday[df$weekday=="목요일"]<-5
df$weekday[df$weekday=="금요일"]<-6
df$weekday[df$weekday=="토요일"]<-7


#분할표 작성

#분할표내에서 사용할 변수선언
weekday<-numeric(0)
hour <-numeric(0)
weather <-numeric(0)
months<-numeric(0)
count <-numeric(0)

#일부 변수는 제외 : workingday -> holiday로 대체, season -> months로 대체
#추후 선택해야 하는 변수 : holiday & weekday => holiday중 weekday가 차지하는 비중이 높아 다중공선성의 문제가 발생할 여지가 있기 때문에 선택예정
#

for (h in 1:7){ # of weekday
for (i in 1:4){ # of weather
  for(j in 1:24){ # of hour
    for(k in 1:12){ # of months
      weekday<-c(weekday,h)
      weather<-c(weather,i)
      hour<-c(hour,j)
      months<-c(months,k)
      count<-c(count,sum(df$count[df$weekday==h & df$weather==i & df$hour==j & df$months==k]))
      }
    }
  }
}
ctgctable<-data.frame(weekday,weather,hour,months,count)
rm(weekday,weather,hour,months,count,h,i,j,k)

#model fitting

ftd.model0 <- glm(count~weekday+weather+hour+months,data=ctgctable)
summary(ftd.model0)

#한계점 메모 :
#데이터 전처리 알고리즘에 따라서 전처리의 속도가 크게 달라질 것 같은데. 이번의 경우 데이터의 양이 그렇게 많지 않아
#큰 문제가 없었지만 데이터의 크기가 많이 커지면 이런 방법이 문제가 될 수 있을 수도 있다는 생각이 들었음. 대용량의 데이터를 다룰때는
#사용하는 프로그램의 백그라운드에 대해서 세밀히 파악하고 있어야 할 것으로 보임
#
#
#
