df <- read.table(file="./DataSource/train.csv",header = TRUE, sep = ",")

#자료 Overview
which(is.na(df)==T)#결측치 확인
str(df) #자료 형태 관찰
head(df) #자료 미리보기
summary(df) #자료 요약값 제공

#데이터 전처리
df$season <- factor(df$season)
df$holiday <- factor(df$holiday)
df$workingday <- factor(df$workingday)
df$months <- months(as.Date(df$datetime))
df$weekday <- weekdays(as.Date(df$datetime))
df$hour <- as.numeric(substring(df$datetime,12,13))
#weather의 경우 1에서 4로 갈수록 악천후의 성격을 띄고 있어서 순서형 범주로 보고 int 형태로 유지.

plot(df) # Scotter plot

boxplot(df$count~df$weather)
boxplot(df$count~df$hour)
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

#자전거 대여량 범주화
for(i in 0:3){ # bike 대여 count에 따라 범주화(250단위씩 끊어서)
  df$CatCount[df$count >= i*250 & df$count < (i+1)*250] <- (i)
}
rm(i) #불필요한 변수 삭제
df$dmfcc =1 #dummy for cell counting. all has '1'
str(df)



#분할표 작성, 설명변수 : weekday, weather,hour,months
#분할표내에서 사용할 변수선언
weekday<-numeric(0)
hour <-numeric(0)
weather <-numeric(0)
months<-numeric(0)
frq1 <-numeric(0)
frq2 <-numeric(0)
frq3 <-numeric(0)
frq4 <-numeric(0)
frq5 <-numeric(0)
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
        frq1<-c(frq1,sum(df$dmfcc[df$weekday==h & df$weather==i & df$hour==j & df$months==k & df$CatCount==0]))
        frq2<-c(frq2,sum(df$dmfcc[df$weekday==h & df$weather==i & df$hour==j & df$months==k & df$CatCount==1]))
        frq3<-c(frq3,sum(df$dmfcc[df$weekday==h & df$weather==i & df$hour==j & df$months==k & df$CatCount==2]))
        frq4<-c(frq4,sum(df$dmfcc[df$weekday==h & df$weather==i & df$hour==j & df$months==k & df$CatCount==3]))
        }
    }
  }
}
# 빈도 관찰이 적어서 수행불가.


#분할표작성2, 반응변수 : 시간

hour <-numeric(0)
frq1 <-numeric(0)
frq2 <-numeric(0)
frq3 <-numeric(0)
frq4 <-numeric(0)
for(j in 1:24){ # of hour
  hour<-c(hour,j)
  frq1<-c(frq1,sum(df$dmfcc[df$hour==j & df$CatCount==0]))
  frq2<-c(frq2,sum(df$dmfcc[df$hour==j & df$CatCount==1]))
  frq3<-c(frq3,sum(df$dmfcc[df$hour==j & df$CatCount==2]))
  frq4<-c(frq4,sum(df$dmfcc[df$hour==j & df$CatCount==3]))
}
table.hour<-data.frame(hour,frq1,frq2,frq3,frq4)
table.hour

#빈도 0인 셀 관찰됨, 시간 그룹화, 대여량 범주 단순화


# 06~08,17~19 : 출퇴근(1)
df$CatHour <-0
df$CatHour[df$hour>=6 & df$hour <=8] <-1
df$CatHour[df$hour>=17 & df$hour <=19] <-1

df$CatCount[df$count >= 0 & df$count < 100] <- 0
df$CatCount[df$count >= 100 & df$count < 200] <- 1
df$CatCount[df$count >= 200 ] <- 2

CatHour <-numeric(0)
frq1 <-numeric(0)
frq2 <-numeric(0)ex
frq3 <-numeric(0)
for(j in 0:1){ # of hour
  CatHour<-c(CatHour,j)
  frq1<-c(frq1,sum(df$dmfcc[df$CatHour==j & df$CatCount==0]))
  frq2<-c(frq2,sum(df$dmfcc[df$CatHour==j & df$CatCount==1]))
  frq3<-c(frq3,sum(df$dmfcc[df$CatHour==j & df$CatCount==2]))
}

table.CatHour<-data.frame(CatHour,frq1,frq2,frq3)
rm(CatHour,frq1,frq2,frq3,j)

table.CatHour

str(table.CatHour)
table.CatHour$CatHour <- as.factor(table.CatHour$CatHour)
library(VGAM)
ftd.model.0 <-vglm(cbind(frq1,frq2,frq3)~CatHour, family=cumulative(parallel=TRUE), data=table.CatHour)
summary(ftd.model.0)
fitted(ftd.model.0)

ftd.model.1 <-vglm(cbind(frq1,frq2,frq3)~CatHour, family=cumulative(parallel=FALSE), data=table.CatHour)
summary(ftd.model.1)
fitted(ftd.model.1)


pchisq(deviance(ftd.model.0)-deviance(ftd.model.1),
       df=df.residual(ftd.model.0)-df.residual(ftd.model.1), lower.tail=FALSE)
deviance(ftd.model.1)
df.residual(ftd.model.0)












ctgctable<-data.frame(weekday,weather,hour,months,frq1,frq2,frq3,frq4,frq5)
rm(weekday,weather,hour,months,count,CatCount,frq1,frq2,frq3,frq4,frq5,g,h,i,j,k,l)


ctgctable$weekday<-as.factor(ctgctable$weekday)
ctgctable$weather<-as.integer(ctgctable$weather)
ctgctable$hour<-as.factor(ctgctable$hour)
ctgctable$months<-as.factor(ctgctable$months)
#ctgctable$holiday<-as.factor(ctgctable$holiday)
ctgctable$frq1<-as.integer(ctgctable$frq1)
ctgctable$frq2<-as.integer(ctgctable$frq2)
ctgctable$frq3<-as.integer(ctgctable$frq3)
ctgctable$frq4<-as.integer(ctgctable$frq4)
ctgctable$frq5<-as.integer(ctgctable$frq5)

str(ctgctable)




#model fitting

ftd.model0 <- glm(CatCount~weekday*weather*hour*months,data=ctgctable)
install.packages("VGAM")
library(VGAM)
ftd.model.hour <-vglm(cbind(frq1,frq2,frq3,frq4,frq5)~hour,family=cumulative(parallel = FALSE),data=ctgctable)

summary(ftd.model0)

#한계점 메모 :
#데이터 전처리 알고리즘에 따라서 전처리의 속도가 크게 달라질 것 같은데. 이번의 경우 데이터의 양이 그렇게 많지 않아
#큰 문제가 없었지만 데이터의 크기가 많이 커지면 이런 방법이 문제가 될 수 있을 수도 있다는 생각이 들었음. 대용량의 데이터를 다룰때는
#사용하는 프로그램의 백그라운드에 대해서 세밀히 파악하고 있어야 할 것으로 보임
#
#
#
