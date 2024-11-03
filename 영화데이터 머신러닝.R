

### 작성자  강현욱


# 전처리들
IMBD = read.csv("F:\\데이터마이닝 및 실습\\IMBD.csv", header=T)
str(IMBD)

is.na(IMBD)
IMBD = na.omit(IMBD)
IMBD
library(dplyr); IMBD <- mutate(IMBD, genre = as.integer(factor(genre)))
IMBD$time = as.numeric(IMBD$time)
class(IMBD$time)
str(IMBD)
head(IMBD$time,20)
length = with(IMBD, ifelse(is.na(time),0,ifelse(
  time <= 60, 1, ifelse(time <= 120, 2, 3))))
IMBD = cbind(IMBD, length)
IMBD$rating <- replace(IMBD$rating, IMBD$rating == "", NA)
str(IMBD)
IMBD = IMBD[,-c(2)]
IMBD = IMBD[IMBD$dollar!=0, ]
str(IMBD)


## 산점도와 히스토그램
attach(IMBD)
par(mfrow=c(3,4))
plot(dollar~rating)
plot(dollar~time)
plot(time~rating)
hist(time)
hist(dollar)
hist(rating)
hist(log(dollar))
hist(log(time))
plot(log(dollar)~rating)
plot(log(dollar)~log(time))



### time 이상치 제거, 변환
par(mfrow=c(1,2))
boxplot(log(IMBD$time))
boxplot(IMBD$time) 
is.integer(IMBD$time)
IMBD$time = as.integer(IMBD$time)
is.integer(IMBD$time)

Q1 = quantile(log(IMBD$time), 0.25, na.rm=TRUE)
Q3 = quantile(log(IMBD$time), 0.75, na.rm=TRUE)
IQR = Q3 - Q1
lower = Q1 - 1.5 * IQR
upper = Q3 + 1.5 * IQR
IMBD2 = IMBD[!(log(IMBD$time) < lower | log(IMBD$time) > upper),]
# 300개정도 이상치 제거


######### rating 이상치 제거, 변환
attach(IMBD2)
par(mfrow=c(2,2))
boxplot(rating); hist(rating)
boxplot((rating^2))
hist((rating)^2) # 정규분포 가까워짐

Q1 = quantile((rating^2), 0.25)
Q3 = quantile((rating^2), 0.75)
IQR = Q3 - Q1
lower = Q1 - 1.5 * IQR
upper = Q3 + 1.5 * IQR
IMBD3 = IMBD2[!((rating^2) < lower | (rating^2) > upper),]
str(IMBD3) # 80개정도 이상치 제거
df = IMBD3
boxplot(df$rating^2)




# 바이오그래피, 게임쇼, 리얼리티티비 제거
df = df[,-c(7)]
df = df[,-c(8)]
df = df[,-c(25)]







set.seed(1234)
#### 2. 장르와의 연관분석; 장르에 따라 레이팅, 매출액의 변화가 있는가?
# 2-1 장르를 분류하는 방법; 계층적 군집분석
# 0 1 코딩이므로 표준화는 안해주었습니다
gerne = df[,c(2:24)] 
d <- dist(gerne, method="euclidean") # 유클리디안 거리 사용
fit1 <- hclust(d, method="complete") # 최장연결법 사용 
plot(fit1) # 모습이 썩 좋아보이지 않습니다


fit2 <- hclust(d, method="ward.D")  # 왈드 방식 사용
plot(fit2) # 보다 나아보입니다. 
#직관적으로 7개군집으로 분류가 적합함을 짐작할 수 있습니다



# 이 외의 군집분석 시도
#mean = colMeans(gerne)
#cov = cov(gerne)
#m.dist = mahalanobis(gerne, center=mean, cov=cov)
#fit2 = hclust(m.dist, method="complete")
#sum(is.na(gerne)) ## 마할라노비스 거리로 피팅이 안됩니다

#nstall.packages("lsa")
#library(lsa)
#cos = 1-cosine(t(gerne)) 
#fit3 = hclust(cos, method="complete")
# 코사인 거리도 작동이 안 됩니다...



par(mfrow=c(1,1))
plot(fit2)
rect.hclust(fit2, k=7, border = "red")

groups = cutree(fit2, k=7)
aa = gerne[groups==1,]
colSums(aa)/nrow(aa) # 스릴러, 호러, 액션

bb = gerne[groups==2,]
colSums(bb)/nrow(bb)  # 드라마 

cc= gerne[groups==3,]
colSums(cc)/nrow(cc)  # 어드벤쳐, 코메디

dd = gerne[groups==4,]
colSums(dd)/nrow(dd)  # 범죄

ee = gerne[groups==5,] 
colSums(ee)/nrow(ee) # 코메디

ff = gerne[groups==6,] # 다큐멘터리
colSums(ff)/nrow(ff) 

gg = gerne[groups==7,] # 로맨스, 드라마, 코메디
colSums(gg)/nrow(gg) 




# 데이터프레임 생성
df 
nn<-as.numeric(names(groups))
tt<-data.frame(nn,groups)
library(dplyr)
tt<-tt %>%arrange(nn)
tt
head(tt)
dim(tt)
dim(df)
df2 = df
df2$ind<-as.numeric(rownames(df2))
df2<-df2%>%arrange(ind)
head(df2)
df2$groups <-tt$groups
df2$ind<-NULL
head(df2)


attach(df2)
## anova 
#1-1. 단편, 중편, 장편에 따라 매출 차이가 있는가?
aov1 = aov(log(dollar) ~ length, data = df2)
summary(aov1) # 있다.
#1-2. 단편, 중편, 장편에 따라 점수 차이가 있는가?
aov2 <- aov((rating^2) ~ length, data = df2)
summary(aov2) # 있다.


#2. 장르에 따라 매출 차이가 있는가? +  점수에 따른 매출 차이도
detach(df2)
장르 = df2$groups
장르 = replace(장르, 장르 == 1, "스릴러, 호러, 액션")
장르 = replace(장르, 장르 == 2, "드라마")
장르 = replace(장르, 장르 == 3, "어드벤쳐, 코메디")
장르 = replace(장르, 장르 == 4, "범죄")
장르 = replace(장르, 장르 == 5, "코메디")
장르 = replace(장르, 장르 == 6, "다큐멘터리")
장르 = replace(장르, 장르 == 7, "로멘스, 드라마, 코메디디")
length(장르)

df2 = cbind(df2, 장르)


attach(df2)
rate = rating^2
logdollar = log(dollar)
df3 = cbind(df2, rate, logdollar)
df2 = df3

aov3 = aov(logdollar ~ 장르, data = df2)
summary(aov3) 
boxplot(dollar~장르)
boxplot(logdollar~장르) # 차이가 있다.




########## time과과 rate를 매개변수로 하여 분산분석
plot(log(time), rate)
aov4 <- lm(logdollar ~ log(time)+rate, data = df2)
summary(aov4) # 둘 다 수치형이니 범주형 변수로 변환해주어야 함
lmtr <- lm(logdollar ~ log(time)+rate, data = df2)
summary(lmtr)

#### ratdf2#### rate 범주화를 위한 군집분석; 비계층
### K중심 방식 사용
#############################################################################


install.packages("NbClust")
library(NbClust)
set.seed(1234)
로그시간 = log(df2$time)
표준화로그시간 = scale(로그시간)
점수제곱 = df2$rate
표준화점수제곱 = scale(점수제곱)
df4 = as.data.frame(cbind(표준화로그시간, 표준화점수제곱))
head(df4)
lmscale = lm(df2$logdollar~표준화로그시간+표준화점수제곱)
summary(lmscale)

# nc = NbClust(df4, min.nc = 2, max.nc = 10, method = "kmeans")
# 비계층적 군집분석 결과가 안 나옵니다
install.packages("factoextra")
library(factoextra)
install.packages("flexclust")
library(flexclust)
# 다른함수사용
# fviz_nbclust(df4[,1:2], kmeans, method = "wss")
# 작동안합니다 ㅜ
plot(df4, xlab="표준화로그시간", ylab="표준화멱승점수")
set.seed(1234)
km = kmeans(df4, centers = 4)
fviz_cluster(km, data = df4, 
             xlab="표준화로그시간", ylab="표준화멱승점수")
# 1번군집: 저득점 단편
# 2번군집: 고득점 단편
# 3번군집: 저득점 장편
# 4번군집: 고득점 장편


# 원래 데이터프레임에 결합
kmgroup=km$cluster
df2 = cbind(df2, kmgroup)
시간점수 = kmgroup
시간점수 = replace(시간점수, 시간점수 == 1, "저득점 단편")
시간점수 = replace(시간점수, 시간점수 == 2, "고득점 단편")
시간점수 = replace(시간점수, 시간점수 == 3, "저득점 장편")
시간점수 = replace(시간점수, 시간점수 == 4, "고득점 장편")
시간점수
df2 = cbind(df2, 시간점수)

#### 시간과 점수로 나눈 군집에 따른 매출 차이
aov5 = aov(logdollar ~ 시간점수, data = df2)
summary(aov5)
boxplot(logdollar~시간점수) # 당연하게도 차이가 있다...
# 의의: 연속형 변수인 점수와 시간을 임의로 나누지 않고 
# 군집분석을 활용해 분류할 수 있었다...
# 회귀분석 추가
lm = lm(df2$logdollar~df2$시간점수, data=df2)
summary(lm)




#### 장르에 따라 점수차이
aov6 = aov(rate~장르,data=df2)
summary(aov6) # 있다. 다큐멘터리 예술병 1승
boxplot(rate~장르)
lm2 = lm(rate~장르)
summary(lm2)





### 데이터셋 분류
# 장르 가변수 코딩
dummy = df2$groups-1
df2 = cbind(df2, dummy)
set.seed(1234)
train = df2[sample(nrow(df2), 10000),]
test = df2[-sample(nrow(df2), 10000),]
str(train)
str(test)
dim(train)
dim(test)

library(car)

## 매출예측을 위한 중회귀분석
#1. 매출에 로그, 레이팅에 멱승변환, 장르를 가변수로 한 모델
# 근데 생각해보니 범주형 데이터를 가변수르 넣는게 맞나요. 팩터로 변환하면 되나..
# 근데 팩터로 변환할거면 그냥 깡으로 문자를 때려넣어도 되는 것 아닐까요?
lm3 = lm(logdollar~rate+time+장르, data = train)
summary(lm3) # 설명력 16퍼센트, 모든 변수 유의하다
vif(lm3) # 다중공선성 없음
##
pred1 = predict(lm3, newdata=test)
exp.pred1 = exp(pred1)
length(exp.pred1)
오차1 = test$dollar-exp.pred1
realdollar = test$dollar
movie = test$movie
test1 = cbind(movie, realdollar, exp.pred1, 오차1)
head(test1)
MSE1 = sum(오차1^2)/2421
print(sqrt(MSE1)) # 50388814







###### 신경망모형으로 매출예측
install.packages("neuralnet")
library(neuralnet)
train2<-train%>%select(logdollar, rate, dummy,time)
test2<-test%>%select(logdollar, rate, dummy,time)
# 데이터 정규화(min-max)
maxs_tr<-apply(train2,2,max)
mins_tr<-apply(train2,2,min)
mins_tr
train2<-scale(train2,center = mins_tr, scale = maxs_tr-mins_tr)
test2<-scale(test2,center = mins_tr, scale = maxs_tr-mins_tr)

# 신경망모형적합
ne3 = neuralnet(logdollar~rate+dummy+time, 
                train2, hidden=3, threshold=0.01,linear.output=TRUE,
                stepmax=10e10) 
plot(ne3)
print(ne3)
predicted_ne3<-predict(ne3, test2) 
# 원래 크기로 변환
exp.pred3 <- exp(predicted_ne3[,1] * (maxs_tr[1] - mins_tr[1]) + mins_tr[1])
오차3 = test$dollar-exp.pred3
MSE2 = sum(오차3^2)/2421
print(sqrt(MSE2)) # 50954477



# 신경망모형적합 은닉노드=5
ne5 = neuralnet(logdollar~rate+dummy+time, 
                train2, hidden=5, threshold=0.01,linear.output=TRUE,
                stepmax=10e10) 
plot(ne5)
print(ne5)
predicted_ne5<-predict(ne5, test2) 
# 원래 크기로 변환
exp.pred5 <- exp(predicted_ne5[,1] * (maxs_tr[1] - mins_tr[1]) + mins_tr[1])
오차5 = test$dollar-exp.pred5
MSE2 = sum(오차5^2)/2421
print(sqrt(MSE2)) # 50568891




# 신경망모형적합 은닉노드=10
ne10 = neuralnet(logdollar~rate+dummy+time, 
                train2, hidden=10, threshold=0.01,linear.output=TRUE,
                stepmax=10e10) 
plot(ne10)
print(ne10)
predicted_ne10<-predict(ne10, test2) 
# 원래 크기로 변환
exp.pred10 <- exp(predicted_ne10[,1] * (maxs_tr[1] - mins_tr[1]) + mins_tr[1])
오차10 = test$dollar-exp.pred10
MSE10 = sum(오차10^2)/2421
print(sqrt(MSE10)) 

# exp.pred1은 중회귀분석모형 예측값, 오차1은 중회귀분석모형의 오차입니다.
# exp.pred10은 10개노드모형의 예측값, 오차10은 10개노드모형의 오차입니다.
# 오차1-오차10은 두 오차간의 차입니다. 
오차의차 = 오차1-오차10
test.ne = cbind(test1, exp.pred10, 오차10, 오차의차)
head(test.ne)


                
                
##다층신경망모형
ne5.5 = neuralnet(logdollar~rate+dummy+time, 
                 train2, hidden=c(5,5), threshold=0.01,linear.output=TRUE,
                 stepmax=10e10) 
plot(ne5.5)
print(ne5.5)
predicted_ne5.5<-predict(ne5.5, test2) 
# 원래 크기로 변환
exp.pred5.5 <- exp(predicted_ne5.5[,1] * (maxs_tr[1] - mins_tr[1]) + mins_tr[1])
오차5.5 = test$dollar-exp.pred5.5
MSE5.5 = sum(오차5.5^2)/2421
print(sqrt(MSE5.5)) 

ne5.5 = neuralnet(logdollar~rate+dummy+time, 
                 train2, hidden=5.5, threshold=0.01,linear.output=TRUE,
                 stepmax=10e10) 
plot(ne5.5)
print(ne5.5)
predicted_ne5.5<-predict(ne5.5, test2) 
# 원래 크기로 변환
exp.pred5.5 <- exp(predicted_ne5.5[,1] * (maxs_tr[1] - mins_tr[1]) + mins_tr[1])
오차5.5 = test$dollar-exp.pred5.5
MSE5.5 = sum(오차5.5^2)/2421
print(sqrt(MSE5.5)) 

