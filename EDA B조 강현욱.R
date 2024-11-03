# B조 0610 정리
dat_ch <- read.csv("E:\\탐색적자료분석\\팀플\\6.4 춘천 아파트 데이터 전처리 완료.csv", header=T,
                   fileEncoding = "euc-kr")
dat_seoul <- read.csv("E:\\탐색적자료분석\\팀플\\6.4 강남 아파트 데이터 전처리 완료.csv", header=T,
                      fileEncoding = "euc-kr")
library(aplpack)
library(rpart)


dat_ch$시공사.규모[dat_seoul$시공사.규모=="공기업"] = "공단"

boxplot(dat_ch$전용면적...~dat_ch$시군구1, main="춘천 동별 전용면적적 상자그림",
        xlab="동", ylab="전용면적", data=dat_ch)
abline(h=mean(dat_ch$전용면적...),col="red",lwd=2)
legend("topright",legend='meanline',col="red",lwd=2)


######### 06 10 지역별 비교
#### 1. 전용면적과 거래금액
# 전처리: 전용면적 로그그변환, 거래금액 멱승변환 후 jitter 처리 
par(mfrow=c(1,1))
# 춘천 회귀선, 로웨스회귀선
lm1 = lm(jitter((sqrt(거래금액.만원.)), factor=10)~log(전용면적...), data=dat_ch)
s1 = lowess(jitter((sqrt(dat_ch$거래금액.만원.)), factor=10)~log(dat_ch$전용면적...),
            f=2/3)
s2 = lowess(jitter((sqrt(dat_ch$거래금액.만원.)), factor=10)~log(dat_ch$전용면적...),
            f=1/3)
# 서울 회귀선, 로웨스회귀선선
lm2 = lm(jitter((sqrt(거래금액.만원.)), factor=10)~log(전용면적...), data=dat_seoul)
s3 = lowess(jitter((sqrt(dat_seoul$거래금액.만원.)), factor=10)~log(dat_seoul$전용면적...),
            f=2/3)
s4 = lowess(jitter((sqrt(dat_seoul$거래금액.만원.)), factor=10)~log(dat_seoul$전용면적...),
            f=1/3)

# 산점도그림으로 
plot(jitter((sqrt(dat_seoul$거래금액.만원.)), factor=10)~log(dat_seoul$전용면적...), main="거래금액-전용면적 산점도
     (춘천, 강남 비교)", ylim=c(0,1100), xlim=c(2.2,6),
     xlab="log(전용면적)", ylab="jitter(sqrt(거래금액(만원)))")
points(jitter((sqrt(dat_ch$거래금액.만원.)), factor=10)~log(dat_ch$전용면적...), pch=2)
lines(s2, col="green", lwd=2, lty=1)
abline(lm1$coef, col="blue", lwd=1, lty=1)
lines(s1, col="red", lwd=2, lty=1)
lines(s3, col="red", lwd=2, lty=1)
abline(lm2$coef, col="blue", lwd=1, lty=1)
lines(s4, col="green", lwd=2, lty=1)
legend("topleft",legend=c("f=2/3","f=1/3", "lm"),fill=c("red","green", "black"))

# 보자기그림으로
bagplot(x=log(dat_ch$전용면적...),y=jitter(sqrt(dat_ch$거래금액.만원.), factor=10),show.whiskers = F,
        main="거래금액-전용면적 보자기 플롯
        (춘천, 강남 비교)", xlab="log(전용면적)", ylab="jitter(sqrt(거래금액.만원))",
        ylim=c(0,1100), xlim=c(2.2,6), pch=2)
par(new=TRUE)
bagplot(x=log(dat_seoul$전용면적...),y=jitter(sqrt(dat_seoul$거래금액.만원.), factor=10),show.whiskers = F, 
        xlab="", ylab="", ylim=c(0,1100), xlim=c(2.2,6), pch=1)
lines(s2, col="green", lwd=2, lty=1)
abline(lm1$coef, col="blue", lwd=1, lty=1)
lines(s1, col="red", lwd=2, lty=1)
lines(s3, col="red", lwd=2, lty=1)
abline(lm2$coef, col="blue", lwd=1, lty=1)
lines(s4, col="green", lwd=2, lty=1)
legend("topleft",legend=c("f=2/3","f=1/3", "lm"),fill=c("red","green", "blue"))

mean((dat_seoul$전용면적...))
exp(mean(log(dat_seoul$전용면적...)))
exp(4.4)
median(log(dat_seoul$전용면적...))

# 결론 
exp(4.2)
# 면적대비 거래금액의 상승량이 강남이 춘천에 비해 크다
# 로웨스 적합 결과를 보면, 강남의 경우는 기울기가 완만하다 가파라지는 반면,
# 춘천의 경우는 기울기가 가파르게 증가하다 완만해진다다
# 보자기플롯의 경우 춘천에에 아웃라이어가 많다
# 전용면적이 커질수록 거래금액의 분산이 커지는 경향이 있다.





#### 2. 전용면적과 건축년도
# 전처리: 전용면적 멱승변환, 건축년도 jitter 적용용
#산점도로 비교교
par(mfrow=c(1,2))
# 춘천 전용면적과 건축년도 산점도
bagplot(sqrt(dat_ch$전용면적...)~jitter(dat_ch$건축년도, factor=2),
     xlab="jitter(건축년도)",ylab="sqrt(전용면적)",ylim=c(3,20),xlim=c(1970,2030),main="춘천 전용면적-건축년도 산점도")
lm4 = lm(sqrt(dat_ch$전용면적...)~jitter(dat_ch$건축년도, factor=2))
s4 = lowess(sqrt(dat_ch$전용면적...)~jitter(dat_ch$건축년도, factor=2), f=2/3)
abline(lm4$coef, col="green", lwd=2, lty=1)
lines(s4, col="red", lwd=2, lty=1)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)
# 강남 전용면적과 건축년도 산점도
plot(sqrt(dat_seoul$전용면적...)~jitter(dat_seoul$건축년도, factor=2), 
     xlab="jitter(건축년도)",ylab="",ylim=c(3,20),xlim=c(1970,2030),main="강남 전용면적-건축년도 산점도")
lm5 = lm(sqrt(dat_seoul$전용면적...)~jitter(dat_seoul$건축년도, factor=2))
s5 = lowess(sqrt(dat_seoul$전용면적...)~jitter(dat_seoul$건축년도, factor=2), f=2/3)
abline(lm5$coef, col="green", lwd=2, lty=1)
lines(s5, col="red", lwd=2, lty=1)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)

#보자기플롯으로
# 춘천
bagplot(x=jitter(dat_ch$건축년도, factor=2),y=sqrt(dat_ch$전용면적...),show.whiskers = F,
        main="춘천 적용면적-건축년도 보자기 플롯" ,xlim=c(1970,2030), ylim=c(3,20),
        xlab="jitter(건축년도)", ylab="sqrt(전용면적)", pch=2)
abline(lm4$coef, col="green", lwd=2, lty=1)
lines(s4, col="red", lwd=2, lty=1)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)
#강남
bagplot(x=jitter(dat_seoul$건축년도, factor=2),y=sqrt(dat_seoul$전용면적...),show.whiskers = F, 
           main="강남 적용면적-건축년도 보자기 플롯" ,ylim=c(3,20),xlim=c(1970,2030),xlab="jitter(건축년도)", ylab="", pch=1)
abline(lm5$coef, col="green", lwd=2, lty=1)
lines(s5, col="red", lwd=2, lty=1)

# 결론
# 춘천의 경우 건축년도가 최근이 될 수록 전용면적도 커지는 특징을 가지고 있고, 
# lowess 적합선을 보았을 때 2010 년 이전까지 상향하다 이후에는 하락하는 특징을 가진다.
# 강남의 경우 건축년도가 옛날과 최근 면적이 분포가 고르게 나타나 큰 특징을 갖지 않는 것 처럼 보이지만
# lm회귀선을 보았을 때 건축년도가 최근이 될 수록 전용면적은 작아지는 것을 확일 할 수 있고
# lowess회귀직선 역시 우하향하는 것을 볼 수 있고 2000년대에 잠깐 우상향 하는 모습을 볼 수 있지만 다시 우하향한다.
# 춘천의 경우 아웃라이어가 많이 보입니다.





####3. 거래금액-층 비교 (전처리함)
# 전처리: 거래금액 sqrt, 층 log 변환 
library(MASS)
# 춘천
par(mfrow=c(1,2))
bagplot(y=sqrt(dat_ch$거래금액.만원.), x=log(dat_ch$층),
     main="(춘천)거래금액-층 산점도", ylim=c(50, 1000), xlim=c(0,5),
     xlab="log(층)", ylab="sqrt(거래금액)",show.whiskers = F)
lm1<-lqs(sqrt(dat_ch$거래금액.만원.)~log(dat_ch$층),method="lms")
s1<-lowess(sqrt(dat_ch$거래금액.만원.)~log(dat_ch$층),f=2/3)
abline(lm1,col="green",lwd=2,lty=1)
lines(s1,col="red",lwd=2,lty=1)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)
#강남
bagplot(y=sqrt(dat_seoul$거래금액.만원.), x=log(dat_seoul$층),show.whiskers = F,
     main="(강남)거래금액-층 산점도", ylim=c(50, 1000), xlim=c(0,5),
     xlab="log(층)", ylab="sqrt(거래금액)")
lm1<-lm(sqrt(dat_seoul$거래금액.만원.)~log(dat_seoul$층))
s1<-lowess(sqrt(dat_seoul$거래금액.만원.)~log(dat_seoul$층),f=2/3)
abline(lm1,col="green",lwd=2,lty=1)
lines(s1,col="red",lwd=2,lty=1)

hist(dat_ch$층)
exp(2.5)
# 결론
# 춘천: 저층의 경우 거래금액이 완만하게 증가하다, 12층 이후부터 가파르게 증가한다
# 강남: 저층의 경우 거래금액이 상수를 가진다. 12층 이후부터 가파르게 증가한다
# 층 변수의 경우 춘천과 강남에서 거래금액에 유사한 효과를 준다고 볼 수 있다.
# 춘천보다 강남 아파트에 고층이 많은 경향이 보인다.
# 같은 층수여도 강남 아파트의 거래금액이 더 크다. 즉 회귀식에서 강남의 인터셉트가 더 크다.
# 춘천보다 강남 아파트의 거래금액 분산이 더 크다.

# 층에 전처리 안할 경우
 par(mfrow=c(1,2))
 bagplot(y=sqrt(dat_ch$거래금액.만원.),x=dat_ch$층,main="(춘천)거래금액-층 산점도",ylim=c(50,1000),xlim=c(0,70), show.whiskers = F,
         xlab="층", ylab="sqrt(거래금액)")
 lm1<-lqs(sqrt(dat_ch$거래금액.만원.)~dat_ch$층,method="lms")
 s1<-lowess(sqrt(dat_ch$거래금액.만원.)~dat_ch$층,f=2/3)
 abline(lm1,col="green",lwd=2,lty=1)
 lines(s1,col="red",lwd=2,lty=1)
 legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)
 bagplot(y=sqrt(dat_seoul$거래금액.만원.), x=dat_seoul$층,main="(강남)거래금액-층 산점도",ylim=c(50,1000),xlim=c(0,70), show.whiskers = F,
         xlab="층", ylab="sqrt(거래금액)")
 lm1<-lm(sqrt(dat_seoul$거래금액.만원.)~dat_seoul$층)
 s1<-lowess(sqrt(dat_seoul$거래금액.만원.)~dat_seoul$층,f=1/3)
 abline(lm1,col="green",lwd=2,lty=1)
 lines(s1,col="red",lwd=2,lty=1)

# 커널밀도와등고선이미지 추가분석
par(mfrow=c(2,2))
plot(density(sqrt(dat_seoul$거래금액.만원.)), main="",
     xlim=c(0,1100), ylim=c(0, 0.013), xlab="", ylab="", col="blue")
par(new=T)
plot(density(sqrt(dat_ch$거래금액.만원.)), main="지역별 sqrt(거래금액) 밀도함수",
     xlim=c(0,1100), ylim=c(0, 0.013),
     xlab="sqrt거래금액(만원)", ylab="밀도", col="red")
legend("topright",legend=c("강남","춘천"),col=c("blue","red"),lwd=1)

plot(density(dat_seoul$층), main="지역별 층 밀도함수",
     xlim=c(0,70), ylim=c(0,0.07), xlab="층", ylab="밀도", col="blue")
par(new=T)
plot(density(dat_ch$층), main="지역별 층 밀도함수",
     xlim=c(0,70), ylim=c(0,0.07), xlab="층", ylab="밀도", col="red")
legend("topright",legend=c("강남","춘천"),col=c("blue","red"),lwd=1)

den1 = kde2d(dat_seoul$층, sqrt(dat_seoul$거래금액.만원.), n=500)
den2 = kde2d(dat_ch$층, sqrt(dat_ch$거래금액.만원.), n=500)
par(mfrow=c(1,2))
image(den1, xlab="층", ylab="sqrt거래금액(만원)", 
      main="(강남)거래금액-층 등고선도", ylim=c(0,800), xlim=c(0,40))
image(den2, xlab="층", ylab="sqrt거래금액(만원)", 
      main="(춘천)거래금액-층 등고선도", ylim=c(0,800), xlim=c(0,40))

plot(density(dat_ch$층), main="층 밀도함수", xlab="층", ylab="밀도")


 
 


#### 4. 거래금액-건축년도
#전처리: 거래금액 멱승변환, 건축년도에 jitter
par(mfrow=c(1,2))
bagplot(y=sqrt(dat_ch$거래금액.만원.),x=jitter(dat_ch$건축년도,factor=2),
     ylim=c(50,1000),xlim=c(1970,2030),main="(춘천)거래금액-건축년도 산점도",
     xlab="jitter(건축년도)", ylab="sqrt(거래금액)", show.whiskers = F)
lm2<-lm(sqrt(dat_ch$거래금액.만원.)~jitter(dat_ch$건축년도,factor=2))
s2<-lowess(sqrt(dat_ch$거래금액.만원.)~jitter(dat_ch$건축년도,factor=2),f=2/3)
abline(lm2,col="green",lty=1,lwd=2)
lines(s2,col="red",lty=1,lwd=2)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)

bagplot(y=sqrt(dat_seoul$거래금액.만원.), x=jitter(dat_seoul$건축년도,factor=2),
     ylim=c(50,1000),xlim=c(1970,2030),main="(강남)거래금액-건축년도 산점도",
     xlab="jitter(건축년도)", ylab="sqrt(거래금액)",show.whiskers = F)
lm3<-lm(sqrt(dat_seoul$거래금액.만원.)~jitter(dat_seoul$건축년도,factor=2))
s3<-lowess(sqrt(dat_seoul$거래금액.만원.)~jitter(dat_seoul$건축년도,factor=2),f=2/3)
abline(lm3,col="green",lty=1,lwd=2)
lines(s3,col="red",lty=1,lwd=2)

#결론
# 춘천에 비해 강남의 거래금액의 분산이 크다. 
# 춘천은 우상향의 선형성이 나타나지만 강남은 그 추세를 직관적으로 알기 어려운 분포다. 
# 강남은 옛날 아파트가 오히려 비싸고, 거래금액이 1995식 아파트까지 급격하게 떨어진다
# 이후 거래금액이 아파트 거래금액이 완만하게 상승하는 모습을 보인다.




#### 5. 거래금액-건축년도 강남 추가분석
# 강남 댄시티
par(mfrow=c(1,2))
plot(density(sqrt(dat_seoul$거래금액.만원.)), main="강남 sqrt(거래금액) 밀도함수")
plot(density(dat_seoul$건축년도), main="강남 건축년도 밀도함수")
den = kde2d(dat_seoul$건축년도, sqrt(dat_seoul$거래금액.만원.), n=1000)
# 강남 보자기
bagplot(x=jitter(dat_seoul$건축년도,factor=2),y=sqrt(dat_seoul$거래금액.만원.),show.whiskers = F
     ,main="(춘천)거래금액-건축년도 산점도",
     xlab="jitter(건축년도)", ylab="sqrt(거래금액)")
abline(lm3,col="green",lty=1,lwd=2)
lines(s3,col="red",lty=1,lwd=2)
# 강남등고선
image(den, xlab="건축년도", ylab="sqrt거래금액(만원)",
      main="강남 거래금액-건축년도 등고선그림림")



# 3) 평당 가격 - 건축년도
# 전처리: 평당 아파트 가격 로그변환, 건축년도 jitter
par(mfrow=c(1,2))
plot(log(dat_ch$평당.아파트.가격)~jitter(dat_ch$건축년도,factor=2),main="(춘천)평당 가격-건축년도",ylim=c(5,11),xlim=c(1970,2030))
lm4<-lm(log(dat_ch$평당.아파트.가격)~jitter(dat_ch$건축년도,factor=2))
s4<-lowess(log(dat_ch$평당.아파트.가격)~jitter(dat_ch$건축년도,factor=2),f=2/3)
abline(lm4,col="green",lwd=2,lty=1)
lines(s4,col="red",lwd=2,lty=1)
legend("topleft",legend=c("lm","lowess"),col=c("green","red"),lwd=2)



plot(log(dat_seoul$평당.가격)~jitter(dat_seoul$건축년도,factor=2),main="(강남)평당 가격-건축년도",ylim=c(5,11),xlim=c(1970,2030))
lm4<-lm(log(dat_seoul$평당.가격)~jitter(dat_seoul$건축년도,factor=2))
s4<-lowess(log(dat_seoul$평당.가격)~jitter(dat_seoul$건축년도,factor=2),f=2/3)
abline(lm4,col="green",lwd=2,lty=1)
lines(s4,col="red",lwd=2,lty=1)

#설명
## 춘천은 우상향의 추세가 나타나지만 강남은 그 추세를 알 수 없이 넓게 분포되어 있다. 
## 전체적으로 강남의 평당 가격이 높게 나타난다. 
## 춘천은 신식 건물일수록 가격이 높게 거래되는 반면 강남은 구식 건물도 높은 가격으로 거래되는 비율이 높다.



boxplot(sqrt(거래금액.만원.)~시군구1, main="춘천 지역(동) 별 거래금액 상자그림")
abline(h=mean(sqrt(거래금액.만원.)),col="red",lwd=2)
legend("topright",legend='meanline',col="red",lwd=2)
