## Script 창으로 들어가는 키 숏컷: Ctrl + 1
## Console 창 들어가는 키 숏컷: Ctrl + 2
## Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
## Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
## Console 클리닝: Ctrl + L
## 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
## R version check: Sys.getenv("R_ARCH") - 32 bit인 분은 R을 다시 인스톨해주시기 바랍니다
## 64 bit for "/x64"
## 32 bit for "/i386"
## 각 라인마다 Ctrl(Cmd) + Enter


#######################################################
####### Section 5. 회귀(Regression) ########

# install.packages("QuantPsyc")

library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)
library(gridExtra)
library(truncnorm)


# setwd("~/oldwork/statistics-r/")

## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")

######################################################
### Album Sales 1.csv

# adverts: 광고 마케팅 비용 
# sales: 앨범 판매량

######################################################

album1 = read.csv("./data/Album Sales 1.csv", header = TRUE)
head(album1)

albumSales.lm1 <- lm(album1$sales ~ album1$adverts)
# albumSales.lm1 <- lm(sales ~ adverts, data = album1)
summary(albumSales.lm1)

cor(album1$sales, album1$adverts)^2

albumSales.scatter <- ggplot(album1, aes(adverts, sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = F) + 
  labs(x = "광고 마케팅 비용", y = "앨범 판매량")
albumSales.scatter

######################################################
### pubs.csv

# pubs: 술집 수
# mortality: 일정 기간의 사망자 수

######################################################

pubs <- read.csv("./data/pubs.csv", header = TRUE)
head(pubs)
View(pubs)

pubsOut <- pubs
pubs <- pubs[1:7, ]

pubs.lplot <- ggplot(pubs, aes(pubs, mortality)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) +
  coord_cartesian(xlim=c(0, 510), ylim=c(0, 10500)) 

pubsOut.lplot <- ggplot(pubsOut, aes(pubs, mortality)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) +
  coord_cartesian(xlim=c(0, 510), ylim=c(0, 10500)) 


grid.arrange(pubs.lplot, pubsOut.lplot, nrow = 1, ncol = 2)

pubsOut.lm <- lm(mortality ~ pubs, data = pubsOut)
summary(pubsOut.lm)

pubsOut$rstudent <- round(rstudent(pubsOut.lm), 2)
pubsOut$resid <- round(resid(pubsOut.lm), 2)
pubsOut$cooks <- round(cooks.distance(pubsOut.lm), 2)
pubsOut$leverage <- round(hatvalues(pubsOut.lm), 2)
pubsOut$dfbeta <- round(dfbeta(pubsOut.lm), 2)
pubsOut$dffit <- round(dffits(pubsOut.lm), 2)

View(pubsOut)

#######################################################



######################################################
### Album Sales 2.csv

# adverts: 광고 마케팅 비용 
# sales: 앨범 판매량
# airplay: 방송 횟수
# attract: 가수의 매력 (0~10, 최빈값 선택)

######################################################

album2 <- read.csv("./data/Album Sales 2.csv", header = TRUE)
head(album2)
summary(album2)

albumSales.lm2 <- lm(sales ~ adverts, data = album2)
albumSales.lm3 <- lm(sales ~ adverts + airplay + attract, data = album2)
# albumSales.lm3 <- update(albumSales.lm2, .~. + airplay + attract)

summary(albumSales.lm2)
summary(albumSales.lm3)

steins_r_squared <- function(n, k, R_2) {
  return( 1 - (( ((n-1) / (n-k-1)) * ((n-2) / (n-k-2)) * ((n+1) / (n))   )  * (1 - R_2)))
}

str(album2) # n = 200, k = 3, R^2 = 0.6647
steins_r_squared(200, 3, 0.6647)

lm.beta(albumSales.lm3)


confint(albumSales.lm3)

F_change <- function(n, k2, k_change, R2_2, R2_2_change) {
  return( ( (n - k2 - 1) * R2_2_change ) / ( k_change * (1 - R2_2) )   )
}

F_change(200, 3, 2, 0.6647, 0.330)

# p-value
pf(F_change(200, 3, 2, 0.6647, 0.330), 2, 196, lower.tail = FALSE)


anova(albumSales.lm2, albumSales.lm3)



album2$resid <- resid(albumSales.lm3)
album2$stz.resid <- rstandard(albumSales.lm3)
album2$stu.resid <- rstudent(albumSales.lm3)
album2$cooks <- cooks.distance(albumSales.lm3)
album2$dfbeta <- dfbeta(albumSales.lm3)
album2$dffit <- dffits(albumSales.lm3)
album2$leverage <- hatvalues(albumSales.lm3)
album2$covratios <- covratio(albumSales.lm3)

View(album2)

album2$stz.resid > 2 |  album2$stz.resid < -2

album2$large.resid <- album2$stz.resid > 2 |  album2$stz.resid < -2

# outlier residual count
sum(album2$large.resid)

# 164, 169 > 2.5 (within 1%), but 169 over 3 
album2[album2$large.resid, c("sales", "airplay", "attract", "adverts", "stz.resid")]

# TODO: cooks.distance > 1 (album2 에서 쿡의 거리가 1보다 큰 데이터가 있는지 조건으로 필터링해보아라.)


# TODO: mean of leverage(hat) value = (0.02*(3 + 1)) / 200 (album2 강의에서 알려준 Hat value 기준으로 필터링 해보아라.)
hat <- ((3 + 1)) / 200; hat
hat * 3
hat * 2
album2[album2$large.resid, c("cooks", "leverage", "covratios")]

## TODO: covariance ratio, CVR (album2 강의자료에서 알려준 CVR 기준으로 필터링 해보아라.)



## 오차 독립성 가정 검정
durbinWatsonTest(albumSales.lm3)
# dwt(albumSales.lm3)

vif(albumSales.lm3)
1/vif(albumSales.lm3)
mean(vif(albumSales.lm3))

par(mfrow=c(2,2))
plot(albumSales.lm3)

hist(album2$stu.resid)
hist(album2$adverts)
qqnorm(album2$stu.resid); qqline(album2$stu.resid)
qqnorm(album2$adverts); qqline(album2$adverts)

# Robust Regression, Bootstrapping
# object <- boot(data, function, repetion)
bootReg <- function(formula, data, indices){
  d <- data[indices,]  
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, 
                    formula = sales ~ adverts + airplay + attract,
                    data = album2, 
                    R = 2000)

# b0
boot.ci(bootResults, type = "bca", index = 1) 
# boot.ci(bootResults, type = "perc", index = 1) 
# boot.ci(bootResults, type = "norm", index = 1) 
# boot.ci(bootResults, type = "basic", index = 1)

# b1 adverts
boot.ci(bootResults, type = "bca", index = 2) 

# b2 airplay
boot.ci(bootResults, type = "bca", index = 3) 

# b3 attract
boot.ci(bootResults, type = "bca", index = 4) 


######################################################
### festivalRegData

# ticknumb: 티켓넘버
# day1: 축제 첫째날 위생(hygiene) 상태
# day2: 축제 둘째날 위생(hygiene) 상태
# day3: 축제 셋째날 위생(hygiene) 상태
## 위생 점수는 0~4
# change: 첫째날과 셋째날의 차이
# music: 음악취향
## - indie kid (얼터너티브 음악) / crusty (힙,포크,앰비언트 장르) / mettaller (헤비메탈 장르) / no musical affiliation (취향 없음)

######################################################
gfr <- read.csv("./data/GlastonburyFestivalRegression.csv", header = TRUE)
head(gfr)
summary(gfr)

contr.treatment(4, base = 4)

contrasts(gfr$music) <- contr.treatment(4, base = 4)
contrasts(gfr$music)

crusty_v_NMA <- c(1, 0, 0, 0)
indie_v_NMA <- c(0, 1, 0, 0)
metal_v_NMA <- c(0, 0, 1, 0)

contrasts(gfr$music) <- cbind(crusty_v_NMA, indie_v_NMA, metal_v_NMA)
contrasts(gfr$music)

gfr.lm <- lm(change ~ music, data = gfr)
summary(gfr.lm)

round(tapply(gfr$change, gfr$music, mean, na.rm = T), 3)

###### 회귀 샘플사이즈
sampleSize <- read.csv("./data/cohen-effect-sample-size.csv", header = TRUE)
head(sampleSize)

sampleSize.lplot <- ggplot(sampleSize, aes(Npredictors, SampleSize, colour = EffectSize)) +
  geom_line() +
  geom_point()
sampleSize.lplot
