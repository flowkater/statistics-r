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


setwd("~/work/statistics-r/")

## Mac for Korean / Mac 에서 한글 깨질때,
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

spider <- read.csv("./data/spider-gsr.csv", header = TRUE)

spider.lm <- lm(gsr ~ size, data = spider)

spider$predicted <- predict(spider.lm)
spider$residuals <- residuals(spider.lm)

spider.SST <- ggplot(spider, aes(size, gsr)) + 
  geom_segment(aes(xend = size, yend = mean(spider$gsr)), alpha = .2) +
  geom_hline(yintercept = mean(spider$gsr), color = "lightgrey") +
  geom_point() + 
  labs(x = "거미 크기", y = "불안(GSR)", title = "SST")
spider.SST

spider.SSR <- ggplot(spider, aes(size, gsr)) + 
  geom_segment(aes(xend = size, yend = predicted), alpha = .2) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) + 
  labs(x = "거미 크기", y = "불안(GSR)", title = "SSR")
spider.SSR

spider.SSE <- ggplot(spider, aes(size, gsr)) + 
  geom_segment(aes(y=predicted, xend = size, yend = mean(spider$gsr)), alpha = .2) +
  geom_hline(yintercept = mean(spider$gsr), color = "lightgrey") +
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) + 
  labs(x = "거미 크기", y = "불안(GSR)", title = "SSE/M")
spider.SSE

grid.arrange(spider.SST, spider.SSR, 
             spider.SSE, nrow = 2, ncol = 2)


## 직선
## Setup up coordinate system (with x==y aspect ratio):
plot(c(-5,5), c(-5,5), type = "n", xlab="x", ylab="y", asp = 1)
## the x- and y-axis, and an integer grid
abline(h=0, v=0, col = "gray60")
abline(h = -5:5, v = -5:5, col = "lightgray", lty=3)

# a 절편, b 기울기
abline(a=2, b=1, col = 2)
abline(a=0, b=1, col = 1)
abline(a=-3, b=3, col = 3)
abline(a=-3, b=-1, col = 3)
abline(a=1/2, b=-2, col = 5)

# ourlier residual

spider <- read.csv("./data/spider-gsr.csv", header = TRUE)
out <- data.frame(24, 7)
names(out) <- c("size", "gsr")

spiderOut <- rbind(spider, out)
spiderOut.lm <- lm(gsr ~ size, data = spiderOut)

spiderOut$predicted <- predict(spiderOut.lm)
spiderOut$residuals <- residuals(spiderOut.lm)



spiderOut.scatter <- ggplot(spiderOut, aes(size, gsr)) + 
  geom_segment(aes(xend = size, yend = predicted), alpha = .2) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) + 
  labs(x = "거미 크기", y = "불안(GSR)", title = "SSR")
spiderOut.scatter

grid.arrange(spider.SSR, 
             spiderOut.scatter, nrow = 1, ncol = 2)

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

## Todo: albumSales.lm1 에서 나온 Coefficients 들의 t 값과 p-value 를 직접 계산해보아라. (검정 파트 참고)


######################################################
### heights.csv

# father: 아버지 키
# son: 아들 키

######################################################

heights <- read.csv("./data/heights.csv", header = T)
head(heights)
str(heights)

heights.lplot <- ggplot(heights, aes(father, son)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) +
  geom_vline(xintercept = mean(heights$father), linetype = "dashed") + 
  geom_hline(yintercept = mean(heights$son), linetype = "dashed") +
  labs(x = "아버지 키", y = "아들 키")
heights.lplot

heights$stz_father <- (heights$father - mean(heights$father)) / sd(heights$father)
heights$stz_son <- (heights$son - mean(heights$son)) / sd(heights$son)

heights.stz_plot <- ggplot(heights, aes(stz_father, stz_son)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "lightblue", se = F) +
  geom_vline(xintercept = mean(heights$stz_father), linetype = "dashed") + 
  geom_hline(yintercept = mean(heights$stz_son), linetype = "dashed") +
  labs(x = "표준화된 아버지 키", y = "표준화된아들 키")
heights.stz_plot

grid.arrange(heights.lplot, heights.stz_plot, nrow = 1, ncol = 2)

heights_r <- cor(heights$father, heights$son);heights_r

x <- seq(from=-3.0, to=3.0, length.out=1078) 
y <- heights_r * x

r_data <- data.frame("father_r" = x, "son_r" = y)
r_data.line <- ggplot(r_data, aes(father_r, son_r)) + 
  geom_point() + 
  geom_vline(xintercept = mean(r_data$father_r), linetype = "dashed") + 
  geom_hline(yintercept = mean(r_data$son_r), linetype = "dashed") +
  labs(x = "표준화된 아버지 키", y = "표준화된아들 키") +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
r_data.line

heights.stz_plot <- heights.stz_plot + coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

grid.arrange(heights.stz_plot, r_data.line, nrow = 2, ncol = 1)

heights_r
heights.lm <- lm(son ~ father, data = heights)
summary(heights.lm)

round(mean(heights$father), 2)
round(mean(heights$son),2)
round(sd(heights$father),2)
round(sd(heights$son),2)


######################################################

install.packages("scatterplot3d") # Install
install.packages("rgl")
library(scatterplot3d)
library(rgl)

######################################################
### pubs.csv

# pubs: 술집 수
# mortality: 일정 기간의 사망자 수

######################################################

pubs <- read.csv("./data/pubs.csv", header = TRUE)
head(pubs)

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

pubsOut$resid <- round(resid(pubsOut.lm), 2)
pubsOut$cooks <- round(cooks.distance(pubsOut.lm), 2)
pubsOut$leverage <- round(hatvalues(pubsOut.lm), 2)
pubsOut$dfbeta <- round(dfbeta(pubsOut.lm), 2)

View(pubsOut)


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


## 3d scatterplot & regression plane
s3d <- scatterplot3d(album2[, c("airplay", "adverts", "sales")], angle=55, pch = 16)
albumSales.lmd <- lm(sales ~ airplay + adverts, data = album2)
s3d$plane3d(albumSales.lmd)

scatter3d(x = album2$airplay, y = album2$adverts, z = album2$sales)

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

# head(album2)
View(album2)

# write.csv(album2, "./data/albumSalesDiagnostics.csv", row.names = F)

album2$stz.resid > 2 |  album2$stz.resid < -2

album2$large.resid <- album2$stz.resid > 2 |  album2$stz.resid < -2

# outlier residual count
sum(album2$large.resid)

# 164, 169 > 2.5 (within 1%), but 169 over 3 
album2[album2$large.resid, c("sales", "airplay", "attract", "adverts", "stz.resid")]

# cooks.distance > 1 ?
# mean of leverage(hat) value = (0.02*(3 + 1)) / 200
hat <- ((3 + 1)) / 200; hat
hat * 3
hat * 2
album2[album2$large.resid, c("cooks", "leverage", "covratios")]

## covariance ratio, CVR
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
# gender: 성별
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
