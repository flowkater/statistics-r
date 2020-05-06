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
# install.packages("truncnorm")

library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)
library(gridExtra)
library(truncnorm)


setwd("~/oldwork/statistics-r/")

## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")

spider <- read.csv("./data/spider-gsr.csv", header = TRUE)
View(spider)

spider.lm <- lm(gsr ~ size, data = spider)
# AIC(spider.lm)

spider$predicted <- predict(spider.lm)
spider$residuals <- residuals(spider.lm)
spider

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
  labs(x = "광고 마케팅 비용", y = "판매량")
albumSales.scatter

## Todo: albumSales.lm1 에서 나온 Coefficients 들의 t 값과 p-value 를 직접 계산해보아라. (검정 파트 참고)

9.612e-02
1.341e+02

x = 1000

0.09612 * x + 134.1


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

View(heights)

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



####


