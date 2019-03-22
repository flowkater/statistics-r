## Script 창으로 들어가는 키 숏컷: Ctrl + 1
## Console 창 들어가는 키 숏컷: Ctrl + 2
## Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
## Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
## Console 클리닝: Ctrl + L
## 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
## Mac for Korean / Mac 에서 한글 깨질때,
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")
## R version check: Sys.getenv("R_ARCH") - 32 bit인 분은 R을 다시 인스톨해주시기 바랍니다
## 64 bit for "/x64"
## 32 bit for "/i386"
## 각 라인마다 Ctrl(Cmd) + Enter


#######################################################
####### Section 3. 확률 ########

library(ggplot2)
library(gridExtra)

## 확률-동전던지기

flip_coin <- sample(x=c(0:1), size = 3, replace = TRUE)
flip_coin
mean(flip_coin)

flip_coin <- function(n) {
  l <- c()
  yy <- c()
  for(x in 1:n){
    flip_coin <- sample(x=c(0:1), size = x, replace = TRUE)
    l <- c(l, mean(flip_coin))
    yy <- c(yy, x)
  }
  
  histogram <- ggplot(data.frame(x = l), aes(x)) + 
    geom_histogram(binwidth = 0.02,
                   colour="black", fill="blue", alpha = 0.5) + 
    coord_cartesian(xlim=c(0, 1))
  
  lineplot <- ggplot(data.frame(x = yy, y = l), aes(x, y)) + geom_line()
  
  grid.arrange(histogram, lineplot, nrow=2, ncol=1)
}

flip_coin(10)
flip_coin(50)
flip_coin(100)
flip_coin(1000)
flip_coin(10000)


## 기대값 계산
x <- c(10000, 1000, 100, 1, 0)
y <- c(1, 5, 15, 180, 99799)

sum(x * y) / sum(y)


27 / 128

## 조합 구하기
choose(20, 7) # 20 * 19 * 6 * 17 * 2

## 이항분포 확률 계산하기
choose(20, 7) * (1/4)^7 * (3/4)^13
dbinom(7, 20, 1/4)


## 표준정규분포 확률분포표
pnorm(1.96)

(1-pnorm(1.96))*2
pnorm(1.64)

a = - (22 / 38.73)
b = 34 / 38.73
pnorm(b) - pnorm(a)

pnorm(1.175) * 7 + 74

qnorm(0.88)

2.306
qt(1-0.025, 8) 



