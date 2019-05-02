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
####### Section 10. 비모수적 검정 ########

# install.packages("clinfun")

library(clinfun)
library(car)
library(ggplot2)
library(pastecs)
library(reshape2)

setwd("~/work/statistics-R/")

######################################################
### drugData

# drug: Ecstasy (마약), Alcohol (알코올)
# sundayBDI: 일요일 우울증 수치
# wedsBDI: 수요일 우울증 수치

######################################################

drugData <- read.csv("./data/Drug.csv", header = T)

## EDA
View(drugData)
summary(drugData)

stat.desc(drugData[,c('sundayBDI', 'wedsBDI')], basic = F, norm = T)

# by(drugData$sundayBDI, drugData$drug, shapiro.test)
# by(drugData$wedsBDI, drugData$drug,shapiro.test)

by(drugData$sundayBDI, drugData$drug, stat.desc, basic = F, norm = T)
by(drugData$wedsBDI, drugData$drug, stat.desc, basic = F, norm = T)

leveneTest(drugData$sundayBDI, drugData$drug)
leveneTest(drugData$wedsBDI, drugData$drug)

## TODO: 그룹별 각 데이터의 분포와 박스플롯을 그려보시오.


## 윌콕슨 순위합 검정(wilcoxon rank sum test) - 독립 t 검정의 비모수적 방법
sunModel <- wilcox.test(sundayBDI ~ drug, data = drugData); sunModel

wedModel <- wilcox.test(wedsBDI ~ drug, data = drugData); wedModel

## tie가 있어 정확한 p 값을 계산할 수 없습니다. => 동순위가 존재하여 참값 계산 불가

# 효과크기
rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  cat((wilcoxModel$data.name), "Effect Size, r = ", r)
}

rFromWilcox(sunModel, 20)
rFromWilcox(wedModel, 20)

## 결과 해석
## - 수요일의 마약 복용자의 우울증 수준(Mdn = 33.50)과 알코올 복용자의 우울증 수준(Mdn = 7.50)의 차이는 W=4, p<.001, r=-.78 로 유의했다.
## - (비모수적 검정에서는 평균보다 중앙값(Mdn)이 더 적합)

## 윌콕슨 부호순위합 검정(wilcoxon signed rank sum test) - 종속 t 검정의 비모수적 방법
alcoholData <- subset(drugData, drugData$drug == "Alcohol");alcoholData
ecstasyData <- subset(drugData, drugData$drug == "Ecstasy");ecstasyData


alcoholModel <- wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI, paired = TRUE, correct = F);alcoholModel
ecstasyModel <- wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, paired = TRUE, correct = F);ecstasyModel
# correct - 연속성 수정 옵션

# 효과크기
rFromWilcox(ecstasyModel, 20)
rFromWilcox(alcoholModel, 20)

## 결과 해석
## - 엑스터시 복용자의 경우, 수요일의 우울증 수준(Mdn = 33.50)이 일요일의 우울증 수준(Mdn = 17.50)에 비해 p = .047, r = -.56 으로 유의하게 높았다.
## - 그러나 알코올 복용시에는 수요일의 우울증 수준(Mdn = 7.50)이 일요일의 우울증 수준(Mdn = 16.00)에 비해 p = .012, r = -.45 로 유의하게 낮았다.

## 크러스컬-월리스 검정(Kruskal-Wallis test) - 일원 독립 분산분석(ANOVA)의 비모수적 방법

#######################################################
### Marketing Revenue.csv

# SMS: 문자 메시지로 구매한 횟수
# Push: 모바일 앱 내의 푸시 메시지로 구매한 횟수
# Banner: 모바일 앱 내의 배너 광고로 구매한 횟수
# Email: 이메일로 구매한 횟수
# Blog: 서비스 운영 블로그 포스팅 링크로 구매한 횟수
# Facebook: 페이스북 paid 광고로 구매한 횟수

#######################################################

marketing <- read.csv("./data/Marketing Revenue.csv", header = T)
str(marketing)
View(marketing)

marketingData <- melt(marketing)
names(marketingData) <- c("channel", "count")
View(marketingData)

# EDA
boxplot(marketing)

## TODO: marketingData 에 대하여 정규성 검정, 레빈 검정을 시행하라.


## TODO: marketingData을 이용하여 ggplot2 을 이용하여 boxplot 을 그려보아라

## Kruskal.test
kruskal.test(count~channel, data = marketingData)

# 순위 및 평균 순위 구하기
marketingData$Ranks <- rank(marketingData$count)
by(marketingData$Ranks, marketingData$channel, mean)

## 결과해석
## - ANOVA 처럼 그룹들간의 차이가 유의하다만 알 수 있다.
## - Boxplot 및 평균 순위를 이용하여 차이를 설명할 수 있다.



