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
####### Section 11. 범주형 분석 ########

install.packages("gmodels")
install.packages("MASS")

library(gmodels)
library(MASS)


## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")
# 
# setwd("~/work/statistics-R/")

## 카이제곱 검정
######################################################
### cats.csv

# Training: 훈련에서 고양이에게 보상으로 준 것 (Food(먹이),Affection(애정))
# Dance: 고양이가 춤을 추었는가

######################################################

catsData <- read.csv("./data/cats.csv", header = T)
head(catsData)

catsTable <- table(catsData);catsTable

CrossTable(catsData$Training, catsData$Dance, fisher = T, chisq = T, expected = T, sresid = T, format="SPSS")




