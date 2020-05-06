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
# 맥에서 한글 깨질때
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")


#######################################################
####### Part 3. Titanic Prediction ########

install.packages("tree")

library(ggplot2)
library(dplyr)
library(tree)



####### 0. Load Dataset  #######


####### 1. EDA #######


####### 2. Feature Engineering #######

### Name



## Todo: Test Data

### Age

## Miss Mean Age


## Todo: Master, Mr, Mrs, Rare, mean age?


## Miss Fare

## Todo: Input Missing Fare Median Fare

## Embarked

## Cabin

## Todo: Test CabinS


## Factor transform


####### 3. Predict #######
# Logit 돌려보기

# Logit predict

# Decision Tree


####### 4. Submit #######


###



