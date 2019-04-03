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
####### Section 0. 통계 모형, 설명 따라가보기 ########

# install.packages("Hmisc")
# install.packages("ggm")
# install.packages("polycor")

library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)



setwd("~/work/statistics-r/")

## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")

######################################################
### examData.csv

# Code: 점수가 속한 참가자를 식별하는 번호
# Revise: 참가자가 복습(시험 준비)에 소비한 시간
# Exam: 참가자의 시험 점수를 퍼센트로 환산한 값
# Anxiety: EAQ 평가 점수
# Gender: 참가자의 성별("male" or "female)

######################################################

examData <- read.csv("./data/examData.csv", header = TRUE)
View(examData)
str(examData)

examData2 <- examData[, c("Revise", "Exam", "Anxiety")]

# covariance 공분산
cov(examData$Exam, examData$Anxiety)
cov(examData2)


# 상관계수 cor
cor(examData2)
cor(examData2, use = "complete.obs", method = "pearson")
cor(examData2, use = "complete.obs", method = "spearman")

cor(examData$Exam, examData$Anxiety,use = "complete.obs", method = "pearson")
cor(examData$Exam, examData$Anxiety,use = "pairwise.complete.obs", method = "pearson")

cor(examData$Exam, examData$Anxiety,use = "complete.obs", method = "kendall")

# rcorr
Exam <- as.matrix(examData$Exam)
Anxiety <- as.matrix(examData$Anxiety)
examDataMatrix <- as.matrix(examData2)

Hmisc::rcorr(Exam, Anxiety, type = "pearson")
Hmisc::rcorr(examDataMatrix, type = "pearson")


# cor.test
cor.test(examData$Exam, examData$Anxiety, method = "pearson")
cor.test(examData$Exam, examData$Anxiety, method = "kendall")

## TODO: 복습시간(Revise), 시험 성적(Exam), 시험 불안(Anxiety)의 모든 가능한 쌍(총 세가지)의 상관계수 신뢰구간을 각각 계산하라.


# R^2 coefficient of determination
cor(examData2)
cor(examData2)^2
cor(examData2)^2 * 100

######################################################
### The Biggest Liar.csv

# Creativity: 창의력 점수(60점 만점)
# Position: 대회 순위 (1-우승, 2-준우승, 3-3등, 4-4등, 5-5등)
# Novice: 이번 대회가 처음인지, 이전에 참가한 적이 있는지 
# 창의적인 사람일수록 거짓말 대회에서 좋은 성적을 거둘까?
######################################################

liarData <- read.csv("./data/The Biggest Liar.csv", header = TRUE)
View(liarData)
str(liarData)

## TODO: Position 변수와 Creativity 변수의 스피어먼 상관계수를 계산해보라.

cor(liarData$Position, liarData$Creativity, method = "spearman")

liarMatrix <- as.matrix(liarData[, c("Position", "Creativity")])
Hmisc::rcorr(liarMatrix)

