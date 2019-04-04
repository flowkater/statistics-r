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

setwd("~/work/statistics-r/")

## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")

######################################################
### Todo Zone

# 아래 각 데이터를 이용하여 상관관계를 분석해보아라.
# 산점도, 히트맵 등 데이터 분석에 필요한 여러가지 그래프도 ggplot2 을 이용하여 같이 그려보아라.
# 자료에서 나온 상관분석보고법을 바탕으로 각 질문에 대한 결론을 내려보아라.
# 구간자료일 때 선형회귀 모델을 적용하여 결론을 내려보아라.

######################################################


######################################################
### EssayMarks.csv

# essay: 퍼센트로 환산한 에세이 점수
# hours: 에세이를 쓰는 데 걸린 시간
# grade: 점수들을 성적 등급
######## - First Class - 1급
######## - Upper Second Class - 상2급
######## - Lower Secod Class - 하2급
######## - Third Class - 3급
######## - Pass - 수료
######## - fail - 낙제

## TODO: 에세이 쓰는데 걸린 시간과 점수, 시간과 등급 간의 상관계수를 계산하고 신뢰구간과 유의한지 판단하라.
### (변수의 특성에 유의하여 적절한 상관계수를 사용하여라.)
## TODO: 에세이 점수를 결과 변수로 하고 걸린 시간을 예측변수로 하여 각각 선형회귀를 구해보아라.
######################################################

essayData <- read.csv("./data/EssayMarks.csv", header = TRUE)
head(essayData)
summary(essayData$grade)




######################################################
### ChickFlick.csv

# ChickFlick - 대체로 남자들보다 여자들이 더 좋아할만한 영화를 뜻하는 속어
# gender: 참가자의 성별(문자열)
# film: 관람한 영화 제목(문자열)
# arousal: 각성 점수 (영화를 얼마나 즐겼는지)

## TODO: 성별과 각성 정도의 사이의 상관관계가 존재하는지 조사하라.
## TODO: 시청한 영화와 각성 정도의 사이의 상관관계가 존재하는지 조사하라.
######################################################

chickFlick <- read.csv("./data/ChickFlick.csv", header = TRUE)
head(chickFlick)
summary(chickFlick)




######################################################
### grades.csv

# stats: 대학교 1학년 학기말 통계학 성적 등급
# math: 수능 수리영역 등급

## TODO: 대학교 1학년 학기말 통계학 성적이 수능 수리영역 등급과 상관관계가 존재하는지 조사하라.
######################################################
gradeData <- read.csv("./data/grades.csv", header = TRUE)
head(gradeData)


######################################################
### examData.csv

# Code: 점수가 속한 참가자를 식별하는 번호
# Revise: 참가자가 복습(시험 준비)에 소비한 시간
# Exam: 참가자의 시험 점수를 퍼센트로 환산한 값
# Anxiety: EAQ 평가 점수
# Gender: 참가자의 성별("male" or "female)
# TODO: Exam 을 결과변수로 하여, Revise, Anxiety 각각을 예측변수로 한 두가지 선형 회귀 모델을 만들고 분석을 해보아라.

######################################################

examData <- read.csv("./data/examData.csv", header = TRUE)


######################################################
### Chamorro-Premuzic.csv

# 학생들의 성격적 특성과 그들이 바라는 강의자의 특성과 상관관계가 있을까?
## 외향적인 성격의 학생이 외향적인 성격의 강의자를 원할까?

# Age: 나이
# Gender: 성별

# N: 신경증(neuroticism)
# E: 외향성(extroversion)
# O: 개방성(openness to experience)
# A: 수용성(agreeableness)
# C: 성실성(conscientiousness)
# studentN/E/O/A/C: 위 각 항목별로 학생들의 성격을 측정한 점수
# lectureN/E/O/A/C: 각 학생이 강의자에게 원하는 성격

## TODO: 학생들의 성격과 그들이 바라는 강의자의 성격과의 상관관계를 도출해보아라. (히트맵, 산점도 등을 이용하여 여러 방법으로 상관계수를 조사하라.)
######################################################
cpData <- read.csv("./data/Chamorro-Premuzic.csv", header = TRUE)
head(cpData)

######################################################
### titanic.csv


# Survivied: 생존여부
# Pclass: 1등석, 2등석, 3등석
# Name : 승객 이름
# Sex : 승객 성별
# Age : 승객 나이 
# SibSp : 동반한 형제자매, 배우자 수
# Patch : 동반한 부모, 자식 수
# Ticket : 티켓의 고유 넘버
# Fare 티켓의 요금
# Cabin : 객실 번호
# Embarked : 승선한 항구명(C : Cherbourg, Q : Queenstown, S : Southampton)

## TODO: 타이타닉 승객 명단 정보를 이용하여 생존 여부와 관련된 상관관계를 분석해보아라. (점이연상관계수 이용)
######################################################
titanic <- read.csv("./data/titanic.csv", header = TRUE, row.names = 1)
head(titanic)


######################################################
# Team Todo
## TODO: 가지고 있는 데이터를 이용하여 상관관계를 분석해보아라.
######################################################

