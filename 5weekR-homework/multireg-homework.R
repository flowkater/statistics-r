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

# setwd("~/work/statistics-r/")

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
## TODO: 에세이 점수를 결과 변수로 하고 변수를 선택하여 다중회귀모형을 만들고 해석해보아라. (다중회귀보고법, F비, 기울기, R2 해석)
######################################################

essayData <- read.csv("./data/EssayMarks.csv", header = TRUE)



######################################################
### examData.csv

# Code: 점수가 속한 참가자를 식별하는 번호
# Revise: 참가자가 복습(시험 준비)에 소비한 시간
# Exam: 참가자의 시험 점수를 퍼센트로 환산한 값
# Anxiety: EAQ 평가 점수
# Gender: 참가자의 성별("male" or "female)
# TODO: Exam 을 결과 변수로 하고 변수를 선택하여 다중회귀모형을 만들고 해석해보아라. (다중회귀보고법, F비, 기울기, R2 해석)
######################################################

examData <- read.csv("./data/examData.csv", header = TRUE)


######################################################
### pubs.csv

# pubs: 술집 수
# mortality: 일정 기간의 사망자 수

## TODO: 술집 수로 사망자 수를 예측하는 단순 회귀 분석을 시행하고 회귀 매개변수들에 부트스트랩(bootstrap) 방법을 적용해서 같은 분석을 해보아라.
### 두 모형을 비교해보고 결론을 요약해보자.
######################################################
pubs <- read.csv("./data/pubs.csv", header = TRUE)
head(pubs)

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

## TODO: 학생들의 성격적 특성 변수가 학생이 원하는 강사의 성격적 특성들을 예측하는지 파악하라.
### lectureN/E/O/A/C: 각 학생이 강의자에게 원하는 성격 각각 5가지의 결과 변수에 대한 다중상관 분석 및 회귀 모형 생성
### 1. 학생의 나이와 성별을 이용하여 각 5가지 결과 변수를 예측하는 5가지 회귀 모델 생성
### 2. 1번 모델에서 studentN/E/O/A/C 를 추가하여 각 5가지 결과 변수를 예측하는 5가지 회귀 모델 생성
### 각 회귀 모형마다 결과들을 표로 작성하고 해석해보아라.

######################################################
cpData <- read.csv("./data/Chamorro-Premuzic.csv", header = TRUE)
head(cpData)


######################################################
### Supermodel.csv

# SALARY: 일일 패션쇼 출연 수입
# AGE: 모델의 나이
# YEARS: 모델 경력 햇수
# BEAUTY: 모델의 매력 (모델 에이전시가 매긴 % 점수)

## TODO: 어떤 변수들이 모델의 수입(Salary)을 예측하는지 다중회귀분석을 수행하고 회귀 모형의 타당성을 검증하라.
# t-value, F 값을 기본으로 이상치, 잔차, 모수적 가정들이 성립하는지 다각도 확인해보고 본인이 만든 회귀 모형의 결과를 정리해보자.
######################################################

smodel <- read.csv("./data/Supermodel.csv", header = TRUE)
head(smodel)
str(smodel)


######################################################
### ChildAggression.csv

## 손위(언니, 오빠, 누나, 형) 형제가 있는 아동 666명을 조사해서 공격성(Aggression)과 그에 대한 여러 잠재적 예측 요인의 관계를 분석한 연구


# Aggression: 공격성
# Television: 텔레비젼 시청 시간 (많을수록 높음)
# Computer_Games: 컴퓨터 게임을 하는 시간 (많을수록 높음)
# Sibling_Aggression: 손위 형제의 공격성
# Diet: 식습관 (식품첨가물이 덜 든 음식을 먹을수록 점수가 높음)
# Parenting_Style: 부모의 가정 교육 스타일 (가정 교육 스타일이 나쁠수록 점수가 높음)

## TODO: 어떤 변수들이 아동의 공격성(Aggression)에 영향을 주는지 다중회귀 분석을 수행하고 회귀 모형의 타당성을 검증하라.
# t-value, F 값을 기본으로 이상치, 잔차, 모수적 가정들이 성립하는지 다각도 확인해보고 본인이 만든 회귀 모형의 결과를 정리해보자.
# 참고: 이전 연구에서 가정 교육 스타일과 형제의 공격성이 손아래 아동의 공격성 수준을 잘 예측한다고 한다.
######################################################
childAggression <- read.csv("./data/ChildAggression.csv", header = T)
head(childAggression)
str(childAggression)
summary(childAggression)

