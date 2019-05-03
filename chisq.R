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



####### Logistic regression ####### 
install.packages("Amelia")

suppressMessages({
  library(Amelia)
})


### titanic 예제 ###
## https://www.kaggle.com/c/titanic
train <- read.csv("./data/titanic.csv", header = T)
summary(train)
summary(train$Sex)

# Unique value를 확인 (같은 값이 여러번 반복되었으면 작은 값)
sapply(train, function(x) length(unique(x)))

# Missing value를 확인
sapply(train, function(x) sum(is.na(x)))

# Missing value를 그래프로 확인
missmap(train, main = "NA vs. Observed")

# 데이터가 거의 없는 Cabin 항목은 포기. Age랑 Embarked는 어떻게 처리할까? 아래에서 다시 recall

# Missing이 많은 Age는 어떻게 할까?
summary(train$Age)

# Systematic하게 없는 걸까? 아니면 그냥 단순히 누락된 걸까?
# Systematic이면 그 상황을 알아서 복구해야하고
# 아니라면 평균값, 중간값 등의 대표값을 써도 무관
# 일단 잘 모르겠으면 Distribution plot을 그려보자

z <- train$Age
s <- 2 * z + 4 + rnorm(length(train$Age)) 
par(mfrow = c(2, 2)) 
hist(z)
plot(s, z)
qqnorm(z)
qqline(z)
plot(1:length(z),log(z), lty = 1)
par(mfrow = c(1, 1)) 

# Age 데이터가 정규분포에 가까우므로 Systematic NA가 아니라고 판단
# (부정확하지만 현재까지 가지고 있는 정보로는 이게 최선)
# Age에서 NA인 부분을 대표값으로 교체

train$Age[is.na(train$Age)] <- mean(train$Age,na.rm=T) # 평균값을 이용한다. 약 29.7세

plot(density(train$Age))

# 여자와 아이를 위주로 구했다던데?
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived)) # 2x2 행렬 총합을 기준으로 확률 계산
prop.table(table(train$Sex, train$Survived),1) # 1x2 행렬 합을 기준으로 확률 계산

# 어린아이도 많이 생존했는지 확인하기 위해서 새 항목 개설
train$Child <- 0
train$Child[train$Age < 18] <- 1

table(train$Child, train$Survived)
prop.table(table(train$Child, train$Survived),1)

# 어린아이도 많이 생존했다면, 성별과 결합해서 보면 어떨까?
aggregate(Survived ~ Child + Sex, data=train, FUN=sum) # 살아남은 사람들의 숫자
aggregate(Survived ~ Child + Sex, data=train, FUN=length) # 각 그룹별 사람들의 총 숫자
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) #각 그룹별 생존률


## 객실 등급별 생존률
# 우선 객실 등급이 어떻게 나눠져 있는지 확인
summary(train$Pclass)
summary(train$Fare)


# 등급은 1,2,3 등급으로 나누어져 있음. 그러나 지불 요금은
# Numeric value로 되어 있으므로 구간을 나누는 수 밖에 없음

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})


# Logit 돌려보기

model <- glm(Survived ~ Child + Sex + Pclass + Fare2 + Embarked, family=binomial(link='logit'), data=train)

summary(model)

# 통계 분석
anova(model, test="Chisq")

## test 데이터 불러와서 예측해보기
test <- read.csv("./data/test.csv", header = T)
summary(test)

# train 데이터에서 만들었던 (Child, Fare2) Test set 에도 같은 데이터 구성

test$Child <- 0
test$Child[test$Age < 18] <- 1

test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'

# Logit predict
fitted.results <- predict(model, newdata = subset(test, select = c('Child', 'Sex', 'Pclass', 'Fare2', 'Embarked')), type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

test$Survived <- 0
test$Survived <- fitted.results

test$Survived

submission <- test[, c('PassengerId', 'Survived')]


write.csv(submission, paste("./submission", Sys.time(),  ".csv", sep = ""), row.names = F)

