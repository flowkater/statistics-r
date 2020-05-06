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
####### Part 2. Titanic Analysis ########

install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

setwd("~/oldwork/statistics-R/")

train <- read.csv("./titanic/train.csv", stringsAsFactors = FALSE)
View(train)

str(train)
summary(train)


# - 실습 예제
# - Pivot table
# - 성별
## 성별 체크

ggplot(train, aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(stat='count')

table(train$Survived, train$Sex)

mosaicplot(~Sex+Survived, data = train, color = T)

summarise(group_by(train, Sex), mean=mean(Survived))


## Todo: Pclass
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(stat='count')

table(train$Survived, train$Pclass)

mosaicplot(~Pclass+Survived, data = train, color = T)

summarise(group_by(train, Pclass), mean=mean(Survived))


## Pivot table

table(train$Survived, train$Sex, train$Pclass)
mosaicplot(~Sex+Pclass+Survived, data = train, color = T)
summarise(group_by(train, Sex, Pclass), mean=mean(Survived))


## Age
train$Age

histogram(train$Age)


## Child < 14? 
## Todo: ifelse, < 14
train$Child <- ifelse(train$Age < 14, "child", "adult")


## Another Way
train$Child[train$Age < 14] <- "child"
train$Child[train$Age >= 14] <- "adult"


ggplot(train, aes(x = Child, fill = factor(Survived))) + 
  geom_bar(stat='count')

table(train$Survived, train$Child)

mosaicplot(~Child+Survived, data = train, color = T)

summarise(group_by(train, Child), mean=mean(Survived))

## graph
ggplot(train, aes(Age, fill=factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex)

## Fill NA?
is.na(train$Age)

sum(is.na(train$Age))

mean(train$Age, na.rm = T)

train$Age[is.na(train$Age)] <- mean(train$Age)


## test data
test <- read.csv("./titanic/test.csv", stringsAsFactors = F)
View(test)

## Todo: Female All Survived?
test$Survived <- ifelse(test$Sex == "female", 1, 0)

View(test)

submission <- test[, c("PassengerId", "Survived")]
View(submission)

write.csv(submission, "./titanic/submission.csv", row.names = FALSE)



## Do your Self! Part 1


## Todo: 나이를 15살 단위로 세분화해보자. train$AgeRange c(0, 1, 2, 3, 4, 5)
## Todo: 거기에 따른 생존율을 분석해보자. (히스토그램, 모자이크플롯, summarise 이용, 앞에서 분석한 값들과 연계해서 피벗값을 구해보기)

train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)

max(train$Age)

train$AgeRange[train$Age <= 15] <- 0
train$AgeRange[train$Age > 15 & train$Age <= 30] <- 1
train$AgeRange[train$Age > 30 & train$Age <= 45] <- 2
train$AgeRange[train$Age > 45 & train$Age <= 60] <- 3
train$AgeRange[train$Age > 60 & train$Age <= 75] <- 4
train$AgeRange[train$Age > 75 & train$Age <= 95] <- 5


## Todo: Family Size 를 구해보자. (SibSp + Parch + 1) train$Fsize

train$Fsize <- train$SibSp + train$Parch + 1


## Todo: Family Size 가 1이면 'singleton', 1~5 사이면 'small', 5 보다 크면 'large'로 구분해보자. train$FsizeD
## Todo: 거기에 따른 생존율을 분석해보자. (히스토그램, 모자이크플롯, summarise 이용, 앞에서 분석한 값들과 연계해서 피벗값을 구해보기)


train$FsizeD[train$Fsize == 1] <- 'singleton'
train$FsizeD[train$Fsize > 1 & train$Fsize < 5] <- 'small'
train$FsizeD[train$Fsize >= 5] <- 'large'


ggplot(train, aes(FsizeD, fill=factor(Survived))) +
  geom_bar(stat = 'count')


mosaicplot(~FsizeD+Survived, data = train, color = T)

summarise(group_by(train, FsizeD), mean=mean(Survived))



## Do your Self! Part 2

## Todo: Fare를 이용하여 생존율을 분석해보자.
ggplot(train, aes(Fare, fill=factor(Survived))) +
  geom_density(alpha = 0.4)

## Todo: Embarked를 이용하여 생존율을 분석해보자.

## Todo: SibSp를 이용하여 생존율을 분석해보자.

## Todo: Parch를 이용하여 생존율을 분석해보자.

## Todo: Name, Ticket을 분석해보자.

## Todo: 앞에서 분석한 내용+ 새로운 내용을 활용하여 titanic Competition 에 제출해보자.








