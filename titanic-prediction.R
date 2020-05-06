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

setwd("~/oldwork/statistics-R/")

####### 0. Load Dataset  #######

train <- read.csv("./titanic/train.csv", stringsAsFactors = FALSE)
View(train)

str(train)
summary(train)

test <- read.csv("./titanic/test.csv", stringsAsFactors = FALSE)
View(test)

str(test)
summary(test)


####### 1. EDA #######
ggplot(train, aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(stat='count')

ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(stat='count')

ggplot(train, aes(x = Embarked, fill = factor(Survived))) + 
  geom_bar(stat='count')


####### 2. Feature Engineering #######

### Name

name <- train$Name[1]
name

name2 <- strsplit(name, ", ")[[1]][2]
strsplit(name2, "[. ]")[[1]][1]


extract_title <- function(name){
  name2 <- strsplit(name, ", ")[[1]][2]
  return(strsplit(name2, "[. ]")[[1]][1])
}

train$Title <- sapply(train$Name, extract_title)
train$Title

table(train$Sex, train$Title)

rare <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the", "Dona")

train$Title[train$Title == 'Mlle'] <- 'Miss'
train$Title[train$Title == 'Ms'] <- 'Miss'
train$Title[train$Title == 'Mme'] <- 'Mrs'
train$Title[train$Title %in% rare] <- 'Rare'

table(train$Sex, train$Title)

## Todo: Test Data
test$Title <- sapply(test$Name, extract_title)
test$Title[test$Title == 'Mlle'] <- 'Miss'
test$Title[test$Title == 'Ms'] <- 'Miss'
test$Title[test$Title == 'Mme'] <- 'Mrs'
test$Title[test$Title %in% rare] <- 'Rare'

table(test$Sex, test$Title)

### Age
summary(train$Age)
sum(is.na(train$Age))

## Miss Mean Age
mean_miss_age <- mean(c(train$Age[train$Title == 'Miss'], test$Age[test$Title == 'Miss']), na.rm = T)

train$Age[train$Title == 'Miss' & is.na(train$Age)] <- mean_miss_age
test$Age[test$Title == 'Miss' & is.na(test$Age)] <- mean_miss_age


## Todo: Master, Mr, Mrs, Rare, mean age?
mean_master_age <- mean(c(train$Age[train$Title == 'Master'], test$Age[test$Title == 'Master']), na.rm = T)
train$Age[train$Title == 'Master' & is.na(train$Age)] <- mean_master_age
test$Age[test$Title == 'Master' & is.na(test$Age)] <- mean_master_age


mean_mr_age <- mean(c(train$Age[train$Title == 'Mr'], test$Age[test$Title == 'Mr']), na.rm = T)
train$Age[train$Title == 'Mr' & is.na(train$Age)] <- mean_master_age
test$Age[test$Title == 'Mr' & is.na(test$Age)] <- mean_master_age


mean_mrs_age <- mean(c(train$Age[train$Title == 'Mrs'], test$Age[test$Title == 'Mrs']), na.rm = T)
train$Age[train$Title == 'Mrs' & is.na(train$Age)] <- mean_master_age
test$Age[test$Title == 'Mrs' & is.na(test$Age)] <- mean_master_age


mean_rare_age <- mean(c(train$Age[train$Title == 'Rare'], test$Age[test$Title == 'Rare']), na.rm = T)
train$Age[train$Title == 'Rare' & is.na(train$Age)] <- mean_master_age
test$Age[test$Title == 'Rare' & is.na(test$Age)] <- mean_master_age

  
## Miss Fare
summary(test$Fare)

ggplot(train, aes(Fare, fill=factor(Survived))) +
  geom_density(alpha = 0.4)

mean(c(train$Fare, test$Fare), na.rm = T)
median(c(train$Fare, test$Fare), na.rm = T)

## Todo: Input Missing Fare Median Fare
test$Fare[is.na(test$Fare)] <- median(c(train$Fare, test$Fare), na.rm = T)

summary(test$Fare)

## Embarked
embark_fare <- train[train$PassengerId != 62 & train$PassengerId != 830 , ]

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot()


train[train$PassengerId == 62 | train$PassengerId == 830 , c('Pclass', 'Fare')]


train$Embarked[train$Embarked == ""] <- "C"

## Cabin
train$Cabin
unique(train$Cabin)

train$Cabin[train$Cabin == ""] <- "U" 

mean(train$Survived[train$Cabin == "U"])
mean(train$Survived[train$Cabin != "U"])

train$CabinS <- sapply(train$Cabin, function(cabin) substr(cabin, 1,1))

View(train)

table(train$Survived, train$CabinS)

## Todo: Test CabinS
test$Cabin[test$Cabin == ""] <- "U" 
test$CabinS <- sapply(test$Cabin, function(cabin) substr(cabin, 1,1))


## Factor transform
full <- rbind(train, test)
View(full)

str(full)

full$Pclass <- factor(full$Pclass)
full$Sex <- factor(full$Sex)
full$Embarked <- factor(full$Embarked)
full$CabinS <- factor(full$CabinS)
full$Title <- factor(full$Title)

train_set <- full[1:891, c("Survived","Pclass", "Sex", "Embarked", "CabinS", "Title", "Age", "Fare")]
test_set <- full[892:1309, c("Pclass", "Sex", "Embarked", "CabinS", "Title", "Age", "Fare")]

# train_set <- full[1:891, c("Survived","Pclass", "Sex", "Embarked", "Title", "Age", "Fare")]
# test_set <- full[892:1309, c("Pclass", "Sex", "Embarked", "Title", "Age", "Fare")]

####### 3. Predict #######
# Logit 돌려보기

logit_model <- glm(factor(Survived) ~., family=binomial(link='logit'), data=train_set)

# Logit predict
fitted.results <- predict(logit_model, test_set, type = 'response')
fitted.results

fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results

# Decision Tree

tree_model <- tree(factor(Survived) ~ ., data = train_set)

fitted.results <- predict(tree_model, test_set, type = "class")
fitted.results

####### 4. Submit #######
test$Survived <- fitted.results

test$Survived

submission <- test[, c('PassengerId', 'Survived')]

write.csv(submission, paste("./titanic/submission", Sys.time(),  ".csv", sep = ""), row.names = F)


###



