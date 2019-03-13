## Script 창으로 들어가는 키 숏컷: Ctrl + 1
## Console 창 들어가는 키 숏컷: Ctrl + 2
## Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
## Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
## Console 클리닝: Ctrl + L
## 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")
## R version check: Sys.getenv("R_ARCH") - 32 bit인 분은 R을 다시 인스톨해주시기 바랍니다
## 64 bit for "/x64"
## 32 bit for "/i386"
## 각 라인마다 Ctrl(Cmd) + Enter


#######################################################
####### Section 0. R 적응하기 - 기초 예제 실습 ########

setwd("~/work/statistics-R/")

racers <- c("Alonso", "Schumacher", "Hamilton", "Vettel", "Ricciardo")

worldChampionRacers <- racers[racers != "Ricciardo"]
activeRaceres <- racers[racers != "Schumacher"]

racers <- c(racers, 'Max')

racersNames <- racers
racersAges <- c(39, 51, 36, 34, 32, 23)

length(racersNames)
length(racersAges)

f1 <- data.frame(Name = racersNames, Age = racersAges)
View(f1)

f1$Name
f1$Age

f1$worldChampionWins <- c(2,7,5,4,0,0)
f1$childAge <- c(14,27,9,12,5,2)

f1$fatherhoodAge <- f1$Age - f1$childAge

View(f1)

birth_day <- c('1981-07-29', '1969-01-03', '1985-01-07', '1987-07-03', '1989-07-01', '1997-09-30')
birth_day[1] - birth_day[6] # Error

birth_day <- as.Date(c('1981-07-29', '1969-01-03', '1985-01-07', '1987-07-03', '1989-07-01', '1997-09-30'))
birth_day[1] - birth_day[6]

f1$birthDay <- birth_day

age_diff <- Sys.Date() - birth_day

# 그룹화 변수(grouping variable)/요인(factor)/부호화 변수(coding variable)

job <- c(1,1,1,1,1,2,2,2,2,2)
job <- c(rep(1, 5), rep(2, 5))

job <- factor(job, levels = c(1:2), labels = c("Racer", "Engineer"))
job

job <- gl(2, 5, labels = c("Racer", "Engineer"))
job

levels(job)

levels(job) <- c("F1 Racer", "F1 Engineer")

job

names <- c("Alonso", "Schumacher", "Hamilton", "Vettel", "Ricciardo", "Tony", "Ryan", "Cooper", "Bennet", "Jaewoo")
friends <- c(10, NA, 25, 12, 23, 2, 3, 6, 10, NA)
income <- c(2200, 2500, 3100, 3000, 850, 30, 20, 15, 20, 100)

f1Data <- data.frame(names, job, friends, income)
View(f1Data)

mean(f1Data$friends)
mean(f1Data$friends, na.rm = TRUE)

write.csv(f1Data, "f1Data.csv", row.names = FALSE)

titanic <- read.csv("./data/titanic.csv", header = TRUE, row.names = 1)
View(titanic)

summary(titanic$Survived)
summary(titanic$Sex)
summary(titanic)
str(titanic)

##### dataframe[ (rows..) , (columns..)]

titanicCore <- titanic[, c("Survived", "Pclass", "Sex", "Age")]
summary(titanicCore)

titanicFemale <- titanic[titanic$Sex=="female", c("Survived", "Pclass", "Sex")]
View(titanicFemale)
mean(titanicFemale$Survived)

titanicOlder <- titanic[titanic$Age < 20, ]
View(titanicOlder)

is.na(titanic$Age)
sum(is.na(titanic$Age))

as.integer(mean(titanic$Age, na.rm = TRUE))

titanic$Age[is.na(titanic$Age)] <- as.integer(mean(titanic$Age, na.rm = TRUE))
titanic$Age[is.na(titanic$Age)]

titanicYounger <- titanic[titanic$Age < 10, c("Survived", "Pclass", "Sex")]
View(titanicYounger)

mean(titanicYounger$Survived)

femaleOrYoung <- titanic[titanic$Sex == "female" | titanic$Age < 10, c("Survived", "Pclass", "Sex", "Age")]
View(femaleOrYoung)

mean(femaleOrYoung$Survived)

# subset
titanicOlder <- subset(titanic, titanic$Age > 40, select = c("Survived", "Pclass", "Sex", "Age"))

mean(titanicOlder$Survived)








