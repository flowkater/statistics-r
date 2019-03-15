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
####### Section 0. 통계 모형, 설명 따라가보기 ########
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("Hmisc")
install.packages("extrafont")
install.packages("reshape")
install.packages("dplyr")
# install.packages("truncnorm")

library(ggplot2)
library(gridExtra)
# library(truncnorm)

# setwd("~/work/statistics-R/")

### 도수 분포 (frequency distribution; 빈도 분포) / 히스토그램(histogram)
StudyTime <- read.csv("./data/study_time.csv", header = TRUE)
View(StudyTime)
histogram <- ggplot(StudyTime, aes(done_second))
histogram + geom_histogram(
                 binwidth = 1000,
                 colour="black", fill="blue", alpha = 0.5) + 
  geom_density(alpha=.2, fill="#6666FF") #+
  # geom_vline(xintercept = mean(StudyTime$done_second), colour = "Red")


ggplot(data = data.frame(x = rnorm(100, 50, 25)), aes(x)) +
  geom_histogram(aes(y=..density..),
                   binwidth = 5,
                   colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#6666FF")

### 정규 분포 (normal distribution)
ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 25))

positive_skewed <- ggplot(data = data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 25)) +
  coord_cartesian(ylim=c(0, 0.02)) +
  labs(y = "", title = "positive_skewed")

negative_skewed <- ggplot(data = data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 150, sd = 25)) +
  coord_cartesian(ylim=c(0, 0.02)) +
  labs(y = "", title = "egative_skewed")

leptokurtic <- ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 30)) +
  coord_cartesian(ylim=c(0, 0.03)) +
  labs(y = "", title = "leptokurtic")

platykurtic <- ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 15)) +
  coord_cartesian(ylim=c(0, 0.03)) +
  labs(y = "", title = "platykurtic")

grid.arrange(positive_skewed, negative_skewed, leptokurtic, platykurtic, nrow=2, ncol=2)

##### 이봉분포

# Creating data 
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))


##### 중심경향성(central tendency) - 도수 분포의 중심 

# mode 최빈값
names(which.max(table(c(1, 2, 2, 2, 3))))

produce101 <- read.csv("./data/produce101.csv", header = TRUE)
View(produce101)
str(produce101)

# 오름차순 정렬
sort(produce101$score)

mean(produce101$score)
median(produce101$score)
IQR(produce101$score)
min(produce101$score)
max(produce101$score)

summary(produce101$score)

par(mfrow=c(1,2))
hist(produce101$score)
boxplot(produce101$score)

var(produce101$score)
sd(produce101$score)

### 표준화(standardization)

test_score <- c(6, 10, 15, 18, 19, 22)
same_index <- c(1, 1, 1, 1, 1, 1)
par(mfrow=c(2,2))
plot(test_score, same_index, col = "blue", cex = 5, pch = 19)
mean(test_score)
sd(test_score)

test_score_10 = test_score + 10
mean(test_score_10)
sd(test_score_10)

plot(test_score_10, same_index, col = "blue", cex = 5, pch = 19)

test_score_x2 = test_score * 2
test_score_x2
mean(test_score_x2)
sd(test_score_x2)

plot(test_score_x2, same_index, col = "blue", cex = 5, pch = 19)

## Centering
test_score_mean = test_score - mean(test_score)
test_score_mean
mean(test_score_mean)
sd(test_score_mean)

plot(test_score_mean, same_index, col = "blue", cex = 5, pch = 19)

## Scaling
test_score_mean_sd = test_score_mean / sd(test_score)
test_score_mean_sd
mean(test_score_mean_sd)
sd(test_score_mean_sd)

plot(test_score_mean_sd, same_index, col = "blue", cex = 5, pch = 19)

## 수능 수능 수리가형 점수
## 2011년 불수능 평균 = 47.8, 표준편차 = 19.7
## 2015년 물수능 평균 = 55.4, 표준편차 = 28.5
## 80 점을 받은 A 학생, 100점을 B 학생 승자는?
A_standard = (80 - 47.8) / 19.7
A_standard
B_standard = (100 - 55.4) / 28.5
B_standard

A_standard > B_standard

koreanSat <- read.csv("./data/korean_sat.csv", header = TRUE)

koreanSat$year <- factor(koreanSat$year, levels = c(2011, 2015), labels = c("2011", "2015"))

ggplot(koreanSat, aes(x = score, fill = year,colour = year)) + 
  geom_histogram(alpha = 0.4, position="dodge") +
  ggtitle("불수능 2011년 vs 물수능 2015년")

# ggplot(koreanSat, aes(x = score, fill = year,colour = year)) + 
#   geom_histogram(alpha = 0.4, position="identity")

###### 단순한 통계적 모형

user_ids <- c(1, 2, 3, 4, 5)
friends <- c(1, 2, 3, 3, 4)

snsData <- data.frame(Id = user_ids, FriendNumber = friends) 
mean(snsData$FriendNumber)

deviance <- ggplot(snsData, aes(snsData$Id, snsData$FriendNumber))
deviance + geom_point(colour = "Blue", size = 5) + 
  geom_hline(yintercept = mean(snsData$FriendNumber)) +
  geom_segment(aes(xend = snsData$Id, yend = mean(snsData$FriendNumber), alpha = 0.5, colour = "Red", size = 0.5)) +
  labs(x = "User Id", y = "# of Friends", title = paste("sd: ", sd(snsData$FriendNumber))) + 
  theme(legend.position = "none")


errors = snsData$FriendNumber - mean(snsData$FriendNumber)
errors

round(sum(errors))
var(snsData$FriendNumber)
sd(snsData$FriendNumber)

# 표준편차에 따른 편차 직선

friends2 <- c(2, 3, 3, 3, 2)
friends3 <- c(4, 1, 5, 1, 2)

snsData2 <- data.frame(Id = user_ids, FriendNumber = friends2) 
mean(snsData2$FriendNumber)

snsData3 <- data.frame(Id = user_ids, FriendNumber = friends3) 
mean(snsData3$FriendNumber)

deviance2 <- ggplot(snsData2, aes(snsData2$Id, snsData2$FriendNumber)) + 
  geom_point(colour = "Blue", size = 5) + 
  geom_hline(yintercept = mean(snsData2$FriendNumber)) +
  geom_segment(aes(xend = snsData2$Id, yend = mean(snsData2$FriendNumber), alpha = 0.5, colour = "Red", size = 0.5)) +
  labs(x = "User Id", y = "# of Friends", title = paste("sd: ", sd(snsData2$FriendNumber))) + 
  coord_cartesian(ylim=c(0, 5)) +
  theme(legend.position = "none")

deviance3 <- ggplot(snsData3, aes(snsData3$Id, snsData3$FriendNumber)) + 
  geom_point(colour = "Blue", size = 5) + 
  geom_hline(yintercept = mean(snsData3$FriendNumber)) +
  geom_segment(aes(xend = snsData3$Id, yend = mean(snsData3$FriendNumber), alpha = 0.5, colour = "Red", size = 0.5)) +
  labs(x = "User Id", y = "# of Friends", title = paste("sd: ", sd(snsData3$FriendNumber))) + 
  coord_cartesian(ylim=c(0, 5)) +
  theme(legend.position = "none")

grid.arrange(deviance2, deviance3, nrow=1, ncol=2)


# 분산에 따른 정규분포도
p1 <- ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 35)) +
  labs(y = "", title = "sd: 35") +
  coord_cartesian(ylim=c(0, 0.03))


p2 <- ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 50, sd = 15)) +
  labs(y = "", title = "sd: 15") +
  coord_cartesian(ylim=c(0, 0.03))


grid.arrange(p1, p2, nrow=1, ncol=2)


# 표준정규분포 
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  ggtitle("Normal Distribution")

dnorm_range <- function(x) {
  y <- dnorm(x) 
  y[x < -1.96 | x > 1.96] <- NA  # 이 범위에는 색깔 없음
  return(y)
}

ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range, geom = "area", fill="grey", alpha = 0.5) +
  ggtitle("Normal Distribution")





