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
####### Section 9. 분산분석(ANOVA) ########

# install.packages("compute.es")
# install.packages("multcomp")
# install.packages("pastecs")
# install.packages("WRS2")

library(ggplot2)
library(reshape2)
library(gridExtra)
library(compute.es)
library(multcomp)
library(pastecs)
library(WRS2)

# setwd("~/work/statistics-R/")
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")

#######################################################
### Marketing Revenue.csv

# SMS: 문자 메시지로 구매한 횟수
# Push: 모바일 앱 내의 푸시 메시지로 구매한 횟수
# Banner: 모바일 앱 내의 배너 광고로 구매한 횟수
# Email: 이메일로 구매한 횟수
# Blog: 서비스 운영 블로그 포스팅 링크로 구매한 횟수
# Facebook: 페이스북 paid 광고로 구매한 횟수

#######################################################

marketing <- read.csv("./data/Marketing Revenue.csv", header = T)
str(marketing)
View(marketing)

boxplot(marketing)

total_array <- c(marketing$SMS, marketing$Push, marketing$Banner, marketing$Email, marketing$Blog, marketing$Facebook);total_array

total_mean <- mean(total_array);total_mean

marketing$meanSMS <- mean(marketing$SMS)
marketing$meanPush <- mean(marketing$Push)
marketing$meanBanner <- mean(marketing$Banner)
marketing$meanEmail <- mean(marketing$Email)
marketing$meanBlog <- mean(marketing$Blog)
marketing$meanFacebook <- mean(marketing$Facebook)

head(marketing)
View(marketing)

## 오차 제곱합 (관측치 - 전체 평균)^2
SST <- sum((total_array - total_mean)^2);SST

## 그룹차에 대한 오차제곱합 (각 그룹 평균치 - 전체 평균)^2
SSM <- sum((marketing$meanSMS - total_mean)^2) +
  sum((marketing$meanPush - total_mean)^2) +
  sum((marketing$meanBanner - total_mean)^2) +
  sum((marketing$meanEmail - total_mean)^2) +
  sum((marketing$meanBlog - total_mean)^2) +
  sum((marketing$meanFacebook - total_mean)^2);SSM

## 그룹과 개인차에 대한 오차제곱합 (관측치 - 각 그룹 평균치)^2
SSR <- sum((marketing$SMS - marketing$meanSMS)^2) +
  sum((marketing$Push - marketing$meanPush)^2) +
  sum((marketing$Banner - marketing$meanBanner)^2) +
  sum((marketing$Email - marketing$meanEmail)^2) +
  sum((marketing$Blog - marketing$meanBlog)^2) +
  sum((marketing$Facebook - marketing$meanFacebook)^2);SSR

SST
SSM + SSR
SST == SSM + SSR

marketing <- read.csv("./data/Marketing Revenue.csv", header = T)
melted_marketing <- melt(marketing)
names(melted_marketing) <- c("channel", "count")

melted_marketing$id <- as.numeric(row.names(melted_marketing))
View(melted_marketing)

melted_marketing$mean <- 0

for(channel in levels(melted_marketing$channel)){
  melted_marketing[melted_marketing$channel == channel, ]$mean <- mean(melted_marketing[melted_marketing$channel == channel, ]$count)
}

View(melted_marketing)

head(melted_marketing, 20)


marketing.SST <- ggplot(melted_marketing, aes(id, count, colour = melted_marketing$channel)) + 
  geom_point() + 
  geom_hline(yintercept = mean(melted_marketing$count)) +
  geom_segment(aes(xend = melted_marketing$id, yend = mean(melted_marketing$count), alpha = 0.5))
marketing.SST

marketing.SSR <- ggplot(melted_marketing, aes(id, count, colour = channel)) + 
  geom_segment(aes(xend = melted_marketing$id, yend = melted_marketing$mean, alpha = 0.5)) +
  geom_point() + 
  geom_hline(yintercept = mean(melted_marketing$count)) +
  geom_line(aes(y = melted_marketing$mean, col = melted_marketing$channel))
marketing.SSR

marketing.SSM <- ggplot(melted_marketing, aes(id, count, colour = channel)) + 
  geom_segment(aes(y = melted_marketing$mean, xend = melted_marketing$id, yend = mean(melted_marketing$count), alpha = 0.5)) +
  geom_point() + 
  geom_hline(yintercept = mean(melted_marketing$count)) +
  geom_line(aes(y = melted_marketing$mean, col = melted_marketing$channel))
marketing.SSM

grid.arrange(marketing.SST+ theme(legend.position = "none"), marketing.SSR+ theme(legend.position = "none"), 
             marketing.SSM, nrow = 2, ncol = 2)

k <- length(levels(melted_marketing$channel));k
n <- length(melted_marketing$id);n

MSM <- SSM / (k-1);MSM
MSR <- SSR / (n-k);MSR

f_value <- MSM/MSR;f_value

qf(0.95, k-1, n-k)
pf(f_value, k-1, n-k, lower.tail = F)

###### F-distribution plot

x <- c(0:1000 / 100)
y <- df(x, k-1, n-k)

F_plot <- ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) +
  geom_line() +
  geom_vline(xintercept = qf(0.95, k-1, n-k)) +
  labs(title = "자유도 df1 = 5 (k-1), 자유도 df2 = 66 (n-k) F 분포",
       x = "F 값",
       y = "밀도");F_plot

F_plot + geom_vline(xintercept = f_value) + geom_vline(xintercept = qf(0.95, k-1, n-k))

######


#######################################################
### Exam Score by howto.csv

# line_repeat: 밑줄 그으면서 반복 읽기로 공부한 학생들의 시험 점수
# summary: 중요 내용을 요약정리하면서 공부한 학생들의 시험 점수
# test: 핵심 내용을 책을 안보고 직접 테스트를 하면서 공부한 학생들의 시험 점수

#######################################################

exam <- read.csv("./data/Exam Score by howto.csv", header = T)
View(exam)
str(exam)

# Todo: 데이터의 boxplot 을 그려보시오.

# Todo: 관측치의 전체 평균을 구하시오.

# Todo: 각 그룹의 평균값을 구하시오.

# Todo: SST, SSM, SSR 을 각각 계산하고 SST == SSM + SSR => TRUE 인지 테스트해보시오.

# Todo: MSM, MSR 을 구하고 F-비를 구하시오.

# Todo: F-비가 유의한지 p-value 로 보이시오.

# Todo: aov 를 통해 계산되는 모델과 동일한 결과가 나오는지 확인하시오.

# Todo: melt 를 이용하여 데이터를 녹여 ggplot 그래프로 SST, SSR, SSM 그래프를 그려보시오.


### aov

str(melted_marketing)
marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary(marketing.aov)

# 등분산성이 깨질때, Robust F-statistics 

marketing.oneway <- oneway.test(count ~ channel, data = melted_marketing)
marketing.oneway


# WRS2, Wilcox Robust ANOVA
t1way(count ~ channel, data = melted_marketing)

med1way(count ~ channel, data = melted_marketing)

t1waybt(count ~ channel, data = melted_marketing)

View(melted_marketing)

# 사후 검정(post hoc test)
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "bonferroni")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "holm")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "hochberg")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "hommel")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "BH")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "BY")
pairwise.t.test(melted_marketing$count, melted_marketing$channel, p.adjust.method = "fdr")

# Tukey
postHocs <- glht(marketing.aov, linfct = mcp(channel = "Tukey"))
summary(postHocs)
confint(postHocs)

# Dunnet
postHocs <- glht(marketing.aov, linfct = mcp(channel = "Dunnet"), base = 1)
summary(postHocs)
confint(postHocs)

# Robust 사후 검정
lincon(count ~ channel, melted_marketing)
mcppb20(count ~ channel, melted_marketing)


# w^2 (오메가 제곱) 전반적인 효과크기
w_squared <- function(ssm, sst, dfm, msr) {
  return((ssm - (dfm*msr)) / (sst + msr))
}

SSM
SST
SSR
MSR
dfm <- k - 1

summary.lm(marketing.aov)
w_squared(SSM, SST, dfm, MSR)

# 그룹들 차이에 대한 효과크기 compute.es
mean(marketing$Facebook)
sd(marketing$Facebook)
length(marketing$Facebook)

mean(marketing$Banner)
sd(marketing$Banner)
length(marketing$Banner)

mes(mean(marketing$Facebook), mean(marketing$Banner), sd(marketing$Facebook), sd(marketing$Banner), length(marketing$Facebook), length(marketing$Banner))

