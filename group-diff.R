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
####### Section 8. 두 그룹의 차이 ########
install.packages("WRS2")

library(ggplot2)
library(reshape2)
library(gridExtra)
library(pastecs)
library(WRS2)

setwd("~/oldwork/statistics-R/")

#######################################################
### SpiderLong.csv

# Group: 그룹 (Picture, Real Spider)
# Anxiety: 불안

#######################################################

spiderLong <- read.csv("./data/SpiderLong.csv", header = T)
str(spiderLong)
summary(spiderLong)
head(spiderLong)
View(spiderLong)

spiderLong.bar <- ggplot(spiderLong, aes(Group, Anxiety)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", colour = "Red") +
  coord_cartesian(ylim=c(0, 70))
spiderLong.bar

spiderLong.boxplot <- ggplot(spiderLong, aes(Group, Anxiety)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0, 70))
spiderLong.boxplot

grid.arrange(spiderLong.bar, spiderLong.boxplot, nrow = 1, ncol = 2)

by(spiderLong$Anxiety, spiderLong$Group, stat.desc, basic = F, norm = T)

#######################################################
### SpiderWide.csv

# picture: 사진으로 본 그룹
# real: 실제로 본 그룹

#######################################################

spiderWide <- read.csv("./data/SpiderWide.csv", header = T)
str(spiderWide)
summary(spiderWide)
View(spiderWide)

#######################################################
### Independent t test

#######################################################
ind.t.test <- t.test(Anxiety ~ Group, data = spiderLong)
ind.t.test

ind.t.test <- t.test(spiderWide$picture, spiderWide$real)
ind.t.test

# mean(spiderWide$picture) - mean(spiderWide$real)

### Robust independence t test
yuen(Anxiety ~ Group, data = spiderLong)
yuen(Anxiety ~ Group, data = spiderLong, tr = .1)

yuenbt(Anxiety ~ Group, data = spiderLong)

pb2gen(Anxiety ~ Group, data = spiderLong)

### Effect Size
t <- ind.t.test$statistic[[1]]; t
df <- ind.t.test$parameter[[1]]; df

r <- sqrt(t^2 / (t^2 + df));r
round(r, 3)


#######################################################
### Paired-sample t test

#######################################################

spiderWide <- read.csv("./data/SpiderWide.csv", header = T)
spiderLong <- read.csv("./data/spiderLong.csv", header = T)

str(spiderWide)
summary(spiderWide)


stat.desc(spiderWide, basic = F, norm = T)

## TODO: spiderWide 데이터의 정규성 가정을 점검하라. (첨도, 왜도, 히스토그램 등으로 시각화)


dep.t.test <- t.test(spiderWide$real, spiderWide$picture, paired = T)
dep.t.test

### Robust dependence t test
yuend(spiderWide$real, spiderWide$picture)

### Effect Size
t <- dep.t.test$statistic[[1]]; t
df <- dep.t.test$parameter[[1]]; df

r <- sqrt(t^2 / (t^2 + df));r
round(r, 3)


######## 반복측정 설계시 오차막대 그래프의 문제 #########

melted_spiderWide <- melt(spiderWide)
names(melted_spiderWide) <- c("Group", "Anxiety")
head(melted_spiderWide)


spiderWide.bar  <- ggplot(melted_spiderWide, aes(Group, Anxiety)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", colour = "Red") + 
  coord_cartesian(ylim=c(0, 70))
spiderWide.bar

grid.arrange(spiderLong.bar, spiderWide.bar, nrow=1, ncol=2)

## problem spiderWide
# 각 개인의 picture + real 의 평균값 계산
spiderWide$pMean <- (spiderWide$picture + spiderWide$real) / 2;spiderWide$pMean

# 모든 자료의 평균값 계산
grandMean <- mean(c(spiderWide$picture, spiderWide$real));grandMean

# 모든 자료의 평균값 - 각 개인의 평균값 = 조정 인자 (설명되지 않는 변동, 개인차)
spiderWide$adj <- grandMean - spiderWide$pMean;spiderWide$adj

# 각 관측치 + 조정인자
spiderWide$picture_adj <- spiderWide$picture + spiderWide$adj
spiderWide$real_adj <- spiderWide$real + spiderWide$adj
spiderWide$pMean2 <- (spiderWide$picture_adj + spiderWide$real_adj) / 2;spiderWide$pMean2
head(spiderWide)

View(spiderWide)

spiderWideAdj <- spiderWide[, c("picture_adj", "real_adj")]
head(spiderWideAdj)

melted_spiderWideAdj <- melt(spiderWideAdj)
names(melted_spiderWideAdj) <- c("Group", "Anxiety")
head(melted_spiderWideAdj)

spiderWideAdj.bar  <- ggplot(melted_spiderWideAdj, aes(Group, Anxiety)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", colour = "Blue") +
  coord_cartesian(ylim=c(0, 70))
spiderWideAdj.bar

grid.arrange(spiderLong.bar, spiderWideAdj.bar, nrow=1, ncol=2)