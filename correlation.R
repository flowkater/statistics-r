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
####### Section 5. 상관계수(Correlation) ########

# install.packages("Hmisc")
# install.packages("ggm")
# install.packages("polycor")

library(boot)
library(ggm)
library(ggplot2)
library(gridExtra)
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

## scatterplot 산점도

# AE - Anxiety, Exam
AE.base <- ggplot(examData, aes(examData$Anxiety, examData$Exam)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Blue") + 
  labs(x = "시험 불안", y = "시험 성적 %")

AE.group <- ggplot(examData, aes(examData$Anxiety, examData$Exam, colour = examData$Gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = examData$Gender), se = F) + 
  labs(x = "시험 불안", y = "시험 성적 %", colur = examData$Gender) # + theme(legend.position = "none")

grid.arrange(AE.base, AE.group, nrow = 2, ncol = 1)

## TODO: 복습 시간과 시험 성적에 대한 산점도와 gender에 따른 그룹별 산점도를 그려보시오.
# RE - Revise, Exam

grid.arrange(RE.base, RE.group, nrow = 2, ncol = 1)

## TODO: 복습 시간과 시험 불안도에 대한 산점도와 gender에 따른 그룹별 산점도를 그려보시오.
# RA - Revise, Anxiety
RA.base <- ggplot(examData, aes(Revise, Anxiety)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Blue") + 
  labs(x = "복습 시간", y = "시험 불안")
RA.group <- ggplot(examData, aes(Revise, Anxiety, colour = Gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = examData$Gender), se = F) + 
  labs(x = "복습 시간", y = "시험 불안", colur = examData$Gender)

grid.arrange(RA.base, RA.group, nrow = 2, ncol = 1)

## 전체 그래프
grid.arrange(AE.base, RE.base, RA.base,
             AE.group, RE.group, RA.group,
             nrow = 2, ncol = 3)

# covariance 공분산
examData2 <- examData[, c("Revise", "Exam", "Anxiety")]


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

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

## kendall's tau
cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

## bootstrap
bootTau <- function(liarData, i) {cor(liarData$Position[i], liarData$Creativity[i], use ="complete.obs", method = "kendall")}

boot_kendall <- boot(liarData, bootTau, 2000)
boot_kendall

boot.ci(boot_kendall)

## TODO: examData2 데이터프레임의 피어슨 상관과 스피어먼 상관에 대해 부트스트랩 분석을 수행하라.

######################################################
### pbcorr.csv

# time: 한 주(week)에서 고양이가 집을 떠나서 보낸 시간, 단위는 시간(hour)
# gender: 고양이의 성별, 수컷은 1, 암컷은 0으로 부호화
# recode: gender 를 반대로 부호화한 것
######################################################

catData <- read.csv("./data/pbcorr.csv", header = TRUE)
View(catData)
str(catData)

## TODO: time 과 gender 를 피어슨 상관분석을 수행하라 cor.test()
## 점이연(point-biserial) 상관계수
cor.test(catData$time, catData$gender)
r_pb <- cor(catData$time, catData$gender); r_pb
R2_pb <- r_pb^2; R2_pb

cor.test(catData$time, catData$recode)

## 이연(biserial) 상관계수
r_b_from_pb <- function(r_pb, p, q, y){
  return( (r_pb*sqrt(p*q)) / y )
}

catFrequencies <- table(catData$gender); catFrequencies
p <- prop.table(catFrequencies)[1];p
q <- prop.table(catFrequencies)[2];q


## y value 
qnorm(p)
x <- qnorm(p)
# 1- qnorm(q)
y <- 1/sqrt(2*pi)*exp(-x^2/2); y
y <- dnorm(qnorm(p))

r_b <- r_b_from_pb(r_pb, p, q, y) # 이연 상관계수

polyserial(catData$time, catData$gender)

## 이연 상관계수 표준오차
SE_r_b <- function(p, q, y, N) {
  return(sqrt(p * q) / (y * sqrt(N)))
}

N <- length(catData$time)

## 이연 상관계수 z값 및 유의확률 구하기
z_r_b <- (r_b - 0) / SE_r_b(p, q, y, N);z_r_b
(1 - pnorm(z_r_b)) < 0.05
(1 - pnorm(z_r_b)) < 0.01

## 편상관
cor(examData2)
cor(examData2)^2

cor(examData2$Exam, examData2$Anxiety)
cor(examData2$Exam, examData2$Anxiety)^2
pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2));pc
pc^2

# 편상관 유의성 검정
pcor.test(pc, 1, 103)


## 독립적인 r들의 비교 
## TODO: subset() 함수를 이용해서 examData 를 남성과 여성의 시험불안과 시험 성적 사이의 상관계수들을 구하라.
mexamData <- subset(examData[, c("Exam", "Anxiety", "Revise","Gender")], examData$Gender == "Male")
str(mexamData)
fexamData <- subset(examData[, c("Exam", "Anxiety", "Revise","Gender")], examData$Gender == "Female")
str(fexamData)

r1 <- cor(mexamData$Exam, mexamData$Anxiety); r1
r2 <- cor(fexamData$Exam, fexamData$Anxiety); r2


## z_r
z_r <- function(r) {
  return((1/2)*log((1+r)/(1-r)))
}

z_r(r1)
z_r(r2)

## z 차이
z_diff <- function(z_r1, z_r2, N1, N2) {
  return((z_r1 - z_r2) / sqrt( (1/(N1 - 3)) + (1/(N2 - 3)) ))
}

length(mexamData$Exam)
length(fexamData$Exam)

zd <- z_diff(z_r(r1), z_r(r2), 52, 51);zd

pnorm(zd)
pnorm(zd) * 2 


## 종속적인 r들의 비교
# 시험 불안과 시험 성적의 상관계수(r_xy)와 복습 시간과 시험 성적의 상관계수(r_zy)에 유의한 차이가 있는가?

r_xy <- cor(examData$Anxiety, examData$Exam); r_xy # 시험 불안과 시험 성적

r_zy <- cor(examData$Revise, examData$Exam); r_zy # 복습 시간과 시험 성적

r_xz <- cor(examData$Anxiety, examData$Revise); r_xz # 시험 불안과 복습 시간

t_diff <- function(r_xy, r_zy, r_xz, n) {
  return( (r_xy - r_zy) * sqrt( ((n - 3)*(1 + r_xz)) / (2 * (1 - r_xy^2 - r_xz^2 - r_zy^2 + (2 * r_xy * r_xz * r_zy )) )) )
}

length(examData$Anxiety)

td <- t_diff(r_xy, r_zy, r_xz, 103); td
qt(.025, 100) # p < .05
qt(.005, 100) # p < .01

pt(td, 100)

## Heatmap
cormat <- round(cor(examData2), 2);cormat

library(reshape2)

melted_cormat <- melt(cormat)
View(melted_cormat)

heatmap.basic <- ggplot(melted_cormat, aes(x = Var1, y = Var2, fill=value)) + geom_tile()
heatmap.basic

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat);lower_tri

melted_cormat <- melt(lower_tri, na.rm = TRUE)

heatmap.lower_tri <- ggplot(melted_cormat, aes(Var1,Var2, fill=value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()
  
heatmap.lower_tri


heatmap.text <- ggplot(melted_cormat, aes(Var1,Var2, fill=value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + 
  geom_text(aes(Var1,Var2, label=value), colour = "black",size = 4)
  
heatmap.text

## R 내장 mtcars 데이터
mycars <- mtcars[, c(1,3,4,5,6,7)]

melted_cardata <- melt(get_lower_tri(round(cor(mycars), 2)), na.rm = TRUE);melted_cardata

heatmap.mycars <- ggplot(melted_cardata, aes(Var1,Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + 
  geom_text(aes(Var1,Var2, label=value), colour = "black",size = 4)

heatmap.mycars

