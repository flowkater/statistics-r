## Script 창으로 들어가는 키 숏컷: Ctrl + 1
## Console 창 들어가는 키 숏컷: Ctrl + 2
## Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
## Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
## Console 클리닝: Ctrl + L
## 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
## Mac for Korean / Mac 에서 한글 깨질때,
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")
## R version check: Sys.getenv("R_ARCH") - 32 bit인 분은 R을 다시 인스톨해주시기 바랍니다
## 64 bit for "/x64"
## 32 bit for "/i386"
## 각 라인마다 Ctrl(Cmd) + Enter


#######################################################
####### Section 4. 정규성 검정 ########
install.packages("car")
install.packages("pastecs")
install.packages("psych")
install.packages("SuppDists")


library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(gridExtra)
library(truncnorm)
library(SuppDists)

setwd("~/work/statistics-R/")

######################################################
### festivalDataNoOutlier(이상치 제거 버전)

# ticknumb: 티켓넘버
# gender: 성별
# day1: 축제 첫째날 위생(hygiene) 상태
# day2: 축제 둘째날 위생(hygiene) 상태
# day3: 축제 셋째날 위생(hygiene) 상태
# 위생 점수는 0~4

######################################################

dlf <- read.csv("./data/festivalDataNoOutlier.csv", header = TRUE)
View(dlf)

hist.day1 <- ggplot(dlf, aes(day1)) +
  theme(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "day 1 위생 상태 점수", y = "밀도") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(dlf$day1, na.rm = TRUE), 
                            sd = sd(dlf$day1, na.rm = TRUE)), 
                colour = "black", 
                size = 1)

qqplot.day1 <- ggplot(dlf, aes(sample = dlf$day1)) + 
  stat_qq() + stat_qq_line(colour = "Red")
qqplot.day1

## TODO: Day2, Day3 Histogram + Q-Q Plot 을 작성해보세요.

####


grid.arrange(hist.day1, qqplot.day1,hist.day2, qqplot.day2,hist.day3, qqplot.day3, nrow=3, ncol=2)



### psych
describe(dlf[,c('day1', 'day2', 'day3')])
stat.desc(dlf[,c('day1', 'day2', 'day3')], basic = FALSE, norm = TRUE)
round(stat.desc(dlf[,c('day1', 'day2', 'day3')], basic = FALSE, norm = TRUE), digits = 3)


######################################################
### Rxam (시험 점수)

# exam: R 시험 점수 (%)
# computer: 컴퓨터 사용 능력 점수 (%)
# lecture: R 강의 출석률 (%)
# numeracy: 수치를 다루는 능력 (15점 만점)
# uni: 0이면 국내대학생(domestic), 1이면 국제대학생(international)

######################################################

rexam <- read.csv("./data/Rxam.csv", header = TRUE)
View(rexam)
str(rexam)
summary(rexam)

## uni -> factor
rexam$uni <- factor(rexam$uni, levels = c(0:1), labels = c("domestic", "international"))
str(rexam)

#### TODO
### psych::stat.desc 를 이용하여 exam, computer, lectures, numeracy 에 대한 서술적 통계량들을 출력하시오. (round(, digits = 3) 를 쓰세요.)
### 비대칭도(skewness), 첨도(kurtosis), 앞서 배웠던 skew.2SE, kurt.2SE 를 이용하여 통계량을 해석해보아라


### TODO: exam, computer, lecture, numeracy 에 대한 히스토그램을 출력하시오.
#### y축은 밀도로 테두리는 검은색, 색깔은 흰색으로 채움
#### 해당 데이터의 평균과 분산을 이용하여 정규분포 그래프 추가


###
grid.arrange(hist.exam, hist.computer, hist.lectures, hist.numeracy, nrow=2, ncol=2)

### TODO: exam, computer, lecture, numeracy 에 대한 Q-Q plot을 출력하시오.



###
grid.arrange(qqplot.exam, qqplot.computer, qqplot.lectures, qqplot.numeracy, nrow=2, ncol=2)


### by 그룹별 분석
by(data = rexam$exam, INDICES = rexam$uni, FUN = stat.desc, basic = FALSE, norm = TRUE)
by(data = rexam[, c("exam", "numeracy")], INDICES = rexam$uni, FUN = stat.desc, basic = FALSE, norm = TRUE)
by(data = rexam[, c("exam", "computer","lectures","numeracy")], INDICES = rexam$uni, FUN = stat.desc, basic = FALSE, norm = TRUE)

domestic <- subset(rexam, rexam$uni == "domestic")
international <- subset(rexam, rexam$uni == "international")

## 그룹별로 히스토그램 살펴보기
hist.exam.domestic <- ggplot(domestic, aes(exam)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "R 시험 점수", y = "밀도", title = "국내") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(domestic$exam, na.rm = TRUE), 
                            sd = sd(domestic$exam, na.rm = TRUE)), 
                colour = "blue", 
                size = 1)

hist.exam.international <- ggplot(international, aes(exam)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "R 시험 점수", y = "밀도", title = "국제(해외)") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(international$exam, na.rm = TRUE), 
                            sd = sd(international$exam, na.rm = TRUE)), 
                colour = "blue", 
                size = 1)


hist.numeracy.domestic <- ggplot(domestic, aes(numeracy)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "수치 해석 능력", y = "밀도", title = "국내") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(domestic$numeracy, na.rm = TRUE), 
                            sd = sd(domestic$numeracy, na.rm = TRUE)), 
                colour = "blue", 
                size = 1)

hist.numeracy.international <- ggplot(international, aes(numeracy)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "수치 해석 능력", y = "밀도", title = "국제(해외)") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(international$numeracy, na.rm = TRUE), 
                            sd = sd(international$numeracy, na.rm = TRUE)), 
                colour = "blue", 
                size = 1)


###
grid.arrange(hist.exam.domestic, hist.exam.international, hist.numeracy.domestic, hist.numeracy.international, nrow=2, ncol=2)

## 그룹별로 히스토그램 살펴보기
qqplot.exam.domestic <- ggplot(domestic, aes(sample = domestic$exam)) + 
  stat_qq() + stat_qq_line(colour = "Red")

qqplot.exam.international <- ggplot(international, aes(sample = international$exam)) + 
  stat_qq() + stat_qq_line(colour = "Red")

qqplot.numeracy.domestic <- ggplot(domestic, aes(sample = domestic$numeracy)) + 
  stat_qq() + stat_qq_line(colour = "Red")

qqplot.numeracy.international <- ggplot(international, aes(sample = international$numeracy)) + 
  stat_qq() + stat_qq_line(colour = "Red")


###
grid.arrange(qqplot.exam, qqplot.exam.domestic, qqplot.exam.international, 
             qqplot.numeracy, qqplot.numeracy.domestic, qqplot.numeracy.international, nrow=2, ncol=3)


##### 기존의 데이터로 정규분포를 다시 만든다. rtruncnorm
##### 기존 데이터의 exam 과 정규분포로 만든 nexam
norm_exam <- rtruncnorm(n = length(rexam$exam),a = 0, b = 100, mean = mean(rexam$exam), sd = sd(rexam$exam))

nexam <- data.frame(x = norm_exam)

hist.norm_exam <- ggplot(nexam, aes(x)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "R 시험 점수", y = "밀도", title = "정규분포") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(nexam$x, na.rm = TRUE), 
                            sd = sd(nexam$x, na.rm = TRUE)), 
                colour = "blue", 
                size = 1)

qqplot.norm_exam <- ggplot(nexam, aes(sample = nexam$x)) + 
  stat_qq() + stat_qq_line(colour = "Red")

grid.arrange(hist.exam, hist.norm_exam, 
             qqplot.exam, qqplot.norm_exam, nrow=2, ncol=2)

stat.desc(cbind(rexam$exam, nexam$x), basic = FALSE, norm = TRUE)

##### TODO: 기존 데이터의 numeracy (0~15)를 정규분포로 만들어서 histogram과 Q-Q plot 을 그려보아라.
##### TODO: 위 작업 후에 통계량을 출력해서 비교해보아라.


####
grid.arrange(hist.numeracy, hist.norm_numeracy, 
             qqplot.numeracy, qqplot.norm_numeracy, nrow=2, ncol=2)

round(stat.desc(cbind(rexam$numeracy, nnumeracy$x), basic = FALSE, norm = TRUE), digits = 3)

### Shapiro-Wilk Test
#### 
round(stat.desc(rexam[, c("exam", "numeracy")], basic = FALSE, norm = TRUE), digits = 3)

shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)

by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)

### 분산의 동질성 (homogeneity of varaince)
#### concert-homovar.csv - 동질
#### concert-hetervar.csv - 이질
#### 연속된 콘서트가 이명에 영향을 줄것인가?

concert <- read.csv("./data/concert-homovar.csv", header = TRUE)
View(concert)

conRing <- stack(concert)
names(conRing) <- c("Ringing", "Concert")
View(conRing)

line.homo <- ggplot(conRing, aes(conRing$Concert, conRing$Ringing, group = 1)) + 
  geom_point() +
  stat_summary(fun.y = mean, geom = "point", shape = 3, size=5) +
  stat_summary(fun.y = mean, geom = "line", size = 1, colour = 'deeppink') +
  coord_cartesian(ylim=c(0, 70)) +
  labs(x = "Concert", y = "Ringing (Hours)")

concerth <- read.csv("./data/concert-hetervar.csv", header = TRUE)
View(concerth)

conRingh <- stack(concerth)
names(conRingh) <- c("Ringing", "Concert")
View(conRingh)

line.hete <- ggplot(conRingh, aes(conRingh$Concert, conRingh$Ringing, group = 1)) + 
  geom_point() +
  stat_summary(fun.y = mean, geom = "point", shape = 3, size=5) +
  stat_summary(fun.y = mean, geom = "line", size = 1, colour = 'deeppink') +
  coord_cartesian(ylim=c(0, 70)) +
  labs(x = "Concert", y = "Ringing (Hours)")

grid.arrange(line.homo, line.hete, nrow=1, ncol=2)

### 레빈 검정 (Levene's test)
leveneTest(conRing$Ringing, conRing$Concert)
leveneTest(conRing$Ringing, conRing$Concert, center = mean)
leveneTest(conRingh$Ringing, conRingh$Concert)


### TODO: rexam 의 exam, computer, lectures, numeracy 에 대해서도 레빈 검정을 해보아라. 
### 그리고 분산의 동질성을 유지하는 것과 동질성이 깨지는 것은 무엇인가?


### 하틀리 F_max 검정 (Hartley's F_max test)
### 

fmax <- function(var1, var2) {
  if (var1 > var2) {
    return(var1 / var2)
  } else {
    return(var2 / var1)
  }
}

qmaxFratio(0.95, 49, 2)
fmax_exam <- fmax(var(domestic$exam), var(international$exam))
fmax_exam

fmax_exam < qmaxFratio(0.95, 49, 2)

1 - pmaxFratio(fmax_exam, 49, 2)

qmaxFratio(0.95, 49, 2)
fmax_numeracy <- fmax(var(domestic$numeracy), var(international$numeracy))
fmax_numeracy

fmax_numeracy < qmaxFratio(0.95, 49, 2)

1 - pmaxFratio(fmax_numeracy, 49, 2)

#### qnorm

qnorm(1 - (0.001 / 2))
qnorm(1 - (0.05 / 2))
qnorm(1 - (0.01 / 2))


############# 표준정규분포 

dnorm_range999 <- function(x) {
  y <- dnorm(x) 
  y[x < -3.29 | x > 3.29] <- NA  # 이 범위에는 색깔 없음
  return(y)
}

dnorm_range99 <- function(x) {
  y <- dnorm(x) 
  y[x < -2.58 | x > 2.58] <- NA  # 이 범위에는 색깔 없음
  return(y)
}

dnorm_range95 <- function(x) {
  y <- dnorm(x) 
  y[x < -1.96 | x > 1.96] <- NA  # 이 범위에는 색깔 없음
  return(y)
}

ggplot(data.frame(x=c(-4,4)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range999, geom = "area", fill="blue", alpha = 0.5) +
  stat_function(fun=dnorm_range99, geom = "area", fill="white", alpha = 0.5) +
  stat_function(fun=dnorm_range95, geom = "area", fill="white", alpha = 0.5) +
  ggtitle("Normal Distribution")


## 바른 정렬 역수변환 예제
x <- c(1, 5, 9, 10)
1/x
1/(max(x)+1 - x)

## 몇가지 연산
dlf$day1PlusDay2 <- dlf$day1 + dlf$day2
dlf$day1PlusDay2

dlf$day2MinusDay1 <- dlf$day2 - dlf$day1
dlf$day2MinusDay1

dlf$day2Times5 <- dlf$day1 * 5
dlf$day2Times5

dlf$day2Squared <- dlf$day2 ** 2
dlf$day2Squared

dlf$day2Squared <- dlf$day2 ^ 2
dlf$day2Squared

dlf$day1LessThanOne <- dlf$day1 < 1
dlf$day1LessThanOne

dlf$day1LessThanOrEqualOne <- dlf$day1 <= 1
dlf$day1LessThanOrEqualOne

dlf$day1GreaterThanOne <- dlf$day1 > 1
dlf$day1GreaterThanOne

dlf$day1GreaterThanOrEqualOne <- dlf$day1 >= 1
dlf$day1GreaterThanOrEqualOne

dlf$male <- dlf$gender == "Male"
dlf$male

dlf$notMale <- dlf$gender != "Male"
dlf$notMale

dlf <- read.csv("./data/festivalDataNoOutlier.csv", header = TRUE)
View(dlf)

## 행의 평균
rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3), na.rm = TRUE)

## 행의 합
rowSums(cbind(dlf$day1, dlf$day2, dlf$day3), na.rm = TRUE)

## 제곱근
sqrt(dlf$day2)
sqrt(c(1, -1, -2, 0, NA, 10, 4))

## 절댓값
abs(dlf$day1)
abs(c(1, -1, -2, 0, NA, 10, 4))

## 상용로그(밑이 10인 로그)
log10(dlf$day1)
log10(c(1, -1, -2, 0, NA, 10, 4))
log10(c(1, -1, -2, 0, NA, 10, 4) + 3)

## 자연로그(ln)
log(dlf$day1)
log(c(1, -1, -2, 0, NA, 10, 4))
log(c(1, -1, -2, 0, NA, 10, 4) + 3)

## is.na()
is.na(dlf$day1)
is.na(dlf$day2)

sum(is.na(dlf$day2))
mean(is.na(dlf$day2))

## log Transformation
dlf$logday1 <- log(dlf$day1 + 1)
dlf$logday1

dlf$logday2 <- log(dlf$day2 + 1)
dlf$logday2

### TODO: 3일차도 똑같이 로그변환을 적용하고 logday1, logday2, logday3 3일간 로그 변환된 위생 점수들로 히스토그램을 작성하라.


####
grid.arrange(hist.day1, hist.logday1, 
             hist.day2, hist.logday2, 
             hist.day3, hist.logday3, nrow=3, ncol=2)


### TODO: 제곱근 변환을 적용하고 sqrtday1, sqrtday2, sqrtday3 3일간 로그 변환된 위생 점수들로 히스토그램을 작성하라.



####
grid.arrange(hist.day1, hist.sqrtday1, 
             hist.day2, hist.sqrtday2, 
             hist.day3, hist.sqrtday3, nrow=3, ncol=2)


### 역수변환 
### 0이 아닌 값으로 만들기, 나눌때 0으로 나누면 error
dlf$recday1 <- 1/(dlf$day1 + 1)
dlf$recday1

### TODO: day2, day3 도 역수변환 recday2, recday3 해보시고 히스토그램을 그려보시오



####
grid.arrange(hist.day1, hist.recday1, 
             hist.day2, hist.recday2, 
             hist.day3, hist.recday3, nrow=3, ncol=2)


### 전체 그래프
####
grid.arrange(hist.day1, hist.logday1, hist.sqrtday1, hist.recday1, 
             hist.day2, hist.logday2, hist.sqrtday2, hist.recday2, 
             hist.day3, hist.logday3, hist.sqrtday3, hist.recday3, 
             nrow=3, ncol=4)


### TODO: 위와 같이 똑같이 변환된 Q-Q Plot 을 그려보아라.
### TODO: stat.desc, shapiro.test, pmaxFratio 등을 이용하여 위에서 변환된 자료의 서술 통계량과 자료의 정규성, 또는 그 차이를 서술하시오.


### ifelse(조건, 조건이 TRUE 일때 value, FALSE 일때 value)
fd <- read.csv("./data/festivalData.csv", header = TRUE)
View(fd)

fd[fd$day1 > 4,]
fd$day1NoOutlier <- ifelse(fd$day1 > 4, NA, fd$day1)
  
fd[fd$day1NoOutlier > 4,]  

### TODO: day1 의 이상치(outlier)를 평균에서 표준편차의 3.29배를 더한 값 이상이라고 정의하고 (replaceValue) 그 값보다 클때, 
### 이상치를 그 값replaceValue로 대체하라. (fd$day1NoOutlier2)



### NA value 처리
apples <- 2
oranges <- NA
apples + oranges

dlf$meanHygiene <- rowMeans(cbind(dlf$day1, 
                                  dlf$day2, 
                                  dlf$day3))
dlf$meanHygiene

### 점수가 하나라도 있으면 포함
dlf$meanHygiene <- rowMeans(cbind(dlf$day1, 
                                  dlf$day2, 
                                  dlf$day3), 
                            na.rm = TRUE)
dlf$meanHygiene


### 하나 정도 빠져도 포함시키되, 아예 없거나 한개밖에 없을때 제외하고 싶으면
dlf$daysMissing <- rowSums(cbind(is.na(dlf$day1), 
                                 is.na(dlf$day2), 
                                 is.na(dlf$day3)))
dlf$daysMissing

dlf$meanHygiene <- ifelse(dlf$daysMissing < 2,
                          rowMeans(cbind(dlf$day1,
                                         dlf$day2, 
                                         dlf$day3), 
                                   na.rm = TRUE),
                          NA)
dlf$meanHygiene

