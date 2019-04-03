## Script 창으로 들어가는 키 숏컷: Ctrl + 1
## Console 창 들어가는 키 숏컷: Ctrl + 2
## Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
## Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
## 주석 달기: Ctrl(Cmd) + Shift + c
## Console 클리닝: Ctrl + L
## 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
## Mac for Korean / Mac 에서 한글 깨질때,
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")
## R version check: Sys.getenv("R_ARCH") - 32 bit인 분은 R을 다시 인스톨해주시기 바랍니다
## 64 bit for "/x64"
## 32 bit for "/i386"
## 각 라인마다 실행 Ctrl(Cmd) + Enter

#######################################################
####### Section 2. ggplot2 시각화 예제 따라쳐보기 ########

library(ggplot2)
library(extrafont)
library(reshape)
library(Hmisc)

setwd("~/work/statistics-R/")

######################################################
### facebookData.csv

# id: 참가자(프로필 사진의 소유자)의 식별 번호
# NPQC_R_TOTAL: 자기도취 설문지의 총점
# Rating_Type: 프로필 사진 평가 종류. 즉, 멋짐, 글래머, 패션 감각, 매력을 담는 변수이다. (각 종류를 문자열로 표현)
# Rating: 프로필 사진 평가 점수(1~5)

######################################################

facebookData <- read.csv("./data/facebookData.csv", header = TRUE)
View(facebookData)

str(facebookData)
summary(facebookData)

# NPQC_R_TOTAL (자기도취) 와 Rating (프로필 사진 평가 점수)의 관계 표시

graph <- ggplot(facebookData, aes(facebookData$NPQC_R_Total, facebookData$Rating))

graph + geom_point()
graph + geom_point(shape = 15)
graph + geom_point(size = 6)
graph + geom_point(shape = 17, size = 6)
graph + geom_point(aes(colour = facebookData$Rating_Type))
graph + geom_point(aes(colour = facebookData$Rating_Type), position = "jitter")
graph + geom_point(aes(shape = facebookData$Rating_Type), position = "jitter")

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
summary(examData)

# Scatter Plot

scatter <- ggplot(examData, aes(examData$Anxiety, examData$Exam))
scatter + geom_point()
scatter + geom_point() + labs(x = "시험 불안", y = "시험 성적 %")

scatter + geom_point() + geom_smooth() + labs(x = "시험 불안", y = "시험 성적 %")

scatter + geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "시험 불안", y = "시험 성적 %")
scatter + geom_point() + geom_smooth(method = "lm", se = F) + labs(x = "시험 불안", y = "시험 성적 %")

scatter <- ggplot(examData, aes(examData$Anxiety, examData$Exam)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Blue") + 
  labs(x = "시험 불안", y = "시험 성적 %")
scatter

scatter <- ggplot(examData, aes(examData$Anxiety, examData$Exam, colour = examData$Gender)) + 
  geom_point() + 
  geom_smooth(method = "lm",aes(fill = examData$Gender), colour = "Red", alpha = 0.1, fill = "Blue") + 
  labs(x = "시험 불안", y = "시험 성적 %", colur = examData$Gender)
scatter

######################################################
### festivalData

# ticknumb: 티켓넘버
# gender: 성별
# day1: 축제 첫째날 위생(hygiene) 상태
# day2: 축제 둘째날 위생(hygiene) 상태
# day3: 축제 셋째날 위생(hygiene) 상태
# 위생 점수는 0~4

######################################################
### 이상치 검출
# histogram

festivalData <- read.csv("./data/festivalData.csv", header = TRUE)
View(festivalData)

str(festivalData)
summary(festivalData)

festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
festivalHistogram + geom_histogram()
festivalHistogram + geom_histogram(binwidth = 0.4)

festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "축제 Day 1 위생", y = "빈도수")

# boxplot
festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "성별", y = "축제 Day 1 위생")

# replace outlier
festivalOutlier <- festivalData[festivalData$day1 > 4, ]
View(festivalOutlier)

festivalData[611, ]$day1 <- 2.02

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "성별", y = "축제 Day 1 위생")

# density plot
density <- ggplot(festivalData, aes(day1))
density + geom_density() + labs(x = "축제 Day 1 위생", y = "Density Estimate")

######################################################
### ChickFlick.csv

# ChickFlick - 대체로 남자들보다 여자들이 더 좋아할만한 영화를 뜻하는 속어
# gender: 참가자의 성별(문자열)
# film: 관람한 영화 제목(문자열)
# arousal: 각성 점수 (영화를 얼마나 즐겼는지)

######################################################

chickFlick <- read.csv("./data/ChickFlick.csv", header = TRUE)
View(chickFlick)
str(chickFlick)
summary(chickFlick)

# bar chart

bar <- ggplot(chickFlick, aes(chickFlick$film, chickFlick$arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "Black") +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", colour = "Red") +
  labs(x = "Film", y = "Mean Arousal")

bar <- ggplot(chickFlick, aes(chickFlick$film, chickFlick$arousal, fill = chickFlick$gender))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) +
  labs(x = "Film", y = "Mean Arousal", fill = "Gender") # +
  # scale_fill_manual("Gender", values = c("Female" = "#3366FF", "Male" = "#336633"))

bar <- ggplot(chickFlick, aes(chickFlick$film, chickFlick$arousal, fill = chickFlick$film))
bar + stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_wrap( ~ gender) +
  labs(x = "Film", y = "Mean Arousal") #+
  # theme(legend.position = "none")

######################################################
### Hiccups.csv

# Hiccups - 딸꾹질 치료법에 대한 효과 유무
# 딸국질로 고생하는 15명의 자료 - 각 열이 서로 다른 처치 조건, 모든 사람에게 모든 처치를 시행한 반복측정 설계
# Baseline: 1분간 딸꾹질 횟수를 측정한 기저
# Tongue: 혀를 잡아당긴 후의 딸꾹질 횟수
# Carotid: 경동맥 마사지 이후의 딸꾹질 횟수
# Rectum: 직상 수지 마사지 이후의 딸꾹질 횟수

######################################################

hiccupsData <- read.csv("./data/Hiccups.csv", header = TRUE)
View(hiccupsData)
str(hiccupsData)
summary(hiccupsData)

hiccups <- stack(hiccupsData)
names(hiccups) <- c("Hiccups", "Intervention")

View(hiccups)
str(hiccups)
summary(hiccups)

# hiccups$Intervention_Factor <- factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])
# View(hiccups)
# str(hiccups)

line <- ggplot(hiccups, aes(hiccups$Intervention, hiccups$Hiccups))
line + stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Intervention", y = "Mean Number of Hiccups")

######################################################
### TextMessages.csv

# TextMessages - 휴대폰 사용이 아이들의 문법 실력에 영향을 줄까? 25명은 휴대폰 사용 그룹, 25명은 금지 그룹
# 첫번째 독립변수 - 문자 메시지 사용량(문자를 허용한 실험군 대 허용하지 않은 대조군)
# 두번째 독립변수 - 문법 실경을 측정한 시기 (기저 또는 6개월 후)
# Group: 참가자가 문자 메시지를 허용한 실험군('Text Messages')에 속하는지 대조군('Controls')에 속하는지를 나타낸다.
# Baseline: 기저 조건(조작을 가하기 전)의 문법 시험 점수
# Six_months: 6개월 이후의 문법 시험 점수
# 

######################################################

textData <- read.csv("./data/TextMessages.csv", header = TRUE)
View(textData)
str(textData)
summary(textData)

text <- melt(textData, id = c("Group"), measured = c("Baseline", "Six_months"))
names(text) <- c("Group", "Time", "Grammer_Score")
View(text)

line <- ggplot(text, aes(text$Time, text$Grammer, colour = text$Group))
line + stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = text$Group)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(x = "Time", y = "Mean Grammer Score", colour = "Group")


