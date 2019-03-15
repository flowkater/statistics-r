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
####### Section 0-1. 통계 모형, 설명 따라가보기 t-test example ########
library("dplyr")
library("ggplot2")

y = c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0)
summary(y)
sd(y)
par(mfrow=c(2,2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y)
hist(y, prob=TRUE)
lines(density(y), lty=2)

t.test(y)
t.test(y, alternative = "greater")

curve(dnorm(x, 0, sd(y)), -4, 4)

options(digits = 3)
set.seed(1606)
(y_star <- rnorm(10, 0, sd(y)))
mean(y_star-0); sd(y_star)
(t_star <- mean(y_star-0) / (sd(y_star) / sqrt(length(y_star))))

set.seed(1606)
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
sds_star <- rep(NA, B)
ts_star <- rep(NA, B)
for(b in 1:B) {
  y_star <- rnorm(n, 0, sd(y))
  m <- mean(y_star)
  s <- sd(y_star)
  xbars_star[b] <- m
  sds_star[b] <- s
  ts_star[b] <- m / (s / sqrt(n))
}
opar <- par(mfrow=c(2,2))
hist(xbars_star, nclass = 100)
abline(v = 0.75, col='red')
hist(sds_star, nclass=100)
abline(v = 1.789, col='red')
hist(ts_star, nclass=100)
abline(v = 1.789, col='red')
qqnorm(ts_star); qqline(ts_star)
par(opar)

length(which(ts_star > 1.3257)) / B

set.seed(1606)
(y_star <- rnorm(10, 1, 1.8))
t.test(y_star)$conf.int

set.seed(1606)
B <- 1e2
conf_intervals <-
  data.frame(b=rep(NA, B),
             lower=rep(NA, B),
             xbar=rep(NA, B),
             upper=rep(NA, B))
true_mu <- 1
for(b in 1:B) {
  (y_star <- rnorm(10, true_mu, 1.8))
  conf_intervals[b, ] = c(b=b,
                          lower=t.test(y_star)$conf.int[1],
                          xbar=mean(y_star),
                          upper=t.test(y_star)$conf.int[2])
}

conf_intervals <- conf_intervals %>%
  mutate(lucky = (lower <= true_mu & true_mu <= upper))

glimpse(conf_intervals)
table(conf_intervals$lucky)
conf_intervals %>% ggplot(aes(b, xbar, col=lucky)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_hline(yintercept=true_mu, col='red')

par(mfrow=c(2,1))
hist(c(0,1), nclass=100, prob=TRUE, main='Individual sleep time increase')
set.seed(1606)
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
for(b in 1:B){
  xbars_star[b] <- mean(sample(c(0,1), size=n, replace=TRUE))
}
hist(xbars_star, nclass=100, main = 'Sample mean of 10 obs')

diff(t.test(y)$conf.int)
mean(y)
diff(t.test(y)$conf.int)/2





