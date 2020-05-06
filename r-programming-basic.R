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
####### Part 1. Basic ########

####### 기본 사칙 연산 #######
1 + 2

10 - 2

11 * 2

5 / 2

5 %% 2

5 %/% 2

5^2

####### 변수 선언 #######

a = 2
a <- 3


a <- 10 - 2
a

b <- 3
b

a / b


####### 논리연산 #######

TRUE
FALSE

a <- TRUE
a <- T
a

b <- F
b

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

TRUE | FALSE
FALSE | TRUE
FALSE | FALSE

!TRUE
!FALSE

####### 비교연산 #######
a <- 1
b <- 2

a == b

a != b

a < b
a <= b

a > b
a >= b




####### 문자열 #######
"Hello World!"

'Hello World!'

# "Hello "Mr.Stranger"!"
"Hello 'Mr.Stranger'!"
'Hello "Mr.Stranger"!'


hello <- "Hello World!"

# 문자열 길이
nchar(hello)

nchar('What The XXXX!')


# 문자열 합치기 
paste("Hey", "!")
paste("Hey", "!", sep="")


# 문자열 일부 추출
substr(hello, 1, 4)
substr(hello, 2, 4)
substr(hello, 5, 6)


# 문자열 나누기
strsplit(hello, " ")


# 포맷팅

name <- "조재우"
paste("안녕하세요, 제 이름은 ", name, "입니다.", sep="")

install.packages("glue")
library(glue)

glue("안녕하세요, 제 이름은 {name}입니다.")


####### Vector #######
x <- c(1, 2, 3)
x
typeof(x)

hero <- c("아이언맨", "캡틴아메리카", "블랙위도우")
hero
typeof(hero)

z <- c(TRUE, TRUE, FALSE, FALSE)
z
typeof(z)

w <- c(1, 2, 3, "a", "b", "c")
w
typeof(w)

c(x, hero)

# 다양한 벡터 선언

v1 <- 50:90
v1

v2 <- c(1, 2, 5, 30:40)
v2

v3 <- seq(1, 10, 2)
v3

v4 <- seq(0.1, 1.0, 0.1)
v4

v5 <- rep(1, times=5)
v5

v6 <- rep(1:3, times=5)
v6

v6 <- rep(c("a", "b","c"), each = 3)
v6


# 벡터값 접근

hero[2]


v7 <- 1:10

v7
length(v7)
v7[1:5]
v7[c(1, 3, 5)]
v7[seq(1,length(v7),2)]

# 벡터 제외

v7[-2]
v7[-c(2,4,6)]


# 벡터값 수정
v7[2] = 10
v7

# 벡터 합치기 및 추가

hero

hero2 <- c("캡틴마블", "스파이더맨", "블랙팬서")
hero2


hero3 <- c(hero, hero2)
hero3

"캡틴마블" %in% hero3
"닥터스트레인지" %in% hero3
c("블랙팬서", "닥터스트레인지") %in% hero3

2 %in% v7
10 %in% v7

c(1,3,5) %in% v7


# 벡터 연산
a <- 1:10
a

a * 2
a - 5
3 * a + 4
a / 2
a > 5
a < 5
a == 5

b <- 11:20
b

a < b
2*a < b


a + b
a - b
a * b

sum(a)
sum(2 * a)
length(a)
mean(a)
max(a)
min(a)
sort(a)
sort(a, decreasing = TRUE)

median(a)
sum(a) / length(a)

####### Condition (if-else) #######
a <- 1
b <- 2

if(a < b) {
  print("b가 더 크다")
} else {
  print("같거나 a가 더 크다")
}

a = 3
b = 2

if(a < b) {
  print("b가 더 크다")
}else if(a > b){
  print("a가 더 크다")
} else {
  print("같다")
}


result <- ifelse(a == b, "같다", "다르다")
result

x <- 1:10
ifelse(x %% 2 == 0, "짝수", "홀수")

ifelse(x %% 2 == 0, x, 0)

# TODO: 1~10 사이에서 3 또는 5의 배수인 숫자를 출력하시오.

# TODO: 1~10 사이에서 3 또는 5의 배수인 숫자의 합을 구하시오. (답: 23)



####### for문 #######
for(hero_name in hero3){
  print(hero_name)
}
  
for(i in 1:10){
  print(i * 2)
}



####### 함수 #######
sign <- function(x){
  if(x > 0){
    return('양수')
  }else if(x < 0){
    return('음수')
  }else{
    return('0')
  }
}

sign(10)
sign(-10)
sign(0)



# TODO: 10보다 작은 자연수 중에서 3 또는 5의 배수는 3, 5, 6, 9 이고, 이것을 모두 더하면 23입니다.
# 1000보다 작은 자연수 중에서 3 또는 5의 배수를 모두 더하면 얼마일까요?

x <- c(1:9)
sum(ifelse(x %% 3 == 0 | x %% 5 == 0, x, 0))

x <- c(1:999)
sum(ifelse(x %% 3 == 0 | x %% 5 == 0, x, 0))


# TODO: 위에서 구현한 함수를 n 보다 작은 자연수 중에서 a 또는 b의 배수를 모두 더한 결과값을 반환해주는 함수로 만들어보시오.

sum_times <- function(a, b, n) {
  x <- c(1:(n-1))
  return(sum(ifelse(x %% a == 0 | x %% b == 0, x, 0)))
}

sum_times(3, 5, 1000)
