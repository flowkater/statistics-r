# 통계학 스터디 관련 안내

## Source Code

- statistics-model.R: 기본 통계 모형, 설명 따라가보기 (뒤에서 쓰는 패키지 설치)
- t-test-example.R: 수면제 효과 테스트하기
- basic.R: R 기본 사용법
- ggplot2.R: ggplot2 기본 예제

## "./data" Folder

기본적으로 csv 포맷으로 데이터 저장

## RStudio 기본 단축키

- Script 창으로 들어가는 키 숏컷: Ctrl + 1
- Console 창 들어가는 키 숏컷: Ctrl + 2
- Script 여러개일 때 다음 Script 고르기: Ctrl + Tab
- Script 여러개일 때 이전 Script 고르기: Ctrl + Shift + Tab
- Console 클리닝: Ctrl + L
- 메모리상의 모든 변수 및 데이터 삭제: rm(list = ls())
- 각 라인마다 실행 Ctrl(Cmd) + Enter

## RStudio 기본 설정

### Mac 사용자 설정 관련

*1. 소스코드내 한글 깨짐 해결*
[참고](https://rfriend.tistory.com/10)

*2. 그래프 등 한글 깨질때*
Mac for Korean / Mac 에서 한글 깨질때,

```r

Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

```

*3. 작업 경로 지정*
setwd 코드의 경로를 내 소스코드가 있는 폴더로 변경

```r

setwd("~/work/statistics-R/")

```

### Windows 사용자 설정 관련

*1. 소스코드내 한글 깨짐 해결*
[참고](https://kkokkilkon.tistory.com/8)

1. Tools -> Global Options
2. Code탭 -> Saving
3. Default text encoding: UTF-8 선택
4. R 스튜디오 껏다키기
5. 소스 코드 모두 닫고 다시 불러오기

*2. 작업 경로 지정*
setwd 코드의 경로를 내 소스코드가 있는 폴더로 변경

```r

setwd("C:\\Users\\User\\Desktop") # 변경 (이 경우 바탕화면)

```
