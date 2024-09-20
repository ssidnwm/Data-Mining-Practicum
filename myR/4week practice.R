library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# URL에서 텍스트 파일을 읽기
# 헤더가 없는 경우
bible <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/asv.csv", header = FALSE)

# 첫 몇 줄 출력
head(bible)
