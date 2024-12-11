library(ggplot2)
library(dplyr)


admission <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/ex2data1.txt", header = F, col.names = c('exam1','exam2','admitted'))
ggplot(admission, aes(x=exam1, y=exam2, col = as.factor(admitted))) +
  geom_point() +
  scale_color_manual('admitted', values = c('grey','skyblue'),
                     labels = c('not admitted','admitted'))+
  theme_classic()

theta_vector <- c(0,0,0)
feature.df <- admission %>%
  mutate(bias = 1) %>%
  select(bias, exam1, exam2)
labelVector <- admission$admitted

sigmoid <- function(x){
  1 / (1 + exp(-x))
}
h <- function(x, theta_vector){
  sigmoid(theta_vector %*% x)
}
costFunction <- function(theta_vector){
  hx <- apply(feature.df, 1, function(x){h(x, theta_vector)})
  costV <- labelVector * log(hx) + (1-labelVector) * log(1-hx)
  -mean(costV)
}
costFunction(theta_vector)

feature.df[,2:3] <- sapply(feature.df[,2:3], function(x){(x-mean(x))/sd(x)})
summary(feature.df)

#graident descent
theta_vector <- c(0, 0, 0)
costDF <- data.frame(iter = 0,
                     t0 = theta_vector[1],
                     t1 = theta_vector[2],
                     t2 = theta_vector[3],
                     cost = costFunction(theta_vector))
## find optimal theta
num_iter <- 1000
m <- nrow(feature.df)
alpha = 1
for(i in 1:num_iter){
  theta_update <-
    (t(as.matrix(feature.df)) %*% (apply(feature.df, 1, function(x) { h(x, theta_vector) }) -
                                     labelVector)) / m * alpha
  theta_vector <- theta_vector- theta_update[,1]
  costDF <- rbind(costDF, c(i, theta_vector, costFunction(theta_vector)))
}


#learning Curve
costDF %>% ggplot(aes(x = iter, y= cost)) +
  geom_line() +
  ggtitle('learning curve') + theme_bw()

slope <- - theta_vector[2] / theta_vector[3]
bias <- - theta_vector[1] / theta_vector[3]
ggplot(feature.df, aes(x=exam1, y=exam2)) +
  geom_point(aes( col = as.factor(labelVector))) +
  geom_abline(slope = slope, intercept = bias, col = 'red')+
  scale_color_manual('admitted', values = c('grey',
                                            'skyblue'), labels = c('not admitted','admitted'))+
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 = 0",theta_vector[1],theta_vector[2],theta_vector[3]))+
  theme_classic()
costFunction(theta_vector)

mu1 <- mean(admission$exam1)
mu2 <- mean(admission$exam2)
sig1 <- sd(admission$exam1)
sig2 <- sd(admission$exam2)

# theta_vector를 원래 스케일로 변환
theta_origin <- c(
  theta_vector[1] - (mu1 / sig1) * theta_vector[2] - (mu2 / sig2) * theta_vector[3],
  theta_vector[2] / sig1,
  theta_vector[3] / sig2
)

# 경계선의 기울기와 절편 계산
slope <- -theta_origin[2] / theta_origin[3]
bias <- -theta_origin[1] / theta_origin[3]

ggplot(admission, aes(x=exam1, y=exam2)) +
  geom_point(aes( col = as.factor(labelVector))) +
  geom_abline(slope = slope, intercept = bias, col = 'red')+
  scale_color_manual('admitted', values = c('grey',
                                            'skyblue'), labels = c('not admitted','admitted'))+
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 = 0",theta_origin[1],theta_origin[2],theta_origin[3]))+
  theme_classic()



## Task 2

# 추가적인 다항 변수 생성: x1^2, x2^2, x1 * x2
admission <- admission %>%
  mutate(exam1_sq = exam1^2,
         exam2_sq = exam2^2,
         exam1_exam2 = exam1 * exam2)
ggplot(admission, aes(x = exam1, y = exam2, color = as.factor(admitted))) +
  geom_point() +
  scale_color_manual('admitted', values = c('grey', 'skyblue'),
                     labels = c('not admitted', 'admitted')) +
  theme_classic()
# 추가 변수를 포함한 특징 데이터프레임 생성 및 정규화
feature.df <- admission %>%
  mutate(bias = 1) %>%
  select(bias, exam1, exam2, exam1_sq, exam2_sq, exam1_exam2)

# exam1, exam2, exam1^2, exam2^2, exam1 * exam2 정규화
feature.df[, 2:6] <- sapply(feature.df[, 2:6], function(x) (x - mean(x)) / sd(x))

# 레이블 벡터 준비
labelVector <- admission$admitted

# 확장된 변수에 대한 theta 벡터 초기화
theta_vector <- rep(0, 6)

# 시그모이드 및 비용 함수 정의
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

h <- function(x, theta_vector) {
  sigmoid(theta_vector %*% x)
}

costFunction <- function(theta_vector) {
  hx <- apply(feature.df, 1, function(x) { h(x, theta_vector) })
  costV <- labelVector * log(hx) + (1 - labelVector) * log(1 - hx)
  -mean(costV)
}

# 경사 하강법을 사용한 학습
num_iter <- 1000
m <- nrow(feature.df)
alpha <- 1
costDF <- data.frame(iter = 0,
                     t0 = theta_vector[1],
                     t1 = theta_vector[2],
                     t2 = theta_vector[3],
                     t3 = theta_vector[4],
                     t4 = theta_vector[5],
                     t5 = theta_vector[6],
                     cost = costFunction(theta_vector))

for (i in 1:num_iter) {
  theta_update <- (t(as.matrix(feature.df)) %*% (apply(feature.df, 1, function(x) { h(x, theta_vector) }) - labelVector)) / m * alpha
  theta_vector <- theta_vector - theta_update[,1]
  costDF <- rbind(costDF, c(i, theta_vector, costFunction(theta_vector)))
}

# 학습 곡선 그리기
ggplot(costDF, aes(x = iter, y = cost)) +
  geom_line() +
  ggtitle('Learning Curve with Additional Features') +
  theme_bw()

#Task3 결정 경계의 기울기와 절편 계산
# 결정 경계를 위한 x1, x2 값 생성 (그리드 생성)
x1_vals <- seq(min(admission$exam1), max(admission$exam1), length.out = 100)
x2_vals <- seq(min(admission$exam2), max(admission$exam2), length.out = 100)
grid <- expand.grid(exam1 = x1_vals, exam2 = x2_vals)
head(grid)
# 다항 특징 추가 및 정규화
grid <- grid %>%
  mutate(exam1_sq = exam1^2,
         exam2_sq = exam2^2,
         exam1_exam2 = exam1 * exam2)

# 그리드에 있는 특징들을 정규화 (원래 feature.df와 동일하게)
grid_norm <- as.data.frame(sapply(grid, function(x) (x - mean(x)) / sd(x)))
grid_norm <- cbind(bias = 1, grid_norm) # 편향 추가

# 각 grid 포인트에 대해 예측 확률 계산
prob_vals <- apply(grid_norm, 1, function(x) h(x, theta_vector))

# 예측 확률을 데이터프레임으로 추가
grid$prob <- prob_vals

# 시각화 - 결정 경계를 등고선으로 그리기
ggplot(admission, aes(x = exam1, y = exam2, color = as.factor(admitted))) +
  geom_point() +
  geom_contour(data = grid, aes(x = exam1, y = exam2, z = prob), breaks = 0.5, color = 'red') +
  scale_color_manual('admitted', values = c('grey', 'skyblue'), labels = c('not admitted', 'admitted')) +
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 + %.2f * x1^2 + %.2f * x2^2 + %.2f * x1*x2 = 0", 
                  theta_vector[1], theta_vector[2], theta_vector[3], theta_vector[4], theta_vector[5], theta_vector[6])) +
  theme_classic()
sprintf("%.2f + %.2f * x1 + %.2f * x2 + %.2f * x1^2 + %.2f * x2^2 + %.2f * x1*x2 = 0", 
        theta_vector[1], theta_vector[2], theta_vector[3], theta_vector[4], theta_vector[5], theta_vector[6])
