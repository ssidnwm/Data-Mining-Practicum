library(ggplot2)
library(dplyr)

exData <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/ex1data1.txt", header =  F, col.names = c('pop','profit'))
summary(exData)

theta0 = 0
theta1 = 1

h <- function(x, t0, t1){
  t0+t1*x
  
}

costJ <- function(t0, t1){
  m = nrow(exData)
  1/(2*m) * sum((h(exData$pop, t0, t1) - exData$profit) ** 2)
}

costJ(theta0, theta1)

costDF<-data.frame(t1 =seq(-1, 2, 0.1), 
                   cost =seq(-1, 2, 0.1) %>%
                     sapply(function(x) { costJ(theta0, x)}))

costDF %>% ggplot(aes(x = t1, y = cost))+ geom_point()

gradient_theta0 <-function(t0, t1){
  mean(h(exData$pop, t0, t1) -exData$profit)}

gradient_theta1 <-function(t0, t1){
  mean((h(exData$pop, t0, t1) -exData$profit) *exData$pop)}

num_iter <- 1500
alpha <- 0.01

theta0 = 0
theta1 = 1

costDF = data.frame(iter = 0, t0 = theta0, t1 = theta1, cost = costJ(theta0,
                                                                     theta1))

for( i in 1:num_iter) {
  theta0_update <- gradient_theta0(theta0, theta1)
  theta1_update <- gradient_theta1(theta0, theta1)
  
  theta0 <- theta0 - alpha * theta0_update
  theta1 <- theta1 - alpha * theta1_update
  
  costDF <- rbind(costDF, c(i, theta0,theta1, costJ(theta0, theta1)))
}

costDF %>%
  ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)



# alpha 값 설정
alpha_values <- c(0.001, 0.005, 0.01, 0.05, 0.1)
# 초기 theta 값 설정
theta0 <- 0
theta1 <- 1
num_iter <- 1500

# 데이터를 저장할 데이터프레임 초기화
cost_data <- data.frame()

# 여러 alpha 값에 대해 반복 실행
for (alpha in alpha_values) {
  theta0 <- 0
  theta1 <- 1
  temp_costDF <- data.frame(iter = 0, t0 = theta0, t1 = theta1, cost = costJ(theta0, theta1), alpha = alpha)
  
  for (i in 1:num_iter) {
    theta0_update <- gradient_theta0(theta0, theta1)
    theta1_update <- gradient_theta1(theta0, theta1)
    
    theta0 <- theta0 - alpha * theta0_update
    theta1 <- theta1 - alpha * theta1_update
    
    # 각 반복마다 Cost 값 저장
    temp_costDF <- rbind(temp_costDF, c(i, theta0, theta1, costJ(theta0, theta1), alpha))
  }
  
  cost_data <- rbind(cost_data, temp_costDF)
}

# 그래프 그리기
ggplot(cost_data, aes(x = iter, y = cost, color = as.factor(alpha))) +
  geom_line() +
  labs(title = "Learning Curve (with several alpha)", x = "Iteration", y = "Cost", color = "Alpha") +
  theme_minimal()

exData$pop <- scale(exData$pop)
exData$profit <- scale(exData$profit)



### Task2
# alpha 값 설정
alpha_values <- c(0.001, 0.005, 0.01, 0.05, 0.1)
tolerance <- 1e-6  # Cost 변화가 이 값 이하로 작아지면 수렴했다고 판단
max_iter <- 1500   # 최대 반복 횟수 제한

# 결과를 저장할 데이터프레임 초기화
iteration_data <- data.frame(alpha = numeric(), iterations = numeric())

# 각 alpha에 대해 경사 하강법 실행
for (alpha in alpha_values) {
  theta0 <- 0
  theta1 <- 1
  cost_prev <- costJ(theta0, theta1)
  
  for (i in 1:max_iter) {
    # 경사 하강법으로 theta0, theta1 업데이트
    theta0_update <- gradient_theta0(theta0, theta1)
    theta1_update <- gradient_theta1(theta0, theta1)
    
    theta0 <- theta0 - alpha * theta0_update
    theta1 <- theta1 - alpha * theta1_update
    
    # 새로운 Cost 계산
    cost_curr <- costJ(theta0, theta1)
    
    # 수렴 확인 (변화량이 tolerance보다 작아지면 중단)
    if (abs(cost_curr - cost_prev) < tolerance) {
      iteration_data <- rbind(iteration_data, data.frame(alpha = alpha, iterations = i))
      break
    }
    
    # 이전 Cost 값 갱신
    cost_prev <- cost_curr
    
    # 최대 반복 횟수까지 완료하면 수렴 실패로 간주
    if (i == max_iter) {
      iteration_data <- rbind(iteration_data, data.frame(alpha = alpha, iterations = max_iter))
    }
  }
}

# iteration_data를 출력하여 확인
print(iteration_data)

# iteration_data를 그래프로 시각화
ggplot(iteration_data, aes(x = as.factor(alpha), y = iterations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Iterations Needed for Convergence by Alpha", x = "Alpha", y = "Iterations") +
  theme_minimal()


### Task 3:

exData2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/ex1data2.txt", header =  F, col.names = c('size','num_bedroom','price'))

mean1 <- mean(exData2$size)
sd1 <- sd(exData2$size)
mean2 <- mean(exData2$num_bedroom)
sd2 <- sd(exData2$num_bedroom)

exData2$size.norm <- (exData2$size - mean1) / sd1
exData2$num_bedroom.norm <- (exData2$num_bedroom- mean2) / sd2

featureDF <- exData2 %>%
  mutate(bias = 1)%>%
  select(bias, ends_with('norm'))

labelVector <- exData2$price
theta_vector <- c(0,0,0)

h <- function(x, theta_vector){
  theta_vector %*% x
  
}

costFun <- function(theta_vector){
  v <- as.matrix(featureDF) %*% theta_vector - labelVector
  (t(v) %*% v / (2*nrow(featureDF)))[1,1]
}
costFun(theta_vector)

costDF <- data.frame(iter = 0,
                     t0 = theta_vector[1],
                     t1 = theta_vector[2],
                     t2 = theta_vector[3],
                     cost = costFun(theta_vector))

num_iter = 1500
alpha <- 0.05
n <- nrow(featureDF)

for( i in 1:num_iter){
  errors <- as.matrix(featureDF) %*% theta_vector - labelVector
  
  # 각 theta에 대한 기울기 계산
  theta_update <- alpha * (t(as.matrix(featureDF)) %*% errors) / n
  theta_vector <- theta_vector - theta_update
  costDF <- rbind(costDF, c(i, theta_vector, costFun(theta_vector)))
}

label_ko_num = function(num){
  ko_num = function(x){
    new_num = x %/% 10**10
    return(paste(new_num,'x10^10', sep = ''))
  }
  return(sapply(num, ko_num))
}
costDF %>% ggplot(aes(x = iter, y = cost))+
  geom_line()+
  scale_y_continuous(labels = label_ko_num)

### Task 4 부동산 가격 예측
library(readxl)
estate <- read_excel("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/Real estate valuation data set.xlsx")
head(estate)

colnames(estate) <- c("No", "transaction_date", "house_age", "distance_to_mrt", 
                      "num_convenience", "latitude", "longitude", "price")
# 표준화(정규화) 적용
estate$house_age_norm <- (estate$house_age - mean(estate$house_age)) / sd(estate$house_age)
estate$distance_to_mrt_norm <- (estate$distance_to_mrt - mean(estate$distance_to_mrt)) / sd(estate$distance_to_mrt)
estate$num_convenience_norm <- (estate$num_convenience - mean(estate$num_convenience)) / sd(estate$num_convenience)
estate$latitude_norm <- (estate$latitude - mean(estate$latitude)) / sd(estate$latitude)
estate$longitude_norm <- (estate$longitude - mean(estate$longitude)) / sd(estate$longitude)

# 특징 행렬 생성 (정규화된 변수와 bias 항 추가)
featureDF <- estate %>%
  mutate(bias = 1) %>%
  select(bias, house_age_norm, distance_to_mrt_norm, num_convenience_norm, latitude_norm, longitude_norm)

# 목표 변수 (라벨 벡터)
labelVector <- estate$price
# theta 초기화 (특징 개수에 맞춰 초기화)
theta_vector <- rep(0, ncol(featureDF))

# 가설 함수
h <- function(x, theta_vector) {
  theta_vector %*% x
}

# Cost 함수
costFun <- function(theta_vector) {
  v <- as.matrix(featureDF) %*% theta_vector - labelVector
  (t(v) %*% v / (2 * nrow(featureDF)))[1, 1]
}

num_iter <- 1500
alpha <- 0.01  # 학습률, 데이터에 따라 적절히 조정 가능
n <- nrow(featureDF)
costDF <- data.frame(iter = 0, t(theta_vector), cost = costFun(theta_vector))

# 경사 하강법 반복
for (i in 1:num_iter) {
  # 기울기 계산
  errors <- as.matrix(featureDF) %*% theta_vector - labelVector
  theta_update <- alpha * (t(as.matrix(featureDF)) %*% errors) / n
  
  # theta 업데이트
  theta_vector <- theta_vector - theta_update
  
  # Cost 값 기록
  costDF <- rbind(costDF, c(i, theta_vector, costFun(theta_vector)))
}

# Cost 값 시각화
ggplot(costDF, aes(x = iter, y = cost)) +
  geom_line(color = "blue") +
  labs(title = "Learning Curve", x = "Iteration", y = "Cost") +
  theme_minimal()


# 예측 값 계산
predicted_values <- as.matrix(featureDF) %*% theta_vector

# MSE 계산
mse <- mean((predicted_values - labelVector)^2)

# MAE 계산
mae <- mean(abs(predicted_values - labelVector))

# R^2 계산
ss_total <- sum((labelVector - mean(labelVector))^2)
ss_residual <- sum((labelVector - predicted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)

# 성능 지표 출력
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")


## LM함수를 통한 예측

lm_model <- lm(price ~ house_age_norm + distance_to_mrt_norm + num_convenience_norm + latitude_norm + longitude_norm, data = estate)
# 예측 값 계산
predicted_values_lm <- predict(lm_model, newdata = estate)
# 실제 값
actual_values <- estate$price

# MSE 계산
mse_lm <- mean((predicted_values_lm - actual_values)^2)

# MAE 계산
mae_lm <- mean(abs(predicted_values_lm - actual_values))

# R^2 계산
ss_total_lm <- sum((actual_values - mean(actual_values))^2)
ss_residual_lm <- sum((actual_values - predicted_values_lm)^2)
r_squared_lm <- 1 - (ss_residual_lm / ss_total_lm)

# 성능 지표 출력
cat("Mean Squared Error (MSE):", mse_lm, "\n")
cat("Mean Absolute Error (MAE):", mae_lm, "\n")
cat("R-squared:", r_squared_lm, "\n")
