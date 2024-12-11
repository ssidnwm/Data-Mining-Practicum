library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(recommenderlab)

install.packages("rpart")
install.packages("Metrics")
library(rpart)
library(Metrics)

library(rpart)
train_data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/practice8/train.csv")
test_data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/practice8/test.csv")
sample_data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/practice8/sampleSubmission.csv")


head(train_data)
summary(train_data)
head(sample_data)
#1. Decision Tree 모델 학습


# 3. datetime을 연, 월, 일, 시간으로 분리
train_data$datetime <- as.POSIXct(train_data$datetime)
train_data$year <- as.numeric(format(train_data$datetime, "%Y"))
train_data$month <- as.numeric(format(train_data$datetime, "%m"))
train_data$day <- as.numeric(format(train_data$datetime, "%d"))
train_data$hour <- as.numeric(format(train_data$datetime, "%H"))

# test 데이터도 동일하게 처리
test_data$datetime <- as.POSIXct(test_data$datetime)
test_data$year <- as.numeric(format(test_data$datetime, "%Y"))
test_data$month <- as.numeric(format(test_data$datetime, "%m"))
test_data$day <- as.numeric(format(test_data$datetime, "%d"))
test_data$hour <- as.numeric(format(test_data$datetime, "%H"))


# 4. 불필요한 열 제거
train_data <- subset(train_data, select = -c(datetime, casual, registered))
X_train <- subset(train_data, select = -count)
y_train <- train_data$count

# test 데이터는 count 열이 없으므로 그냥 전체 사용
X_test <- subset(test_data, select = -datetime)

# 5. Decision Tree 모델 학습
model <- rpart(count ~ ., data = train_data, method = "anova")

# 6. test 데이터에 대한 예측
y_pred_test <- predict(model, X_test)
sample_data$count <- y_pred_test

# sample_data를 "sample_submission.csv"로 저장
write.csv(sample_data, "predict dicisiontree1.csv", row.names = FALSE)


### bagging
# 필요한 패키지 설치
install.packages("ipred")

# 패키지 로드
library(ipred)

# Bagging을 사용한 Decision Tree 모델 학습
bagging_model <- bagging(count ~ ., data = train_data, nbagg = 25)  # nbagg: 트리의 개수

# 테스트 데이터에 대한 예측
y_pred_test_bagging <- predict(bagging_model, test_data)

# 예측 결과를 sample_data의 count 열에 저장
sample_data$count <- y_pred_test_bagging

# CSV 파일로 저장
write.csv(sample_data, "predict_bagging_decisiontree1.csv", row.names = FALSE)

## 시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model)


# 첫 번째 트리 추출
first_tree <- bagging_model$mtrees[[1]]$btree

# 첫 번째 트리 시각화
rpart.plot(first_tree)

second_tree <- bagging_model$mtrees[[2]]$btree
rpart.plot(second_tree)

second_tree <- bagging_model$mtrees[[4]]$btree
rpart.plot(second_tree)


### random forest
# 패키지 설치
install.packages("randomForest")

# 패키지 로드
library(randomForest)

# Random Forest 모델 학습
set.seed(42)  # 재현성을 위해 설정
rf_model <- randomForest(count ~ ., data = train_data, ntree = 100, importance = TRUE)

# 테스트 데이터에 대한 예측
y_pred_test_rf <- predict(rf_model, X_test)

# 예측 결과를 sample_data에 저장
sample_data$count <- y_pred_test_rf

# CSV 파일로 저장
write.csv(sample_data, "random_forest_submission1.csv", row.names = FALSE)


# Feature Importance 계산
importance(rf_model)

# Feature Importance 시각화
varImpPlot(rf_model)

# xgboost 예측
# XGBoost 패키지 설치
install.packages("xgboost")

# 패키지 로드
library(xgboost)

# Matrix 형태로 변환
X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)
y_train_matrix <- as.matrix(y_train)

# XGBoost 모델 학습
dtrain <- xgb.DMatrix(data = X_train_matrix, label = y_train_matrix)
dtest <- xgb.DMatrix(data = X_test_matrix)

params <- list(
  objective = "reg:squarederror",  # 회귀 문제이므로 제곱 오차 사용
  eta = 0.1,  # 학습률
  max_depth = 9,  # 트리 깊이
  subsample = 0.8,  # 데이터 샘플 비율
  colsample_bytree = 0.8  # 피처 샘플링 비율
)

# 모델 학습
xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100,  # 트리 생성 수
  watchlist = list(train = dtrain),
  verbose = 1
)

# 테스트 데이터에 대한 예측
y_pred_test_xgb <- predict(xgb_model, dtest)

# 예측 결과를 sample_data에 저장
sample_data$count <- y_pred_test_xgb
# 예측값이 음수일 경우 0으로 변환
y_pred_test_xgb <- pmax(y_pred_test_xgb, 0)

# CSV 파일로 저장
write.csv(sample_data, "xgboost_submission1.csv", row.names = FALSE)
