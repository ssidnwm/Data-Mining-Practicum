library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
install.packages("caret")


phone <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/train.csv")
head(phone)
summary(phone)
phone$Activity


# Target 변수 변환 (factor 형태로 변환)
phone$Activity <- as.factor(phone$Activity)

# Train/Validation Split (80:20 비율)
set.seed(2024)  # 결과 재현성을 위해 시드 설정
trainIndex <- createDataPartition(phone$Activity, p = 0.8, list = FALSE)
trainData <- phone[trainIndex, ]
validationData <- phone[-trainIndex, ]

# Logistic Regression 모델 생성 (모든 변수 사용)
logistic_model <- multinom(Activity ~ ., data = trainData)

# Validation Set으로 예측
predictions <- predict(logistic_model, validationData)

# 성능 평가 (Confusion Matrix & Accuracy)
conf_matrix <- confusionMatrix(predictions, validationData$Activity)
print(conf_matrix)

# Accuracy 출력
cat("Validation Accuracy:", conf_matrix$overall["Accuracy"], "\n")

#특성 중요도 확인
library(randomForest)
# Random Forest 모델 학습
rf_model <- randomForest(Activity ~ ., data = trainData, importance = TRUE)

# 특성 중요도 확인
importance <- importance(rf_model)
print(importance)

# 특성별 평균 중요도 계산
importance_df <- as.data.frame(importance)
mean_importance <- rowMeans(importance_df)

# 중요도 높은 순으로 정렬
sorted_importance <- sort(mean_importance, decreasing = TRUE)

# 상위 10개 특성 선택
top_features <- names(sorted_importance[1:10])

# 상위 10개 특성만으로 모델 재학습
trainData_reduced <- trainData[, c(top_features, "Activity")]
validationData_reduced <- validationData[, c(top_features, "Activity")]

# Logistic Regression 재학습
logistic_model_reduced <- multinom(Activity ~ ., data = trainData_reduced, maxit = 1000)

# Validation 예측 및 평가
predictions <- predict(logistic_model_reduced, validationData_reduced)
conf_matrix <- confusionMatrix(predictions, validationData_reduced$Activity)
print(conf_matrix)

###TOp_20개 

# 상위 10개 특성 선택
top_features <- names(sorted_importance[1:20])

# 상위 10개 특성만으로 모델 재학습
trainData_reduced <- trainData[, c(top_features, "Activity")]
validationData_reduced <- validationData[, c(top_features, "Activity")]

# Logistic Regression 재학습
logistic_model_reduced <- multinom(Activity ~ ., data = trainData_reduced, maxit = 1000)

# Validation 예측 및 평가
predictions <- predict(logistic_model_reduced, validationData_reduced)
conf_matrix <- confusionMatrix(predictions, validationData_reduced$Activity)
print(conf_matrix)


### 실제 예측
test<- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/test.csv")
test$Activity

# 2. 테스트 데이터에서 중요 특성만 선택
test_reduced <- test[, c(top_features, "Activity")]  # 중요 특성과 Activity 열 포함

# 3. Activity를 factor로 변환
test_reduced$Activity <- as.factor(test_reduced$Activity)

# 4. 예측 수행
test_predictions <- predict(logistic_model_reduced, test_reduced[, top_features])  # Activity 열 제외

# 5. 성능 평가 (Confusion Matrix)
test_conf_matrix <- confusionMatrix(test_predictions, test_reduced$Activity)
print(test_conf_matrix)

# 6. 정확도 출력
cat("Test Accuracy:", test_conf_matrix$overall["Accuracy"], "\n")
