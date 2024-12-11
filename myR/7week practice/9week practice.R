library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
install.packages("recommenderlab")
library(recommenderlab)

book_train <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/dataset for recommendation watcha/book_train.csv",
                       fileEncoding = "CP949", # 또는 "EUC-KR"
                       stringsAsFactors = FALSE)
book_test <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/dataset for recommendation watcha/book_test.csv",
                      fileEncoding = "CP949", # 또는 "EUC-KR"
                      stringsAsFactors = FALSE)



#Task2: 평점 비교

book_train<-book_train %>%
  group_by(book_code)%>%
  mutate(reviews = n())

head(book_train)  
book_train<-book_train %>%
  group_by(book_code) %>%
  mutate(avg_score = mean(score))%>%
  arrange(desc(avg_score))%>%
  distinct(book_code, .keep_all = TRUE)

book_train %>%
  filter(reviews > 50)%>%
  arrange(desc(avg_score))

book_train%>%
  arrange(desc(reviews))
  


ggplot(book_train, aes(x = reviews, y = avg_score, label = book_title)) +
  geom_point(aes(color = avg_score), size = 3) + # 각 책을 점으로 표시, 평균 평점에 따라 색상 지정
  scale_color_gradient(low = "red", high = "blue") + # 색상 변화 (평균 점수에 따라)
  labs(title = "Books by Reviews and Average Score",
       x = "Number of Reviews",
       y = "Average Score") +
  theme_minimal()


top_books <- book_train %>%
  filter(reviews < 10) %>%  # 리뷰 수가 적은 책 필터링 (예: 리뷰가 10 미만인 책)
  arrange(desc(avg_score)) %>%  # 평균 평점이 높은 순서로 정렬
  head(10)  # 상위 10개 도서 선택
top_books

# 평점이 낮고 리뷰가 많은 상위 10개 도서 찾기
bottom_books <- book_train %>%
  filter(avg_score < 3) %>%  # 평균 평점이 낮은 도서 필터링 (예: 평점이 3 미만인 책)
  arrange(desc(reviews)) %>%  # 리뷰 수가 많은 순서로 정렬
  head(10)  # 상위 10개 도서 선택

bottom_books

#예측하기
# 1. train 데이터와 test 데이터를 결합
combined_data <- rbind(book_train, book_test)

# 2. test 데이터의 평점을 NA로 설정
combined_data[combined_data$user_code %in% book_test$user_code, "score"] <- NA

# 3. 결합된 데이터를 realRatingMatrix로 변환
combined_matrix <- as(combined_data, "realRatingMatrix")

# 4. UBCF 모델 학습
ubcf_model <- Recommender(combined_matrix, method = "UBCF")

# 5. NA로 설정된 테스트 데이터의 평점을 예측
ubcf_prediction <- predict(ubcf_model, combined_matrix, type = "ratings")

# 6. 예측된 평점을 행렬 형식으로 변환

predicted_ratings <- as(ubcf_prediction, "matrix")

# 7. 테스트 데이터에 해당하는 예측된 평점만 추출
# book_test의 user_code와 book_code에 해당하는 예측 평점만 필터링
test_users <- book_test$user_code
test_books <- book_test$book_code

# 테스트 데이터의 유저와 책 코드에 해당하는 예측 평점을 정확히 매칭하여 추출
predicted_test_ratings <- matrix(NA, nrow = length(test_users), ncol = 1)
for (i in 1:length(test_users)) {
  user <- test_users[i]
  book <- test_books[i]
  predicted_test_ratings[i] <- predicted_ratings[user, book]
}

# 8. 실제 test 데이터에서 평점 추출
# 실제 평점도 test 데이터에 맞게 추출하여 크기 일치
actual_test_ratings <- book_test$score

# 9. MAE 계산 함수 정의
calculate_mae <- function(predicted, actual) {
  # 결측값(NA)이 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)
  
  # 유효한 비교 값이 없을 경우 NA 반환
  if (sum(valid_idx) == 0) {
    return(NA)
  }
  
  # 실제 값과 예측된 값의 차이의 절대값을 계산한 후 평균
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))
  return(mae)
}

# 10. 예측된 평점과 실제 평점 간의 MAE 계산 (필터링된 데이터만 처리)
mae <- calculate_mae(predicted_test_ratings, actual_test_ratings)

# 11. 결과 출력
print(paste("MAE:", mae))

print(sum(is.na(predicted_test_ratings)))  # 예측된 평점에서 NA 개수
print(sum(is.na(actual_test_ratings)))  # 실제 평점에서 NA 개수

# 예측된 평점과 실제 평점에서 유효한 값 확인
print(sum(!is.na(predicted_test_ratings) & !is.na(actual_test_ratings)))  # 비교 가능한 데이터 수


# IBCF
# 1. train 데이터와 test 데이터를 결합
combined_data <- rbind(book_train, book_test)

# 2. test 데이터의 평점을 NA로 설정
combined_data[combined_data$user_code %in% book_test$user_code, "score"] <- NA

# 3. 결합된 데이터를 realRatingMatrix로 변환
combined_matrix <- as(combined_data, "realRatingMatrix")

# 4. IBCF 모델 학습
ibcf_model <- Recommender(combined_matrix, method = "IBCF")

# 5. NA로 설정된 테스트 데이터의 평점을 예측
ibcf_prediction <- predict(ibcf_model, combined_matrix, type = "ratings")

# 6. 예측된 평점을 행렬 형식으로 변환
predicted_ratings <- as(ibcf_prediction, "matrix")

# 7. 테스트 데이터에 해당하는 예측된 평점만 추출
test_users <- book_test$user_code
test_books <- book_test$book_code

# 테스트 데이터의 유저와 책 코드에 해당하는 예측 평점을 정확히 매칭하여 추출
predicted_test_ratings <- matrix(NA, nrow = length(test_users), ncol = 1)
for (i in 1:length(test_users)) {
  user <- test_users[i]
  book <- test_books[i]
  predicted_test_ratings[i] <- predicted_ratings[user, book]
}

# 8. 실제 test 데이터에서 평점 추출
actual_test_ratings <- book_test$score

# 9. MAE 계산 함수 정의
calculate_mae <- function(predicted, actual) {
  # 결측값(NA)이 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)
  
  # 유효한 비교 값이 없을 경우 NA 반환
  if (sum(valid_idx) == 0) {
    return(NA)
  }
  
  # 실제 값과 예측된 값의 차이의 절대값을 계산한 후 평균
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))
  return(mae)
}

# 10. 예측된 평점과 실제 평점 간의 MAE 계산
mae <- calculate_mae(predicted_test_ratings, actual_test_ratings)

# 11. 결과 출력
print(paste("MAE:", mae))

# 예측된 평점과 실제 평점에서 결측값 확인
print(sum(is.na(predicted_test_ratings)))  # 예측된 평점에서 NA 개수
print(sum(is.na(actual_test_ratings)))  # 실제 평점에서 NA 개수

# 예측된 평점과 실제 평점에서 유효한 값 확인
print(sum(!is.na(predicted_test_ratings) & !is.na(actual_test_ratings)))  # 비교 가능한 데이터 수


# 테스트
# 1. train 데이터를 realRatingMatrix로 변환
train_matrix <- as(book_train, "realRatingMatrix")

# 2. realRatingMatrix를 일반 행렬로 변환하여 결측값 확인
train_data_matrix <- as(train_matrix, "matrix")

# 3. NA 값의 개수 세기
na_count <- sum(is.na(train_data_matrix))

# 4. NA 개수 출력
print(paste("Number of NA values in train data:", na_count))


# book_train과 book_test에서 book_code가 동일한 항목을 추출
common_books <- intersect(book_train$book_code, book_test$book_code)

# 동일한 book_code 개수 세기
common_books_count <- length(common_books)

# 결과 출력
print(paste("Number of common book_code between train and test:", common_books_count))


# 1. train 데이터를 realRatingMatrix로 변환
train_matrix <- as(book_train, "realRatingMatrix")

# 2. realRatingMatrix를 일반 행렬로 변환
train_data_matrix <- as(train_matrix, "matrix")

# NA 값이 75% 이상인 행과 열을 제거하는 기준으로 수정
threshold <- 0.75

# 1. 열에서 NA 값이 75% 이상인 열을 먼저 제거
cols_to_keep <- colSums(is.na(train_data_matrix)) <= (nrow(train_data_matrix) * threshold)
filtered_matrix_by_cols <- train_data_matrix[, cols_to_keep]

# 2. 행에서 NA 값이 75% 이상인 행을 제거
rows_to_keep <- rowSums(is.na(filtered_matrix_by_cols)) <= (ncol(filtered_matrix_by_cols) * threshold)
filtered_matrix_by_rows <- filtered_matrix_by_cols[rows_to_keep, ]



# 5. 결과 확인
print(paste("Number of rows after filtering:", nrow(filtered_matrix_by_rows)))
print(paste("Number of columns after filtering:", ncol(filtered_matrix_by_rows)))






# 이거 안되면 걍 포기함 ㅈㅈ
# 1. train 데이터와 test 데이터를 병합
combined_data <- rbind(book_train, book_test)

# 2. train에 없는 사용자는 test에서 제거
train_users <- unique(book_train$user_code)
combined_data <- combined_data[combined_data$user_code %in% train_users, ]

# 3. train에 없는 책(book_code)도 test에서 제거
train_books <- unique(book_train$book_code)
combined_data <- combined_data[combined_data$book_code %in% train_books, ]

# 4. 병합된 데이터를 realRatingMatrix로 변환
combined_matrix <- as(combined_data, "realRatingMatrix")

# 5. train 데이터를 realRatingMatrix로 변환
train_matrix <- as(book_train, "realRatingMatrix")

# 6. 병합된 데이터와 train 데이터에서 동일한 행을 비교 및 제거
train_data_matrix <- as(train_matrix, "matrix")
combined_data_matrix <- as(combined_matrix, "matrix")

# 병합된 데이터와 train 데이터의 행을 비교하여 동일한 행을 제거
different_rows <- !(apply(combined_data_matrix, 1, function(x) any(apply(train_data_matrix, 1, function(y) all(x == y)))))
filtered_combined_matrix <- combined_data_matrix[different_rows, ]

# 7. IBCF 모델 학습 (필터링된 병합된 데이터를 이용)
ibcf_model <- Recommender(as(filtered_combined_matrix, "realRatingMatrix"), method = "IBCF")

# 8. NA로 설정된 테스트 데이터의 평점을 예측
ibcf_prediction <- predict(ibcf_model, as(filtered_combined_matrix, "realRatingMatrix"), type = "ratings")

# 9. 예측된 평점을 행렬 형식으로 변환
predicted_ratings <- as(ibcf_prediction, "matrix")

# 10. test 데이터에서 사용자의 user_code와 book_code에 해당하는 예측 평점을 추출
test_users <- book_test$user_code
test_books <- book_test$book_code

# 사용자와 책 코드를 인덱스화하기 위해 행과 열의 이름을 가져옴
user_indices <- match(test_users, rownames(predicted_ratings))  # 사용자 코드가 predicted_ratings의 행에서 어떤 인덱스를 차지하는지 찾음
book_indices <- match(test_books, colnames(predicted_ratings))  # 책 코드가 predicted_ratings의 열에서 어떤 인덱스를 차지하는지 찾음

# 테스트 데이터의 유저와 책 코드에 해당하는 예측 평점을 추출
predicted_test_ratings <- matrix(NA, nrow = length(test_users), ncol = 1)
for (i in 1:length(test_users)) {
  user_index <- user_indices[i]
  book_index <- book_indices[i]
  
  # 예측된 평점에서 인덱스를 사용해 값 추출
  if (!is.na(user_index) && !is.na(book_index)) {
    predicted_test_ratings[i] <- predicted_ratings[user_index, book_index]
  } else {
    predicted_test_ratings[i] <- NA  # 유효하지 않은 인덱스의 경우 NA
  }
}
print(predicted_test_ratings[1:10])  # 예측된 평점 일부 출력
print(head(user_indices))  # 사용자 인덱스 확인
print(head(book_indices))  # 책 인덱스 확인
# 11. 실제 test 데이터에서 평점 추출
actual_test_ratings <- book_test$score

# 12. MAE 계산 함수 정의
calculate_mae <- function(predicted, actual) {
  valid_idx <- !is.na(predicted) & !is.na(actual)
  if (sum(valid_idx) == 0) {
    return(NA)
  }
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))
  return(mae)
}

# 13. 예측된 평점과 실제 평점 간의 MAE 계산
mae <- calculate_mae(predicted_test_ratings, actual_test_ratings)

# 14. 결과 출력
print(paste("MAE:", mae))


### 다시시자아아악
# 1. 각 book_code에 대해 평점을 남긴 횟수를 기준으로 상위 30개의 book_code 추출
top_30_books <- book_train %>%
  group_by(book_code) %>%
  summarise(total_reviews = n()) %>%  # 책별 평점을 남긴 횟수(리뷰 수) 계산
  arrange(desc(total_reviews)) %>%  # 리뷰 수 기준 내림차순 정렬
  head(30) %>%
  select(book_code)

# 2. 상위 30개의 책 중에서 15개 이상 평가한 사용자 추출
users_with_15_books <- book_train %>%
  filter(book_code %in% top_30_books$book_code) %>%
  group_by(user_code) %>%
  summarise(book_count = n_distinct(book_code)) %>%  # 각 사용자가 평가한 고유한 책 개수 계산
  filter(book_count >= 15) %>%  # 15개 이상 평가한 사용자만 필터링
  select(user_code)

# 결과 확인
print(top_30_books)
print(users_with_15_books)

# 1. top_30_books와 users_with_15_books로 필터링된 데이터 추출
filtered_data <- book_train %>%
  filter(book_code %in% top_30_books$book_code, user_code %in% users_with_15_books$user_code)

# 2. 필요한 열 (user_code, book_code, score)만 선택
matrix_data <- filtered_data %>%
  select(user_code, book_code, score)

# 1. matrix_data의 user_code와 동일한 book_test 데이터 필터링
test_data_selected <- book_test %>%
  filter(user_code %in% matrix_data$user_code) %>%
  select(user_code, book_code, score)

# 2. rbind로 matrix_data와 test 데이터 결합
combined_data <- rbind(matrix_data, test_data_selected)

# 결과 확인
head(combined_data)


#악
# 1. matrix_data를 realRatingMatrix로 변환
rating_matrix <- as(matrix_data, "realRatingMatrix")

# 결과 확인
print(rating_matrix)

# 1. realRatingMatrix를 일반 매트릭스로 변환
rating_matrix_as_matrix <- as(rating_matrix, "matrix")

# 2. 매트릭스를 데이터프레임으로 변환
rating_matrix_as_df <- as.data.frame(rating_matrix_as_matrix)

# 결과 확인
View(rating_matrix_as_df)  # 데이터프레임으로 보기

# 1. book_test에서 user_code가 "US000003"인 데이터 필터링
test_user_data <- book_test %>%
  filter(user_code == "US000003") %>%
  select(book_code, score)

# 2. rating_matrix_as_matrix에 열 추가 (test_user_data의 book_code를 기준으로)
for (i in 1:nrow(test_user_data)) {
  book_code <- test_user_data$book_code[i]  # book_code 가져오기
  
  # 만약 해당 book_code가 rating_matrix에 없다면, 새로운 열 추가
  if (!book_code %in% colnames(rating_matrix_as_matrix)) {
    rating_matrix_as_matrix <- cbind(rating_matrix_as_matrix, NA)  # 새로운 열 추가 (NA로 초기화)
    colnames(rating_matrix_as_matrix)[ncol(rating_matrix_as_matrix)] <- book_code  # 새 열 이름 설정
  }
  
  # US000003 사용자의 해당 book_code에 평점을 NA로 설정
  rating_matrix_as_matrix["US000003", book_code] <- NA
}

# 결과 확인
View(rating_matrix_as_matrix)

# 1. rating_matrix_as_matrix를 realRatingMatrix로 변환
rating_matrix_real <- as(rating_matrix_as_matrix, "realRatingMatrix")

# 2. UBCF 모델 학습 (method = "UBCF")
ubcf_model <- Recommender(rating_matrix_real, method = "UBCF")

# 3. 특정 사용자(US000003)에 대해 평점 예측
predicted_ratings_us000003 <- predict(ubcf_model, rating_matrix_real["US000003", ], type = "ratings")

# 4. 예측된 평점을 매트릭스로 변환
predicted_ratings_us000003_matrix <- as(predicted_ratings_us000003, "matrix")

# 5. 결과 확인 (US000003에 대한 예측된 평점)
print(predicted_ratings_us000003_matrix)
View(predicted_ratings_us000003_matrix)




#사람에 대한 예측
# 1. US000003의 train 데이터와 test 데이터에서 평가한 book_code 추출
us000003_books <- matrix_data %>%
  filter(user_code == "US000003") %>%
  select(book_code) %>%
  distinct()

# 2. US000003과 10개 이상의 book_code를 평가한 다른 사용자 찾기
similar_users <- matrix_data %>%
  filter(book_code %in% us000003_books$book_code) %>%
  group_by(user_code) %>%
  summarise(common_books = n_distinct(book_code)) %>%
  filter(common_books >= 10) %>%  # 10개 이상의 공통된 책을 평가한 사용자
  select(user_code)

# 3. similar_users의 데이터를 기존 rating_matrix_real에 추가
similar_users_data <- matrix_data %>%
  filter(user_code %in% similar_users$user_code)

# 4. similar_users와 US000003의 데이터를 하나의 매트릭스로 합침
combined_data <- rbind(matrix_data, similar_users_data)

# 5. combined_data를 realRatingMatrix로 변환
combined_rating_matrix <- as(combined_data, "realRatingMatrix")

# 6. UBCF 모델 학습
ubcf_model <- Recommender(combined_rating_matrix, method = "UBCF")

# 7. US000003에 대한 평점 예측
predicted_ratings_us000003 <- predict(ubcf_model, combined_rating_matrix["US000003", ], type = "ratings")

# 8. 예측된 평점을 매트릭스로 변환
predicted_ratings_us000003_matrix <- as(predicted_ratings_us000003, "matrix")

# 결과 확인
View(predicted_ratings_us000003_matrix)
View()



## 다시 마지막 US000003예측
# 1. train 데이터와 test 데이터를 병합
combined_data <- rbind(book_train, book_test)

# 2. US000003의 book_code 추출 (train + test 데이터에서 평가한 모든 책)
us000003_books <- combined_data %>%
  filter(user_code == "US000003") %>%
  select(book_code) %>%
  distinct()

# 3. US000003과 가장 많이 공통 책을 읽은 상위 30명의 사용자 찾기
similar_users <- combined_data %>%
  filter(book_code %in% us000003_books$book_code) %>%
  group_by(user_code) %>%
  summarise(common_books = n_distinct(book_code)) %>%
  arrange(desc(common_books)) %>%
  filter(user_code != "US000003") %>%
  head(30) %>%
  select(user_code)

# 4. similar_users와 US000003의 데이터를 필터링
similar_users_data <- combined_data %>%
  filter(user_code %in% similar_users$user_code | user_code == "US000003")

# 5. US000003의 test 데이터 점수를 NA로 변환
us000003_test_na <- book_test %>%
  filter(user_code == "US000003") %>%
  mutate(score = NA)

# 6. combined_data에 test 데이터에서 NA 처리된 US000003의 데이터 추가
combined_data_na <- rbind(similar_users_data, us000003_test_na)

# 7. realRatingMatrix로 변환
rating_matrix_real <- as(combined_data_na, "realRatingMatrix")

# 8. realRatingMatrix를 매트릭스로 변환
rating_matrix <- as(rating_matrix_real, "matrix")

# 9. NA 비율이 특정 비율 이상인 열(책) 삭제 (예: NA가 50% 이상인 열 삭제)
na_threshold <- 0.5  # 50% 이상 NA가 있는 열을 삭제하기 위한 기준
cols_to_keep <- colSums(is.na(rating_matrix)) / nrow(rating_matrix) < na_threshold
filtered_rating_matrix <- rating_matrix[, cols_to_keep]

# 10. 결과 확인 (필터링된 매트릭스)
View(filtered_rating_matrix)

# 1. 필터링된 매트릭스를 realRatingMatrix로 변환
filtered_rating_matrix_real <- as(filtered_rating_matrix, "realRatingMatrix")

# 2. UBCF 모델 학습
ubcf_model_filtered <- Recommender(filtered_rating_matrix_real, method = "UBCF")

# 3. US000003에 대한 평점 예측
predicted_ratings_us000003_filtered <- predict(ubcf_model_filtered, filtered_rating_matrix_real["US000003", ], type = "ratings")

# 4. 예측된 평점을 매트릭스로 변환
predicted_ratings_us000003_filtered_matrix <- as(predicted_ratings_us000003_filtered, "matrix")

# 5. 결과 확인 (US000003에 대한 예측된 평점)
View(predicted_ratings_us000003_filtered_matrix)
# 1. US000003에 대한 예측된 평점 추출 (이미 수행된 상태)
predicted_test_books_filtered <- predicted_ratings_us000003_filtered_matrix

# 2. book_test에서 US000003의 실제 평점 데이터 추출
actual_ratings_us000003 <- book_test %>%
  filter(user_code == "US000003") %>%
  select(book_code, score)

# 3. 예측된 책과 실제 책의 book_code를 비교하여 공통된 book_code 필터링
common_books <- intersect(colnames(predicted_test_books_filtered), actual_ratings_us000003$book_code)

# 4. 공통된 book_code에 대해 예측된 평점과 실제 평점 필터링
predicted_test_books_filtered_common <- predicted_test_books_filtered[, common_books, drop = FALSE]
actual_ratings_filtered_common <- actual_ratings_us000003 %>%
  filter(book_code %in% common_books)

# 5. 예측된 값과 실제 값 비교 데이터프레임 생성
comparison <- data.frame(
  book_code = common_books,
  predicted_score = as.vector(predicted_test_books_filtered_common),
  actual_score = actual_ratings_filtered_common$score
)

# 6. 결과 출력
print(comparison)

# MAE 계산 함수 정의
calculate_mae <- function(predicted, actual) {
  # NA가 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)  # NA가 아닌 유효한 값들만 필터링
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))  # 절대값 차이의 평균 계산
  return(mae)
}

# comparison 데이터프레임에서 예측값과 실제값을 추출하여 MAE 계산
mae <- calculate_mae(comparison$predicted_score, comparison$actual_score)

# MAE 결과 출력
print(paste("Mean Absolute Error (MAE):", mae))




## 다른 사람에 대해서 예측
# 1. train 데이터와 test 데이터를 병합
combined_data <- rbind(book_train, book_test)

# 2. US000003의 book_code 추출 (train + test 데이터에서 평가한 모든 책)
us000003_books <- combined_data %>%
  filter(user_code == "US000009") %>%
  select(book_code) %>%
  distinct()

# 3. US000003과 가장 많이 공통 책을 읽은 상위 30명의 사용자 찾기
similar_users <- combined_data %>%
  filter(book_code %in% us000003_books$book_code) %>%
  group_by(user_code) %>%
  summarise(common_books = n_distinct(book_code)) %>%
  arrange(desc(common_books)) %>%
  filter(user_code != "US000009") %>%
  head(30) %>%
  select(user_code)

# 4. similar_users와 US000003의 데이터를 필터링
similar_users_data <- combined_data %>%
  filter(user_code %in% similar_users$user_code | user_code == "US000009")

# 5. US000003의 test 데이터 점수를 NA로 변환
us000003_test_na <- book_test %>%
  filter(user_code == "US000009") %>%
  mutate(score = NA)

# 6. combined_data에 test 데이터에서 NA 처리된 US000003의 데이터 추가
combined_data_na <- rbind(similar_users_data, us000003_test_na)

# 7. realRatingMatrix로 변환
rating_matrix_real <- as(combined_data_na, "realRatingMatrix")

# 8. realRatingMatrix를 매트릭스로 변환
rating_matrix <- as(rating_matrix_real, "matrix")

# 9. NA 비율이 특정 비율 이상인 열(책) 삭제 (예: NA가 50% 이상인 열 삭제)
na_threshold <- 0.5  # 50% 이상 NA가 있는 열을 삭제하기 위한 기준
cols_to_keep <- colSums(is.na(rating_matrix)) / nrow(rating_matrix) < na_threshold
filtered_rating_matrix <- rating_matrix[, cols_to_keep]

# 10. 결과 확인 (필터링된 매트릭스)
View(filtered_rating_matrix)

# 1. 필터링된 매트릭스를 realRatingMatrix로 변환
filtered_rating_matrix_real <- as(filtered_rating_matrix, "realRatingMatrix")

# 2. UBCF 모델 학습
ubcf_model_filtered <- Recommender(filtered_rating_matrix_real, method = "UBCF")

# 3. US000003에 대한 평점 예측
predicted_ratings_us000003_filtered <- predict(ubcf_model_filtered, filtered_rating_matrix_real["US000009", ], type = "ratings")

# 4. 예측된 평점을 매트릭스로 변환
predicted_ratings_us000003_filtered_matrix <- as(predicted_ratings_us000003_filtered, "matrix")

# 5. 결과 확인 (US000003에 대한 예측된 평점)
View(predicted_ratings_us000003_filtered_matrix)
# 1. US000003에 대한 예측된 평점 추출 (이미 수행된 상태)
predicted_test_books_filtered <- predicted_ratings_us000003_filtered_matrix

# 2. book_test에서 US000003의 실제 평점 데이터 추출
actual_ratings_us000003 <- book_test %>%
  filter(user_code == "US000009") %>%
  select(book_code, score)

# 3. 예측된 책과 실제 책의 book_code를 비교하여 공통된 book_code 필터링
common_books <- intersect(colnames(predicted_test_books_filtered), actual_ratings_us000003$book_code)

# 4. 공통된 book_code에 대해 예측된 평점과 실제 평점 필터링
predicted_test_books_filtered_common <- predicted_test_books_filtered[, common_books, drop = FALSE]
actual_ratings_filtered_common <- actual_ratings_us000003 %>%
  filter(book_code %in% common_books)

# 5. 예측된 값과 실제 값 비교 데이터프레임 생성
comparison <- data.frame(
  book_code = common_books,
  predicted_score = as.vector(predicted_test_books_filtered_common),
  actual_score = actual_ratings_filtered_common$score
)

# 6. 결과 출력
print(comparison)

# MAE 계산 함수 정의
calculate_mae <- function(predicted, actual) {
  # NA가 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)  # NA가 아닌 유효한 값들만 필터링
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))  # 절대값 차이의 평균 계산
  return(mae)
}

# comparison 데이터프레임에서 예측값과 실제값을 추출하여 MAE 계산
mae <- calculate_mae(comparison$predicted_score, comparison$actual_score)

# MAE 결과 출력
print(paste("Mean Absolute Error (MAE):", mae))





## K값 조정으로
# 1. train 데이터를 realRatingMatrix로 변환
train_matrix <- as(book_train, "realRatingMatrix")

# 2. UBCF 모델 학습 (train 데이터만 사용)
total_users_train <- nrow(train_matrix)  # train 데이터의 전체 사용자 수
k_value_train <- round(total_users_train * 0.1)  # train 데이터의 사용자 수의 10%를 K값으로 설정

ubcf_model_train <- Recommender(train_matrix, method = "UBCF", parameter = list(nn = k_value_train))

# 3. train 데이터에 대한 예측 수행 (train 데이터로 학습된 모델로 train 데이터에 대해 예측)
ubcf_prediction_train <- predict(ubcf_model_train, train_matrix, type = "ratings")

# 4. 예측된 평점을 행렬 형식으로 변환 (train 데이터에서 예측된 결과)
predicted_ratings_train <- as(ubcf_prediction_train, "matrix")
View(predicted_ratings_train)
# 5. 테스트 데이터에 해당하는 예측된 평점만 추출
# book_test의 user_code와 book_code에 해당하는 예측 평점만 필터링
test_users <- book_test$user_code
test_books <- book_test$book_code

# 테스트 데이터의 유저와 책 코드에 해당하는 예측 평점을 정확히 매칭하여 추출
predicted_test_ratings <- matrix(NA, nrow = length(test_users), ncol = 1)
for (i in 1:length(test_users)) {
  user <- test_users[i]
  book <- test_books[i]
  # train에서 예측된 값 중, test 데이터와 동일한 user와 book에 대한 예측값 추출
  if (!is.na(predicted_ratings_train[user, book])) {
    predicted_test_ratings[i] <- predicted_ratings_train[user, book]
  }
}
View(predicted_test_ratings)
# 6. 실제 test 데이터에서 평점 추출
actual_test_ratings <- book_test$score

# 7. MAE 계산 함수 정의 (이전과 동일)
calculate_mae <- function(predicted, actual) {
  # 결측값(NA)이 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)
  
  # 유효한 비교 값이 없을 경우 NA 반환
  if (sum(valid_idx) == 0) {
    return(NA)
  }
  
  # 실제 값과 예측된 값의 차이의 절대값을 계산한 후 평균
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))
  return(mae)
}

# 8. 예측된 평점과 실제 평점 간의 MAE 계산 (필터링된 데이터만 처리)
mae <- calculate_mae(predicted_test_ratings, actual_test_ratings)

# 9. 결과 출력
print(paste("MAE:", mae))




### IBCF
# 1. train 데이터를 realRatingMatrix로 변환
train_matrix <- as(book_train, "realRatingMatrix")

# 2. IBCF 모델 학습 (train 데이터만 사용)
ibcf_model_train <- Recommender(train_matrix, method = "IBCF")

# 3. train 데이터에 대한 예측 수행 (train 데이터로 학습된 모델로 train 데이터에 대해 예측)
ibcf_prediction_train <- predict(ibcf_model_train, train_matrix, type = "ratings")

# 4. 예측된 평점을 행렬 형식으로 변환 (train 데이터에서 예측된 결과)
predicted_ratings_train_ibcf <- as(ibcf_prediction_train, "matrix")

# 5. 테스트 데이터에 해당하는 예측된 평점만 추출
# book_test의 user_code와 book_code에 해당하는 예측 평점만 필터링
test_users <- book_test$user_code
test_books <- book_test$book_code

# 테스트 데이터의 유저와 책 코드에 해당하는 예측 평점을 정확히 매칭하여 추출
predicted_test_ratings_ibcf <- matrix(NA, nrow = length(test_users), ncol = 1)
for (i in 1:length(test_users)) {
  user <- test_users[i]
  book <- test_books[i]
  # train에서 예측된 값 중, test 데이터와 동일한 user와 book에 대한 예측값 추출
  if (!is.na(predicted_ratings_train_ibcf[user, book])) {
    predicted_test_ratings_ibcf[i] <- predicted_ratings_train_ibcf[user, book]
  }
}

# 6. 실제 test 데이터에서 평점 추출
actual_test_ratings <- book_test$score

# 7. MAE 계산 함수 정의 (이전과 동일)
calculate_mae <- function(predicted, actual) {
  # 결측값(NA)이 아닌 값들만 비교
  valid_idx <- !is.na(predicted) & !is.na(actual)
  
  # 유효한 비교 값이 없을 경우 NA 반환
  if (sum(valid_idx) == 0) {
    return(NA)
  }
  
  # 실제 값과 예측된 값의 차이의 절대값을 계산한 후 평균
  mae <- mean(abs(predicted[valid_idx] - actual[valid_idx]))
  return(mae)
}

# 8. 예측된 평점과 실제 평점 간의 MAE 계산 (IBCF 모델 결과에 대해 처리)
mae_ibcf <- calculate_mae(predicted_test_ratings_ibcf, actual_test_ratings)

# 9. 결과 출력
print(paste("MAE (IBCF):", mae_ibcf))
