library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
install.packages("tidytext")
# URL에서 텍스트 파일을 읽기
# 헤더가 없는 경우
bible1 <- readLines("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/NIV_English_Bible/01-Genesis.txt")
bible2 <- readLines("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/NIV_English_Bible/02-Exodus.txt")
bible3 <- readLines("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/NIV_English_Bible/02-Leviticus.txt")

# 첫 몇 줄 출력
head(bible1)

bible <- data_frame(line =1:4, text = bible1)
# 'bible1'이 1533줄이므로 이에 맞게 line 열 생성
bible <- tibble(line = 1:length(bible1), text = bible1)

# 결과 확인
print(bible)

# 책 이름은 첫 번째 행에 있음
book_name <- bible$text[1]

# 나머지 줄 (Chapter:Verse와 Script 포함) 추출
script_lines <- bible$text[-1]

# 데이터프레임을 만들기 위한 빈 벡터 초기화
chapter <- c()
verse <- c()
script <- c()

# 각 줄을 분석해서 "Chapter", "Verse", "Script"로 분리
for (line in script_lines) {
  # 정규식을 사용해 Chapter:Verse와 나머지 Script 부분 분리
  match <- regexpr("^(\\d+):(\\d+)\\s(.+)$", line, perl=TRUE)
  
  # 패턴이 맞는 경우에만 처리
  if (match[1] != -1) {
    # Chapter:Verse 추출
    chapter_verse <- regmatches(line, regexpr("^(\\d+):(\\d+)", line))
    chapter_verse_split <- strsplit(chapter_verse, ":")[[1]]
    
    # Chapter와 Verse로 나누기
    chapter <- c(chapter, as.numeric(chapter_verse_split[1]))
    verse <- c(verse, as.numeric(chapter_verse_split[2]))
    
    # Script 추출 (정규식으로 나머지 텍스트 추출)
    script <- c(script, regmatches(line, regexpr("\\s(.+)$", line)))
  }
}

# 데이터프레임 생성
bible_df <- tibble(
  Book = book_name,
  Chapter = chapter,
  Verse = verse,
  Script = script
)

# 결과 확인
print(bible_df)

#정규식을 사용하는 것 까지는 맞는데, 이렇게 한장한장을 읽고 바꾸는 것은 너무 비효율적
# 폴더 경로 지정
folder_path <- "C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/NIV_English_Bible/"

# 폴더 내의 모든 텍스트 파일을 리스트로 가져오기
file_list <- list.files(folder_path, pattern = "*.txt", full.names = TRUE)

# 빈 리스트 생성
bible_texts <- list()

# 파일 리스트를 반복하면서 파일 읽기
for (file in file_list) {
  # 파일 내용 읽기
  bible_content <- readLines(file, encoding = "UTF-8")
  
  # 파일 이름(책 이름) 추출
  book_name <- tools::file_path_sans_ext(basename(file))  # 확장자 제거
  
  # 리스트에 저장
  bible_texts[[book_name]] <- bible_content
}

# 결과 확인 (책 이름과 첫 몇 줄 출력)
for (book in names(bible_texts)) {
  cat("Book:", book, "\n")
  cat("Content:", head(bible_texts[[book]]), "\n\n")
}

# 최종 데이터를 저장할 데이터프레임
bible_df <- tibble(Book = character(), Chapter = numeric(), Verse = numeric(), Script = character())

# 각 파일을 처리하여 데이터프레임에 추가
for (book_name in names(bible_texts)) {
  script_lines <- bible_texts[[book_name]][-1]  # 첫 줄(책 이름)을 제외한 나머지
  
  # 각 줄을 분석해서 "Chapter", "Verse", "Script"로 분리
  chapter <- c()
  verse <- c()
  script <- c()
  
  for (line in script_lines) {
    # 정규식으로 Chapter:Verse와 Script 분리
    match <- regexpr("^(\\d+):(\\d+)\\s(.+)$", line, perl=TRUE)
    
    if (match[1] != -1) {
      # Chapter와 Verse 추출
      chapter_verse <- regmatches(line, regexpr("^(\\d+):(\\d+)", line))
      chapter_verse_split <- strsplit(chapter_verse, ":")[[1]]
      chapter <- c(chapter, as.numeric(chapter_verse_split[1]))
      verse <- c(verse, as.numeric(chapter_verse_split[2]))
      
      # Script 부분 추출
      script <- c(script, regmatches(line, regexpr("\\s(.+)$", line)))
    }
  }
  
  # 각 파일의 결과를 데이터프레임으로 추가
  temp_df <- tibble(
    Book = rep(book_name, length(chapter)),
    Chapter = chapter,
    Verse = verse,
    Script = script
  )
  
  # 최종 데이터프레임에 추가
  bible_df <- bind_rows(bible_df, temp_df)
}

# 데이터프레임 확인
print(bible_df)

# Book 변수에서 숫자와 하이픈 제거
bible_df$Book <- gsub("^\\d+-", "", bible_df$Book)

# 결과 확인
print(bible_df)

head(bible_df)



# 구약성서
ot_books <- c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy", "Joshua", "Judges", 
              "Ruth", "Samuel-1", "Samuel-2", "Kings-1", "Kings-2", "Chronicles-1", "Chronicles-2", 
              "Ezra", "Nehemiah", "Esther", "Job", "Psalms", "Proverbs", "Ecclesiastes", 
              "Song of Solomon", "Isaiah", "Jeremiah", "Lamentations", "Ezekiel", "Daniel", 
              "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah", "Nahum", "Habakkuk", 
              "Zephaniah", "Haggai", "Zechariah", "Malachi")

#신약성서
nt_books <- c("Matthew", "Mark", "Luke", "John", "Acts", "Romans", "Corinthians-1", "Corinthians-2", 
              "Galatians", "Ephesians", "Philippians", "Colossians", "Thessalonians-1", 
              "Thessalonians-2", "Timothy-1", "Timothy-2", "Titus", "Philemon", "Hebrews", 
              "James", "Peter-1", "Peter-2", "John-1", "John-2", "John-3", "Jude", "Revelation")








#Task1-2 토큰화 진행
bible_df %>%
  unnest_tokens(word, Script)%>%
  count(word,sort = TRUE)%>%
  top_n(20, n) %>%  
  ggplot(aes(x = word, y = n))+
  geom_point()+
  labs(title = "상위 20개 단어의 빈도수", x = "단어", y = "빈도")


# 구약과 신약을 나눠서 빈도 분석을 시행해보자
bible_df %>%
  unnest_tokens(word, Script)%>%
  filter(Book %in% ot_books) %>% # 책의 순서를 기준으로 필터링
  count(word, sort = TRUE)  %>%
  top_n(20, n) %>%  
  ggplot(aes(x = word, y = n))+
  geom_point()+
  labs(title = "상위 20개 단어의 빈도수", x = "단어", y = "빈도")

unique(bible_df$Book)

bible_df %>%
  unnest_tokens(word, Script)%>%
  filter(Book >= "Matthew" & Book <= "Revelation") %>%  # 책의 순서를 기준으로 필터링
  count(word, sort = TRUE)  %>%
  top_n(20, n) %>%  
  ggplot(aes(x = word, y = n))+
  geom_point()+
  labs(title = "신약 상위 20개 단어의 빈도수", x = "단어", y = "빈도")

# 두 그래프를 합쳐보자
# 구약 성경 (Genesis ~ Malachi) 상위 20개 단어 데이터프레임 생성
ot_top_words <- bible_df %>%
  unnest_tokens(word, Script) 
# 구약 성경 (Genesis ~ Malachi) 상위 20개 단어 데이터프레임 생성
ot_top_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>%  # 구약 범위
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "Old Testament")  # 범주 추가 (구약)

# 신약 성경 (Matthew ~ Revelation) 상위 20개 단어 데이터프레임 생성
nt_top_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% nt_books) %>%  # 신약 범위
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "New Testament")  # 범주 추가 (신약)

# 두 데이터프레임을 결합
top_words <- bind_rows(ot_top_words, nt_top_words)

# 하나의 그래프로 결합하여 시각화
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_point() +
  facet_wrap(~Testament, scales = "free") +  # 구약, 신약으로 구분
  coord_flip() +  # 단어를 보기 쉽게 가로로 회전
  labs(title = "구약 및 신약 상위 20개 단어의 빈도수", 
       x = "단어", y = "빈도") +
  theme_minimal() # 구약 범위
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "Old Testament")  # 범주 추가 (구약)

# 신약 성경 (Matthew ~ Revelation) 상위 20개 단어 데이터프레임 생성
nt_top_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% nt_books) %>%  # 신약 범위
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "New Testament")  # 범주 추가 (신약)

# 두 데이터프레임을 결합
top_words <- bind_rows(ot_top_words, nt_top_words)

# 하나의 그래프로 결합하여 시각화
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_point() +
  facet_wrap(~Testament, scales = "free") +  # 구약, 신약으로 구분
  coord_flip() +  # 단어를 보기 쉽게 가로로 회전
  labs(title = "구약 및 신약 상위 20개 단어의 빈도수", 
       x = "단어", y = "빈도") +
  theme_minimal()



#불용어 제거
data("stop_words")

bible_df %>%
  unnest_tokens(word, Script) %>%
  anti_join(stop_words, by = "word") 

custom_stopwords <- tibble(word = c("the", "and", "of", "to", "you", "in","will","he","a","i","is","his",
                                    "for","they","your","who","my","with","from","him","that","it"))
all_stopwords <- bind_rows(stop_words, custom_stopwords)

bible_df %>%
  unnest_tokens(word, Script)%>%
  filter(Book %in% ot_books) %>%  # 책의 순서를 기준으로 필터링
  anti_join(all_stopwords, by = "word") %>% 
  count(word, sort = TRUE)  %>%
  top_n(20, n) %>%  
  ggplot(aes(x = word, y = n))+
  geom_point()+
  labs(title = "구약 상위 20개 단어의 빈도수", x = "단어", y = "빈도")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bible_df %>%
  unnest_tokens(word, Script)%>%
  filter(Book %in% ot_books) %>%  # 책의 순서를 기준으로 필터링
  anti_join(all_stopwords, by = "word") %>% 
  count(word, sort = TRUE)  %>%
  top_n(20, n) %>%  
  ggplot(aes(x = word, y = n))+
  geom_point()+
  labs(title = "신약 상위 20개 단어의 빈도수", x = "단어", y = "빈도")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#데이터 합치기

ot_top_words1 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book >= "Genesis" & Book <= "Malachi") %>%  # 구약 범위
  anti_join(all_stopwords, by = "word") %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "Old Testament")  # 범주 추가 (구약)

# 신약 성경 (Matthew ~ Revelation) 상위 20개 단어 데이터프레임 생성
nt_top_words1 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book >= "Matthew" & Book <= "Revelation") %>%  # 신약 범위
  anti_join(all_stopwords, by = "word") %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(Testament = "New Testament")  # 범주 추가 (신약)

# 두 데이터프레임을 결합
top_words1 <- bind_rows(ot_top_words1, nt_top_words1)

# 하나의 그래프로 결합하여 시각화
ggplot(top_words1, aes(x = reorder(word, n), y = n)) +
  geom_point() +
  facet_wrap(~Testament, scales = "free") +  # 구약, 신약으로 구분
  coord_flip() +  # 단어를 보기 쉽게 가로로 회전
  labs(title = "구약 및 신약 상위 20개 단어의 빈도수", 
       x = "단어", y = "빈도") +
  theme_minimal()


#구약과 신약 출현빈도 계산
ot_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book >= "Genesis" & Book <= "Malachi") %>%
  count(word) %>%
  filter(n >= 10) 

nt_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book >= "Matthew" & Book <= "Revelation") %>%
  count(word) %>%
  filter(n >= 10) 

# 구약의 전체 단어 수
total_ot_words <- sum(ot_words$n)

# 신약의 전체 단어 수
total_nt_words <- sum(nt_words$n)

# 구약에서의 출현 비율 계산
ot_words <- ot_words %>%
  mutate(prop_ot = n / total_ot_words)  # 구약에서의 출현 비율

# 신약에서의 출현 비율 계산
nt_words <- nt_words %>%
  mutate(prop_nt = n / total_nt_words)  # 신약에서의 출현 비율

# 구약과 신약의 단어 데이터를 병합 (inner join 사용)
word_comparison <- inner_join(ot_words, nt_words, by = "word", suffix = c("_ot", "_nt"))

# 로그 비율 계산
word_comparison <- word_comparison %>%
  mutate(log_ratio = log(prop_nt / prop_ot))  # 로그 비율 계산


# 상위 20개 (신약에서 더 많이 등장하는 단어)
top_20 <- word_comparison %>%
  arrange(desc(log_ratio)) %>%
  head(20)

# 하위 20개 (구약에서 더 많이 등장하는 단어)
bottom_20 <- word_comparison %>%
  arrange(log_ratio) %>%
  head(20)

top_20%>%
  ggplot(aes(x = reorder(word, log_ratio), y = log_ratio))+
  geom_col()+
  coord_flip() +
  labs(title = "신약에서 더 많이 등장하는 상위 20단어",x = "단어",y = "등장 비율")

bottom_20%>%
  ggplot(aes(x = reorder(word, log_ratio), y = log_ratio))+
  geom_col()+
  coord_flip() +
  labs(title = "구약에서 더 많이 등장하는 상위 20단어",x = "단어",y = "등장 비율")

# 결과 확인
print(top_20)
print(bottom_20)




# 구약 단어 등장 횟수 계산
ot_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>% 
  count(word) %>%
  filter(n >= 50)
head(ot_words)
# 신약 단어 등장 횟수 계산
  nt_words <- bible_df %>%
    unnest_tokens(word, Script) %>%
    filter(Book %in% nt_books) %>% 
    count(word) %>%
    filter( n > 50)
head(nt_words)

# 전체 단어 등장 횟수 계산 (구약+신약)
total_words <- bible_df %>%
  unnest_tokens(word, Script) %>%
  count(word)%>%
  filter(n > 50)
head(total_words)
# 구약과 신약에서 등장하는 단어를 병합 (각 단어의 구약, 신약 빈도수를 함께)

word_comparison <- total_words %>%
  left_join(ot_words, by = "word", suffix = c("_total", "_ot")) %>%
  left_join(nt_words, by = "word", suffix = c("_ot", "_nt")) # 신약에 "_nt" 접미사 추가
head(word_comparison)
# NA 처리 후 비율 계산 (NA는 0으로 처리)
word_comparison <- word_comparison %>%
  mutate(
    n_ot = ifelse(is.na(n_ot), 0, n_ot),   # 구약에서 등장하지 않은 단어는 0으로 처리
    n_nt = ifelse(is.na(n), 0, n),         # 신약에서 등장하지 않은 단어는 0으로 처리
    prop_ot = n_ot / n_total,              # 구약에서의 비율
    prop_nt = n_nt / n_total               # 신약에서의 비율
  )


# 구약과 신약의 차이 계산
word_comparison <- word_comparison %>%
  mutate(diff = abs(prop_ot - prop_nt))  # 구약과 신약 비율 차이 계산

# 차이가 큰 상위 20개 단어 추출
top_20_diff <- word_comparison %>%
  arrange(desc(diff)) %>%
  head(20)

# 결과 확인
top_20_diff

# 상위 20개 단어의 차이 시각화
top_20_diff %>%
  ggplot(aes(x = reorder(word, diff), y = diff)) +
  geom_col() +
  coord_flip() +
  labs(title = "구약과 신약 사이에서 차이가 큰 상위 20단어",
       x = "단어", y = "비율 차이") +
  theme_minimal()

#이번에는 구약과 신약 각각 더 많이 사용된 단어를 찾아보자
top_10_ot <- word_comparison %>%
  arrange(desc(prop_ot)) %>%
  head(10)

top_10_nt <- word_comparison %>%
  arrange(desc(prop_nt)) %>%
  head(10)

top_10_ot %>%
  ggplot(aes(x = reorder(word, diff), y = diff)) +
  geom_col() +
  coord_flip() +
  labs(title = "구약에서 더 많이 사용된 10단어",
       x = "단어", y = "비율 차이") +
  theme_minimal()

top_10_nt %>%
  ggplot(aes(x = reorder(word, diff), y = diff)) +
  geom_col() +
  coord_flip() +
  labs(title = "신약에서 더 많이 사용된 10단어",
       x = "단어", y = "비율 차이") +
  theme_minimal()
head(top_20_diff)
# 감정분석
sentiments <- get_sentiments("bing")

ot_sentiment <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  count(sentiment)  # 감정별로 단어 수 계산

print(ot_sentiment)

nt_sentiment <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% nt_books) %>% 
  inner_join(sentiments, by = "word") %>%
  count(sentiment)

print(nt_sentiment)


#감정분석 결과
ot_positive_top10 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  filter(sentiment == "positive") %>%  # 긍정적인 단어만 선택
  count(word, sort = TRUE) %>%  # 단어 빈도 계산
  top_n(10, n) 
print(ot_positive_top10)

ot_positive_top10 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  filter(sentiment == "positive") %>%  # 긍정적인 단어만 선택
  count(word, sort = TRUE) %>%  # 단어 빈도 계산
  top_n(10, n) 
ot_negative_top10 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% ot_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  filter(sentiment == "negative") %>%  # 긍정적인 단어만 선택
  count(word, sort = TRUE) %>%  # 단어 빈도 계산
  top_n(10, n) 
print(ot_negative_top10)


nt_positive_top10 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% nt_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  filter(sentiment == "positive") %>%  # 긍정적인 단어만 선택
  count(word, sort = TRUE) %>%  # 단어 빈도 계산
  top_n(10, n) 

nt_negative_top10 <- bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book %in% nt_books) %>% 
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  filter(sentiment == "negative") %>%  # 긍정적인 단어만 선택
  count(word, sort = TRUE) %>%  # 단어 빈도 계산
  top_n(10, n) 

print(nt_positive_top10)
print(nt_negative_top10)


ot_positive_top10 <- ot_positive_top10 %>%
  mutate(Testament = "Old Testament", Sentiment = "Positive")

ot_negative_top10 <- ot_negative_top10 %>%
  mutate(Testament = "Old Testament", Sentiment = "Negative")

nt_positive_top10 <- nt_positive_top10 %>%
  mutate(Testament = "New Testament", Sentiment = "Positive")

nt_negative_top10 <- nt_negative_top10 %>%
  mutate(Testament = "New Testament", Sentiment = "Negative")

# 2. 네 개의 데이터프레임을 하나로 결합
top_words <- bind_rows(ot_positive_top10, ot_negative_top10, nt_positive_top10, nt_negative_top10)

# 3. 시각화: 각 감정과 성경 범위별 단어 빈도 시각화
ggplot(top_words, aes(x = reorder(word, n), y = n, fill = Sentiment)) +
  geom_col() +
  facet_wrap(~Testament + Sentiment, scales = "free") +  # 구약/신약, 긍정/부정으로 구분
  coord_flip() +
  labs(title = "Top 10 Positive and Negative Words in Old and New Testament",
       x = "Words", y = "Word Count") +
  theme_minimal()


#각 성경별 감정분석
book_sentiment <- bible_df %>%
  unnest_tokens(word, Script) %>%
  inner_join(sentiments, by = "word") %>%  # 감정 사전과 매칭
  group_by(Book, sentiment) %>%  # 책별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0)%>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative) 

ggplot(book_sentiment, aes(x = reorder(Book, sentiment_score), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Each Book of the Bible",
       x = "Book",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive")) +
  theme_minimal()

book_sentiment %>%
  arrange(desc(sentiment_score)) %>%
  head(10)  # 상위 10개 긍정적인 책

book_sentiment %>%
  arrange(sentiment_score) %>%
  head(10)  # 상위 10개 부정적인 책



book_sentiment <- bible_df %>%
  unnest_tokens(word, Script) %>%
  inner_join(sentiments, by = "word", relationship = "many-to-many") %>%  # 감정 사전과 매칭
  filter(Book %in% ot_books) %>%
  group_by(Book, sentiment) %>%  # 책별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0)%>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative)
  
  

  
unique(book_sentiment$Book)
unique(bible_df$Book)

ggplot(book_sentiment, aes(x = reorder(Book, sentiment_score), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "구약성경 감정분석",
       x = "Book",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive")) +
  theme_minimal()


book_sentiment <- bible_df %>%
  unnest_tokens(word, Script) %>%
  inner_join(sentiments, by = "word", relationship = "many-to-many") %>%   # 감정 사전과 매칭
  filter(Book %in% nt_books) %>%
  group_by(Book, sentiment) %>%  # 책별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0)%>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative) 

ggplot(book_sentiment, aes(x = reorder(Book, sentiment_score), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "신약성경 감정분석",
       x = "Book",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive")) +
  theme_minimal()


#책별 빈도수 체크크
unique(bible_df$Book)


bible_df %>%
  unnest_tokens(word, Script) %>%
  group_by(Book) %>% 
  summarise(total_words = n()) %>%  # 각 책에서의 전체 단어 수 계산
  top_n(3, total_words) %>%  # 상위 3개의 책 추출
  arrange(desc(total_words))  # 내림차순 정렬

#시편 감정분석
bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book == "Psalms") %>%  # 시편만 필터링
  inner_join(sentiments, by = "word") %>%
  group_by(Chapter, sentiment) %>%  # 챕터별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0) %>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative) %>%  # 긍정 - 부정으로 감정 점수 계산
  arrange(Chapter)%>%
  ggplot(aes(x = Chapter, y= sentiment_score, fill = sentiment_score > 0))+
  geom_col()+
  labs(title = "시편 챕터별 감정분석",
       x = "챕터",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive"))+
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # X축 텍스트 제거
        axis.ticks.x = element_blank())  # X축 눈금 제거

#예레미아서 감정분석
bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book == "Jeremiah") %>%  # 시편만 필터링
  inner_join(sentiments, by = "word") %>%
  group_by(Chapter, sentiment) %>%  # 챕터별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0) %>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative) %>%  # 긍정 - 부정으로 감정 점수 계산
  arrange(Chapter)%>%
  ggplot(aes(x = Chapter, y= sentiment_score, fill = sentiment_score > 0))+
  geom_col()+
  labs(title = "예레미아서 챕터별 감정분석",
       x = "챕터",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive"))+
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # X축 텍스트 제거
        axis.ticks.x = element_blank())  # X축 눈금 제거

#에스겔서 감정분석
bible_df %>%
  unnest_tokens(word, Script) %>%
  filter(Book == "Ezekiel") %>%  # 시편만 필터링
  inner_join(sentiments, by = "word") %>%
  group_by(Chapter, sentiment) %>%  # 챕터별로 그룹화하고 감정별로 나눔
  count() %>%  # 각 감정별로 단어 수 세기
  spread(sentiment, n, fill = 0) %>%  # 긍정과 부정 단어 개수를 나란히 표시
  mutate(sentiment_score = positive - negative) %>%  # 긍정 - 부정으로 감정 점수 계산
  arrange(Chapter)%>%
  ggplot(aes(x = Chapter, y= sentiment_score, fill = sentiment_score > 0))+
  geom_col()+
  labs(title = "에스겔서 챕터별 감정분석",
       x = "챕터",
       y = "Sentiment Score (Positive - Negative)") +
  scale_fill_manual(values = c("red", "blue"), name = "Sentiment", labels = c("Negative", "Positive"))+
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # X축 텍스트 제거
        axis.ticks.x = element_blank())  # X축 눈금 제거
  