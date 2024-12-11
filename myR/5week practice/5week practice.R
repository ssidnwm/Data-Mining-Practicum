library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
install.packages("tidytext")
# URL에서 텍스트 파일을 읽기


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







# Task 1-1 성경 텍스트 데이터 세트에 토큰화를 수행하고 설명적 통계를 제공합니다.
bible_token <- bible_df%>%
  unnest_tokens(word, Script)
print(bible_token)
bi_document<-tidy(bible_token, metrix = "beta")

bible_df%>%
  unnest_tokens(word, Script)%>%
  nrow()

bible_df%>%
  unnest_tokens(word, Script)%>%
  group_by(Book)%>%
  nrow()

bible_df %>% 
  unnest_tokens(word, Script)%>%
  distinct(word) %>% 
  nrow()

bible_df %>%
  unnest_tokens(word, Script) %>%
  count(word) %>%
  filter(n == 1) %>%
  nrow()

bible_df %>% 
  unnest_tokens(word, Script) %>%
  count(word, sort = TRUE) %>% 
  head(10)

bible_df %>%
  unnest_tokens(word, Script) %>%
  group_by(Book) %>%
  summarise(total_words = n())%>%
  ggplot(aes(x = reorder(Book,total_words), y = total_words))+
  geom_col()+
  coord_flip()+
  labs(x = "words", y="book")

install.packages("udpipe")
library(udpipe)
#udpipe라이브러리 다운
ud_model <- udpipe_download_model(language = "english")
#udpipe라이브러리에서 영어 모델 다운로드
#NER 적용
# udpipe 모델 로드
ud_model <- udpipe_load_model(file = ud_model$file_model)
# 성경 텍스트를 대상으로 NER 수행
bidle_token<-bible_df%>%
  unnest_tokens(word, Script, to_lower = FALSE)
annotations <- udpipe_annotate(ud_model, x = bidle_token$word)
# 데이터 프레임으로 변환
annotations_df <- as.data.frame(annotations)
# 사람 이름 추출 (명사구 중에서 고유명사만 추출)
person_names <- annotations_df %>%
  filter(upos == "PROPN") %>%
  count(token, sort = TRUE)%>%
  head(20)
print(person_names)

verb_usage <- annotations_df %>%
  filter(upos == "VERB") %>%
  count(token, sort = TRUE) %>%
  head(10)
noun_usage <- annotations_df %>%
  filter(upos == "NOUN") %>%
  count(token, sort = TRUE)%>%
  head(10)
print(noun_usage)


## 2-1 구약, 신약에 대해 각각을 그룹화하기
ot_books <- c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy", "Joshua", "Judges", 
              "Ruth", "Samuel-1", "Samuel-2", "Kings-1", "Kings-2", "Chronicles-1", "Chronicles-2", 
              "Ezra", "Nehemiah", "Esther", "Job", "Psalms", "Proverbs", "Ecclesiastes", 
              "Song of Solomon", "Isaiah", "Jeremiah", "Lamentations", "Ezekiel", "Daniel", 
              "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah", "Nahum", "Habakkuk", 
              "Zephaniah", "Haggai", "Zechariah", "Malachi")
ot_Law <- c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy")
ot_History <- c("Joshua", "Judges", 
                "Ruth", "Samuel-1", "Samuel-2", "Kings-1", "Kings-2", "Chronicles-1", "Chronicles-2", 
                "Ezra", "Nehemiah", "Esther")
ot_Poetry <- c("Job", "Psalms", "Proverbs", "Ecclesiastes", 
               "Song of Solomon")
ot_Prophecy <- c( "Isaiah", "Jeremiah", "Lamentations", "Ezekiel", "Daniel", 
                  "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah", "Nahum", "Habakkuk", 
                  "Zephaniah", "Haggai", "Zechariah", "Malachi")
nt_books <- c("Matthew", "Mark", "Luke", "John", "Acts", "Romans", "Corinthians-1", "Corinthians-2", 
              "Galatians", "Ephesians", "Philippians", "Colossians", "Thessalonians-1", 
              "Thessalonians-2", "Timothy-1", "Timothy-2", "Titus", "Philemon", "Hebrews", 
              "James", "Peter-1", "Peter-2", "John-1", "John-2", "John-3", "Jude", "Revelation")

nt_Gospels <- c("Matthew", "Mark", "Luke", "John")
nt_History<- c("Acts")
nt_Letters <- c("Romans", "Corinthians-1", "Corinthians-2", 
                 "Galatians", "Ephesians", "Philippians", "Colossians", "Thessalonians-1", 
                 "Thessalonians-2", "Timothy-1", "Timothy-2", "Titus", "Philemon", "Hebrews", 
                 "James", "Peter-1", "Peter-2", "John-1", "John-2", "John-3", "Jude")
nt_Prophecy <- c("Revelation")
bible_token%>%
  filter(Book %in% ot_Law)%>%
  count(Book,word, sort = TRUE)%>%
  ungroup()%>%
  bind_tf_idf(word,Book,n)%>%
  group_by(Book)%>%
  slice_max(tf_idf, n = 2) %>%
  arrange(desc(tf_idf))
ot_Poetry
bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% ot_History)%>% #구약_법률에 대한 데이터를 필터링함
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  group_by(Book)%>% #책별로 그룹화를 진행
  slice_max(tf_idf, n = 2) %>% #그룹별(책별) 두 개의 값만 가지고 옴
  arrange(desc(tf_idf)) # 이후 정렬

bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% ot_Poetry)%>% #구약_시에 대한 데이터를 필터링함
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  group_by(Book)%>% #책별로 그룹화를 진행
  slice_max(tf_idf, n = 2) %>% #그룹별(책별) 두 개의 값만 가지고 옴
  arrange(desc(tf_idf)) # 이후 정렬

bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% ot_Prophecy)%>% #구약_시에 대한 데이터를 필터링함
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  group_by(Book)%>% #책별로 그룹화를 진행
  slice_max(tf_idf, n = 1) %>% #그룹별(책별) 두 개의 값만 가지고 옴
  arrange(desc(tf_idf)) # 이후 정렬

bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% nt_Gospels)%>% #신약_계율에 대한 데이터를 필터링함
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  group_by(Book)%>% #책별로 그룹화를 진행
  arrange(desc(tf_idf))%>% # 이후 정렬
  head(20)
bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% nt_books)%>% 
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  filter(Book %in% nt_History)%>% #신약_사도행전에 대한 데이터를 필터링함
  arrange(desc(tf_idf)) # 이후 정렬

bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% nt_Letters)%>% #신약_계율에 대한 데이터를 필터링함
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  group_by(Book)%>% #책별로 그룹화를 진행
  slice_max(tf_idf, n = 1) %>% #그룹별(책별) 두 개의 값만 가지고 옴
  arrange(desc(tf_idf))%>% # 이후 정렬
  head(20)

bible_token%>% #토큰화된 성경 데이터를 사용
  filter(Book %in% nt_books)%>% 
  count(Book,word, sort = TRUE)%>% # Book별로 단어수를 카운트하고 정렬
  ungroup()%>% #count이후 book별 그룹을 해제
  bind_tf_idf(word,Book,n)%>% #Book의 word에 대한 tf-idf를 계산
  filter(Book %in% nt_Prophecy)%>% #신약_예언서서에 대한 데이터를 필터링함
  arrange(desc(tf_idf))%>% # 이후 정렬
  head(20)


#Task3 토픽 모델링
install.packages("topicmodels")
install.packages("reshape2")
library(reshape2)
library(tidytext)
library(topicmodels)
data("stop_words")
custom_stopwords <- tibble(word = c("the", "and", "of", "to", "you", "in","will","he","a","i","is","his",
                                    "for","they","your","who","my","with","from","him","that","it","lord",
                                    "god","people","day","son"))
all_stopwords <- bind_rows(stop_words, custom_stopwords)


bible_token_clean <- bible_token %>%
  anti_join(stop_words, by = c("word" = "word"))  %>%
  anti_join(all_stopwords, by = "word") 

dtm <- bible_token_clean %>%
  count(Book, word) %>%  # 각 책(Book)에서 단어별 빈도 계산
  cast_dtm(Book, word, n) 

# LDA 모델 생성: k는 찾고자 하는 토픽의 수
lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))

# 토픽별로 중요한 단어들 추출 (beta 값이 높은 단어들)
topics <- tidy(lda_model, matrix = "beta")

head(topics)

topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_terms <- topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)
# 각 토픽에서 상위 10개의 단어 추출


print(top_terms)

top_terms %>%
  mutate(term = reorder_within(term,beta,topic))%>%
  ggplot(aes(x = reorder_within(term, beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  labs(x = "단어", y = "Beta 값", title = "전체 성경 각 토픽별 중요한 단어")

dtmot<-bible_token_clean%>%
  filter(Book %in% ot_books)%>%
  count(Book, word) %>%  # 각 책(Book)에서 단어별 빈도 계산
  cast_dtm(Book, word, n) 
# LDA 모델 생성: k는 찾고자 하는 토픽의 수
lda_model <- LDA(dtmot, k = 4, control = list(seed = 1234))

# 토픽별로 중요한 단어들 추출 (beta 값이 높은 단어들)
topics <- tidy(lda_model, matrix = "beta")

head(topics)

topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_terms <- topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)
top_terms %>%
  mutate(term = reorder_within(term,beta,topic))%>%
  ggplot(aes(x = reorder_within(term, beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  labs(x = "단어", y = "Beta 값", title = "구약 각 토픽별 중요한 단어")



dtmnt <-bible_token_clean%>%
  filter(Book %in% nt_books)%>%
  count(Book, word) %>%  # 각 책(Book)에서 단어별 빈도 계산
  cast_dtm(Book, word, n) 

# LDA 모델 생성: k는 찾고자 하는 토픽의 수
lda_model <- LDA(dtmnt, k = 4, control = list(seed = 1234))

# 토픽별로 중요한 단어들 추출 (beta 값이 높은 단어들)
topics <- tidy(lda_model, matrix = "beta")

head(topics)

topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_terms <- topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_terms %>%
  mutate(term = reorder_within(term,beta,topic))%>%
  ggplot(aes(x = reorder_within(term, beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  labs(x = "단어", y = "Beta 값", title = "신약 각 토픽별 중요한 단어")
# 문서별로 토픽 비중 추출 (gamma 값이 높을수록 해당 토픽에 가까움)
doc_topics <- tidy(lda_model, matrix = "gamma")

# 문서별로 가장 높은 토픽 추출
doc_top_topic <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_top_topic%>%
  mutate()

##시편에 대한 토픽 모델링 적용
dtmpsa<-bible_token_clean%>%
  filter(Book == "Psalms")%>%
  count(Book, word) %>%  # 각 책(Book)에서 단어별 빈도 계산
  cast_dtm(Book, word, n) 
# LDA 모델 생성: k는 찾고자 하는 토픽의 수
lda_model <- LDA(dtmpsa, k = 4, control = list(seed = 1234))

# 토픽별로 중요한 단어들 추출 (beta 값이 높은 단어들)
topics <- tidy(lda_model, matrix = "beta")

head(topics)

topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_terms <- topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)
top_terms %>%
  mutate(term = reorder_within(term,beta,topic))%>%
  ggplot(aes(x = reorder_within(term, beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  labs(x = "단어", y = "Beta 값", title = "시편 각 토픽별 중요한 단어")
