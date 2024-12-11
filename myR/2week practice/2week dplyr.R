load(url('https://github.com/hbchoi/SampleData/raw/master/join_practice.RData'))
library(dplyr)
left_join(bands,artists,by = c("first", "last"))

right_join(bands,artists,by = c("first","last"))

full_join(artists,bands,by = c("first","last"))%>%
  full_join(albums,by = "band")%>%
  full_join(bands,by = "band")

artists %>%
  semi_join(songs,by = c("first","last"))
#artist들 중에서 실제 작곡을 한 사람만 찾는 코드

artists%>%
  right_join(songs,by = c("first","last"))%>%
  filter(!is.na(instrument))%>%
  select(first,last, instrument)
#semi_join을 쓰지 않고 위와 동일하게 artist들 중 실제 작곡을 한 사람만 찾는 코드
#단계별로 살펴보자면 먼저 artists와 songs를 first와 last 변수를 기준으로 합친 후 instrument가 NA가 아닌 값들로
#필터를 거침, 이후 FIRST와 LAST, instrument변수만 선택하여 출력하는 복잡한 절차를 거침

artists%>%
  anti_join(bands,by = c("first","last"))
#솔로 artist만 찾을떄

#exercise
#1. band가 만들지 않은 album이 뭐가 있는가
#2. album중에서 band에서 만든 것들의 수를 세어보자

albums%>%
  anti_join(bands,by = "band")

albums%>%
  semi_join(bands,by = "band")%>%
  summarise(count = n())
#3. use filtering join to count how many rows in songs match a label in labels

songs%>%
  semi_join(labels,by = "album")%>%
  nrow()

aerosmith%>%
  intersect(greatest_hits)

library(tibble)
stage_songs%>%
  rownames_to_column('song')%>%
  left_join(stage_writers,by = 'song')
#변환하고자 하는 key의 col이 rownames일때의 변환


movie_years%>%
  left_join(movie_studios,by = "movie")%>%
  rename(artist = name.x, studio = name.y)
#변환하고 보니 동일한 col이 있어 name.x, name.y로 표시되는 상황

elvis_movies%>%
  left_join(elvis_songs, by = c('name' = 'movie'))%>%
  rename(movie = name, song = name.y)
#key로 만들고 싶은 것은 영화의 이름인데, daf1에서는 노래의 이름이 name이고 영화제목이 movie로, daf2에서는 영화이름이 
#name일때, key를 일치시키기 위해 벡터로 변수이름을 일치시킨 후, rename을 통해 변수명을 수정함

movie_years%>%
  left_join(movie_directors, by = c("movie" = "name"))%>%
  rename(artist = name)%>%
  select(year,movie,artist,director,studio)
