getwd()
library(dplyr)
library(tidyr)
library(stringr)
#Tesk 1.Apartment Rent Data
setwd('C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR')
apt_data <- read.csv('apartments_for_rent_classified_100K/apartments_for_rent_classified_100K.csv',
                            sep = ";", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)



#Q1. Find out the average rental price for different room types (2 bed + 1 bath, 3 bed 1 bath, etc).

summary(apt_data)
head(apartments_data)
#badrooms와 bathrooms가 각각 다른 col로 정리되어 있다.
apt_data %>%
  arrange(bathrooms,bedrooms)%>%
  group_by(bathrooms,bedrooms)%>%
  mutate(price = as.numeric(gsub("[^0-9.-]", "", price)))%>%
  summarise(room_price = mean(price, na.rm = T)) %>%
  select(bathrooms,bedrooms,room_price)%>%
  arrange(room_price)
#필요없는 부분은 bt, bd에서 제거하기


#Q2. Find out the top 10 cities with most expensive and cheapest rental price. 
#Use the price normalized by the size (square feet) for the fairness.
unique(apt_data$price_type)
#가장 비싸고, 가장 싼 상위 10개의 도시를 확인 도시를 기준으로 그룹화를 진행한 후, square_feet별로 순위를 정함
apt_data %>%
  group_by(cityname)%>%
  mutate(price = as.numeric(gsub("[^0-9.-]", "", price)))%>%
  mutate(square_feet = as.numeric(gsub("[^0-9.-]", "", square_feet)))%>%
  mutate(price = ifelse(price_type == "weekly", price * 4, price))%>%
  mutate(feetprice = price / square_feet)%>%
  select(cityname,feetprice)%>%
  arrange(feetprice)

#그런데 중요한점은, price의 type이 weekly인지, monthly인지를 고려해야 함



#Q3.  Using the normalized rental price from above, compare the average price and variance of each state.
#Estimate the living expenses for housing of different states. Which state is most affordable? 
#which state would you like to live if you need considering jobs and other environment.

#지금은 feetprice로 평방피트당 가격을 구하였으나, 실제 크기가 도시마다 다르므로 각 주의 평균가격은 또 다를 것이다
# 똑같이 도시를 기준으로 그룹화를 한 뒤, feetprice가 아닌, 도시 자체의 price의 평균을 구하고 계산한다.
apt_data %>%
  group_by(cityname)%>%
  mutate(price = as.numeric(gsub("[^0-9.-]", "", price)))%>%
  mutate(square_feet = as.numeric(gsub("[^0-9.-]", "", square_feet)))%>%
  mutate(price = ifelse(price_type == "weekly", price * 4, price))%>%
  summarise(cityprice = mean(price))%>%
  arrange(cityprice)

apt_data %>%
  group_by(state)%>%
  mutate(price = as.numeric(gsub("[^0-9.-]", "", price)))%>%
  mutate(square_feet = as.numeric(gsub("[^0-9.-]", "", square_feet)))%>%
  mutate(price = ifelse(price_type == "weekly", price * 4, price))%>%
  mutate(state = ifelse(grepl("^[0-9.]+$", state) | state %in% c("Norfolk", "Burlington", "Trenton", "Aurora", 
                                                                 "Fort Myers", "Marshfield", "Suitland", ""),
                        NA, state))%>%
  summarise(stateprice = mean(price))%>%
  arrange(stateprice)
#michigan City가 가장 저렴함

#Q4Calculate the frequency of different room types. Make a frequency table to compare each state. 
#What are the most common types of apartment of different region?
#주택 빈도를 확인하기 위해 unique를 사용함
unique(apt_data$category)
unique(apt_data$state)

#각 도시별로 그리고 주별로  category를 표시하고, 가장 일반적인 유형을 찾음
apt_data %>%
  mutate(state = ifelse(grepl("^[0-9.]+$", state) | state %in% c("Norfolk", "Burlington", "Trenton", "Aurora", 
                                                                 "Fort Myers", "Marshfield", "Suitland", ""),
                        NA, state))%>%
  group_by(category,state)%>%
  summarise(count = n())%>%
  arrange(-count)

#Task2
drm <- read.csv('kdrama.csv',)

#Q1 Find out top 10 directors, screenwriters, and production companies with highest average rating? 
#Are you familiar with them? Do you have any favorite directors, screenwriters, or production companies in the list?
#What are the main characteristics or styles of them?
unique(drm$Rating)
#감독, 시나리오 작가, 제작사에 대한 평점을 정리한다
drm%>%
  group_by(Director)%>%
  summarise(dcrate = mean(Rating))%>%
  arrange(desc(dcrate))


drm%>%
  group_by(Screenwriter)%>%
  summarise(swrate = mean(Rating))%>%
  arrange(desc(swrate))

drm%>%
  group_by(Production.companies)%>%
  summarise(pdcrate = mean(Rating))%>%
  arrange(desc(pdcrate))

#Q2 Find out top 10 directors, screenwriters, and production companies who worked most dramas om the top 250 dataset?
#Are you familiar with them? Do you have any favorite directors, screenwriters, or production companies in the list? 
#What are the main characteristics or styles of them?

#똑같이, group_by로 나눈 후 count를 센다.

drm%>%
  group_by(Director)%>%
  summarise(count = n())%>%
  arrange(desc(count))

drm%>%
  group_by(Screenwriter)%>%
  summarise(count = n())%>%
  arrange(desc(count))

drm%>%
  group_by(Production.companies)%>%
  summarise(count = n())%>%
  arrange(desc(count))
#다른 특징은 없을까?
drm%>%
  group_by(Director,Year.of.release)%>%
  summarise(count = n())%>%
  arrange(desc(count))
#년도로 바꾸니 kim wou suk분은 2019년에 3개 작품을 냈다.
drm%>%
  group_by(Screenwriter,Year.of.release)%>%
  summarise(count = n())%>%
  arrange(desc(count))
#시나리오 작업자 역시 2019년에 3개를 낸 사람이 있다.
drm%>%
  group_by(Production.companies,Year.of.release)%>%
  summarise(count = n())%>%
  arrange(desc(count))
#대체적으로 2019년에 작품을 많이 낸 경향이 있다.


#Q3Consider the cast. Who is the actor or actress who stars most of the drama in the datasets? 
#Considering the released year, how the famous actor or actress are changed?

#cast에 대한 내용을 찾아보자, 그런데 각 작품별로 cast의 수가 다르고 또 많아서 데이터의 변형이 필요하다
head(drm$Cast)
unique(drm$Cast)

drm%>%
  separate_rows(Cast, sep = ', ')%>%
  group_by(Cast)%>%
  summarise(count = n())%>%
  arrange(desc(count))
#Lee joon Hyuk, ahn bo hyun, shin hye sun 순으로 작품에 많이 출연한 것으로 확인이 되었다, 그렇다면 년도별 데이터도 추가해 보자
drm%>%
  separate_rows(Cast, sep = ', ')%>%
  group_by(Cast,Year.of.release)%>%
  summarise(count=n())%>%
  arrange(desc(count))
drm%>%
  separate_rows(Cast, sep = ', ')%>%
  group_by(Cast,Year.of.release)%>%
  summarise(count=n(),.groups='drop')%>%
  filter(Year.of.release == 2017)%>%
  arrange(desc(count))
#이제 년도별로 가장 높은 count를 가진 사람을 새로운 df로 찾아보자.
cast_count<-drm%>%
  separate_rows(Cast, sep = ', ')%>%
  group_by(Cast,Year.of.release)%>%
  summarise(count=n(),.groups='drop')%>%
  group_by(Year.of.release)%>%
  slice_max(count,n=3, with_ties = FALSE)
cast_count%>%
  count(Cast)
library(ggplot2)
cast_count %>%
  ggplot(aes(x = Year.of.release, y  = count))+
  geom_point()

cast_count%>%
  filter(Cast %in% c("Lee Joon Hyuk","Ahn Bo Hyun","Shin Hye Sun"))%>%
  ggplot(aes(x = Year.of.release, y = count, color = Cast))+
  geom_point()# 이렇게 확인하니 전반적인 추세가 보이지 않는다.

new_cast <- drm%>%
  separate_rows(Cast, sep = ', ')%>%
  group_by(Cast,Year.of.release)%>%
  summarise(count=n(),.groups='drop')%>%
  group_by(Year.of.release)%>%
  filter(Cast %in% c("Lee Joon Hyuk","Ahn Bo Hyun","Shin Hye Sun","Jung So Min", "Kim Ji Won"))
  
new_cast%>%
  filter(Cast %in% c("Lee Joon Hyuk","Ahn Bo Hyun","Shin Hye Sun","Jung So Min", "Kim Ji Won"))%>%
  ggplot(aes(x = Year.of.release, y = count, color = Cast))+
  geom_point()+
  geom_line()

#년도를 다양하게 바꿔가며 가장 많이 출연한 출연자들을 확인해 본다.



#Q4Compare OTT and cable TV channel, which one more popular and highly rated? 
#Support your argument with the data? Is it changed over time? 
#how did it change from 2003 to 2022?
head(drm$Rank)
unique(drm$Year.of.release)

#original.network를 확인하여 각 방송사별로 rank의 평균을 계산하고, 년도별로도 그룹화하여 rank의 변화를 확인한다?
drm%>%
  mutate(Rank = as.numeric(gsub("#", "", Rank)))%>%
  group_by(Original.Network,Year.of.release)%>%
  summarise(pop = mean(Rank),.groups='drop')%>%
  arrange(pop)%>%
  filter(Year.of.release == 2021)



#Task3: More about dpylr
#4-1:slice()
#filter와 비슷한 역할, filter은 조건에 따라 자르지만, slice는 행에 따라 자름
drm%>%
  mutate(Rank = as.numeric(gsub("#", "", Rank)))%>%
  group_by(Original.Network,Year.of.release)%>%
  summarise(pop = mean(Rank),.groups='drop')%>%
  arrange(pop)%>%
  slice_head(n = 5)



#slice head()
#rating이 높은 순서대로 10등까지를 보고싶다.
drm%>%
  group_by(Rating)%>%
  slice_head(n = 10)
#slice tail()
#이번에는 가장 낮은 rank를 가진 드라마를 확인하고 싶다
drm%>%
  select(Name)%>%
  slice_tail(n = 10)
#slice max()
#에피소드가 가장 많은 드라마를 확인하고 싶다.
drm%>%
  slice_max(Number.of.Episodes)%>%
  select(Name,Number.of.Episodes)
#slice min()
#가장 회당 방영시간이 짧은 드라마는?
drm%>%
  mutate(
    # "hr."을 분으로 변환
    minutes = ifelse(str_detect(Duration, "hr"),
                     as.numeric(str_extract(Duration, "\\d+(?= hr)")) * 60, 0),
    # "min."을 그대로 유지
    minutes = minutes + ifelse(str_detect(Duration, "min"),
                               as.numeric(str_extract(Duration, "\\d+(?= min)")), 0)
  ) %>%
  slice_min(minutes)%>%
  select(Name,minutes)
#slice sample()
#행에서 무작위로 n개를 추출하는 함수 
#15세 이상 영화에서 3개를 추천받고 싶어
drm%>%
  group_by(Content.Rating)%>%
  filter(Content.Rating == '15+ - Teens 15 or older')%>%
  slice_sample(n = 3)%>%
  select(Name, Content.Rating)


#4-2:mutate_all()
#모든 열에 대해 같은 함수를 적용시킴
#mutate(across())와 동일함,
drm %>%
  group_by(Rank) %>%
  mutate_all(~ if(is.numeric(.)) . * 2 else .)

drm %>%
  group_by(Rank) %>%
  mutate(across(where(is.numeric), ~ . * 2))




#mutate_if()
#특정 열에만 함수를 적용시킴
drm%>%
  mutate_if(is.numeric,~. *2)%>%
  select(Name,Year.of.release,Number.of.Episodes,Rating,Rank)%>%
  head(10)
#숫자형 데이터의 값을 2배로 만듬
#mutate_at()
#특정 열에 대해 함수를 적용시키나, 조건이 아닌 이름으로 정함
drm%>%
  mutate_at(vars(Cast), ~ str_split(., ", "))%>%
  select(Name,Cast)%>%
  head(10)
#mutate_at의 경우, sep을 사용할수 없고 대신 string()의 str_split을 사용해야 한다.
#4-3:summarise_all()
#mutate_all과 같이 모든 값에 대해 요약통계를 함,
drm%>%
  group_by(Director,Screenwriter)%>%
  summarise(count=n())
#summarise_if()
#특정 조건을 만족하는 열에 대해서만 요약을 수행
drm%>%
  summarise_if(is.numeric,sum)

#summarise_at()
#특정 열을 지정하여, 그 열들에 대한 요약 통계를 계산함함
drm%>%
  group_by(Year.of.release)%>%
  summarise_at(vars(Number.of.Episodes),mean)%>%
  ggplot(aes(x = Year.of.release, y = Number.of.Episodes))+
  geom_point()
#최근 2020년대에 들어서는 이전처럼 드라마들의 ep수가 적은 것이 선호되고 있다.


#4-4:across()
#동일 내용을 반복하는 함수,
#all, if, at을 굳이 mutate나 summarise에 적용할때, summarise_at, mutate_all을 쓰지 않고도 가능하게 한다.
drm%>%#across
  mutate(across(where(is.numeric),~. *2))%>%
  select(Name,Year.of.release,Number.of.Episodes,Rating,Rank)%>%
  head(10)

#if any()
#조건을 만족하는 열이 하나라도 있는 행을 선택함
#Lee joon Hyuk이 출연한 작품 찾아보기
drm%>%
  filter(if_any(Cast, ~ str_detect(., "Lee Joon Hyuk")))%>%
  select(Name,Cast)
  
#if all()
drm%>%
  filter(if_all(Original.Network, ~str_detect(., "Netflix, tvN")))%>%
  select(Name,Original.Network)
#4-5:sample n()
#무작위 n개의 행을 샘플링할때 사용함 slice sample과 동일
drm%>%
  group_by(Content.Rating)%>%
  filter(Content.Rating == '15+ - Teens 15 or older')%>%
  sample_n(size = 3)%>%
  select(Name, Content.Rating)
#sampel frac()
#frac은 비율로 계산한다는 점에서 n과 다름
drm%>%
  group_by(Rating)%>%
  sample_frac(size = 0.1)%>%
  select(Name)
