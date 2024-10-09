library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/public_emdat_project.csv")

names(data)
unique(data$Historic)
unique(data$Classification.Key)
unique(data$Disaster.Group)
# 1. 시간에 따른 disaster Group별 발생 빈도 추이
data%>%
  group_by(Disaster.Group)%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Group)
#년도별 재앙그룹

unique(data$Start.Month)
data%>%
  group_by(Disaster.Group)%>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month))+
  geom_bar()+
  facet_wrap(~Disaster.Group)
#월별 재앙그룹

data %>%
  mutate(Season = case_when(
    Start.Month %in% c(12, 1, 2) ~ "Winter",
    Start.Month %in% c(3, 4, 5) ~ "Spring",
    Start.Month %in% c(6, 7, 8) ~ "Summer",
    Start.Month %in% c(9, 10, 11) ~ "Autumn"
  )) %>%
  ggplot(aes(x = Season, fill = Disaster.Group)) +
  geom_bar(position = "dodge")


#년도별 재해종류류
data%>%
  group_by(Disaster.Subgroup)%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subgroup)



data%>%
  group_by(Disaster.Subgroup)%>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month))+
  geom_bar()+
  facet_wrap(~Disaster.Subgroup)


data %>%
  mutate(Season = case_when(
    Start.Month %in% c(12, 1, 2) ~ "Winter",
    Start.Month %in% c(3, 4, 5) ~ "Spring",
    Start.Month %in% c(6, 7, 8) ~ "Summer",
    Start.Month %in% c(9, 10, 11) ~ "Autumn"
  )) %>%
  ggplot(aes(x = Season, fill = Disaster.Subgroup)) +
  geom_bar(position = "dodge")


#disaster type
unique(data$Disaster.Type)
unique(data$Disaster.Subgroup)

data%>%
  group_by(Disaster.Type)%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Type)

unique(data$Disaster.Subtype)

data%>%
  select(Disaster.Group,Disaster.Subgroup,Disaster.Type,Disaster.Subtype)%>%
  group_by(Disaster.Group,Disaster.Subgroup,Disaster.Type,Disaster.Subtype)%>%
  filter(Disaster.Subgroup == "Meteorological")%>%
  distinct(Disaster.Type)
unique(data$Disaster.Subgroup)

data %>%
  select(Disaster.Type,Disaster.Subgroup) %>%
  filter(Disaster.Subgroup == "Biological") %>%
  distinct()

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Type)+
  labs(title = "시간별 산업 재해 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Biological")%>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month))+
  geom_bar()+
  facet_wrap(~Disaster.Type)+
  labs(title = "월별 생물학적 재해 빈도수")


data %>%
  select(Disaster.Type,Disaster.Subtype) %>%
  filter(Disaster.Type == "Glacial lake outburst flood") %>%
  distinct()




data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Animal incident")%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "년도별 동물재해 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Animal incident")%>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month))+
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "월별 해충재해 빈도수")


data %>%
  mutate(Storm.Group = case_when(
    Disaster.Subtype %in% c("Storm (General)", "Severe weather", "Lightning/Thunderstorms") ~ "Severe Weather",
    Disaster.Subtype %in% c("Tropical cyclone", "Extra-tropical storm") ~ "Tropical and Extra-tropical Cyclones",
    Disaster.Subtype %in% c("Blizzard/Winter storm", "Severe winter conditions") ~ "Winter and Cold Weather",
    Disaster.Subtype %in% c("Tornado", "Derecho", "Sand/Dust storm") ~ "Wind and Dust",
    Disaster.Subtype %in% c("Storm surge", "Hail") ~ "Water-Related Events",
    TRUE ~ "Other"
  ))%>%
  filter(Storm.Group == "Water-Related Events") %>%
  ggplot(aes(x = Start.Year)) +
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "년도별 물 관련 빈도수")



data %>%
  mutate(Storm.Group = case_when(
    Disaster.Subtype %in% c("Storm (General)", "Severe weather", "Lightning/Thunderstorms") ~ "Severe Weather",
    Disaster.Subtype %in% c("Tropical cyclone", "Extra-tropical storm") ~ "Tropical and Extra-tropical Cyclones",
    Disaster.Subtype %in% c("Blizzard/Winter storm", "Severe winter conditions") ~ "Winter and Cold Weather",
    Disaster.Subtype %in% c("Tornado", "Derecho", "Sand/Dust storm") ~ "Wind and Dust",
    Disaster.Subtype %in% c("Storm surge", "Hail") ~ "Water-Related Events",
    TRUE ~ "Other"
  ))%>%
  filter(Storm.Group == "Water-Related Events") %>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month)) +
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "월별 물 관련 빈도수")


data %>%
  mutate(Storm.Group = case_when(
    Disaster.Subtype %in% c("Storm (General)", "Severe weather", "Lightning/Thunderstorms") ~ "Severe Weather",
    Disaster.Subtype %in% c("Tropical cyclone", "Extra-tropical storm") ~ "Tropical and Extra-tropical Cyclones",
    Disaster.Subtype %in% c("Blizzard/Winter storm") ~ "Winter and Cold Weather",
    Disaster.Subtype %in% c("Tornado", "Derecho", "Sand/Dust storm") ~ "Wind and Dust",
    Disaster.Subtype %in% c("Storm surge", "Hail") ~ "Water-Related Events",
    TRUE ~ "Other"
  ))%>%
  filter(Storm.Group == "Winter and Cold Weather") %>%
  ggplot(aes(x = Start.Year)) +
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "년도별 겨울 및 한파 빈도수")


data %>%
  mutate(Storm.Group = case_when(
    Disaster.Subtype %in% c("Storm (General)", "Severe weather", "Lightning/Thunderstorms") ~ "Severe Weather",
    Disaster.Subtype %in% c("Tropical cyclone", "Extra-tropical storm") ~ "Tropical and Extra-tropical Cyclones",
    Disaster.Subtype %in% c("Blizzard/Winter storm") ~ "Winter and Cold Weather",
    Disaster.Subtype %in% c("Tornado", "Derecho", "Sand/Dust storm") ~ "Wind and Dust",
    Disaster.Subtype %in% c("Storm surge", "Hail") ~ "Water-Related Events",
    TRUE ~ "Other"
  ))%>%
  filter(Storm.Group == "Winter and Cold Weather") %>%
  mutate(Start.Month = factor(Start.Month))%>%
  ggplot(aes(x = Start.Month)) +
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "월별 눈보라/겨울폭풍 빈도수")


data%>%
  group_by(Disaster.Subgroup)%>%
  filter(Disaster.Subgroup == "Meteorological")%>%
  filter(Country == "Japan") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()


data%>%
  group_by(Disaster.Subgroup)%>%
  filter(Disaster.Subgroup == "Meteorological")%>%
  filter(Country == "Republic of Korea") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subgroup)
unique(data$Country)


data %>%
  group_by(Disaster.Subgroup, Disaster.Group) %>%
  summarise(Total_Affected = sum(No..Affected, na.rm = TRUE), .groups = "keep") %>%
  arrange(desc(Total_Affected))

data%>%
  filter(Disaster.Type == "Storm")%>%
  filter(Country == "Republic of Korea") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    Frequency = n()                                     # 재해 발생 빈도 (건수)
  ) %>%
  mutate(Affected_per_Event = Total_Affected / Frequency)%>%
  filter(Affected_per_Event > 1)%>%
  ggplot(aes(x = Start.Year,  y = Affected_per_Event))+
  geom_point()+
  labs(title = "한국 년도별 폭풍피해 추산",x = "년도", y = "재해당 피해자 수")


data%>%
  filter(Disaster.Type == "Storm")%>%
  filter(Country == "Republic of Korea") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    Frequency = n()                                     # 재해 발생 빈도 (건수)
  ) %>%
  mutate(Affected_per_Event = Total_Affected / Frequency)%>%
  filter(Affected_per_Event > 1)

#아프리카의 세균성, 바이러스성 질환 빈도수
unique(data$Disaster.Subtype)
data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subtype == "Viral disease")%>%
  filter(Region == "Africa") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "아프리카 년도별 바이러스 질환 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subtype == "Viral disease")%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "전 세계 년도별 바이러스 질환 빈도수")


data_africa1 <- data %>%
  filter(Disaster.Subtype == "Viral disease") %>%
  filter(Region == "Africa") %>%
  mutate(Region = "Africa")  # 지역을 "Africa"로 명시

# 두 번째 데이터에 "Global" 명시
data_global1 <- data %>%
  filter(Disaster.Subtype == "Viral disease") %>%
  mutate(Region = "Global")  # 전 세계 데이터를 "Global"로 명시

# 두 데이터를 결합
combined_data1 <- bind_rows(data_africa1, data_global1)

# 결합된 데이터로 시각화
combined_data1 %>%
  ggplot(aes(x = Start.Year)) +
  geom_bar() +
  facet_wrap(~Region) +  # Region별로 시각화
  labs(title = "아프리카 및 전 세계 년도별 세균성 질환 빈도수",
       x = "연도",
       y = "빈도수") +
  theme_minimal()

combined_data1 %>%
  ggplot(aes(x = Start.Year, fill = Region)) +
  geom_bar(position = "dodge") +  # Region별로 나란히 표시
  labs(title = "아프리카 및 전 세계 년도별 세균성 질환 빈도수",
       x = "연도",
       y = "빈도수") +
  scale_fill_manual(values = c("Africa" = "skyblue", "Global" = "orange")) +  # 색상 설정
  theme_minimal()
data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subtype == "Viral disease")%>%
  filter(Region == "Africa") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  facet_wrap(~Disaster.Subtype)+
  labs(title = "아프리카 년도별 바이러스 질환 빈도수")

data%>%
  filter(Disaster.Subtype == "Bacterial disease")%>%
  filter(Region == "Africa") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
                                # 재해 발생 빈도 (건수)
  ) %>%
  filter(Total_Affected > 1)%>%
  ggplot(aes(x = Start.Year,  y = Total_Affected))+
  geom_col()+
  labs(title = "아프리카 년도별 바이러스 질환 피해 추산",x = "년도", y = "재해 피해자 수")

data%>%
  filter(Disaster.Subtype == "Bacterial disease")%>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    # 재해 발생 빈도 (건수)
  ) %>%
  filter(Total_Affected > 1)%>%
  ggplot(aes(x = Start.Year,  y = Total_Affected))+
  geom_col()+
  labs(title = "아프리카 년도별 바이러스 질환 피해 추산",x = "년도", y = "재해 피해자 수")


# 첫 번째 데이터(Africa) 처리
data_africa2 <- data %>%
  filter(Disaster.Subtype == "Bacterial disease") %>%
  filter(Region == "Africa") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE)
  ) %>%
  filter(Total_Affected > 1) %>%
  mutate(Region = "Africa")  # 지역을 "Africa"로 명시

# 두 번째 데이터(Global) 처리
data_global2 <- data %>%
  filter(Disaster.Subtype == "Bacterial disease") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE)
  ) %>%
  filter(Total_Affected > 1) %>%
  mutate(Region = "Global")  # 지역을 "Global"로 명시

# 두 데이터를 결합
combined_data2 <- bind_rows(data_africa2, data_global2)

# 결합된 데이터로 시각화
combined_data2 %>%
  ggplot(aes(x = Start.Year, y = Total_Affected, fill = Region)) +
  geom_col(position = "dodge") +  # 두 데이터를 나란히 비교
  labs(title = "년도별 바이러스성 질환 피해 추산 (아프리카 및 전 세계)",
       x = "년도",
       y = "재해 피해자 수") +
  scale_fill_manual(values = c("Africa" = "skyblue", "Global" = "orange")) +  # 색상 설정
  theme_minimal()

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Road")%>%
  filter(Region == "Africa") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "아프리카 년도별 교통사고 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Road")%>%
  filter(Region == "Asia") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "아시아 년도별 교통사고 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Road")%>%
  filter(Region == "Asia") %>%
  filter(Country == "Republic of Korea")%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "한국 년도별 교통사고 빈도수")

data%>%
  filter(Disaster.Type == "Road")%>%
  filter(Country == "Republic of Korea")%>%
  summary(sum())

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Type == "Road")%>%
  filter(Region == "Europe") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "유럽 년도별 교통사고 빈도수")

data%>%
  filter(Disaster.Type == "Road")%>%
  filter(Region == "Asia")%>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    # 재해 발생 빈도 (건수)
  ) %>%
  ggplot(aes(x = Start.Year,  y = Total_Affected))+
  geom_col()+
  labs(title = "아시아 교통사고 피해 추산",x = "년도", y = "재해 피해자 수")


data%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia")%>%
  select(Start.Year,Country,Disaster.Type,No..Affected)%>%
  filter(!is.na(No..Affected))%>%
  group_by(Start.Year)

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia") %>%
  filter(Country == "India")%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "중국 년도별 산업재해 빈도수")

data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "아시아 년도별 산업재해 빈도수")


data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Europe") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "유럽 년도별 산업재해 빈도수")


data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Africa") %>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "아프리카 년도별 산업재해 빈도수")


# 첫 번째 데이터 (Asia) 처리
data_asia4 <- data %>%
  filter(Disaster.Subgroup == "Industrial accident") %>%
  filter(Region == "Asia") %>%
  group_by(Start.Year) %>%
  summarise(Frequency = n()) %>%
  mutate(Region = "Asia")  # 지역을 "Asia"로 명시

# 두 번째 데이터 (China) 처리
data_china4 <- data %>%
  filter(Disaster.Subgroup == "Industrial accident") %>%
  filter(Region == "Asia") %>%
  filter(Country == "China") %>%
  group_by(Start.Year) %>%
  summarise(Frequency = n()) %>%
  mutate(Region = "China")  # 지역을 "China"로 명시

# 두 데이터를 결합
combined_data4 <- bind_rows(data_asia4, data_china4)

# 결합된 데이터로 시각화
combined_data4 %>%
  ggplot(aes(x = Start.Year, y = Frequency, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +  # 막대 나란히 표시
  labs(title = "아시아 및 중국의 년도별 산업재해 빈도수",
       x = "년도",
       y = "재해 빈도수") +
  scale_fill_manual(values = c("Asia" = "skyblue", "China" = "orange")) +  # 색상 설정
  theme_minimal()
#피해자 추가가
data%>%
  group_by(Disaster.Group)%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia") %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    # 재해 발생 빈도 (건수)
  )%>%
  ggplot(aes(x = Start.Year))+
  geom_bar()+
  labs(title = "아시아 년도별 산업재해 빈도수")

data%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia")%>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    # 재해 발생 빈도 (건수)
  ) %>%
  ggplot(aes(x = Start.Year,  y = Total_Affected))+
  geom_col()+
  labs(title = "아시아 산업재해 피해자 추산",x = "년도", y = "재해 피해자 수")


data%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Country == "China")%>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    # 재해 발생 빈도 (건수)
  ) %>%
  ggplot(aes(x = Start.Year,  y = Total_Affected))+
  geom_col()+
  labs(title = "중국 산업재해 피해자 추산",x = "년도", y = "재해 피해자 수")


data%>%
  filter(Disaster.Subgroup == "Industrial accident")%>%
  filter(Region == "Asia")%>%
  filter(Start.Year == 2014)%>%
  select(Country,No..Affected)%>%
  filter(!is.na(No..Affected))


data%>%
  filter(Disaster.Type == "Storm")%>%
  filter(Country == "Republic of Korea") %>%
  group_by(Start.Year) %>%
  summarise(
    Total_Affected = sum(No..Affected, na.rm = TRUE),   # 피해자 수 총합
    Frequency = n()                                     # 재해 발생 빈도 (건수)
  ) %>%
  mutate(Affected_per_Event = Total_Affected / Frequency)%>%
  filter(Affected_per_Event > 1)%>%
  ggplot(aes(x = Start.Year,  y = Affected_per_Event))+
  geom_col()+
  labs(title = "한국 년도별 폭풍피해 추산",x = "년도", y = "재해당 피해자 수")
