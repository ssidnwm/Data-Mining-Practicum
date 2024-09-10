library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

JobP <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/job_postings.csv")
jobP는 직책, 설명, 급여, 근무 유형, 위치 등 각 채용 공고에 대한 자세한 정보가 포함되어 있음



summary(JobP)

comp <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/companies.csv")
comp에는 회사명, 웹사이트, 설명, 규모, 위치 등 채용공고를 올린 각 회사에 대한 자세한 정보가 포함되어 있음

comp_i <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/company_industries.csv")
comp_i에는 회사별 id와 산업에 대한 정보가 포함되어있어, comp에 회사 id와 join하면 좋을 것 같다

comp_s <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/company_specialities.csv")
comp_s에는 회사별 id와 전문분야에 대한 정보가 포함되어 있다.

emp_c <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/employee_counts.csv")
emp_c에는 각 회사별 id와 직원, 팔로워 수에 대한 정보가 포함되어 있다.

ben <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/benefits.csv")
ben에는 직업 id와 관련된 혜택에 대한 정보가 있다.

job_i <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/job_industries.csv")
job_i에는 각 직업과 관련된 산업에 대한 정보가 있다.

job_s <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 마이닝 실습/Data-Mining-Practicum/myR/job_skills.csv")
jos_s에는 job id와 직업과 관련된 스킬이 있다.

# Task1: 모든 csv파일을 각각의 dataFrame으로 하여 각 데이터별별 column의 의미와 type, value등을 파악하여 보자
job_postings
head(JobP)
summary(JobP)
#JobP에 관련해서, 총 27개의 변수가 있고, 이 내용에는 회사 id, 

JobP$remote_allowed

unique(ben$type)
head(ben)
unique(job_s$skill_abr)
unique(job_i$industry_id)
summary(comp)
unique(comp_i$industry)
# Task1-2: 8개의 데이터 프레임을 어떻게 병합할 것인지 계획을 세운 후 그 순서와 방법에 대해 기술하기, +이왕이면
#그림을 사용하여 병합 방법에 대한 계획을 세울 것
job_s <- job_s %>%
  group_by(job_id) %>%
  summarise(skill_abr_combined = paste(skill_abr, collapse = ", "))
#job_s에 대해 하나의 직업에 특징이 전부 들어가게끔 처리
ben<- ben%>%
  filter(inferred >= 1)%>%
  group_by(job_id)%>%
  summarise(benif = paste(type, collapse = ", "))
unique(job_s$skill_abr)
#ben역시 하나의 직업에 그 혜택이 모두 들어가게 처리

job_i<-job_i%>%
  group_by(job_id)%>%
  summarise(industry_id = paste(industry_id, collapse = ", "))

job_full<-JobP%>%
  left_join(job_i, by = "job_id")%>%
  left_join(job_s, by = "job_id")%>%
  left_join(ben,by = "job_id")

unique(JobP$remote_allowed)

comp_i3<-comp_i %>%
  group_by(company_id, industry) %>%  # company_id와 industry 둘 다 기준으로 그룹화
  filter(n() > 1) %>%                 # 중복된 항목만 필터링
  ungroup() %>%                       # 그룹화 해제
  distinct(company_id, industry, .keep_all = TRUE)%>%# 중복된 항목 중 하나만 남기기 distinct를 사용함
  group_by(company_id)%>%
  summarise(industry = paste(industry, collapse = ", "))



comp_s<-comp_s%>%
  group_by(company_id)%>%
  summarise(speciality = paste(speciality, collapse = ", "))

#이번에는 company를 기준으로 분류 speciality를 company별로 정렬함
# 중복된 company_id 확인 후, 해당 행들만 출력
emp_c<-emp_c %>%
  group_by(company_id) %>%
  filter(n() > 1) %>%
  ungroup()%>%
  arrange(company_id)%>%
  distinct(company_id, .keep_all = TRUE)
#중복된 내용이 많은것을 발견함


#이제 comp_i2와 comp_s, emp_c를 comp df에 병합하기로 함
#여기서 단계별 진행 필요요
comp_full<-comp%>%
  left_join(comp_i3,by = 'company_id')%>%
  left_join(comp_s, by = 'company_id')%>%
  left_join(emp_c, by = 'company_id')


#이로서 comp와 job에 대해 1차적인 병합이 완료되었다.

#다음은 job_full에 대해 comP_ful에서 일치하는 내용을 병합해, 최근 직업 posting을 한 기업에 대한 정보도 추가적으로 
#첨부하도록 새로운 df를 만들어 보자

job<-job_full%>%
  left_join(comp_full,by = "company_id")


head(comp_ful)

job %>%
  group_by(industry) %>%
  mutate(salary_range = max(max_salary, na.rm = TRUE) - min(min_salary, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(salary_range)) %>%
  filter(!is.na(industry))%>%
  select(salary_range, industry)


# Task 1-3: 계획된 방법을 사용하여 실제 병합을 진행한 후, 데이터프레임을 점검하여, 정상적으로 병합되었는지 확인
#체크리스트를 통해 병합이 잘 되었는지 확인하고, 최대한 오류가 적은 병합을 완료하자


# Task 2: 병합 결과를 활용하여, Job Market에 대해 파악하기 위한 EDA 및 데이터 분석을 수행하자
# 산업별로 salary range가 얼마나 다른지,
IndJ<-job %>%
  mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                               max_salary - min_salary, NA))%>%
  select(job_id,salary_range)

IndJ%>%
  filter(salary_range > 100000)%>%
  select(title,salary_range,industry_id)

IndJ %>%
  left_join(job_i, by = "job_id") %>%          # 'job_id' 기준으로 병합
  filter(complete.cases(.)) %>%                # NA 제거
  group_by(industry_id) %>%                    # 'industry_id'로 그룹화
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%  # 그룹별 평균 계산
  mutate(industry_id = as.character(industry_id)) %>% 
  filter(mean_salary_range > 100000)%>%
  select(mean_salary_range,industry_id)



IndJ %>%
  left_join(job_i, by = "job_id") %>%          # 'job_id' 기준으로 병합
  filter(complete.cases(.)) %>%                # NA 제거
  group_by(industry_id) %>%                    # 'industry_id'로 그룹화
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%  # 그룹별 평균 계산
  mutate(industry_id = as.character(industry_id)) %>% 
  ggplot(aes(x = industry_id, y = mean_salary_range)) +  # 'industry_id'와 'mean_salary_range' 시각화
  geom_point()+
  ggtitle("Salary ranges by industry")+
  labs(x = "Industry ID", y = "Salary Range")

IndJ %>%
  left_join(job_i, by = "job_id") %>%          # 'job_id' 기준으로 병합
  filter(complete.cases(.)) %>%                # NA 제거
  group_by(industry_id) %>%                    # 'industry_id'로 그룹화
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE))%>%
  arrange(-mean_salary_range)





# 요구하는 기술에 따라 Salary range가 얼마나 다른지,
IndJ %>%
  left_join(job_s, by = "job_id") %>%          # 'job_id' 기준으로 병합
  filter(complete.cases(.)) %>%                # NA 제거
  group_by(skill_abr) %>%                    # 'skill_abr'로 그룹화
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%  # 그룹별 평균 계산
  mutate(skill_abr = as.character(skill_abr)) %>% 
  ggplot(aes(x = skill_abr, y = mean_salary_range)) +  # 'skill_abr'와 'mean_salary_range' 시각화
  geom_point()+
  ggtitle("Salary ranges by skill abr")+
  labs(x = "skill abr", y = "Salary Range")

job %>%
  mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                               max_salary - min_salary, NA))%>%
  select(job_id,salary_range,formatted_experience_level)%>%
  filter(complete.cases(.))%>%
  group_by(formatted_experience_level)%>%
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%
  ggplot(aes(x = formatted_experience_level, y = mean_salary_range))+
  geom_point()+
  ggtitle("How the salary range varies depending on the skills required")+
  labs(x = "required skills",y = "Mean of Salary Range")


job %>%
  mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                               max_salary - min_salary, NA))%>%
  select(job_id,salary_range,formatted_experience_level)%>%
  mutate(formatted_experience_level = ifelse(formatted_experience_level == "", 
                                             "Notting", 
                                             formatted_experience_level)) %>%
  filter(complete.cases(.))%>%
  group_by(formatted_experience_level)%>%
  summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%
  mutate(formatted_experience_level = factor(formatted_experience_level,
                                             levels = c("Notting","Internship", "Entry level", "Associate", 
                                                        "Mid-Senior level", "Director", "Executive")))%>%
  ggplot(aes(x = formatted_experience_level, y = mean_salary_range))+
  geom_point()+
  ggtitle("How the salary range varies depending on the skills required")+
  labs(x = "required skills",y = "Mean of Salary Range")



# 회사의 규모(직원수)가 많은 회사는 모집도 많이 하는지
#employ_count 와 jobP에 공고를 올린 횟수
job%>%
  select(company_id,employee_count,name)%>%
  filter(complete.cases(.))%>%
  group_by(company_id)%>%
  mutate(Number_of_recruits  = n())%>%
  distinct(company_id, .keep_all = TRUE)%>%
  ggplot(aes(x = Number_of_recruits, y = employee_count))+
  geom_point()+
  scale_y_log10("Employee Count", labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  ggtitle("Recruits to Employee Count")+
  labs(x = "Number Of Recruits")+
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red")
  



unique(job$country)

# 국가나 지역에 따른 job에서 어떤 차이가 있는지
job%>%
  left_join(job_s, by = "job_id")%>%
  select(country,skill_abr)%>%
  filter(complete.cases(.))%>%
  group_by(country, skill_abr) %>%  # country와 skill_abr_combined으로 그룹화
  summarise(skill_count = n()) %>%  # 각 지역과 기술의 빈도수 계산
  top_n(1, skill_count) %>%  # 각 지역에서 가장 많이 등장한 기술 추출
  arrange(desc(skill_count))
job%>%
  select(title,skill_abr_combined,country)%>%
  group_by(country, skill_abr_combined) %>%  # country와 skill_abr_combined으로 그룹화
  summarise(skill_count = n()) %>%  # 각 지역과 기술의 빈도수 계산
  top_n(1, skill_count) %>%  # 각 지역에서 가장 많이 등장한 기술 추출
  arrange(desc(skill_count))  # 빈도수 순으로 정렬

job%>%
  left_join(job_s, by = "job_id")%>%
  filter(country == "US")%>%
  select(title, country, skill_abr)%>%
  filter(complete.cases(.))%>%
  group_by(skill_abr)%>%
  summarise(skill_count = n()) %>%
  ggplot(aes(x = skill_abr,y = skill_count))+
  geom_point()+
  ggtitle("US job skill abr")+
  labs(x = "Skill abr", y = "Skill Count")


# Remote job과 on-site job사이에 발견되는 특이점이 있는지
job%>%
  mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                               max_salary - min_salary, NA))%>%
  group_by(remote_allowed)%>%
  ggplot(aes(x = remote_allowed, y = salary_range))+
  geom_point()
  

#등등의 인사이트를 낼 것.
names(JobP)

#Task 3: 해외취업을 한다고 할 때, 원하는 직업의 요구 직무, salary 수준, 복지, 위치, 기술 등에 대한 조건을 정리한 후
#원하는 조건에 맞는 job posting을 찾고, 선호하는 순서 top 3를 제시해보자
unique(job$pay_period)

job%>%
  select(title,country,formatted_experience_level,min_salary,skill_abr_combined,pay_period,work_type)%>%
  filter(complete.cases(.))%>%
  mutate(min_salary = ifelse(pay_period == "YEARLY", min_salary / 12,ifelse(pay_period == "HOURLY", min_salary * 160,min_salary)))%>%
  filter(min_salary > 186, 
         country == "US", 
         formatted_experience_level %in% c("Notting", "Internship"))%>%
  filter(str_detect(skill_abr_combined, "IT")) %>%
  select(title,formatted_experience_level,min_salary,skill_abr_combined,work_type)%>%
  arrange(desc(min_salary))


job%>%
  select(title,country,formatted_experience_level,min_salary,skill_abr_combined,pay_period)%>%
  filter(complete.cases(.))%>%
  mutate(min_salary = ifelse(pay_period == "YEARLY", min_salary / 12,ifelse(pay_period == "HOURLY", min_salary * 160,min_salary)))%>%
  filter(country == "KR")%>%
  filter(str_detect(skill_abr_combined, "IT")) %>%
  select(title,formatted_experience_level,min_salary,skill_abr_combined)%>%
  arrange(desc(min_salary))

job %>%
  mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                               max_salary - min_salary, NA))%>%
  filter(!is.na(salary_range))%>%
  ggplot(aes(x = industry_id, y = salary_range))+
  geom_point()

unique(job$industry_id)

JobP %>%
  mutate(original_listed_time_converted = as.POSIXct(original_listed_time / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  select(original_listed_time, original_listed_time_converted) %>%
  head()
