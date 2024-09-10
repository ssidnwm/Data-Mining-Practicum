# 데마실 pc2

생성자: 벼리 문
생성 일시: 2024년 4월 15일 오후 7:28

- 문벼리
    
    병합 관련된 생각정리
    
    1- job
    
    직업에 관련하여, 데이터 프레임에는
    
    job post가 있고, job_s가 있고, job i가 있다.
    
    직업에 대한 모집글이 올라가 있는 곳에 추가로 job_s에서 필요 기술을 넣으면 될 것 같고, job_i에서도 산업정보를 입력하면 좋을 것같다. 또한 각 직업에 대해 ben으로 혜택에 대한 정보를 추합하고, 
    
    현재 공고 모집중인 직업에 대한 전체적인 정보 df를 만드는게 좋을 듯, 여기서 중요한점은 모든 직업에 대한 정리가 아닌, 모집 공고중인 직업에 대한 데이터를 다뤄야 하기 때문에
    
    1차적으로 만들 데이터프레임은 jobP를 기준으로 가지고 와야 한다는 것. 
    
    기존 jobP에서 필요하지 않을만한 내용을 빼는게 좋을듯
    
    그리고 NA값에 대해서 우선은 전처리를 하지 않고 진행
    
    급여에 대해서도, 타입과 파트타임인지, 풀타임인지 등에 대한 정리가 필요하며.
    
    두번째는 회사에 대한 병합
    
    세번째는 각 회사별로 찾고자 하는 직업이 있기 때문에 이에 대한 내용도 정리가 필요
    
    ```jsx
    library(dplyr)
    library(tidyr)
    library(stringr)
    
    ```
    
    ```jsx
    JobP <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_postings.csv")
    jobP는 직책, 설명, 급여, 근무 유형, 위치 등 각 채용 공고에 대한 자세한 정보가 포함되어 있음
    
    summary(JobP)
    
    comp <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/companies.csv")
    comp에는 회사명, 웹사이트, 설명, 규모, 위치 등 채용공고를 올린 각 회사에 대한 자세한 정보가 포함되어 있음
    
    comp_i <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/company_industries.csv")
    comp_i에는 회사별 id와 산업에 대한 정보가 포함되어있어, comp에 회사 id와 join하면 좋을 것 같다
    
    comp_s <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/company_specialities.csv")
    comp_s에는 회사별 id와 전문분야에 대한 정보가 포함되어 있다.
    
    emp_c <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/employee_counts.csv")
    emp_c에는 각 회사별 id와 직원, 팔로워 수에 대한 정보가 포함되어 있다.
    
    ben <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/benefits.csv")
    ben에는 직업 id와 관련된 혜택에 대한 정보가 있다.
    
    job_i <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_industries")
    job_i에는 각 직업과 관련된 산업에 대한 정보가 있다.
    
    job_s <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_skills.csv")
    jos_s에는 job id와 직업과 관련된 스킬이 있다.
    ```
    
    링크는 추후 github의 링크를 이용
    
    ## Task1: 모든 csv파일을 각각의 dataFrame으로 하여 각 데이터별 column의 의미와 type, value등을 파악하여 보자
    
    ```jsx
    JobP <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_postings.csv")
    jobP는 직책, 설명, 급여, 근무 유형, 위치 등 각 채용 공고에 대한 자세한 정보가 포함되어 있음
    
    ```
    
    링크드인에 올라간 job posting DF, 총 27개의 변수가 있으며 각각에 대한 설명 15886행
    
    - 변수-설명-타입
        
        
        | job_id | 직업 코드 | num |
        | --- | --- | --- |
        | company_id | 기업 코드 | num |
        | title | 직업 명 | char |
        | description | jobPosting소개 | char |
        | max_salary | 최대 급여 | num |
        | med_salary | 중간 급여 | num |
        | min_salary | 최저 급여 | num |
        | pay_period | 급여 종류-월,년 | char |
        | formatted-work-type | 근무 요건(시간, 계약) | char |
        | location | 근무지 | char |
        | applies | 지원자 수 | num |
        | original_listed_time | 목록이 올라간 시간? | num |
        | remote_allowed | 원격근무 여부? | num |
        | views | 읽은 사람 수 | num |
        | job_posting_url | posting 링크 | char |
        | application_url | 기업 정보링크 | char |
        | application_type | 지원서 제출 타 | char |
        | expiry | 만료 기간 | num |
        | closed_time | 종료 시간 | num |
        | formatted_experience_level | 직급 | char |
        | skills_desc | 공란 |  |
        | listed_time | 올라간 시간 | num |
        | posting_domain | 회사 위치 | char |
        | sponsored | 스폰서 여부 | num |
        | work_type | 직업 타입 formatted와 동일 | char |
        | currency | 지불 종류 | char |
        | compensation_type | 급여 종류-기본급 등 | char |
    
    ```jsx
    
    job_i <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_industries")
    job_i에는 각 직업과 관련된 산업에 대한 정보가 있다.
    ```
    
    산업과 관련된 파일 2개 변수 21993행
    
    - 변수-설명-타입
        
        
        | job_id | 직업코드 | num |
        | --- | --- | --- |
        | inderstry_id | 산업코드 | num |
    
    특이사항으로 산업코드가 숫자형식으로 되어있어, 어떤 산업인지 확인이 어려움
    
    ```jsx
    job_s <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/job_skills.csv")
    jos_s에는 job id와 직업과 관련된 스킬이 있다.
    ```
    
    직업 기술과 관련된 파일, 2개 변수 27899개 행
    
    - 변수-설명-타입
        
        
        | job_id | 직업코드 | num |
        | --- | --- | --- |
        | skill_abr | 기술종류 | char |
    
    특이사항으로  skill_abr에는 직무의 약어가 쓰여 있으며, 하나의 직업이 여러개의 약어를 가질 수 있는 것으로 보임. 이에대해 각 직업별 약어를 하나의 변수에 넣는것이 필요하다고 생각함
    
    ```jsx
    ben <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/benefits.csv")
    ben에는 직업 id와 관련된 혜택에 대한 정보가 있다.
    ```
    
    직업별 관련 혜택에 대한 정보 3개변수 13761행
    
    - 변수-설명-타입
        
        
        | job_id | 직업코드 | num |
        | --- | --- | --- |
        | inferred | 제공되는가 아닌가 | 0/1 |
        | type | 제공되는 혜택 | char |
    
    특이사항으로 ben 데이터프레임은  tidy가 적용되어 직업코드별 혜택이 모아져 있으므로 spread를 사용하여 혜택을 바꾸는 작업이 선행되어야 함
    
    ```jsx
    comp <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/companies.csv")
    comp에는 회사명, 웹사이트, 설명, 규모, 위치 등 채용공고를 올린 각 회사에 대한 자세한 정보가 포함되어 있음
    
    ```
    
    회사에 대한 구체적인 정보를 가진 df, 10개 변수 6063행
    
    - 변수-설명-타입
        
        
        | company_id | 회사id | num |
        | --- | --- | --- |
        | name | 회사명 | char |
        | description | 회사소개 | char |
        | company_size | 회사 규모 | num |
        | state | 지역 | char |
        | country | 국가 | char |
        | city | 도시 | char |
        | zip_code | 우편번호 | char |
        | address | 주소 | char |
        | url | 회사 링크 | char |
    
    특이사항으로 회사 뿐 아니라 지역에 대한 정보가 많아 하나로 합쳐도 괜찮을듯, 혹은 그룹화를 통해 같은 지역에 위치한 도시 혹은 국가에 대해 확인이 가능함
    
    ```jsx
    comp_i <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/company_industries.csv")
    comp_i에는 회사별 id와 산업에 대한 정보가 포함되어있어, comp에 회사 id와 join하면 좋을 것 같다
    
    ```
    
    회사별 id와 산업에 대한 정보, 2개 변수 15880행
    
    - 변수-설명-타입
        
        
        | company_id | 회사코드 | num |
        | --- | --- | --- |
        | industry | 산업 | char |
    
    특이사항으로 하나의 company_id에 대해 중복되어 행이 만들어진 경우가 있어 전처리가 필요함, 다른 df와 병합할때 이 부분을 고려해야 함 시작부터 company_id가 중복되는 값들을 확인한 후 이 값들의 industry도 동일하다면 그 값은 지워서 데이터의 중복을 피하고자 함
    
    ```jsx
    comp_s <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/company_specialities.csv")
    comp_s에는 회사별 id와 전문분야에 대한 정보가 포함되어 있다.
    ```
    
    회사별 id와 회사의 전문분야 2개변수 128355행
    
    - 변수-설명-타입
        
        
        | company_id | 회사코드 | num |
        | --- | --- | --- |
        | speciality | 전문분야 | char |
    
    특이사항으로, 하나의 회사가 많은 양의 전문분야를 가지고 있어 데이터의 크기가 상당히 부풀려져 있음. 이를 정리한 후 병합을 시도해야 함
    
    ```jsx
    emp_c <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Mining-Practicum/main/myR/employee_counts.csv")
    emp_c에는 각 회사별 id와 직원, 팔로워 수에 대한 정보가 포함되어 있다.
    ```
    
    회사별 id와 직원수, 팔로워 수에 대한 정보 포함
    
    - 변수-설명-타입
        
        
        | company_id | 회사코드 | num |
        | --- | --- | --- |
        | employee_count | 사원 수 | num |
        | follower_count | 이용자 수 | num |
        | time_recorded | 기록된 시간 | num |
    
    employee_count가 comp의 company_size와는 다른것인지 확인 필요
    
    ## Task1-2: 8개의 데이터 프레임을 어떻게 병합할 것인지 계획을 세운 후 그 순서와 방법에 대해 기술하기, +이왕이면 그림을 사용하여 병합 방법에 대한 계획을 세울 것
    
    1.comp와 job 두개의 큰 대분류로서 데이터 프레임 병합
    
    job의 경우
    
    jobP에서 회사별로 그룹을 나눠 하나의 회사에서 뽑는 직무를 확인하고
    
    다음으로는 직업별로 그룹을 나눠 분류하여 회사들의 산업을 그룹화함 → 의미없음
    
    job_s의 경우 하나의 직업을 기준으로 직무를 정리하여 전처리를 마친 후 마찬가지로 하나의 직업에 대해 혜택을 정리한 ben과 병합
    
    이후 job_i도 병합시킴
    
    job_full을 완성 
    
    job_i에 대해서 마찬가지로 하나의 직업에 대해 산업을 정리한 후, JobP와 병합하여  모집공고가 올라온 직업에 대해 산업들을 병합하고 이후 여기에 comp_i를 붙여 코드와 함께 직업이 뭐가 있는지 확인
    
    다음으로 comp의 경우 
    
    comp_s에서 직업 기업별 전문분야를 하나로 정리하고, comp_i를 jobP와 comp에 병합하여 기업이 어느 산업에 투자했는지 확인, 이후 comp_s역시 comp에 병합해 전문분야와 주력하는 산업 등에 대한 내용 확인
    
    마지막으로 comp_c도 병합
    
    ![image.png](image.png)
    
    ![image.png](image%201.png)
    
    ## Task 1-3: 계획된 방법을 사용하여 실제 병합을 진행한 후, 데이터프레임을 점검하여, 정상적으로 병합되었는지 확인 체크리스트를 통해 병합이 잘 되었는지 확인하고, 최대한 오류가 적은 병합을 완료하자
    
    실제 병합된 job코드 첨부
    
    ```jsx
    job_s <- job_s %>%
      group_by(job_id) %>%
      summarise(skill_abr_combined = paste(skill_abr, collapse = ", "))
    #jos_s를 병합에 용이하도록 skill_abr 변수의 내용들을 하나의 job_id에 밀어넣음
    
    ben<- ben%>%
      filter(inferred >= 1)%>%
      group_by(job_id)%>%
      summarise(benif = paste(type, collapse = ", "))
    
    #혜택에 대해서 혜택이 없는 행을 제거한 후, 하나의 id에 병합
    
    job_i<-job_i%>%
      group_by(job_id)%>%
      summarise(industry_id = paste(industry_id, collapse = ", "))
    #산업에 관련해서도 병합
    
    job_full<-JobP%>%
      left_join(job_i, by = "job_id")%>%
      left_join(job_s, by = "job_id")%>%
      left_join(ben,by = "job_id")
    #job에 대하여 병합
    
    comp_i3<-comp_i %>%
      group_by(company_id, industry) %>%  # company_id와 industry 둘 다 기준으로 그룹화
      filter(n() > 1) %>%                 # 중복된 항목만 필터링
      ungroup() %>%                       # 그룹화 해제
      distinct(company_id, industry, .keep_all = TRUE)%>%# 중복된 항목 중 하나만 남기기 distinct를 사용함
      group_by(company_id)%>%
      summarise(industry = paste(industry, collapse = ", "))
    #회사별 산업에 대하여 중복된 내용을 필터링한 후 병합
    
    comp_s<-comp_s%>%
      group_by(company_id)%>%
      summarise(speciality = paste(speciality, collapse = ", "))
    #회사별 기술에 대해서 동일 id에 대해 하나의 행으로 정렬
    
    emp_c<-emp_c %>%
      group_by(company_id) %>%
      filter(n() > 1) %>%
      ungroup()%>%
      arrange(company_id)%>%
      distinct(company_id, .keep_all = TRUE)
    #회사별 인원수에 대해서도 동일하게 진행
    
    comp_full<-comp%>%
      left_join(comp_i3,by = 'company_id')%>%
      left_join(comp_s, by = 'company_id')%>%
      left_join(emp_c, by = 'company_id')
      
    #회사에 대해 병합 후
    
    job<-job_full%>%
      left_join(comp_full,by = "company_id")
    #company_id를 기반으로 jobP와 comp를 병합
    ```
    
    확인 체크리스트
    
    job_id수에 대한 숫자비교
    
    병합시 obs 수 비교
    
    ## Task 2: 병합 결과를 활용하여, Job Market에 대해 파악하기 위한 EDA 및 데이터 분석을 수행하자
    
    ### Industry별로 Salary range가 어떻게 다른지
    
    ```jsx
    IndJ<-job %>%
      mutate(salary_range = ifelse(!is.na(max_salary) & !is.na(min_salary), 
                                   max_salary - min_salary, NA))%>%
      select(job_id,salary_range)
    #salary_range를 계산하고, 또한 job_id와 함께 하나의 데이터프레임으로 만들어 병합을 준비
    
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
      
    #IndJ df에 job_id를 기준으로 job_i와 병합을 시도한 후, 계산에 방해가 되는 NA값들을 모두 제거함
    #이후 industry_id로 그룹화를 한 후 그룹별 평균 = 산업별 평균 salary_range를 계산함
    #이후 industry_id가 수치형 변수이기 때문에 이를 character로 바꾸어준 후 시각화로 표현함
    ```
    
    ![image.png](image%202.png)
    
    보는바와 같이 industry_id가 너무 많아 정확한 확인이 어려움
    
    salary Range가 일반적인 값보다 조금 더 높은 몇개의 직업이 있다. 이 특이값에 대해 필터를 적용한 후 확인해보자
    
    ```jsx
    IndJ %>%
      left_join(job_i, by = "job_id") %>%          # 'job_id' 기준으로 병합
      filter(complete.cases(.)) %>%                # NA 제거
      group_by(industry_id) %>%                    # 'industry_id'로 그룹화
      summarise(mean_salary_range = mean(salary_range, na.rm = TRUE)) %>%  # 그룹별 평균 계산
      mutate(industry_id = as.character(industry_id)) %>% 
      filter(mean_salary_range > 100000)%>%
      select(mean_salary_range,industry_id)
    ```
    
    ![image.png](image%203.png)
    
    산업코드 113, 129, 141이 가장 salary_range가 크다, 이는 곧 최대급여와 최소급여간의 격차가 크다는 것을 의미한다. 
    
    ### 요구하는 기술에 따라 Salary range가 얼마나 다른지
    
    요구하는 기술에 대해서 두가지의 생각을 했다. 직업과 관련된 스킬인 job_s의 skills_abr변수와 job df에 있는 formatted_experience_level변수이다. formatted_experience_level변수의 경우 직업 리쿠르팅에서 원하는 요구기술의 숙련정도를 말하는 부분이기에, 두가지 각각에 대해 정리하고 시각화를 진행하였다.
    
    ```jsx
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
    #formatted_experience_level변수를 사용하여 원하는 기술수준에 따른 Salary Range를 정리했다.
    #salary_range를 계산하고 formatted_experience_level를 그룹화한 뒤, 그룹별 salary_range의 평균을
    계산하였다. 이후 ggplot로 나타내 보았다.
    #추가적으로 데이터프레임에서 공백으로 나타난 부분에 대해 Notting으로 요구하는 기술수준이 없다는
    것을 보인 후, 그래프상으로 x축도 "Notting","Internship", "Entry level", "Associate", "Mid-Senior level", 
    "Director", "Executive" 순으로 정렬했다.
    
    ```
    
    ![image.png](image%204.png)
    
    기술 수준에 따라 가격격차가 점점 올라가는것이 확인됨.
    
    ```jsx
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
    #직업별 요구하는 기술 수준에 따른 salary range를 계산하였다.
    ```
    
    ![image.png](image%205.png)
    
    ### 회사의 규모(직원수)가 많은 회사는 모집도 많이 하는지
    
    회사의 직원수에 따른 모집을 확인하기 위해서는
    
    회사의 규모(직원수)에 대한 변수와, 회사별 리쿠르팅을 하는 횟수에 대한 내용이 필요하다.
    
    jobP df와 company를 비교하여 기업당 리쿠르팅 횟수를 체크하고,
    
    company별 규모를 확인한 후 둘 사이의 관계를 비교하고자 한다.
    
    ```jsx
    job%>%
      select(company_id,employee_count,name)%>%
      filter(complete.cases(.))%>%
      group_by(company_id)%>%
      mutate(Number_of_recruits  = n())%>%
      distinct(company_id, .keep_all = TRUE)%>%
      arrange(desc(employee_count))
    #na값을 전부 제거한 후, 직원수와 company_id의 수를 세어 리쿠르팅의 횟수를 세었다.
    
    ```
    
    ![image.png](image%206.png)
    
    직원수를 보면 누구나 알듯한 대기업이며, 이 대기업들의 규모에 대해 리쿠르팅 수는 다음과 같았다.
    
    이번에는 arrange를 리쿠르팅 수로 다시 계산해 보았다.
    
    ![image.png](image%207.png)
    
    아마존과 구글 외에 다른 기업은 규모가 다른 대기업에 비해서는 조금 적은 거 같다. 
    
    다음은 이를 시각화하여 더 확인하기 편하게 만들어보고자 한다.
    
    ```jsx
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
      labs(x = "Number Of Recruits")
    
     3
    ```
    
    ![image.png](image%208.png)
    
    대부분의 모집이 직원수와 리쿠르팅이 많이 않지만 리쿠르팅이 많은 기업들은 대부분 중견 이상의 기업임을 확인할수 있다.
    
    보다 시각화를 위해 직원수가 500명 이상, 그리고 리쿠르팅 수가 50회 이상인 경우를 찾아 선을 그어보겠다.
    
    ```jsx
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
      geom_hline(yintercept = 500, linetype = "dashed", color = "blue") +
      geom_vline(xintercept = 50, linetype = "dashed", color = "red")
    ```
    
    ![image.png](image%209.png)
    
    리쿠르팅이 50회를 넘는 거의 대부분의 경우, 직원의 수가 500명 이상임을 확인할 수 있다. 조금 더 선을 내려서 직원의 수를 100명 이상으로 제한한다면 
    
    ![image.png](image%2010.png)
    
    50회 이상 리쿠르팅한 모든 기업이 최소 100명 이상의 직원을 가졌음을 알 수 있다.
    
    ### 국가나 지역에 따른 job에서 어떤 차이가 있는지
    
    ```jsx
    job%>%
      left_join(job_s, by = "job_id")%>%
      select(country,skill_abr)%>%
      filter(complete.cases(.))%>%
      group_by(country, skill_abr) %>%  # country와 skill_abr_combined으로 그룹화
      summarise(skill_count = n()) %>%  # 각 지역과 기술의 빈도수 계산
      top_n(1, skill_count) %>%  # 각 지역에서 가장 많이 등장한 기술 추출
      arrange(desc(skill_count))
    지역별로 skill의 특성에 따라 가장 많이 등장한 기술에 대해 추출해보았다.
    ```
    
    대부분의 국가에서 IT기술에 대한 직업 리쿠르팅이 많은 것을 확인할 수 있고 그중에서도 미국에서의 IT기업 수요가 상당하다는 것을 확인할 수 있다. 여기서 추가로 미국에 조금 더 집중해보자
    
    ![image.png](image%2011.png)
    
    ```jsx
    job%>%
      left_join(job_s, by = "job_id")%>%
      filter(country == "US")%>%
      select(title, country, skill_abr)%>%
      filter(complete.cases(.))%>%
      group_by(skill_abr)%>%
      summarise(skill_count = n()) %>%
      arrange(desc(skill_count))
    #미국에서 리쿠르팅한 직업들의 특성수를 조사해 보았다.
    ```
    
    ![image.png](image%2012.png)
    
    IT업무와 영업, 관리직종 순으로 높은 순위를 기록하고 있는 것을 볼 수 있었다.
    
    ### Task 3: 해외취업을 한다고 할 때, 원하는 직업의 요구 직무, salary 수준, 복지, 위치, 기술 등에 대한 조건을 정리한 후 원하는 조건에 맞는 job posting을 찾고, 선호하는 순서 top 3를 제시해보자
    
    문벼리:
    
    It직무, salary수준은 186 USD, 위치는 1순위 KR, 2순위 US, 복지는 상관 없음. formatted_experience_level는 인턴쉽 수준
    
    ```jsx
    job%>%
      select(title,country,formatted_experience_level,min_salary,skill_abr_combined,pay_period)%>%
      filter(complete.cases(.))%>%
      mutate(min_salary = ifelse(pay_period == "YEARLY", min_salary / 12,ifelse(pay_period == "HOURLY", min_salary * 160,min_salary)))%>%
      filter(min_salary > 186, 
             country == "KR", 
             formatted_experience_level %in% c("Notting", "Internship"))%>%
      filter(str_detect(skill_abr_combined, "IT")) %>%
      select(title,formatted_experience_level,min_salary,skill_abr_combined)%>%
      arrange(desc(min_salary))
    #한국에서 할수있고, 급여는 월급으로 환산하여 186USD(250,000), 직무는 IT계열이며 인턴쉽 수준이거나
    혹은 경력을 확인하지 않는 직무로 찾아보았다. 그러나 이렇게 했을때 아무것도 확인되지 않았다.
    
    job%>%
      select(title,country,formatted_experience_level,min_salary,skill_abr_combined,pay_period)%>%
      filter(complete.cases(.))%>%
      mutate(min_salary = ifelse(pay_period == "YEARLY", min_salary / 12,ifelse(pay_period == "HOURLY", min_salary * 160,min_salary)))%>%
      filter(country == "KR")%>%
      filter(str_detect(skill_abr_combined, "IT")) %>%
      select(title,formatted_experience_level,min_salary,skill_abr_combined)%>%
      arrange(desc(min_salary))
    #조금 더 광범위하게 찾기 위해 지역을 KR로, 직종을 IT기업으로만 나누니 한개의 링크드인 리쿠르팅 정보가
    나왔다.
    
                              title formatted_experience_level min_salary skill_abr_combined
    1 Senior Cyber Security Analyst           Mid-Senior level       9660            IT, ENG
    ```
    
    이번에는 국가를 미국으로 조금 더 올려보자
    
    ```jsx
    job%>%
      select(title,country,formatted_experience_level,min_salary,skill_abr_combined,pay_period)%>%
      filter(complete.cases(.))%>%
      mutate(min_salary = ifelse(pay_period == "YEARLY", min_salary / 12,ifelse(pay_period == "HOURLY", min_salary * 160,min_salary)))%>%
      filter(min_salary > 186, 
             country == "US", 
             formatted_experience_level %in% c("Notting", "Internship"))%>%
      filter(str_detect(skill_abr_combined, "IT")) %>%
      select(title,formatted_experience_level,min_salary,skill_abr_combined)%>%
      arrange(desc(min_salary))
    ```
    
    ![image.png](image%2013.png)
    
    연구 인턴쉽, 지휘자 연수생 - 1차 리콜- 얼라이언스, 텍사스주, 이제조 공정 엔지니어 인턴, 2024 아마존 운영 재무 순환 프로그램 여름 인턴십 등등이 확인되었다.
    
    조건 내에서 아무래도 대부분 인턴쉽에 대한 내용이니, 추가적으로 work_type도 확인해보자
    
    ```jsx
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
    ```
    
    ![image.png](image%2014.png)
    
    아무래도 Full Time근무를 더 선호하므로 나는 최저급여가 높은순으로 선택하는게 좋은 것 같다.