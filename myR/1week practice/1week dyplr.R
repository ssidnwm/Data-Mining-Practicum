library(dplyr)
x <- 1:10
x %>% sum()
mtcars
mtcars %>%
  subset(cyl ==8) %>%
  select(mpg,wt)


load(url('https://github.com/hbchoi/SampleData/raw/master/hflights.RData'))

hf_data %>%
  select(ends_with("Delay"))

hf_data %>%
  mutate(ActualGrandTime = ActualElapsedTime - AirTime,
         GroundTime = TaxiIn + TaxiOut) %>%
  select(ActualGrandTime,ActualElapsedTime,AirTime,GroundTime)




hf_data %>%
  filter(Cancelled==1)

hf_data %>%
  filter(Distance >= 3000)

hf_data %>%
  filter(Cancelled ==1, !is.na(DepDelay)) %>%
  arrange(CancellationCode)

hf_data %>%
  arrange(CancellationCode)
 
hf_data %>%
  arrange(UniqueCarrier, desc(DepDelay))

hf_data %>%
  summarise(min_dist = min(Distance), max_dist = max(Distance))

hf_data %>%
  group_by(UniqueCarrier) %>%
  summarise(aveDep = mean(DepDelay, na.rm = T),
            aveArr = mean(ArrDelay, na.rm = T)) %>%
  arrange(aveArr, aveDep)


#연습문제 1 각 항공사(Unique carrier)별로 비행편수를 계산해서, 해당 기간동안 가장 많은 비행편수가 많은 항공사부터 
#정렬하기 가장 비행편을 많이 운행한 항공사는 어디이며, 가장 적게 운행한 항공사는 어디인가.
unique(hf_data$UniqueCarrier)

#데이터에서 항공사별로 그룹으로 묶고, 각각을 summarise를 통해 count, 각 그룹별 수를 계산함
hf_data %>%
  group_by(UniqueCarrier)%>%
  summarise(count = n())%>%
  arrange(count)


#연습문제 2 각 목적지 별로 Dest ) 운항시간 A(ctualElapsedTime) 의 평균을 계산하여 , 평균적으로 가
#장 오래걸리는 목적지는 어디이며 가장 빨리도착하는 목적지는 어디인지 찾아보자

#똑같이 Dest별로 그룹으로 묶은 뒤, 운항시간의 평균을 계산하고 정렬하면 좋을 것 같다.
hf_data %>%
  group_by(Dest)%>%
  summarise(Times = mean(ActualElapsedTime,na.rm =  T)) %>%
  arrange(Times)

#연습문제 3 각 항공사별로 cancel 이 된 항공편의 비율을 계산해보고 , 비율이 높은 항공사부터 정렬
#하여라 , cancel 이 될 확률이 가장 높은 항공사는 어디이며 확률이 얼마나 되는가

# 1. 항공사별 그룹화, 이후 summrise를 이용해 cancel의 비율 계산, 이후 arrange로 정렬
hf_data %>%
  group_by(UniqueCarrier)%>%
  summarise(cancelrate = Cancelled/n(), na.rm = T)
#error, 이 경우, 나는 전체에 대한 cancelled를 계산하려고 하였으며, na에 대한 처리 역시 잘못되었다.

hf_data %>%
  group_by(UniqueCarrier)%>%
  summarise(Cancelrate = sum(Cancelled, na.rm = T) / n())%>%
  arrange(Cancelrate)
#이렇게 하면 cancelrate는 na값을 제외한 cancel의 값들을 더하고, 이후 이를 n값에서 나눈다

