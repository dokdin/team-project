# 직장인수대비매출액
# 필요 패키지 설치 및 로드 
# install.packages('dplyr')
# install.packages('corrplot')
library(dplyr)
library(corrplot)

# csv 파일 불러오기 (엑셀 전처리 과정 생략)
coworker<-read.csv('c:/data/team/직장인2.csv')
sales<-read.csv('c:/data/team/매출정보.csv')
View(coworker)
View(sales)


# 구별 매출액
sales_gu <-sales %>% 
            group_by(구) %>%
                 summarise(구별평균매출=mean(매출금액),
                 총매출건수=sum(매출건수),
                 총점포수=sum(점포수))

View(sales_gu)

#구별 직장인수 
coworker_gu <-coworker %>%
            group_by(구) %>%
            summarise(총직장인=sum(총직장인),
            총남성인구=sum(총남성인구),
            총여성인구=sum(총여성인구),
            총남성직장인10대=sum(남성직장인10대),
            총남성직장인20대=sum(남성직장인20대),
            총남성직장인30대=sum(남성직장인30대),
            총남성직장인40대=sum(남성직장인40대),
            총남성직장인50대=sum(남성직장인50대),
            총남성직장인60대=sum(남성직장인60대),
            총여성직장인10대=sum(여성직장인10대),
            총여성직장인20대=sum(여성직장인20대),
            총여성직장인30대=sum(여성직장인30대),
            총여성직장인40대=sum(여성직장인40대),
            총여성직장인50대=sum(여성직장인50대),
            총여성직장인60대=sum(여성직장인60대))

View(coworker_gu)

#  직장인데이터와 매출액 조인
raw1<-left_join(sales_gu,coworker_gu)
View(raw1)

# 구는 명목변수이므로 상관관계시 분석할 시 오류가 발생할수있으므로 제거 
raw2<-raw1[,!(names(raw1) %in% c("구"))]

# 상관계수
# 시각화하기
workersales_cor<-cor(raw2)
round(workersales_cor,2)
corrplot(workersales_cor, method="number")
 


 