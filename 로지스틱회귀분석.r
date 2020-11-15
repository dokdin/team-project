
# install.packages('corrplot')
# install.packages('ROSE')
# install.packages('caret')
# install.packages("car")


library(car)
library(dplyr)
library(corrplot)
library(ROSE)
library(caret)
# 파일불러오기
df<-read.csv('c:/data/project/team/데이터최종본r.csv')


View(df)

# 범주형변수인 도로명주소, 구 주소제외 
df_remove<-df%>%select(-도로명,-구,-개업율,-폐업률,-극장,-폐업점포수,-관공서,-남성상주인구,-여성상주인구,-남성10대상주인구,-남성20대상주인구,-유사업종점포수,
-남성30대상주인구,-남성40대상주인구,-남성50대상주인구,
-여성20대상주인구,-여성30대상주인구,-여성40대상주인구,-여성50대상주인구,-여성60대상주인구,-총상주인구,-비아파트가구,-총가구,-유동인구남성,-유동인구여성,
-유동인구10대,-유동인구20대,-유동인구30대,-유동인구40대,-유동인구50대,-유동인구60대이상,-남성직장인10대,-마트,-아파트가구,
,-남성직장인20대,-남성직장인30대,-남성직장인40대,-남성직장인50대,-여성직장인10대,-여성직장인20대,-여성직장인30대,-여성직장인40대,-여성직장인50대,-여성직장인60대,-대중교통,
-상주인구10대,-상주인구20대,-상주인구30대,-상주인구40대,-상주인구50대,-상주인구60대이상,-가구1인,-가구2인,-가구3인,-가구4인,-가구5인,-가구6인,-가구7인이상,
-평균영업기간,-월매출평균,-임대료19년,-임대료16년,-임대료17년,-임대료18년)


ncol(df_remove)

# 제외되었나 확인
View(df_remove)

 
#상관도 확인
corrmatrix<-cor(df_remove)
corrplot(cor(df_remove),method='circle')

# 종속변수 데이터(빈도)수 확인 
tbl<-table(df_remove$result)
tbl

#시각화
barplot(tbl,beside=TRUE,legend=TRUE,col=rainbow(2))


# UnderSampling
df_samp<-ovun.sample(result~.,data=df_remove,seed=1,method="under",N=796)$data
tbl<-table(df_samp$result)
tbl
# 시각화
barplot(tbl,beside=TRUE,legend=TRUE,col=rainbow(2))

# Random Seed고정 
set.seed(123)
df_samp$result
idx_train<-createDataPartition(y=df_samp$result, p=0.8, list=FALSE)


# 변수 개수 확인
ncol(df_remove)

 #학습용
train<-df_samp[idx_train,]
X_train<-train[,-13]
y_train<-train[,13]


View(y_train)

#검증용
test<-df_samp[-idx_train,]
X_test<-test[,-13]
y_test<-test[,13]


head(X_train)
head(y_train)

model<-glm(result~.,
data=train,
family=binomial(link="logit"))

print(summary(model))

(coef1<-coef(model))

 #다중공선성확인
vif(model)
 
