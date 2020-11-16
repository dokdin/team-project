
# install.packages('corrplot')
# install.packages('ROSE')
# install.packages('caret')
# install.packages("car")

packageVersion("dplyr")
sessionInfo()


library(car)
library(dplyr)
library(corrplot)
library(ROSE)
library(caret)
# 파일불러오기
df_remove<-read.csv('c:/data/project/team/데이터최종본r.csv')


View(df)

# quantile(df$평균영업기간,probs=c(0.5))



# 평균영업기간을 통해서 종속변수(0,1) 만들어주기
df_result <- df%>%mutate(Result=ifelse(평균영업기간>quantile(df$평균영업기간,probs=c(0.5)),1,0))

# View(df_result)
# # 범주형변수인 도로명주소, 구 주소제외 
df_remove<-df_result%>%select(-도로명,-구,-개업율,-폐업률,-평균영업기간,-관공서,-남성상주인구,-여성상주인구,-남성10대상주인구,-남성20대상주인구,-유사업종점포수,
-남성30대상주인구,-남성40대상주인구,-남성50대상주인구,-극장,-폐업점포수,
-여성20대상주인구,-여성30대상주인구,-여성40대상주인구,-여성50대상주인구,-여성60대상주인구,-총상주인구,-비아파트가구,-총가구,-유동인구남성,-유동인구여성,
-유동인구10대,-유동인구20대,-유동인구30대,-유동인구40대,-유동인구50대,-유동인구60대이상,-남성직장인10대,-마트,-아파트가구,
,-남성직장인20대,-남성직장인30대,-남성직장인40대,-남성직장인50대,-여성직장인10대,-여성직장인20대,-여성직장인30대,-여성직장인40대,-여성직장인50대,-여성직장인60대,-대중교통,
-상주인구10대,-상주인구20대,-상주인구30대,-상주인구40대,-상주인구50대,-상주인구60대이상,-가구1인,-가구2인,-가구3인,-가구4인,-가구5인,-가구6인,-가구7인이상,
-평균영업기간,-월매출평균,-임대료19년,-임대료16년,-임대료17년,-임대료18년)

# write.csv(df_remove,file="c:/data/project/team/로지스틱r변수")

ncol(df_remove)

# 제외되었나 확인
View(df_remove)

 
#상관도 확인
corrmatrix<-cor(df_remove)
corrplot(cor(df_remove),method='circle')

# 종속변수 데이터(빈도)수 확인 
tbl<-table(df_remove$Result)
tbl

#시각화
barplot(tbl,beside=TRUE,legend=TRUE,col=rainbow(2))


# UnderSampling
df_samp<-ovun.sample(Result~.,data=df_remove,seed=1,method="under",N=796)$data
tbl<-table(df_samp$Result)
tbl
# 시각화
barplot(tbl,beside=TRUE,legend=TRUE,col=rainbow(2))

# Random Seed고정 
set.seed(123)
df_samp$Result
idx_train<-createDataPartition(y=df_samp$Result, p=0.8, list=FALSE)


# 변수 개수 확인
ncol(df_remove)

 #학습용
train<-df_samp[idx_train,]
X_train<-train[,-ncol(df_remove)]
y_train<-train[,ncol(df_remove)]


View(y_train)

#검증용
test<-df_samp[-idx_train,]
X_test<-test[,-ncol(df_remove)]
y_test<-test[,ncol(df_remove)]


head(X_train)
head(y_train)

model<-glm(Result~.,
data=train,
family=binomial(link="logit"))

print(summary(model))

(coef1<-coef(model))

 다중공선성확인
vif(model)


# 후진제거법 / 전진제거법 / 두개를 같이 사용 
output1=step(model,direction=c("backward"))
output2=step(model,direction=c("forward"))
output3=step(model,direction=c("both"))


# summary(output1)
# summary(output2)
# summary(output3)


pred<-predict(model,newdata=X_test,type='response')
pred

result<-ifelse(pred>0.5,1,0)

# Accuracy 
mean(y_test==result)
# 오분류표 
table(y_test,result)



