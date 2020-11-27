
# 필요 패키지 설치 
# install.packages("ggplot2")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("dplyr")
# install.packages('raster')
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages('showtext')

# 필요 패키지 로드 

library(dplyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(raster)
library(showtext)

# 파일불러오기
df<-read.csv("c:/data/project/team/raw/rent.csv")
seoulid<-read.csv("c:/data/project/team/raw/seoul_id.csv")
gu_name <- read.csv("c:/data/project/team/raw/gu_name.csv",
                       header = TRUE)

# 파일확인
# View() : 테이블로 보기
# dim () : 데이터프레임 차원수 확인 (행 열) 
# str () : 데이터 구조 변수 개수 변수 명 관치 개수 관찰치 미리보기 
View(df)
View(seoulid)
View(gu_name)
dim(seoulid)
dim(gu_name)
dim(df)
str(seoulid)
str(gu_name)
str(df)

# 변수명이 깨지는 걸 방지하여 한글 변수명 변경
names(df)<-c('seoulgu','seouldong','avg','first','etc')
names(seoulid)<-c('seoulgu','id')


# 구별로 시군구별 평균 임대가격 mean으로 요약하고 저장 
View(df)
seoul_gu<-df %>%
    group_by(seoulgu) %>%
    summarise(mean_seoul_gu=round(mean(avg),2))


# seoul_gu<-as.numeric(seoul_gu$mean_seoul_gu)
View(seoulid)
str(seoul_gu)


# 서울구 컬럼을 기준으로 왼쪽으로 조인 
seoul <- left_join(seoul_gu, seoulid, by = "seoulgu")

#확인
View(seoul)

# 우리나라 지도 GIS포맷
map_shape <- shapefile("c:/data/project/team/raw/TL_SCCO_SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")
View(map)

# 행정코드를 이용해서 서울만 추출
map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]

# 기존데이터파일이랑 SHP파일 병합
 M <- merge(seoul_map, seoul, by = "id")

View(M)

# M2<-M %>%
#     group_by(id)%>%
#     summarise(mean_lat=mean(lat))
# write.csv(M2,"c:/data/team/lat.csv",row.names=FALSE)

#구글 폰트
font_add_google('Nanum Pen Script', 'pen')
font_add_google('Jua','pen2')
font_add_google('Do Hyeon','pen3')

#자동실행
showtext_auto()

#사이즈 기본값 지정
update_geom_defaults("text", list(size = 4))

ggplot() + 
    geom_polygon(data = M, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = mean_seoul_gu),
                color = "white") +
    scale_fill_gradient(low = "#FBCF61",
                        high = "#00CC99",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "서울 시군구별 평균 임대료") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen2'))+
    geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, mean_seoul_gu, sep = "\n"),family='pen2')) 

