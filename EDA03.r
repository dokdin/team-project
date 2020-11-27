
# 필요 패키지 설치 tt
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
df<-read.csv("C:/data/team/teamproject/openclose.csv")
gu_name<- read.csv("c:/data/team/teamproject/gu_name2.csv")
 # 파일확인
# View() : 테이블로 보기
# dim () : 데이터프레임 차원수 확인 (행 열) 
# str () : 데이터 구조 변수 개수 변수 명 관치 개수 관찰치 미리보기 
View(df)
View(gu_name)
dim(df)
dim(gu_name)





# 우리나라 지도 GIS포맷
map_shape <- shapefile("c:/data/team/TL_SCCO_SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")
View(map)

# 행정코드를 이용해서 서울만 추출
map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]

# 기존데이터파일이랑 SHP파일 병합
 M <- merge(seoul_map, df, by = "id")

View(M)

# M2<-M %>%
#     group_by(id)%>%
#     summarise(mean_lat=mean(lat))

# View(M2)
# # write.csv(M2,"c:/data/team/lat.csv",row.names=FALSE)

#글꼴
font_add_google('Nanum Pen Script', 'pen')
font_add_google('Jua','pen2')
font_add_google('Do Hyeon','pen3')

#자동실행
showtext_auto()

#사이즈 기본값 지정
update_geom_defaults("text", list(size = 4))

# 5년간 서울 시군구별개업현황

ggplot() + 
    geom_polygon(data = M, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = open),
                color = "white") +
    scale_fill_gradient(low = "#D4F4FA",
                        high = "#3DB7CC",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "5년간 서울 시군구별 편의점 개업현황") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen2'))+
    geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, open, sep = "\n"),family='pen2')) 




# 5년간 서울 시군구별폐업현황 
ggplot() + 
    geom_polygon(data = M, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = close),
                color = "white") +
    scale_fill_gradient(low = "#D5D5D5",
                        high = "#4C4C4C",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "5년간 서울 시군구별 편의점 폐업현황") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen2'))+
    geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, close, sep = "\n"),family='pen2')) 

