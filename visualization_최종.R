library(leaflet)
library(dplyr)
library(ggmap)


register_google(key = 'AIzaSyAmVdasdGda1vorEd-sUCC4ypsmwcSSq9g')
seoul_lonlat=unlist(ggmap::geocode('seoul',source = 'google'))
IdBell<-read.csv("total_data_20190818_2.csv", stringsAsFactors = F)


typeof(IdBell$lon)
IdBell$lon <- as.double(IdBell$lon)
typeof(IdBell$lat)
IdBell$lat <- as.double(IdBell$lat)

leaflet(IdBell) %>%
  setView(lng = seoul_lonlat[1], lat = seoul_lonlat[2], zoom = 11) %>%
  addProviderTiles('Stamen.Toner') %>% 
  addCircles(data = IdBell %>% filter( V == '범죄자 거주지 중립 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '범죄자 거주지 중립 지역', 
             color = '#ffbc42', radius=300 ) %>% 
  addCircles(data = IdBell %>% filter( V == '범죄자 거주지 위험 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '범죄자 거주지 위험 지역', 
             color = '#9055a2',radius = 300) %>%
  addCircles(data = IdBell %>% filter( V == '학교 중립 지역(나)'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '학교 중립 지역(나)', 
             color = '#75d701',radius = 300 ) %>%
  addCircles(data = IdBell %>% filter( V == '학교 안전 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '학교 안전 지역', 
             color = '#508aa8',radius = 300 ) %>%
  addCircles(data = IdBell %>% filter( V == '학교 위험 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '학교 위험 지역', 
             color = 'red',radius = 300 ) %>%
  addCircles(data = IdBell %>% filter( V == '학교 중립 지역(가)'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '학교 중립 지역(가)', 
             color = '#9dd1f1',radius = 300 ) %>%
  addCircles(data = IdBell %>% filter( V == '안전 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '안전 지역', 
             color = 'yellow',radius = 300 ) %>%
  addCircles(data = IdBell %>% filter( V == '중립 지역'), 
             lng = ~lon, lat=~lat, popup = ~V, group = '중립 지역', 
             color = 'gray',radius = 300 ) %>%
  addLayersControl(overlayGroups = unique(IdBell$V))
options = layersControlOptions(collapsed = FALSE)

