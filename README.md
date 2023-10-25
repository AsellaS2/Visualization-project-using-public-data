
![](https://github.com/AsellaS2/Visualization-project-using-public-data/assets/69001369/d5e3afb8-916d-4b7f-a91c-57afa75fd35b)
<p align="center"><img src="https://img.shields.io/badge/R-75AADB?style=plastic&logo=Rstudio&logoColor=white">
<img src="https://img.shields.io/badge/leaflet-199900?style=plastic&logo=/LeaFlet&logoColor=white"></p>
---
### 분석 배경

![image](https://github.com/AsellaS2/Visualization-project-using-public-data/assets/69001369/d7b2236a-9026-463b-af24-36959a48ab9d)
  
- 2014년부터 성범죄는 지속적으로 증가 중 📈
- 대한민국정책브리핑의 한국여성정책연구원 분석에 따르면, [<U>***성폭력범죄의 43.4%는 성범죄자가 거주하는 지역***</U>](https://www.yna.co.kr/view/AKR20190424073200005)에서 발생하는 것으로 나타났으며, 성폭력범죄의 발생장소로는 피해자나 가해자 등의 집(34.4%)이 가장 많았음
- 아동·청소년들의 성폭력 촬영범죄가 갈수록 늘어나고 있으며, [***성범죄 장소 중 가장 많은 곳이 ‘집’이며, 학교도 10.0%나 됨***](https://www.yna.co.kr/view/AKR20190424073200005)

---
### 활용 데이터
- [서울시 초등학교 위치데이터](https://data.seoul.go.kr/dataList/OA-20555/S/1/datasetView.do) (2019년 기준) 🏢
- [서울시 CCTV & 안전 비상벨 위치데이터](https://www.localdata.go.kr/lif/lifeCtacDataView.do) (2019년 기준) 📸
- [서울시 성범죄자 거주지 위치데이터](https://www.sexoffender.go.kr/m1s2_login5.nsc#) (2019년 기준) 🐾

활용하여 서울시 방범 장치 위치에 대한 인사이트 제시

---
### 분석/시각화 결과 내용 요약  

- 학교 안전 범위 내에 존재하는 성범죄자 거주지는 100개 이상 발견
- 학교 안전 범위 내에 안전 비상벨이 설치된 지역은 1,000개 이상 발견
- 학교 안전 범위 내에 안전 비상벨이 설치됨과 동시에 성범죄자 거주하는 지역은 100개 이상 발견
- 학교 안전 범위 내에 성범죄자가 거주하지만 안전 비상벨이 설치되지 않은 곳은 50개 정도로 발견
---
### 기대효과  

#### - 학교 및 성범죄자 거주지역의 성범죄 예방
  * 본 연구에서는 학교 안전 범위와 성범죄자의 행동반경이 중첩됨과 동시에 안전 비상벨이 설치되지 않은 지역(학교 위험 지역)과 성범죄자 거주지 300m 이내에 안전 비상벨이 설치되지 않은 지역(성범죄자 거주지 위험 지역)을 도출하고 시각화 함.
  * 성폭력 범죄의 43.4%는 성범죄자가 거주하는 지역에서 발생한다는 사실을 미루어 볼 때, 본 연구에서 도출된 학교 위험 지역과 성범죄자 거주지 위험 지역에 안전 비상벨을 우선적으로 설치한다면, 향후 성범죄 예방에 기여할 것으로 판단됨.
#### - 효율적인 예산 집행을 통한 안전 비상벨 설치
  * 또한, 본 연구에서는 학교 안전 범위와 성범죄자의 행동반경이 중첩됨과 동시에 안전 비상벨이 설치되지 않은 지역(학교 위험 지역)과 성범죄자 거주 지역 300m 이내에 안전 비상벨이 설치된 지역(성범죄자 거주지 중립 지역)을 시각화함으로써 두 지역이 서로 중첩되고 있음을 도출함.
  * 이와 같은 상황은 성범죄자 거주지와 학교가 근접해 있는 경우 안전 비상벨을 설치했으나, 학교 안전 범위와 성범죄자의 행동반경이 중첩되는 곳에 안전 비상벨을 설치되지 않음으로써 안전 비상벨 비효율적으로 설치되었다는 것을 시사함.
  * 한편, 안전 비상벨 300m 이내에 성범죄자 거주지가 없는 지역(안전 지역)은 7,707개로 나타났고, 성범죄자 거주 지역 300m 이내에 안전 비상벨이 설치되지 않은 지역(성범죄자 거주지 위험 지역) 또한 517개로 나타남.
  * 이는 성범죄자 거주지 근처에 안전 비상벨을 설치했으나, 성범죄자 거주지 300m 이내에 안전 비상벨을 설치되지 않음을 시사함.
  * 따라서, 본 연구에서 도출한 개선안을 안전 비상벨 설치시 참고한다면, 적은 예산으로 안전 비상벨을 효과적으로 설치할 수 있을 것으로 기대됨.
---
### Reference
- 경찰청(2018), 경찰범죄통계
- “아동·청소년‘성폭력 쵤영범죄’두배 넘게 늘었다.”,매일경제,2019년 4월 24일.
