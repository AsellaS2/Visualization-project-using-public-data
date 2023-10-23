library(geosphere)
####################READ DATA###############################
setwd('D:/Dropbox/작성폴더/공공데이터포털 공모전/공공데이터 R 코드')
data = read.csv('IDBell.csv', stringsAsFactors = F)
data = na.omit(data)
options(scipen=5)
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)

####################DEVIDE DATA###############################
bell_data = subset(data,
                   select = c(1,2,3),
                   subset = (V=='Bell_ID'))
criminal_data = subset(data,
                       select = c(1,2,3),
                       subset = (V=='ID'))
school_data = subset(data,
                     select = c(1,2,3),
                     subset = (V=='School'))

####################MAKE SCHOOL DATA###############################
s_c_distance_mat = distm(school_data[,c('lon','lat')], criminal_data[,c('lon','lat')], fun = distGeo)
s_c_distance = apply(s_c_distance_mat, 1, min)
s_b_distance_mat = distm(school_data[,c('lon','lat')], bell_data[,c('lon','lat')], fun = distGeo)
s_b_distance = apply(s_b_distance_mat, 1, min)
c_b_distance_mat = distm(criminal_data[,c('lon','lat')], bell_data[,c('lon','lat')], fun = distGeo)
c_b_distance = apply(c_b_distance_mat, 1, min)
b_c_distance = apply(c_b_distance_mat, 2, min)
rm(s_b_distance_mat, s_c_distance_mat, c_b_distance_mat)

criminal_new_data = data.frame(criminal_data$V, cb_dis)

school_new_data = data.frame(school_data$V, s_c_distance, s_b_distance)
colnames(school_new_data) = c('category', 'sc', 'sb')

total = vector()
total = c(1:length(school_new_data$category))
s_c_idx = vector()
s_b_idx = vector()
s_c_idx = which(600>school_new_data$sc) # sc = 1
s_c_neg_idx = setdiff(total, s_c_idx) # sc = 0
s_b_idx = which(300>school_new_data$sb) #sb = 1
s_b_neg_idx = setdiff(total, s_b_idx) #sb = 0

school_new_data$check_s_c = 0
school_new_data$check_s_c[s_c_idx] = 1
school_new_data$check_s_b = 0
school_new_data$check_s_b[s_b_idx] = 1

####case 1: s_c = 1, s_b = 1
case_1_idx = vector()
case_1_idx =  intersect(s_c_idx, s_b_idx)

####case 2: s_c = 1, s_b = 0
case_2_idx = vector()
case_2_idx =  intersect(s_c_idx, s_b_neg_idx)

####case 3: s_c = 0, s_b = 1
case_3_idx = vector()
case_3_idx =  intersect(s_c_neg_idx, s_b_idx)

####case 4: s_c = 0, s_b = 0
case_4_idx = vector()
case_4_idx =  intersect(s_c_neg_idx, s_b_neg_idx)

name = vector()
name = c(1:length(school_new_data$category))
name[case_1_idx] = '학교 중립 지역(가)'
name[case_2_idx] = '학교 위험 지역'
name[case_3_idx] = '학교 안전 지역'
name[case_4_idx] = '학교 중립 지역(나)'
school_data$V = name


####################MAKE CRIMINAL DATA###############################
criminal_new_data = data.frame(criminal_data$V, c_b_distance)
colnames(criminal_new_data) = c('category', 'cb')

total = vector()
total = c(1:length(criminal_new_data$category))
c_b_idx = vector()
c_b_idx = which(300>criminal_new_data$cb) # sc = 1
c_b_neg_idx = setdiff(total, c_b_idx) # sc = 0


criminal_new_data$check_c_b = 0
criminal_new_data$check_c_b[c_b_idx] = 1


name = vector()
name = c(1:length(criminal_new_data$category))

name[c_b_idx] = '범죄자 거주지 위험 지역'
name[-c_b_idx] = '범죄자 거주지 중립 지역'

criminal_data$V = name

####################MAKE BELL DATA###############################
bell_new_data = data.frame(bell_data$V, b_c_distance)
colnames(bell_new_data) = c('category', 'cb')

total = vector()
total = c(1:length(bell_new_data$category))
c_b_idx = vector()
c_b_idx = which(300>bell_new_data$cb) # sc = 1
c_b_neg_idx = setdiff(total, c_b_idx) # sc = 0


bell_new_data$check_c_b = 0
bell_new_data$check_c_b[c_b_idx] = 1


name = vector()
name = c(1:length(bell_new_data$category))

name[c_b_idx] = '중립 지역'
name[-c_b_idx] = '안전 지역'

bell_data$V = name
##############################################


####################MAKE TOTAL DATA###############################
total_data = rbind(school_data, bell_data, criminal_data)
write.csv(total_data, 'total_data_20190818_2.csv', row.names = F)
####################FINISH###############################

