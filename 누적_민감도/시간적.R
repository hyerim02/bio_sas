setwd('C:\\Users\\phl02\\Desktop\\P\\bio_sas')


# 파일불러오기 
time<- read.csv("data\\tcm.csv")
time

# 분석진행 
library(meta)
meta_time <- metabin(event.e,n1,event.c,n2,data=time,studlab = study,
                   sm='RR',method='Inverse')
meta_time


#그래프 
forest(meta_time,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')

## 누적 
meta_time2 <-metacum(meta_time,sortvar=year)
meta_time2


#그래프 
forest(meta_time2,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')


# 결과표 정리 
library(kableExtra)
library(stringr)
result <- matrix(0,7,6)
colnames(result) <- c('year','RR','95% CI_low',
                      '95% CI_up','I^2(%)','P')
result[,1] <- as.matrix(unlist(str_extract_all(meta_time$studlab,"[0-9]{4,}")))
result[,2] <- c(0.65,0.67,0.69,0.73,0.74,0.91,0.92)
result[,3] <- c(0.46,0.53,0.56,0.63,0.64,0.86,0.87)
result[,4] <- c(0.93,0.84,0.84,0.85,0.86,0.96,0.97)
result[,5] <- round(na.omit(meta_time2$I2)*100,2)
result[,6] <- round(na.omit(meta_time2$pval),3)[1:7]

for(i in 1:nrow(result)){
  if (result[i,6] < 0.001) result[i,6]='<.001'
}

kable(result)

