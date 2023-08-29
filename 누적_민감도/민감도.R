setwd('C:\\Users\\phl02\\Desktop\\P\\bio_sas')


# 파일불러오기 
sensi<- read.csv("data\\sm.csv")
sensi

# 분석진행 
library(meta)
meta_sensi <- metagen(g,se,data=sensi,sm='SMD',study)
meta_sensi

#그래프 
forest(meta_sensi,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')


## 민감도 검사 
meta_sensi2<- metainf(meta_sensi)
meta_sensi2

#그래프 
forest(meta_sensi2,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')

# 두 번째 민감도 
baujat(meta_sensi)

# 세 번째 민감도 
library(metafor)
res <- rma(g,se^2,measure = 'SMD',method = 'DL',slab = paste(study),data=sensi)
res
inf<- influence(res)
inf
plot(inf)


