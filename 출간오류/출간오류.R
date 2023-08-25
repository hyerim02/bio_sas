setwd('C:\\Users\\phl02\\Desktop\\P\\bio_sas')


# 파일불러오기 
publica<- read.csv("data\\pb1.csv")
publica

# 분석진행 
library(meta)
meta_pb <- metacont(n1,m1,s1,n2,m2,s2,data=publica,sm='SMD',
                       method.smd ='Hedges',study)
meta_pb

#교정된 표준화된 평균 차이 
smd <-1-2*pnorm(c(meta_pb$TE.random,meta_pb$upper.random,meta_pb$lower.random),0,1)
smd

#그래프 
forest(meta_pb,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')

## 깔때기 그래프 
funnel(meta_pb,studlab = T)


## 대칭성을 검정 
metabias(meta_pb,method.bias = 'linreg')

## 비뚤림 교정 
library(metafor)
fsn(meta_pb$TE,meta_pb$seTE)

meta_pb2 <- trimfill(meta_pb)
summary(meta_pb2)

smd2 <- abs(1-2*pnorm(c(meta_pb2$TE.random,meta_pb2$upper.random,meta_pb2$lower.random),0,1))
smd2

funnel(meta_pb2,studlab = T)
metabias(meta_pb2,method.bias = 'linreg')
fsn(meta_pb2$TE,meta_pb2$seTE)

# 결과표 정리 
library(kableExtra)
result <- matrix(0,2,8)
colnames(result) <- c('k','ES','95% CI_low',
                      '95% CI_up','Q(df)','I^2','P','Fail-safe N')
row.names(result) <- c('랜덤효과모형','수정된 trim-and fill모형')
result[1,1] <- meta_pb$k
result[1,2] <- round(smd[1],3)
result[1,3] <- round(smd[2],3)
result[1,4] <- round(smd[3],3)
result[1,5] <- paste(round(meta_pb$Q,2),'(', meta_pb$df.Q,')')
result[1,6] <- round(meta_pb$I2*100,2)
result[1,7] <- round(meta_pb$pval.random,3)
result[1,8] <- round(fsn(meta_pb$TE,meta_pb$seTE)$fsnum,3)
result[2,1] <- meta_pb2$k
result[2,2] <- round(smd2[1],3)
result[2,3] <- round(smd2[2],3)
result[2,4] <- round(smd2[3],3)
result[2,5] <- paste(round(meta_pb2$Q,2),'(', meta_pb2$df.Q,')')
result[2,6] <- round(meta_pb2$I2*100,2)
result[2,7] <- round(meta_pb2$pval.random,3)
result[2,8] <- round(fsn(meta_pb2$TE,meta_pb2$seTE)$fsnum,3)
kable(result)

