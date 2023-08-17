# 파일 불러오기 
effect <- read.csv("C:\\Users\\phl02\\Desktop\\P\\bio_sas\\data\\con12.csv")
head(effect)

# 분석 진행
library(meta)
meta <- metacont(n1,m1,s1,n2,m2,s2,data=effect,sm='SMD',method.smd ='Hedges',study)
meta

#효과크기를 그래프로 확인 
forest(meta,col.diamond = 'deepskyblue1',col.square = 'deeppink1')

# 환산
meta_result <- c(meta$TE.fixed,meta$upper.fixed,meta$lower.fixed,
                 meta$TE.random,meta$upper.random,meta$lower.random)
Hedges <- round((1-2*pnorm(meta_result,0,1)),2)
Hedges

# 결과 정리 
library(kableExtra)
result <- matrix(0,2,8)
colnames(result) <- c('k','SMD','effect size','95% CI_low','95% CI_up','p','Q(df)',expression('I^2'))
row.names(result) <- c('Fixed','random')
result[,1] <- meta$n.e.pooled
result[1,2] <- round(meta$TE.fixed,2)
result[2,2] <- round(meta$TE.random,2)
result[1,3:5] <- Hedges[1:3]
result[2,3:5] <- Hedges[4:6]
result[1,6] <- meta$pval.fixed
result[2,6] <- meta$pval.random
for (i in 1:2){
  if (result[i,6] <0.001){result[i,6] <- '<.001'} 
}
result[,7] <- paste(round(meta$Q,2),'(', meta$df.Q,')')
result[,8] <- round(meta$I2*100,2)
kable(result)
