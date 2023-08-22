setwd('C:\\Users\\phl02\\Desktop\\P\\bio_sas')

## ANOVA

# 파일불러오기 
library(readxl)
effect <- read_excel("data\\metaanova1.xlsx")
effect

# 분석진행 
library(meta)
library(meta)
meta_anova <- metacont(n1,m1,s1,n2,m2,s2,data=effect,sm='SMD',
                       method.smd ='Hedges',study)
meta_anova

meta_anova1 <- metacont(n1,m1,s1,n2,m2,s2,data=effect,sm='SMD',
                        method.smd ='Hedges',study, subgroup = group)
meta_anova1

meta_anova2 <- metacont(n1,m1,s1,n2,m2,s2,data=effect,sm='SMD',
                        method.smd ='Hedges',study, subgroup = group,
                        tau.common = T)
meta_anova2

#그래프 
forest(meta_anova2,test.subgroup.random = T,resid.hetstat = F,
       layout = 'subgroup',calcwidth.tests = T,
       col.diamond = 'deepskyblue1',col.square = 'deeppink1')

# 회귀분석으로 메타분석의 설명력과 QM을 확인 
metareg(meta_anova2,group)

## 사후 검정
meta2 <- subset(effect,group!='2주')
meta4 <- subset(effect,group!='4주')
meta6 <- subset(effect,group!='6주')

meta2_anova <- metacont(n1,m1,s1,n2,m2,s2,data=meta2,sm='SMD',
                        method.smd ='Hedges',study, subgroup = group)
meta4_anova <- metacont(n1,m1,s1,n2,m2,s2,data=meta4,sm='SMD',
                        method.smd ='Hedges',study, subgroup = group)
meta6_anova <- metacont(n1,m1,s1,n2,m2,s2,data=meta6,sm='SMD',
                        method.smd ='Hedges',study, subgroup = group)

forest(meta2_anova,col.diamond = 'deepskyblue1',col.square = 'deeppink1')
forest(meta4_anova,col.diamond = 'deepskyblue1',col.square = 'deeppink1')
forest(meta6_anova,col.diamond = 'deepskyblue1',col.square = 'deeppink1')


# 결과표 정리 
library(kableExtra)
result <- matrix(0,3,7)
colnames(result) <- c('k','ES','95% CI_low',
                      '95% CI_up','Q(df)','R^2','P')
row.names(result) <- c('2주','4주','6주')
result[,1] <- meta_anova2$k.w
result[,2] <- round(meta_anova2$TE.random.w,3)
result[,3] <- round(meta_anova2$lower.random.w,3)
result[,4] <- round(meta_anova2$upper.random.w,3)
result[,5] <- paste(round(meta_anova2$Q,2),'(', meta_anova2$df.Q,')')
result[,6] <- round(metareg$R2/100,3)
result[,7] <- round(meta_anova2$pval.random,3)
kable(result)





## 회귀분석 
effect_reg <- read.csv('data\\metareg.csv')
effect_reg

#분석
library(meta)
meta_reg <- metabin(a,n1,b,n2,data=effect_reg,sm='RR',
                    method='I',studlab = paste(study))
meta_reg

# 조절효과 
forest(meta_reg,col.diamond = 'deepskyblue1',col.square = 'deeppink1')
 
# 설명력과 QM확인 
metareg1<- metareg(meta_reg,age)
metareg1

#회귀분석 결과
pred_reg<- predict(metareg1)
pred_reg

fit_reg <- exp(fitted(metareg1))
fit_reg

bubble(metareg1,col.line = 'deepskyblue1',bg = 'deeppink1',studlab = T)

# 결과 정리 
library(kableExtra)
result <- matrix(0,2,8)
colnames(result) <- c('RR(95% CI)','hete_I^2','hete_p',
                      'subg_QM','sub_p','meta_B','meta_p','meta_R^2')
row.names(result) <- c('intercept','평균 나이')
result[,1] <- paste('0.81','(0.68,0.97)')
result[,2] <- round(meta_reg$I2,3)
result[,3] <- round(meta_reg$pval.Q,4)
for (i in 1:2){
  if (result[i,3]<0.001){result[i,3]='<.001'}
}
result[,4] <- round(metareg1$QM,3)
result[,5] <- round(metareg1$QMp,3)
result[,6] <- round(metareg1$b,3)
result[,7] <- round(metareg1$pval,3)
result[,8] <- round(metareg1$R2,3)
kable(result)

