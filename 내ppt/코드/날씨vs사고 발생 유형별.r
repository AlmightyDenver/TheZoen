
# Logistic = ln(p / 1 -p) = b0 + b1x1 + b2x2+ ...bpxp (b에  여러 요인들을 적용 x=1, x=2 계산)
             #사고확률 / 비사고확률 = 오즈비
rm(list = ls())
par(mfrow = c(1,1))
install.packages("ROCR")
df = read.csv('C:\\deep\\spam\\h\\comb_mini_c.csv')

library(dplyr)
attach(df)
ill <- transform(df$Cause, ill = ifelse(Cause == "개인질환",'1','0')) 
other <- transform(df$Cause, cause = ifelse(Cause == "산악기타",'1','0'))
rockslide <- transform(df$Cause, cause = ifelse(Cause == "낙석낙빙",'1','0'))
fall <- transform(df$Cause, cause = ifelse(Cause == "실족추락",'1','0'))
climb <- transform(df$Cause, cause = ifelse(Cause == "암벽등반",'1','0'))
dist <- transform(df$Cause, cause = ifelse(Cause == "일반조난",'1','0'))
hypothermia <- transform(df$Cause, cause = ifelse(Cause == "저체온증",'1','0'))
exhaustion <- transform(df$Cause, cause = ifelse(Cause == "탈진탈수",'1','0'))


df$ill = ill[,2]
df$other = other[,2]
df$rockslide= rockslide[,2]
df$fall= fall[,2]
df$climb= climb[,2]
df$dist= dist[,2]
df$hypothermia  = hypothermia[,2]
df$exhaustion = exhaustion[,2]
class(df$other)
df = df[,-1 ]

df= na.omit(df)
sum(is.na(df))
rm(list = 'hypothermia','dist','climb', 'fall','rockslide', 'other', 'ill','exhaustion')


# length(which(df$other == '1'))
table(df$exhaustion)


library(MASS)
colnames(df)
logistic = glm(formula = exhaustion  ~ temp + wind + rain + dew + snow+ humi ,family = 'binomial' ,data = df)
logistic= stepAIC(logistic) #step도 가능 AIC가 낮을수록 적합한 모델 데이터 적합성. 
summary(logistic)
print('odds ratios')
print('승산비가 1보다 크면 예측 변수가 증가함에 따라 사건 발생 확률이 증가한다는 것을 나타냅니다. 승산비가 1보다 작으면 예측 변수가 증가함에 따라 사건 발생 확률이 감소한다는 것을 나타냅니다.')
exp(coef(logistic))  #오즈비 산출


library(ROCR)
p = predict(logistic, newdata=df, type="response")
pr = prediction(p, df$exhaustion )
#ROC 커브의 true positive rate 와 false positive rate 생성
prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
plot(prf, col='blue', lty = 1, lwd = 3, main = 'ROC curve')
abline(0, 1, lty=3)
# #AUC 곡선 성능 평가
# auc = performance(pr, measure = 'auc')
# #AUC 통계량 산출
# auc = auc@y.values[[1]]
# auc





