# Logistic = ln(p / 1 -p) = b0 + b1x1 + b2x2+ ...bpxp (b에  여러 요인들을 적용 x=1, x=2 계산)
             #사고확률 / 비사고확률 = 오즈비
rm(list = ls())
par(mfrow = c(1,1))

library(readxl)
df = read_excel('C:\\R_workspace\\git\\TheZoen\\Data\\CombinedData\\comb_mini_c.xlsx',)

library(dplyr)
attach(df)
ill <- transform(df$Cause, ill = ifelse(Cause == "개인질환",'1','0')) 
other <- transform(df$Cause, cause = ifelse(Cause == "산악기타",'1','0'))
rockslide <- transform(df$Cause, cause = ifelse(Cause == "낙석낙빙",'1','0'))
fall <- transform(df$Cause, cause = ifelse(Cause == "실족추락",'1','0'))
climb <- transform(df$Cause, cause = ifelse(Cause == "암벽등반",'1','0'))
dist <- transform(df$Cause, cause = ifelse(Cause == "일반조난",'1','0'))
hypothermy <- transform(df$Cause, cause = ifelse(Cause == "저체온증",'1','0'))
exhaustion <- transform(df$Cause, cause = ifelse(Cause == "탈진탈수",'1','0'))


df$ill = ill[,2]
df$other = other[,2]
df$rockslide= rockslide[,2]
df$fall= fall[,2]
df$climb= climb[,2]
df$dist= dist[,2]
df$hypothermy  = hypothermy[,2]
df$exhaustion = exhaustion[,2]
class(df$other)
df = df[,-1 ]

df= na.omit(df)
sum(is.na(df))
rm(list = 'hypothermy','dist','climb', 'fall','rockslide', 'other', 'ill','exhaustion')


# length(which(df$other == '1'))
table(df$exhaustion)


# df$dew_c = as.factor(df$dew_c)
# df$wind_c =as.factor(df$wind_c)
# df$temp_c = as.factor(df$temp_c)
# df$humi_c = as.factor(df$temp_c)
# df$rain_c = as.factor(df$rain_c)
# df$snow_c = as.factor(df$snow_c)
library(MASS)
colnames(df)
logistic = glm(formula = exhaustion ~ temp + wind + rain + dew + snow+ humi ,family = 'binomial' ,data = df)
logistic= stepAIC(logistic) #step도 가능 AIC가 낮을수록 적합한 모델 데이터 적합성. 

library(car)
round(vif(logistic),1) 

summary(logistic) #estimate is regression coefficient
par(mar = c(2,2,2,2))

ORplot(logistic, main = "Plot for Odds Ratios" , type =2 , show.OR =TRUE, show.CI = TRUE , pch = 15, lwd=4, col = c('darkblue', 'brown') )


#####과산포(overdispersion) 분석  만약 있다면( x >0.5 ) ,using quasibinomial
library(moonBook)
#step 된 독립변수만을 포함한다
logistic_od = glm(formula = fall ~ temp + wind  + snow + humi+rain ,family = 'quasibinomial' ,data = df)
summary(logistic_od)
pchisq(summary(logistic_od)$dspersion*logistic$df.residual,logistic$df.residual ,lower= F) 


ORplot(logistic, main = "Plot for Odds Ratios" , type =2 , show.OR =TRUE, show.CI = TRUE , pch = 15, lwd=4, col = c('darkblue', 'brown') )
#ORplot(logistic, main = "Plot for Odds Ratios" , type =3 , show.OR =TRUE, show.CI = TRUE , pch = 10, lwd=3, col = c('darkblue', 'red') )


library(car)
round(vif(logistic),1) 

summary(logistic)

####################################################################################
##for odds  (점선)1 을 포함하면 pvalue 의미 x , 점의개수가 많으면 의미있다
# 1<o  = risky , 1>o = protective
#confidence interval(신뢰구간 95%) = 5.4(3.3~7.7) 일때 소극적으로 최소 3.3배 위험 최대 7.8배 위험
# rain = 0.0569 x= 1? accident p =increase 0.0569%
# wind = -0.07 , x=1? acc p =  decrease 0.07%


#나온 독립변수의 회귀계수 = bp 임으로  exp를 취해야 odds ratio 나옴
odd =1/ 1+exp(-logistic$coefficients)
odd
summary(odd)


ORplot(logistic, main = "Plot for Odds Ratios" , type =2 , show.OR =TRUE, show.CI = TRUE , pch = 15, lwd=4, col = c('darkblue', 'brown') )
###################################################################3
library(mgcv)
#generaliaze add model residual 구하기 잔차구하기
logistic_gam = gam(formula = other ~ s(temp) + s(wind) + s(humi)+s(rain) + s(dew) + s(snow) ,family = 'binomial' ,data = df)
summary(logistic_gam)


terms = predict(logistic_gam, type ='terms'  )
summary(terms)

partial_resid = resid(logistic) + terms
summary(partial_resid)

colnames(df)
colnames(partial_resid)
colnames(terms)
df_g = data.frame(temp_g = df[,'temp'], 
                  terms_g = terms[,"s(temp)"],
                  partial_resid_g = partial_resid[,"s(temp)"] )

# 31665 - 31648 when start have to remove n.a
# sum(is.na(df))
# nrow(df)
# nrow(ill_terms)

library(ggplot2)
# Error = dev.off() or reinstall
  
ggplot(df_g, aes(x = df_g$temp, y =partial_resid_g, solid =FALSE))+
geom_point( shape =45, alpha = .4) +
 geom_line(aes(x =df_g$temp, y =terms_g),color = 'red',alpha= 0.5, size = 1.5)+
 labs(y = 'Partial Residual')


 library(IDPmisc)
 iplot(y =  ill , x =  wind + rain+ temp + dew  , data= df)
 


