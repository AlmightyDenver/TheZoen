# Logistic

rm(list = ls())
par(mfrow = c(2,2))

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


df$ill = ill[,2]
df$other = other[,2]
df$rockslide= rockslide[,2]
df$fall= fall[,2]
df$climb= climb[,2]
df$dist= dist[,2]
df$hypothermy  = hypothermy[,2]
class(df$other)
df = df[,-1 ]

df= na.omit(df)
sum(is.na(df))
rm(list = 'hypothermy','dist','climb', 'fall','rockslide', 'other', 'ill')


# length(which(df$other == '1'))
table(df$dist)


# df$dew_c = as.factor(df$dew_c)
# df$wind_c =as.factor(df$wind_c)
# df$temp_c = as.factor(df$temp_c)
# df$humi_c = as.factor(df$temp_c)
# df$rain_c = as.factor(df$rain_c)
# df$snow_c = as.factor(df$snow_c)
library(MASS)
colnames(df)
logistic_ill = glm(formula = fall ~ temp + wind + rain + dew + snow+ humi ,family = 'binomial' ,data = df)
logistic_ill= stepAIC(logistic_ill)

summary(logistic_ill)

#distribution of prob
pred_ill = predict(logistic_ill)
prob_ill = 1/(1+ exp(-pred_ill))
summary(prob_ill)

library(mgcv)
#generaliaze add model 
ill_logistic_gam = gam(formula = fall ~ s(temp) + s(wind) + s(humi) ,family = 'binomial' ,data = df)
summary(ill_logistic_gam)


ill_terms = predict(ill_logistic_gam, type ='terms'  )
summary(ill_terms)

ill_partial_resid = resid(logistic_ill) + ill_terms
summary(ill_partial_resid)

colnames(df)
colnames(ill_partial_resid)
colnames(ill_terms)
df_g = data.frame(temp_g = df[,'temp'], 
                  terms_g = ill_terms[,"s(temp)"],
                  ill_partial_resid_g = ill_partial_resid[,"s(temp)"] )

# 31665 - 31648 when start have to remove n.a
# sum(is.na(df))
# nrow(df)
# nrow(ill_terms)

library(ggplot2)
# Error = dev.off() or reinstall
  
ggplot(df_g, aes(x = df_g$temp, y =ill_partial_resid_g, solid =FALSE))+
geom_point( shape =46, alpha = .4) +
 geom_line(aes(x =df_g$temp, y =terms_g),color = 'red',alpha= 0.5, size = 1.5)+
 labs(y = 'Partial Residual')


 library(IDPmisc)
 iplot(y =  ill , x =  wind + rain+ temp + dew  , data= df)
 


