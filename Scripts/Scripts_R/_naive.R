### over 3 option of Factor 1.(y = 0 or y > 0) , 2.(y = 1 or 2) = 2 binary
##Naive Bayes(나이브베이즈)  => predict x = factor
#conditional probability = given (y = i) Y|X = i prob

rm(list = ls())
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



humi_c <- transform(df$humi_c, humi_c = ifelse(humi_c >= 3 ,'1','0'))
wind_c <- transform(df$wind_c, wind_c = ifelse(wind_c >= 2 ,'1','0'))
temp_c <- transform(df$temp_c, temp_c = ifelse(temp_c >= 3 ,'1','0'))


df$ill = ill[,2]
df$other = other[,2]
df$rockslide= rockslide[,2]
df$fall= fall[,2]
df$climb= climb[,2]
df$dist= dist[,2]
df$hypothermy  = hypothermy[,2]
df$humi_c = humi_c[,2]
df$wind_c = wind_c[,2]
df$temp_c = temp_c[,2]

table(df$temp_c)

df = df[,-1 ]

colnames(df)

rm(list = 'hypothermy','dist','climb', 'fall','rockslide', 'other', 'ill')




###naive and Discriminant analysis


## over 3 option of Factor 1.(y = 0 or y > 0) , 2.(y = 1 or 2) = 2 binary

##Naive Bayes(나이브베이즈)  => predict x = factor
#conditional probability = given (y = i) Y|X = i prob
# install.packages('klaR')
library(klaR)
class(df$fall)
table(df$fall)
# Assume  that the predictor X is independent
fall_naive = NaiveBayes(fall ~  temp_c+ humi_c+wind_c , data = df )
fall_naive$table 

loan_lda = lda(fall ~  temp_c+ humi_c+wind_c , data = df ) 
pred_loan = predict(loan_lda)
summary(pred_loan$posterior)



##linear discriminant analysis(LDA) is measure the importance of predict variable(pv) 
# covariance = the degree to which a variable  change with other variable (+ = possitive , - = negative)
#whatever factor or seq  no problem , over two pv no problem
#using covariance can calculus LDA 
library(MASS)
library(ggplot2)
loan3000 =read.csv('C:\\R_workspace\\Data\\AAA-Practical_Statistics\\loan3000.csv')
colnames(loan3000)
loan_lda = lda(outcome ~ borrower_score+payment_inc_ratio, data =loan3000) 
pred_loan = predict(loan_lda)
head(pred_loan$posterior)

lda_df = cbind(loan3000, prob_default = pred_loan$posterior[,'default'], prob_payoff = pred_loan$posterior[,'paid off']) 

ggplot(data =lda_df, aes(x =borrower_score, y = payment_inc_ratio, col = prob_default))+
  geom_point(alpha = .6)+
  scale_color_gradient2(low = 'white', high = 'blue')+
  geom_line(data= lda_df, col = 'green', size= 2 , alpha = .8)
#그래프가 이상하다... ㄷㄷㄷ






















# library('hexbin')
# #1
# bin = hexbin(gyeongnam$ill ~ gyeongnam$total.Humi + gyeongnam$total.wind + gyeongnam$total.rain+ gyeongnam$total.temp + gyeongnam$total.dew + gyeongnam$total.snow, data= gyeongnam,  xbins = 20)
# plot(bin)
# #2
# smoothScatter(gyeongnam$ill ~ gyeongnam$total.Humi + gyeongnam$total.wind + gyeongnam$total.rain+ gyeongnam$total.temp + gyeongnam$total.dew + gyeongnam$total.snow, data= gyeongnam)
# #3
# install.packages('IDPmisc')
# library(IDPmisc)
# iplot(gyeongnam$ill ~ gyeongnam$total.Humi + gyeongnam$total.wind + gyeongnam$total.rain+ gyeongnam$total.temp + gyeongnam$total.dew + gyeongnam$total.snow, data= gyeongnam)





  