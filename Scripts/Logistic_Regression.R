#Logistic regression (x = independent(cause), y = dependency(result) , yes or no 범주형 결과를 분석시 사용) , (multi linear regression doesn't factor)
#factor is convert not numeric, included regression parameter 
#일반 선형모델 (Ordinary linear model) = a + bx = y = b
#(logistic)일반화 선형모델(General Lnear Model) = ln(y) = e^(a+bx) = y = e^b
rm(list =ls())
library(dplyr)

#Caution  !!!!! n.a not allow  xxxxxxxxxxxxxxx  result = subset(result, factor >0)
loan_data = read.csv('C:\\R_workspace\\Data\\AAA-Practical_Statistics\\loan_data.csv')


#OR= {failure(0) or success(1) ratio} find index(지수) of the denominator 
# y=1  =>  p /1-p = e ^ (bx + bx+ bx ..), inverse odds =  p/ 1+p , 
#log odds =  from prob(odds) = 0 ~ 1  to mapping -infinity ~ +""
# using cut-off we can find class label
#predict value in logistic regression is (y = 1) 1 / 1 + log(odds) 
#odd value = odd(y = 1 | x = 1) / (y = 1 | x = 0) -> if odds value else and elseif molecoules(분자) are twice as high as the denominator
logistic_model = glm(formula = outcome ~ payment_inc_ratio + purpose_ +home_ + emp_len_ +borrower_score, data =loan_data,family = 'binomial' ,na.action = na.omit)
summary(logistic_model)
#outcome is rs(response variable) default is 1 , pay off = 0 
pred_logistic = predict(logistic_model)
prob_logi = 1/(1+exp(-pred_logistic))
summary(prob_logi)

#In linear return, the least square is used for model fitting (RMSE OLS) but
#logistic using maximum likelihood(그럴듯한?) estimation(최대우도측정) , MLE if  find model logodds ratio

#AIC(AIC=−2logL+2K) value가 가장 작을수록 적절한 모형 P(x1 ,x2 ,x3) , Maximize the probability of observation
# reduce_logistic <- step(logistic_model)
# summary(reduce_model)
library(mgcv)
#seq variable  = s()
logistic_gam = gam( outcome~ s(payment_inc_ratio) + purpose_+ home_ +emp_len_ + s(borrower_score), data =loan_data, family = 'binomial')
?s() # spline
# high polynomial return causes shaking, When modeling nonlinear relationships
# splines soften the curvature, you have to set the position of polynomial and knot(매듭)





summary(logistic_gam)
# z-value =  indicate how far away from mean , positive is above average , negative are below average 
# standard diviation = how far from mean , standard error = how far away from population
# p-value =  The probability that null hypothesis is correct
#logistic is Sresidual good!
terms = predict(logistic_gam, type = 'terms')
summary(terms) 
partial_resid = resid(logistic_model) + terms
colnames(terms)

df = data.frame(payment_inc_ratio = loan_data[,'payment_inc_ratio'],
                terms = terms[, "s(payment_inc_ratio)"],
                partial_resid = partial_resid[,'s(payment_inc_ratio)'])
library(ggplot2)
ggplot(df, aes(x = payment_inc_ratio, y =partial_resid, solid =FALSE))+
          geom_point( shape =46, alpha = .4) +
  geom_line(aes(x =payment_inc_ratio, y =terms),color = 'red', alpha =.5, size = 1.5)+
  labs(y = 'Partial Residual')




# P-value =  Indicate the probability that the statistic(통계치) is above the actual observed value,
#assuming the null hypothesis is correct
#random sampling(n =100)'s data mean is how similar will it be to the average of the population
#Methodology(방법론) of how the mean extracted(추출한) at each sampling differs from the mean of the Population
# too lower of p-value = failure  null hypothesis = objection , control hypothesis = allocation
# p = 0.9999 is following the null hypothesis is binary distribution

# To believe coincidence is real = type one error , To belive real is coincidence  = type two error












# require(moonBook)


# extractOR(reduce_model) 
# ORplot(fit, main = " subject")
# ORplot(fit, type = 2, show.OR =FALSE, show.CI = TRUE, pch = 15, lwd = 2, col = c('darkblue', 'red'), main = 'subject')