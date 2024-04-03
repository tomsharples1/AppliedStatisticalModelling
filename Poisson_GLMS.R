###############
# Poisson GLMs
##############

age=rep(c(40,50,60,70,80),2)
smoke=c(rep(1,5),rep(2,5))
deaths=c(32,104,206,186,102,2,2,28,28,31)
years=c(52407, 43248,28612,12663,5317,18790,10673,5710,2585,1462)
smokedata=cbind(age,smoke,deaths,years)
smokedata <- as.data.frame(smokedata)

# Questions 1
# Is the death rate higher for smokers than non-smokers? 

smokef = factor(smoke)
log_age = log(age)
log_years = log(years)

glm <- glm(deaths ~ log_age, data=smokedata, poisson("log"), offset = log_years)

library(ggplot2)

ggplot(smokedata, aes(x=age, y=log(deaths))) + geom_point(aes(col=smokef), size=3) +
  theme_minimal()

# hard to say if it looks more linear with age of log(age)

#########
# TASK 2
#########

model1 <- glm(deaths ~ smokef + log(age), data=smokedata, offset=log_years, 
           family=poisson)
summary(model1)

# Q1 interpretation 
# the coefficient of smokef2, meaning doesn't smoke, is negative so keeping log(age) fixed, 
# the death rate decreases by 0.4885 if they are a non-smoker
# so yes the death rate is higher for smokers 

# Q3
# coefficient for log age is positive which implies that keeping smoking status fixed, 
# death that increases by 5.21 for every year an individual becomes older 

#########
# TASK 3 
#########

fv=model1$fitted.value
p.res=resid(model1,'pear')
d.res=resid(model1,'dev')

plot(model1)

ggplot(smokedata, aes(x=fv, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F, col="green")

ggplot(smokedata, aes(x=p.res, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)
ggplot(smokedata, aes(x=d.res, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F, col="red")

#########
# TASK 4
#########

log_age2 <- log_age^2 

# i) Note the number of regression coefficients in each model, and interpret them.
model2 <- glm(deaths ~ smokef*log_age, offset=log_years, family=poisson, data=smokedata)
summary(model2)
# 4 coefficients, non-smokers reduce the death rate, age increases the death rate

fv2=model2$fitted.value
p.res2=resid(model2,'pear')
d.res2=resid(model2,'dev')

ggplot(smokedata, aes(x=fv2, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)
ggplot(smokedata, aes(x=p.res2, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F, col="red")
ggplot(smokedata, aes(x=d.res2, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)

model3 <- glm(deaths ~ smokef + log_age + log_age2, offset=log_years, 
              family=poisson, data=smokedata)
summary(model3)

# 4 coefficients, non-smoker reduces death rate, log_age increases death rate significantly
# the non-linear term for log_age reduces death rate

fv3=model3$fitted.value
p.res3=resid(model3,'pear')
d.res3=resid(model3,'dev')

ggplot(smokedata, aes(x=fv3, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)
ggplot(smokedata, aes(x=p.res3, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F, col="red")
ggplot(smokedata, aes(x=d.res3, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)

model4 <- glm(deaths ~ smokef*log_age + log_age2, offset=log_years, 
              family=poisson, data=smokedata)
summary(model4)

# 5 coefficients, non-smoker significantly decreases death rate, log age increases death rate a lot
# non-linear term for log age reduces death rate and the interaction term increases the death rate

fv4=model4$fitted.value
p.res4=resid(model4,'pear')
d.res4=resid(model4,'dev')

ggplot(smokedata, aes(x=fv4, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)
ggplot(smokedata, aes(x=p.res4, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F, col="red")
ggplot(smokedata, aes(x=d.res4, y=deaths)) + geom_point() + geom_smooth(method="glm", se=F)

summary(model1)$deviance
summary(model2)$deviance
summary(model3)$deviance
summary(model4)$deviance

test <- anova(model2, model4, test="Chisq")
test 

# I would argue that model 2 is the best in predicting the death rate since it has the second lowest
# deviance and it one of the simpler models to interpret 
# since M2 is nested in M4 we can perform a deviance test and we conclude at the 5% and 1% level that 
# M2 is a better fit ! 

