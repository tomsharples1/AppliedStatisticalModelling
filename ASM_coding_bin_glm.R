###############################
# Applied statistical modelling
# Binomial GLMs
###############################

# construct dataset 

n=c(2,6,3,3,3,1,5,3,6,2)
y=c(1,2,1,1,0,1,3,2,3,1)
ag=c(rep(1,5),rep(2,5))
x=rep(c(500,5000,10000,40000,100000),2)

blooddata = cbind(n, y, ag, x)
head(blooddata)

# To fit bin regression we need to put y and n-y 
# into a response matrix
# first column is sucesses yi
# second column is failures ni-yi

y2 <- cbind(y, n-y)
ag <- factor(ag)

# the scale of x is too large, lets take log(x)

# now fit the model with the same slope for both AG groups

out1 <- glm(y2~ ag + log(x), family = binomial(link="logit"))
summary(out1)

b0 <- summary(out1)$coeff[1]

# probability for surviving at least 52 weeks 

prob <- exp(b0)/(1+exp(b0))
prob # 0.9209 

# variance matrix to estimate b 
summary(out1)$coeff[,"Std. Error"]
vcov(out1) # these are the squares of teh std.errors dummy 

for (i in 1:3){
  sqrt <- sqrt(vcov(out1)[i,i])
  print(sqrt)
}
# these match up 

out2 <- glm(y2~log(x), family = binomial(link="logit"))
out3 <- glm(y2 ~ ag*log(x), family = binomial(link="logit"))

# test for the significance of ag at the 5% level 

anova(out2, out1)
deviance <- anova(out2, out1)[,"Resid. Dev"]

delta_dev <- deviance[1] - deviance[2]

if (delta_dev > qchisq(0.95, 1)){
  print("reject h0 at 5% level")
} else{
  print("Do not reject h0 at 5% level")
}


model1 <- glm(y2~ ag + log(x), family = binomial(link="probit"))
model2 <- glm(y2~ log(x), family = binomial(link="probit"))
summary(model1)
summary(model2)

anova(model2, model1)
dev <- anova(model2, model1)[, "Resid. Dev"]


delta_dev1 <- dev[1] - dev[2]

if (delta_dev1 > qchisq(0.95, 1)){
  print("reject h0")
} else{
  print('dont reject h0')
}
install.packages("VGAM")

glm1 <- glm(y2~ ag + log(x), family = binomial(link='cloglog'))
glm2 <- glm(y2~ log(x), family = binomial(link="cloglog"))

summary(glm1)
summary(glm2)

anova(glm2, glm1)

dev2 <- anova(glm2, glm1)[, "Resid. Dev"]

deltadev <- dev2[2] - dev2[1]

if (deltadev > qchisq(0.95,1)){
  print("reject h0")
} else{
  print("do not reject h0")
}








