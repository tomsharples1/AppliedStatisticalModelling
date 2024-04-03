log_time = c(0.405, 0.916, 1.386, 1.791, 2.484)
count = c(31, 26, 19, 15, 20)

model <- glm(count ~ log_time, family=poisson(link="log"))
summary(model)

qchisq(0.99, 1)

qt(0.975, 56)

#########
# Task 3
########

# Defaulting means failing to pay credit card debt 

library(ggplot2) # For plotting

ggplot(data, aes(x = income, fill = default)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("No"= "green", "Yes"="red")) +
  labs(title = "Distribution of Income by Default Status",
       x = "Income",
       y = "Density",
       fill = "Default Status") +
  theme_minimal()

ggplot(data, aes(x = balance, fill = default)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("No"= "green", "Yes"="red")) +
  labs(title = "Distribution of Balance by Default Status",
       x = "Balance",
       y = "Density",
       fill = "Default Status") +
  theme_minimal()

ggplot(data, aes(x = balance, fill=default)) + geom_boxplot() + 
  scale_fill_manual(values = c("No"= "green", "Yes"="red")) +
  labs(title = "Boxplot of balance by default status") + 
  theme_minimal()

ggplot(data, aes(x=income, fill=default)) + geom_boxplot() + 
  scale_fill_manual(values = c("No"= "green", "Yes"="red")) +
  labs(title = "Boxplot of Income by default status") + 
  theme_minimal()


# Fitting model 

data <- DefaultData
data$default <- factor(data$default, levels=c("No", "Yes"))
data$student <- factor(data$student, levels = c("No", "Yes"))

glm_logit <- glm(default ~ student + balance + income, data=data, binomial(link = "logit"))
summary(glm_logit)$coeff[3]

glm_probit <- glm(default ~ student + balance + income, data=data, binomial(link = "probit"))
summary(glm_probit)$coeff[3]

glm_loglog <- glm(default ~ student + balance + income, data=data, binomial(link = "cloglog"))
summary(glm_loglog)$coeff[3]


help(family)
# Confidence interval 

confint.default(data.glm, level = 0.95)

data$default.pred <- predict(data.glm, type = "response") # 1 is yes 

ggplot(data, aes(x=balance, y=default.pred)) +
  geom_smooth(method="glm", method.args = list(family=binomial("logit")), se=F, col="red") + 
  geom_smooth(method="glm", method.args = list(family=binomial("probit")), se=F) + 
  geom_smooth(method="glm", method.args = list(family=binomial("cloglog")), se=F, col="green") + 
  geom_label(aes(c("logit", "probit", "loglog"))) +
  labs(title= "Relationship between Balance and probability of defaulting", 
       x = "Balance", 
       y = "Predicted default probability") + theme_minimal()





