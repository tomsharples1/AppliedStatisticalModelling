##############
# ASM Design of experiments
##############

shipment=factor(rep(1:5,each=3))
carrier=factor(rep(letters[1:3],times=5))
times=c(15.2,16.9,17.1,
        14.3,16.4,16.1,
        14.7,15.9,15.7,
        15.1,16.7,17.0,
        14.0,15.6,15.5)
Delivery=data.frame(times,shipment,carrier)
Delivery

# usage of rep comment when defining factor variables 
# Matches the structure of the dataset created 

# Exploratry analysis to understand the data a bit 

par(mfrow=c(1,1))
boxplot(times ~ shipment, data=Delivery)

boxplot(times ~ carrier, data=Delivery)
means <- tapply(Delivery$times, Delivery$carrier, mean)
means
# Plots seem to indicated difference across ship and carrier types 
# so RCBD may be appropriate 
# remember we need a constraint such as sum(ai) = 0 etc 

options(contrasts = c("contr.sum", "contr.sum")) # this line ensures the constraints are met

# Under CRD there is no shipment effect and we just treat the delivery 
# times across shipments as replicated for a fixed treament (carrier)

mod.crd=aov(times ~ carrier, data=Delivery)
summary(mod.crd)

summary_mod <- summary(mod.crd)

# since crd is a special case of lm,
# we can use summary.lm to obtain coeff info summary.lm(mode.crd)
linear_sum <- summary.lm(mod.crd)
linear_sum
# First thing to check is for carrier effect on delibery times
# Perform hyp test for H0: a1 = a2 = a3 = 0

F_test <- qf(0.95, 2, 12)
F_test
# alpha = 0.05 
if (linear_sum$fstatistic[1] > F_test){
  print("reject H0")
}

# note that linear_sum$fstatistic gives a list so need to specify 

linear_sum$coefficients
# Intercept is an estimate of y_bar(++) of the overall mean \mu 

overall_mean <- mean(Delivery$times)
overall_mean # 15.747

a_3_hat <- linear_sum$coefficients[2] + linear_sum$coefficients[3]
a_3_hat # correct

boxplot(times ~ carrier, data=Delivery)
means <- tapply(Delivery$times, Delivery$carrier, mean)
means # compare with this visually 

# Significant individual treatment effects H0: ai = 0 vs H1: ai ≠ 0

std_error <- linear_sum$coefficients[, "Std. Error"] # this works 
t_values <- -a_3_hat / sqrt(std_error[3])
t_values

t_table_values <- qt(0.95, 12) # t_12 dist
t_table_values

# QQ-plot of resdiauls to check validity of assumptions on errors

plot(mod.crd, which=2)
# reasonable fit in the middle but some deviation at tails 

##############
# Fitting RCBD
##############

mod.rcbd <- aov(times ~ carrier + shipment, data=Delivery)
summary <- summary(mod.rcbd)
summary

block_test <- qf(0.95, 4, 8)
block_test # not significant reject H0

if (18.82 > block_test){
  print("reject H0")
}

treat_test <- qf(0.95, 2, 8)
treat_test 

if (83.82 > treat_test){
  print("reject H0")
}

lm.summary.rcbd <- summary.lm(mod.rcbd)
lm.summary.rcbd


