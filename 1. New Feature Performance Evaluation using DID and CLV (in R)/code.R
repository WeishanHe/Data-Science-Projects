# loading packages
rm(list=ls())
library(readxl)
library(Hmisc)
library(MASS)
library(caret)
library(regclass)
library(ISLR)
library(boot)
library(vcd)
library(pROC)
library (ROCR)
library(reshape2)
library(EnvStats)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pwr)

###############################################################
# Question 1: Has the online community increased user revenue?#
###############################################################
# Loading data sets
data1 <- read_excel("Data1.xlsx")

# stack the data
data1_stacked <- melt(data1, id.vars=c(1,4))
colnames(data1_stacked) <- c("CustomerID", "Joined", "Month", "Spend")

# create dummy variables
# create a dummy variable to indicate the spend before treatment (Month Before), and the spend after treatment (Month After)
data1_stacked$Time1 = ifelse(data1_stacked$Month == "Month After", 1, 0)

# create an interaction between time and treated
data1_stacked$Did1 = data1_stacked$Time1 * data1_stacked$Joined

# Estimating the DID estimator
didreg_spend <- lm(Spend ~ Joined + Time1 + Did1, data = data1_stacked)
summary(didreg_spend)

##########################################################################
# Question 2&4 Calculating Retention and Qunatify the magnitude of Change#
##########################################################################

# load data2
data2 <- read_excel("Data2.xlsx")
View(data2)
colnames(data2) <- c("CustomerID", "Joined", "Age_With_Firm", "Churn", "Avg_Spend")

# logistic regression Churn ~ Joined + Age_With_Firm + Avg_Spend
logitreg<- glm(Churn ~ Joined + Age_With_Firm + Avg_Spend, data=data2, family=binomial(link="logit"))
summary(logitreg)
exp(coef(logitreg))

# Prediction
preddata <- with(data2, data.frame(CustomerID, Joined, Age_With_Firm, Avg_Spend))
probchurn <- predict(logitreg, newdata = preddata, type="response")
sum(probchurn<0) #check the probabilities of churn are all positive
predchurn <- ifelse(probchurn > 0.5, 1, 0)

# Model Evaluation
missclass <- predchurn!=data2$Churn
misclasserror <- mean(predchurn!=data2$Churn)
print(paste("Accuracy", 1-misclasserror))


#################
# Question 3 CLV#
#################

# divide the customers into Joined and Not Joined
Control <- data2 %>% filter(Joined == 0)
Treatment <- data2 %>% filter(Joined == 1)

# calculate retention rate for each group
table_Churn <- addmargins(table(Control$Churn), margin=1)
table_Churn
ret_ctrl <- 57/117 #refer to the data from table_Churn
table_Treatment <- addmargins(table(Treatment$Churn), margin=1)
table_Treatment
ret_trt <- 24/82 #refer to the data from table_Churn

# calculate CLV for each group
Margin = data2$Avg_Spend*0.5
i=0.1
CLV_ctrl <- Margin*(1+i)/(1+i-ret_ctrl)
CLV_trt <- Margin*(1+i)/(1+i-ret_trt)

# One-tailed t-test
# Hypothesis: H0: CLV_ctrl >= CLV_trt; H1: CLV_ctrl < CLV_trt
t.test(CLV_ctrl, CLV_trt, alternative = "less", conf.level = 0.95)
#INTERPRETATION: Because the p-value is 1, greater than 0.05. Therefore we do not have
#sufficient evidence to reject the null hypothesis. The CLV of the control group is larger than
#or equal to the CLV of the treatment group.

# two-tailed t-test
# Hypothesis: H0: CLV_ctrl = CLV_trt; H1: CLV_ctrl =/= CLV_trt
t.test(CLV_ctrl, CLV_trt, alternative = "two.sided", conf.level = 0.95)
#INTERPRETATION: Because the p-value is 4.312e-12, less than 0.05. Therefore we have
#sufficient evidence to reject the null hypothesis. The CLV of the control group is not equal to
#the CLV of the treatment group.



