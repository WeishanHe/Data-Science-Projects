setwd("file directory")

# load package
library(readxl)
library(dplyr)
library(ggplot2)
library('MASS')

# load data
pre_all <- read_excel("Preferences.xlsx", sheet = 2)
design_matrix<- read_excel("Preferences.xlsx", sheet = 1)

############
# function #
############
WTP_Bootstrap <- function(preferences, plotit=FALSE){
  
  ####################################
  ######## Residual Bootstrap#########
  ####################################
  set.seed(123)
  data1 <- cbind(preferences, design_matrix[,2:6])
  
  y1 <- data1[,1]
  x1 <- data1[,2:6]
  Screen_75_1 <- factor(x1$Screen_75_ich)
  Screen_85_1 <- factor(x1$Screen_85_inch)
  Resolution_4K_1 <- factor(x1$Resolution_4K)
  Sony_1 <- factor(x1$Sony)
  High_price_1 <- factor(x1$High_price)
  
  model1 <- lm(y1~Screen_75_1+Screen_85_1+Resolution_4K_1+Sony_1+High_price_1)
  
  nn1 <- nrow(data1)
  yhat1 <- predict(model1)
  rr1 <- model1$resid						# residuals based on original data, to be used for resampling
  
  bb1 <- 1000							# number of resampling
  partworth.out1 <- matrix(0, bb1, 6 )			# matrix to save partworth from bootstrap
  colnames(partworth.out1) <- c('intercept', 'screen_75', 'screen_85', 'resolution_4k', 'sony', 'high_price')
  
  # Do Residual Bootstrap 1000 times to get 95% CI for R^2
  
  for(ii in 1:bb1) {
    ystar1 <- yhat1 + rr1[sample(nn1, nn1, replace = TRUE)]		# y* with original yhat plus r*
    out.star1 <- lm(ystar1~Screen_75_1+Screen_85_1+Resolution_4K_1+Sony_1+High_price_1)	# lm with new y* and same x to get new bhat*
    partworth.star1 <- coef(summary(out.star1))[,1]
    partworth.out1[ii,] <- t(partworth.star1)
    
  }
  
  Sony_design <- c(1,1,0,1,1,2500)
  Sharp_design <- c(1,0,1,1,0,2000)
  point_val_star1 <- (Sharp_design[6]-Sony_design[6])/partworth.out1[,6]
  
  WTP.star1 <- as.data.frame(point_val_star1*partworth.out1[,2:5])
  colnames(WTP.star1) <- c('screen_75', 'screen_85', 'resolution_4k', 'sony')

  result <- matrix(0, 4, 6)
  rownames(result) <- c('screen_75', 'screen_85', 'resolution_4k', 'sony')
  colnames(result) <- c('resid_bootstrap_2.5%','resid_bootstrap_50%' , 'resid_bootstrap_97.5%', 'data_bootstrap_2.5%', 'data_bootstrap_50%', 'data_bootstrap_97.5%')

  for (i in 1:4) {
    result[i,1] <- quantile(WTP.star1[,i], probs = 0.025)
    result[i,2] <- quantile(WTP.star1[,i], probs = 0.5)
    result[i,3] <- quantile(WTP.star1[,i], probs = 0.975)
  }
  
  ################################
  ######## Data Bootstrap#########
  ################################
  
  data2 <- cbind(preferences, design_matrix[,2:6])
  bb2 <- 1000
  
  partworth.out2 <- matrix(0, bb2, 6)				# new output matrix for R^2
  nn2 <- nrow(data2)
  # Do Data Bootstrap 1000 times to get 95% CI for R^2
  for(ii in 1:bb2) {
    data.star2 <- data2[sample(nn2, nn2, replace = T),]		# create (y*, x*) by resampling rows in original data matrix
    ystar2 <- data.star2[,1]
    xstar2 <- data.star2[,2:6]
    
    Screen_75_2 <- factor(xstar2$Screen_75_ich)
    Screen_85_2 <- factor(xstar2$Screen_85_inch)
    Resolution_4K_2 <- factor(xstar2$Resolution_4K)
    Sony_2 <- factor(xstar2$Sony)
    High_price_2 <- factor(xstar2$High_price)
    
    out.star2 <- lm(ystar2~Screen_75_2+Screen_85_2+Resolution_4K_2+Sony_2+High_price_2)							# lm with new y* and new x* to get new bhat*
    partworth.star2 <- coef(summary(out.star2))[,1]
    partworth.out2[ii,] <- t(partworth.star2)							# save rsq from iteration ii
    
  }
  
  point_val_star2 <- (Sharp_design[6]-Sony_design[6])/partworth.out2[,6]
  
  WTP.star2 <- as.data.frame(point_val_star2*partworth.out2[,2:5])
  colnames(WTP.star2) <- c('screen_75', 'screen_85', 'resolution_4k', 'sony')

  for (i in 1:4) {
    result[i,4] <- quantile(WTP.star2[,i], probs = 0.025)
    result[i,5] <- quantile(WTP.star2[,i], probs = 0.5)
    result[i,6] <- quantile(WTP.star2[,i], probs = 0.975)
  }
 return(result)
}

  