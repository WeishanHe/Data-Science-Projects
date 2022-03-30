# load package
library(readxl)
library(dplyr)
library(ggplot2)

# load data
pre_all <- read_excel("Preferences.xlsx", sheet = 2)
design_matrix<- read_excel("Preferences.xlsx", sheet = 1)

############
# function #
############
my_conjoint <- function(preferences, plotit=FALSE){
  
  # transform data types and store variables
  screen_75 <- factor(design_matrix$Screen_75_ich)
  screen_85 <- factor(design_matrix$Screen_85_inch)
  resolution_4k <- factor(design_matrix$Resolution_4K)
  sony <- factor(design_matrix$Sony)
  high_price <- factor(design_matrix$High_price)
  
  # run regression model and extract the coefficients
  model_reg <- lm(preferences~screen_75+screen_85+resolution_4k+sony+high_price)
  partworth <- coef(summary(model_reg))[,1]
  
  #############################################################
  # output 1: regression model summary for partworth, se, tval#
  #############################################################
  mod_summary <- coef(summary(model_reg))[,1:3]
  
  #################################
  # output 2: attribute importance#
  #################################
  attr_importance <- data.frame(matrix(NA, nrow=4, ncol=3))
  colnames(attr_importance) <- c('Attribute', 'Range', 'Importance')
  rownames(attr_importance) <- c('screen_size', 'resolution', 'brand','price')
  attr_importance['Attribute'] <- mod_summary[2:5,1]
  
  # attribute importantce - Range
  attr_importance[1,2] <- max(mod_summary[2:3,1], 0) - min(mod_summary[2:3,1], 0)
  attr_importance[2,2] <- max(mod_summary[4,1], 0) - min(mod_summary[4,1], 0)
  attr_importance[3,2] <- max(mod_summary[5,1], 0) - min(mod_summary[5,1], 0)
  attr_importance[4,2] <- max(mod_summary[6,1], 0) - min(mod_summary[6,1], 0)
  
  # attribute importantce - Importance
  attr_importance[1,3] <- attr_importance[1,2]/sum(attr_importance$Range)
  attr_importance[2,3] <- attr_importance[2,2]/sum(attr_importance$Range)
  attr_importance[3,3] <- attr_importance[3,2]/sum(attr_importance$Range)
  attr_importance[4,3] <- attr_importance[4,2]/sum(attr_importance$Range)
  
  ##############################
  # output 3: Willingess to Pay#
  ##############################
  Sony_design <- c(1,1,0,1,1,2500)
  Sharp_design <- c(1,0,1,1,0,2000)
  point_val <- (Sharp_design[6]-Sony_design[6])/partworth[6]
  WTP <- as.data.frame(point_val*partworth[1:5], col.names='WTP')
  
  ####################################
  # output 4: conjoint analysis table#
  ####################################
 
   # create black result matrix
  CA <- data.frame(matrix(NA, nrow=12, ncol=6))
  CA[,1] <- seq(1500, 2600, by=100)
  colnames(CA) <- c('price', 'utility_mydesign', 'market_share', 'sales', 'margin', 'profit')
  
  # use for loop to get all the results for each price
  for (i in 1:12){
    
    # create matrix for my_design, sony, sharp, and costs
    my_design <- c(1,0,1,0,0,CA[i,1])
    Sony <- c(1,1,0,1,1,2500)
    Sharp <- c(1,0,1,1,0,2000)
    costs <- c(1000,500,1000,250,250, NA)
    design_info <- rbind(partworth,my_design, Sony, Sharp, costs)
    colnames(design_info) <- c('intercept','screen_75','screen_85','resolution_4k','sony','high_price')
    
    # Utility
    CA[i,2] <- sum(design_info['partworth',1:5] * design_info['my_design',1:5])+ design_info['partworth',6]*((CA[i,1]-design_info['Sharp',6])/(design_info['Sony',6]-design_info['Sharp',6]))
    utility_sony <- sum(design_info['partworth',1:5] * design_info['Sony',1:5])+ design_info['partworth',6]*((design_info['Sony',6]-design_info['Sharp',6])/(design_info['Sony',6]-design_info['Sharp',6]))
    utility_sharp <- sum(design_info['partworth',1:5] * design_info['Sharp',1:5])+ design_info['partworth',6]*((design_info['Sharp',6]-design_info['Sharp',6])/(design_info['Sony',6]-design_info['Sharp',6]))
    
    # Market Share
    CA[i,3] <- exp(CA[i,2])/sum(exp(CA[i,2]),exp(utility_sony),exp(utility_sharp))
    
    # Sales
    market_size <- 100
    CA[i,4] <- market_size*CA[i,3]
    
    # Margin
    Net_cost <- sum(design_info['my_design',1:5]*design_info['costs',1:5])
    CA[i,5] <- CA[i,1]-Net_cost
    
    # Profit
    CA[i,6] <- CA[i,4]*CA[i,5]
  }
  
  #############################################
  # output 4: optical price and maximum profit#
  ############################################
  # convert CA to dataframe
  df_CA <- as.data.frame(CA)
  
  max_profit_val <- max(df_CA$profit)
  max_profit <- sprintf('Maximum Profit: %f', max_profit_val)
  opt_price_val <- df_CA$price[CA$profit == max_profit_val]
  opt_price <- sprintf('Optimal Price: %.0f', opt_price_val)
  
  #######################################################
  # ouput 5: plotting (doesn't work inside the function)#
  #######################################################
  if(plotit){
    # Price vs market share
    plot1 <- ggplot(aes(x=price, y=market_share), data=df_CA) + geom_line() + xlab('Price') + ylab('Market Share') + ggtitle('Market Share vs Price')
    
    # Price vs Profit
    plot2 <- ggplot(aes(x=price, y=profit), data=df_CA) + geom_line() + xlab('Price') + ylab('Profit') + ggtitle('Profit vs Price')
    
    out_graph <- list(plot1, plot2)
    print(out_graph)
  }
  
  
  out_table <- list(mod_summary, attr_importance, WTP, CA, max_profit, opt_price)
  return(out_table)
  
}

# call function for each customer
# Team member 1: Weishan
pre_customer1 <- pre_all$customer1
my_conjoint(pre_customer1, plotit = TRUE)


