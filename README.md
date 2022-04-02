# Data-Science-Projects
Data Science projects including Diff in Diff, CLV, Conjoint Analysis, PCA, PCR, Bootstrap, Logistic Regression, Random Forest, and XGBoost.

**1. New Feature Performance Evaluation using DID and CLV (in R)** <br>
In mid-2021, KyngaCell launched a new online gaming community. This enabled users to connect and interact during and outside of gameplay. It is now 2023 and the Chief Analytics Officer (CAO) requested to evaluate this new service. Our three objectives to assess the effectiveness are: 
  - determine if online gaming increased user revenue
  - determine if the online community led to an increase in user retention, and 
  - determine if the online community led to an increase in CLV

In addition, we took a step further to explain the campaign effectiveness. We apply diff in diff and customer life time value in R to translate raw data to business insights. Based on our findings, the online community successfully increased revenue in the short term but failed to boost retention rate and CLV in the long run. The same ineffectiveness is observed in the campaign. Thus, KyngaCell should provide more incentives to improve user engagement. 

**2. New TV Design Using Conjoint Analysis (in R)** <br>
The company is going to launch a new TV. The DS team is required to use design matrix to conduct conjoint analysis in order to generate an optimal price. The following seven results are generated by the conjoint analysis function that we constructed: 
  - Partworth of each attribute level
  - Attribute Importance of each attribute
  - Willingness to pay for each non-price attribute level
  - Optimal price
  - Maximum profit
  - Plot market shares as a function of prices
  - Plot profit as a function of prices

**3. Use Bootstrap to obtain Confidence Levels for Willingness to Pay (in R)** <br>
Applied Data Bootstrap and Residual Bootstrap to obtain the 95% confidence levels and median for willingness to pay.

**4. Brand Map_Use PCA and PCR to Visualize Brand Competition (in R)** <br>
Build brand maps for car brands and recommend to Infinity's managers what they should do to improve their product design.

**5. Fraud Detection Using Supervised Machine Learning (in Python)**
In this project, I first conducted EDA to learn about the dataset and then applied logistic regression, random forest, and XGBoost on simulated transaction data in order to improve the PaySim’s ability to detect fraudulent transactions. All three models yield high accuracy in detecting fraud payment. We considered logistic regression as the best model for the simulated data given its lower time cost. Real-world transaction data are required to improve model performance and choose the best algorithm.
