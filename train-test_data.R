library(dplyr)
library(ggplot2)
library(GGally)
library(car)

# After loading up the dataset begin training and testing! 
df = read.csv("/Users/christianalcala/documents/Wrangler142-Fall2019.csv")


df_train = subset(df, Year <= 2015,select = c(Unemployment,WranglerQueries,CPI.Energy,CPI.All,WranglerSales,MonthFactor))
df_train
df_test = subset(df, Year >= 2016 & Year <=2019,select = c(Unemployment,WranglerQueries,CPI.Energy,CPI.All,WranglerSales,MonthFactor))
print(df_test)

# I will be using a liner model to help me predict the Sales prices/figure out bad features in the data. 
# First model:
first_model = lm(WranglerSales ~Unemployment + WranglerQueries + CPI.All + CPI.Energy , data = df_train)
print(summary(first_model))
print(vif(first_model))
first_vif = vif(first_model)

# Second model:
second_model = lm(WranglerSales ~  WranglerQueries + CPI.Energy+ CPI.All, data = df_train)
summary(second_model)
# Unemployment has multicollinearity with CPI variables well specifically with CPI.All
print(vif(second_model))
second_vif = vif(second_model)

# Third model: 
third_model = lm(WranglerSales ~ CPI.Energy + WranglerQueries, data = df_train)
summary(third_model)
print(vif(third_model))
thrid_vif = vif(third_model)

# Calculating R^2: 
baseline = mean(df_train$WranglerSales)
y = df_test$WranglerSales
y_hat = predict(second_model, newdata = df_test)
SSE = sum((y-y_hat)^2)
SST = sum((y-baseline)^2)
R_squared = 1- SSE/SST
R_squared
# For the third model I got rid of the high VIF's ( Unemployment and CPI.All ). 
# You can see that these two are the most significant variables.

# Now I will be including seasonality
month_model = lm(formula = WranglerSales ~ MonthFactor+CPI.Energy + WranglerQueries +CPI.All , data = df_train)
summary(month_model)
vif(month_model)
month_vif = vif(month_model) 

# Repeat calculations for new model:
baseline = mean(df_train$WranglerSales)
y = df_test$WranglerSales
y_hat = predict(month_model, newdata = df_test)
SSE = sum((y-y_hat)^2)
SST = sum((y-baseline)^2)
R_squared = 1- SSE/SST
R_squared

# Final Model I will be making for predictions: 
# I used a subset of the independent variables used in the parts above, providing a brief justification for the variables selected. 

# Now I will consider all five independent variables below
final_model =  lm(formula = WranglerSales ~ WranglerQueries +  MonthFactor + CPI.Energy  , data = df_train)
summary(final_model)
vif(final_model)
final_model_vif = vif(final_model)

# Calculating the OSR^2 to get a better understanding of my results. 
baseline = mean(df_train$WranglerSales)
y = df_test$WranglerSales
y_hat = predict(final_model, newdata = df_test)
SSE = sum((y-y_hat)^2)
SST = sum((y-baseline)^2)
OSR_squared = 1- SSE/SST
OSR_squared





