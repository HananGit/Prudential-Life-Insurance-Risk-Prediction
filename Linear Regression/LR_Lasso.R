#Splitting dataset into train and test datasets
set.seed(2) # we set the seed to make sure that the train and test data will not change every time we divide them by running the sample function

sample_index_2 = sample(1:nrow(Prudential_final_Data_2), nrow(Prudential_final_Data_2)*0.8) #length(sample_index) should be %80 of the dataset
Prudential_train_2 = Prudential_final_Data_2[sample_index_2,] #80% of dataset is train data
Prudential_test_2= Prudential_final_Data_2[-sample_index_2,] #20% of dataset is test data
write.csv(Prudential_train_2, file = "C:/Users/Nauka Salot/Desktop/ADS/Assignments_Nauka/Mid-Term Project_2/Prudential_train_2.csv", row.names = FALSE)
write.csv(Prudential_test_2, file = "C:/Users/Nauka Salot/Desktop/ADS/Assignments_Nauka/Mid-Term Project_2/Prudential_test_2.csv", row.names = FALSE)


#Checking the dimensions of the training dataset
dim(Prudential_train_2)
dim(Prudential_test_2)

#Linear Regression
linear_model <- lm(Response ~ Product_Info_4+
                     Medical_History_1+
                     
                     Medical_Keyword_6+
                     
                     Medical_Keyword_45+
                     `Product_Info_2 _ A1`+
                     `Product_Info_2 _ E1`+
                     `Product_Info_2 _ D4`+
                     `Product_Info_2 _ A6`+
                     `Product_Info_2 _ A5`+
                     `Product_Info_2 _ C4`+
                     `Product_Info_2 _ B2`+
                     `Product_Info_2 _ A4`+
                     `Product_Info_6 _ 1`+
                     `Employment_Info_3 _ 1`+
                     `Employment_Info_5 _ 3`+
                     `InsuredInfo_2 _ 2`+
                     `InsuredInfo_5 _ 1` +
                     `InsuredInfo_6 _ 2` +
                     `InsuredInfo_7 _ 1`+
                     `Insurance_History_1 _ 1`+
                     `Medical_History_11 _ 3`+
                     `Medical_History_11 _ 1`+
                     `Medical_History_14 _ 3`+
                     `Medical_History_22 _ 2`+
                     `Medical_History_35 _ 1`+
                     `Medical_History_38 _ 1`+
                     `Medical_History_39 _ 3`
                   , data = Prudential_train_2)


summary(linear_model)

linear_model_2 <- lm(Response ~., data = Prudential_train_2)

summary(linear_model_2)

linear_model_1 <- lm( Response ~  Product_Info_4+Ins_Age+Ht+Wt+Family_Hist_2+Family_Hist_4+
                        Medical_History_1+Medical_Keyword_3+Medical_Keyword_9+Medical_Keyword_11+
                        Medical_Keyword_12+Medical_Keyword_15+Medical_Keyword_16+Medical_Keyword_18+
                        Medical_Keyword_19+Medical_Keyword_25+Medical_Keyword_31+Medical_Keyword_33+
                        Medical_Keyword_34+Medical_Keyword_37+Medical_Keyword_38+`Product_Info_2 _ A1`+
                        `Product_Info_2 _ E1`+`Product_Info_2 _ D4`+`Product_Info_2 _ A7`+
                        `Product_Info_2 _ A6`+`Product_Info_2 _ A5`+`Product_Info_2 _ B2`+
                        `Product_Info_2 _ A4`+`Product_Info_6 _ 1`+`Employment_Info_3 _ 1`+
                        `Employment_Info_5 _ 3`+`InsuredInfo_2 _ 2`+`InsuredInfo_5 _ 1` +
                        `InsuredInfo_6 _ 2`+`InsuredInfo_7 _ 1`+`Insurance_History_3 _ 1`+
                        `Insurance_History_7 _ 3`+`Medical_History_4 _ 1`+`Medical_History_7 _ 1`+
                        `Medical_History_11 _ 3`+`Medical_History_22 _ 2`+`Medical_History_35 _ 1`+
                        `Medical_History_38 _ 1`+`Medical_History_39 _ 3`,data= Prudential_train_2)

summary(linear_model_1)

Prudential_test_2 <- subset(Prudential_test_2, select = -c(Response) )

pred_2<- predict(linear_model, Prudential_test_2)
error_2<-rmse(Prudential_train_2$Response, pred_2)

pred<- predict(linear_model_1, Prudential_test_2)

library(Metrics)
error<-rmse(Prudential_train_2$Response, pred)

dim(Prudential_train_2)

#Lasso Regression
install.packages("glmnet")
library(glmnet)
x<- model.matrix(Response ~ ., Prudential_train_2)[,-65]
y<-Prudential_train_2$Response
grid = 10^seq(10,-2,length=100)
lasso.mode = glmnet(x,y,alpha = 1, lambda = grid)
summary(lasso.mode)
coeficient_lasso<-coef(lasso.mode)
 lasso.mode$beta[,1]

fit.lasso <- lm(Response ~ Product_Info_4+
                  Ins_Age+ouj
                  Ht+
                  Wt+
                  
                  Family_Hist_4+
                  Medical_History_1+
                  
                  Medical_Keyword_3+
                  
                  Medical_Keyword_9+
                  
                  Medical_Keyword_12+
                  Medical_Keyword_15+
                  Medical_Keyword_16+
                  
                  
                  
                  
                  Medical_Keyword_31+
                  Medical_Keyword_33+
                  Medical_Keyword_34+
                  Medical_Keyword_37+
                  Medical_Keyword_38+
                  
                  `Product_Info_2 _ A1`+
                  `Product_Info_2 _ E1`+
                  `Product_Info_2 _ D4`+
                  `Product_Info_2 _ A7`+
                  `Product_Info_2 _ A6`+
                  `Product_Info_2 _ A5`+
                  
                  `Product_Info_2 _ B2`+
                  `Product_Info_2 _ A4`+
                  
                  `Employment_Info_3 _ 1`+
                  `Employment_Info_5 _ 3`+
                  `InsuredInfo_2 _ 2`+
                  `InsuredInfo_5 _ 1` +
                  
                  `InsuredInfo_6 _ 2`+
                  `InsuredInfo_7 _ 1` +
                  
                  `Insurance_History_7 _ 3`+
                  `Medical_History_4 _ 1`+
                  `Medical_History_7 _ 1`+
                  `Medical_History_11 _ 3`+
                  
                  `Medical_History_22 _ 2`+
                  `Medical_History_35 _ 1`+
                  `Medical_History_38 _ 1`+
                  `Medical_History_39 _ 3`, data = Prudential_train_2)

summary(fit.lasso)

#Plotting the graph
plot(fit.lasso)

pred_lasso <- predict(fit.lasso, Prudential_test_2)
pred_lasso <- round(pred_lasso)
summary(pred_lasso)
plot(pred_lasso)

#To be executed

mat_lasso<-table(pred_lasso,Prudential_test_2$Response)
sum(diag(mat_lasso))/sum(mat_lasso) * 100

rm_lasso <- sqrt(mean((Prudential_test_2$response - pred_lasso)^2, na.rm = TRUE))

#backward elimination

#including the column
predict_data <- data.frame(Prudential_test_2, pred_lasso)
write.csv(predict_data, file = "C:/Users/Nauka Salot/Desktop/ADS/Assignments_Nauka/Mid-Term Project_2/Prudential_test_2_results.csv", row.names = FALSE)

#RMSE Value
library(Metrics)
error<-rmse(Prudential_train_2$Response, pred)

#Forecast
install.packages("forecast")
library(forecast)
accuracy(pred_lasso, Prudential_train_2$Response)
