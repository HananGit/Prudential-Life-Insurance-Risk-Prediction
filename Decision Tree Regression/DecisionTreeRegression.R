setwd("C:/Users/h/Desktop/")

#loading neccessary packages
#install.packages("rpart.plot")
#install.packages("forecast")
#install.packages("Metrics")
library(Metrics)
library(rpart.plot)
library(rpart)
#library(forecast)

#Read the csv data
Prudential_Data <-read.csv("Prudential_final_Data.csv", header = T)

#Splitting dataset into train and test datasets
set.seed(2) # we set the seed to make sure that the train and test data will not change every time we divide them by running the sample function

# running sample function to select randomly 80% index numbers of the dataset and use it to divide the dataset into 80% as a train dataset and the remaining 20% as a test dataset
sample_index = sample(1:nrow(Prudential_final_Data), nrow(Prudential_final_Data)*0.8) #length(sample_index) should be %80 of the dataset
Prudential_train = Prudential_final_Data[-sample_index,] #80% of dataset is train data
Prudential_test= Prudential_final_Data[sample_index,] #20% of dataset is test data

#Fitting the Tree Regression model on train dataset
tree_reg_model <- rpart(Response~.,method="anova", data=Prudential_train)

#Printing plots for train data
tree_reg_model
summary(tree_reg_model) # detailed summary


# plot decision tree 
plot(tree_reg_model, uniform=TRUE, main="Regression Tree for Response")
#Label the decision tree plot
text(tree_reg_model, use.n=TRUE, all=TRUE, cex=.8)
#OR
rpart.plot(tree_reg_model)
#OR
rpart.plot(tree_reg_model, type= 4,digits=3, extra = 101 )
#OR
prp(tree_reg_model, faclen = 0, cex = 0.8, extra = 1) 

#Examine the results
printcp(tree_reg_model) # print the cptable
plotcp(tree_reg_model) # Plot cross-validation results


#Predicting the Response for test data
Prediction <- predict(tree_reg_model, Prudential_test[-65])
#Print Prediction summary
summary(round(Prediction))


#Print the confusion matrix
confusion_Matrix <- table(Prediction,Prudential_test$Response)
print(confusion_Matrix)


#Computing accuracy and correlation
accuracy <- (sum(diag(confusion_Matrix))/sum(confusion_Matrix)) * 100 # computing the accuracy by dividing the sum of diagonal matrix (the correct predictions) by the total sum of the matrix
print(accuracy)

correlation_accuracy<- cor(Prediction,Prudential_test$Response) # correlation between the actuals and predicted values can be used as a form of accuracy measure.
print(correlation_accuracy)

#Computing the Root Mean Squared Error (RMSE) between actual and prediction data
RMSE <- rmse(Prudential_test$Response,Prediction)
print(RMSE)
#accuracy(Prediction,Prudential_test$Response)


#Adding the prediction column and writing it to a new file
predicted_Data <- data.frame(Prudential_test[-65],Prediction)
write.csv(predicted_Data, file = "DecisionTreeRegression_Results.csv", row.names = FALSE)

