library(e1071)
# Taking 1000 rows randomly to fit the SVM model because the original dataset is so huge
set.seed(2)
sample_index2 = sample(1:nrow(Prudential_final_Data), size=1000) 
Prudential_data2 = Prudential_final_Data[sample_index2,] 

#Split the new dataset Prudential_data2 to two parts,one part for training, one for testing
sample_index3 = sample(1:nrow(Prudential_data2), size=800) 
Prudential_train2 = Prudential_data2[sample_index3,] #80% of Prudential_data2 is new train data
Prudential_test2= Prudential_data2[-sample_index3,]  #20% of Prudential_data2 is new test data

# Tuning the svm models to get optimal cost and gamma parameter which will be use in next section for creating svm model.
# 1.Using Radial kernel
tune_radial <- tune(svm,Response ~ .,
                    data = Prudential_train2,
                    kernel="radial",
                    ranges=list(cost=10^(-1:2),
                                gamma=c(.5,1,2),
                                scale=F
                    ))
# showing the value of gamma and cost
summary_radial <- summary(tune_radial)
print(summary_radial$best.parameters)

# 2.Using polynomial kernel
tune_poly <- tune(svm, Response ~ .,
                  data = Prudential_train2,
                  kernel="polynomial",
                  ranges=list(cost=10^(-1:7),
                              gamma=c(.5,1,2),
                              scale=F
                  ))
# showing the value of gamma and cost
sumry_poly <- summary(tune_poly)
print(sumry_poly$best.parameters)

# 3.Using sigmoid kernel
svm_tune_sigm <- tune(svm, Response ~ .,
                      data = Prudential_train2,
                      kernel="sigmoid",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                                  scale=F
                      ))
# showing the value of gamma and cost
sumry_sigm <- summary(svm_tune_sigm)
print(sumry_sigm$best.parameters)

# 4.Using linear kernel
svm_tune_linear <- tune(svm, Response ~ .,
                        data = Prudential_train2,
                        kernel="linear",
                        ranges=list(cost=10^(-1:2),
                                    scale=F                        
                        ))
# showing the value of gamma and cost
sumry_linear <- summary(svm_tune_linear)
print(sumry_linear$best.parameters)

# Using the parameters calculated with the above function to build SVM model
svm_radial <- svm(Response ~ ., kernel="radial", cost = 10, gamma=0.5,data = Prudential_train2,scale = F)
svm_poly <- svm(Response ~ ., kernel="polynomial", cost = 100000, gamma=0.000001,data = Prudential_train2, scale=F)
svm_sigm <- svm(Response ~ ., kernel="sigmoid", cost = 100, gamma=0.5,data = Prudential_train2,scale = F)
svm_linear <- svm(Response ~ ., kernel="linear", cost = 1, data = Prudential_train2, scale = F)

# Using predict function on svm models with diffrent kernels
install.packages("forecast")
library(forecast)

prediction1 <-  predict(svm_radial, Prudential_test2[-65])
accuracy(prediction1, Prudential_test2$Response)#RMSE value is 2.436015

prediction2 <-  predict(svm_poly, Prudential_test2[-65])
accuracy(prediction2, Prudential_test2$Response)#RMSE value is 2.517406

prediction3 <-  predict(svm_sigm, Prudential_test2[-65])
accuracy(prediction3, Prudential_test2$Response)#RMSE value is 2.531798

prediction4 <-  predict(svm_linear, Prudential_test2[-65],type="class")
accuracy(prediction4, Prudential_test2$Response)#RMSE value is 2.242062

# After comparing the RMSE of the above 4 models, SVM model with linear kernel is the best one.
# Writing the predicted data to csv file.
predicted_data<-data.frame(Prudential_test2[-65],prediction4)
write.csv(predicted_data,file="/Users/ouyoshimisatoshi/Desktop/Data Science/MidtermProject/data/predictedData.csv",row.names = FALSE)
