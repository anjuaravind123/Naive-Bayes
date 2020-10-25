### Load the data####
#salary_train as train data and salary_test as test data##

library(lattice)
library(ggplot2)
str(salary_train)
str(salary_test)

#Visualization 
# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(salary_train$workclass,salary_train$Salary)
plot(salary_train$education,salary_train$Salary)
plot(salary_train$educationno,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$sex,salary_train$Salary)

#Density Plot 

ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=salary_train,aes(x = salary_train$education, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggplot(data = salary_train,aes(x = salary_train$race, fill = salary_train$Salary)) + geom_density(alpha =0.9,color ='Violet')

#install.packages('naivebayes')
library('naivebayes')

#install.packages("e1071")
library(e1071)

# Naive Bayes Model 
Model <- naiveBayes(salary_train$Salary ~ ., data = salary_train)
Model

##predict on test data
Model_pred <- predict(Model,salary_test)
##test accuracy
table(Model_pred,salary_test$Salary)
mean(Model_pred ==salary_test$Salary)

#Train Accuracy
model_trainpred <- predict(Model,salary_train)
#train Accuracy
table(model_trainpred,salary_train$Salary)
mean(model_trainpred ==salary_train$Salary)
