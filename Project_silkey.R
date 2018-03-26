#library(caret)

mydata <- read.csv(file="C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_train.csv", header=TRUE, sep=",")

basic_feature <- mydata[,c(51:60,281)]
names(basic_feature) <-c("A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A281")
textual_feature <- mydata[,c(63:262,281)]
names(textual_feature)<-paste("A", 63:263, sep="")

head(basic_feature)
head (textual_feature)
#Creating 4 Tests after reading the test data

test1 <- read.csv(file="C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_test-2012.02.01.00_00.csv", header=TRUE, sep=",")
test2 <- read.csv(file="C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_test-2012.02.02.00_00.csv", header=TRUE, sep=",")
test3 <- read.csv(file="C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_test-2012.03.03.00_00.csv", header=TRUE, sep=",")
test4 <- read.csv(file="C:/Users/silke/Desktop/Courses/ML/data/BlogFeedback/blogData_test-2012.03.04.00_00.csv", header=TRUE, sep=",")

#Dividing the columns into basic_features and textual_features for all 4 tests

basic_feature_test1 <- test1[,c(51:60,281)]
names(basic_feature_test1) <-c("A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A281")

basic_feature_test2 <- test2[,c(51:60,281)]
names(basic_feature_test2) <-c("A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A281")

basic_feature_test3 <- test3[,c(51:60,281)]
names(basic_feature_test3) <-c("A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A281")

basic_feature_test4 <- test4[,c(51:60,281)]
names(basic_feature_test4) <-c("A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A281")

textual_feature_test1 <- test1[,c(63:262,281)]
names(textual_feature_test1)<-paste("A", 63:263, sep="")

textual_feature_test2 <- test2[,c(63:262,281)]
names(textual_feature_test2)<-paste("A", 63:263, sep="")

textual_feature_test3 <- test3[,c(63:262,281)]
names(textual_feature_test3)<-paste("A", 63:263, sep="")

textual_feature_test4 <- test4[,c(63:262,281)]
names(textual_feature_test4)<-paste("A", 63:263, sep="")

head(textual_feature_test1)
#Creating generic function to calculate Mean Square Error (mse)

mse = function(y_diff, y) 
  {
  mse = mean((y - y_diff)^2)
  return(mse)
}

####Starting with Experiment 1 : Basic features

## 1. Linear Regression
# Training model with training data


fit_basic_feature_train <- lm(formula=A281 ~ A51+A52+A53+A54+A55+A56+A57+A58+A59+A60, data = basic_feature)

summary(fit_basic_feature_train)
##Predicting value for Training model

pred_basic_feature <-predict(fit_basic_feature_train,basic_feature,se.fit=TRUE)
MSE = mse(pred_basic_feature$fit, basic_feature$A281)
print (mse)

## Predicting values for 4 Tests

#Test 1
pred_basic_feature_test1 <-predict(fit_basic_feature_train,basic_feature_test1,se.fit=TRUE)
MSE = mse(pred_basic_feature_test1$fit, basic_feature_test1$A281)
print (MSE)

#Test 2
pred_basic_feature_test2 <-predict(fit_basic_feature_train,basic_feature_test2,se.fit=TRUE)
MSE = mse(pred_basic_feature_test2$fit, basic_feature_test2$A281)
print (MSE)

#Test 3
pred_basic_feature_test3 <-predict(fit_basic_feature_train,basic_feature_test3,se.fit=TRUE)
MSE = mse(pred_basic_feature_test3$fit, basic_feature_test3$A281)
print (MSE)

#Test 4
pred_basic_feature_test4 <-predict(fit_basic_feature_train,basic_feature_test4,se.fit=TRUE)
MSE = mse(pred_basic_feature_test4$fit, basic_feature_test4$A281)
print (MSE)


## 2. Logistic Regression

# For Training data
basic_feature_mean <- mean(basic_feature$A281, NA.rm = TRUE)
basic_feature_log_n <- basic_feature
basic_feature_log_n$A281 <- sapply(basic_feature_log_n$A281, function(x) {ifelse(x >= basic_feature_mean, 1, 0)})
logfit_basic_feature_train <- glm(formula = A281 ~ A51+A52+A53+A54+A55+A56+A57+A58+A59+A60, data = basic_feature_log_n, family=binomial())
summary(logfit_basic_feature_train)

# For Testdata1
basic_feature_test1_mean <- mean(basic_feature_test1$A281, NA.rm = TRUE)
basic_feature_test1_n <- basic_feature_test1
basic_feature_test1_n$A281 <- sapply(basic_feature_test1_n$A281, function(x) {ifelse(x >= basic_feature_test1_mean, 1, 0)})
log_pred_basic_feature_1 <-predict.glm(logfit_basic_feature_train,basic_feature_test1_n,se.fit=TRUE)
predicted_log <- sapply(log_pred_basic_feature_1$fit, function(x){ifelse(x > .5, 1, 0)})
typeof(basic_feature_test1_n$A281)


############Experiment 2 : textual feature
######1. Linear regression
#Training a model 

fit_textual_feature_train <- lm(textual_feature$A263 ~ ., data = textual_feature)

summary(fit_textual_feature_train)

#Train data
pred_textual_feature <-predict(fit_textual_feature_train,textual_feature,se.fit=TRUE)
MSE = mse(pred_textual_feature$fit, textual_feature$A263)

#Test data 1
pred_textual_feature_test1 <-predict(fit_textual_feature_train,textual_feature_test1,se.fit=TRUE)
MSE = mse(pred_textual_feature_test1$fit, textual_feature_test1$A263)




#Test data 2
pred_textual_feature_test2 <-predict(fit_textual_feature_train,textual_feature_test2,se.fit=TRUE)
MSE = mse(pred_textual_feature_test2$fit, textual_feature_test2$A263)



#Test data 3
pred_textual_feature_test3 <-predict(fit_textual_feature_train,textual_feature_test3,se.fit=TRUE)
MSE = mse(pred_textual_feature_test3$fit, textual_feature_test3$A263)


#Test data 4
pred_textual_feature_test4 <-predict(fit_textual_feature_train,textual_feature_test4,se.fit=TRUE)
MSE = mse(pred_textual_feature_test4$fit, textual_feature_test4$A263)


####### 2. Logistic Regression

# For Training data
textual_feature_mean <- mean(textual_feature$A263, NA.rm = TRUE)

textual_feature_log_n <- textual_feature
textual_feature_log_n$A263 <- sapply(textual_feature_log_n$A263, function(x) {ifelse(x >= textual_feature_mean, 1, 0)})
logfit_textual_features_train <- glm(formula = textual_feature_log_n$A263 ~ ., data = textual_feature_log_n, family=binomial())

summary(logfit_textual_features_train)

#Test data 1
textual_features_test1_mean <- mean(textual_features_test1$A263, NA.rm = TRUE)
textual_features_test1_n <- textual_features_test1
textual_features_test1_n$A263 <- sapply(textual_features_test1_n$A263, function(x) {ifelse(x >= textual_features_test1_mean, 1, 0)})
log_pred_textual_features_1 <-predict.glm(logfit_textual_features_train,textual_features_test1_n,se.fit=TRUE)
predicted_textual_log <- sapply(log_pred_textual_features_test1$fit, function(x){ifelse(x > .5, 1, 0)})


