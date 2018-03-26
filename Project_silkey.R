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
head(fit_basic_feature_train, row=1)

summary(fit_basic_feature_train)
##Predicting value for Training model

pred_basic_feature <-predict(fit_basic_feature_train,basic_feature,se.fit=TRUE)
MSE = mse(pred_basic_feature$fit, basic_feature$A281)
print (MSE)

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
basic_feature_mean <- mean(basic_feature$A281, na.rm = TRUE)
basic_feature_mean
basic_feature_logit <- basic_feature

basic_feature_logit$A281 <- sapply(basic_feature_logit$A281, function(x) {ifelse(x >= basic_feature_mean, 1, 0)})


head(basic_feature_logit)
logit_basic_feature_train <- glm(formula = A281 ~ A51+A52+A53+A54+A55+A56+A57+A58+A59+A60, data = basic_feature_logit, family=binomial())
summary(logit_basic_feature_train)


##Predicting values for training data and test data
#Training data
pred_basic_feature_logit <-predict(logit_basic_feature_train,newdata= basic_feature_logit,se.fit=TRUE)
#pred_basic_feature_logit
MSE = mse(pred_basic_feature_logit$fit, basic_feature_logit$A281)
print (MSE)

# For Testdata1


basic_feature_mean_test1 <- mean(basic_feature_test1$A281, na.rm = TRUE)
basic_feature_mean_test1
basic_feature_logit_test1 <- basic_feature_test1

basic_feature_logit_test1$A281 <- sapply(basic_feature_logit_test1$A281, function(x) {ifelse(x >= basic_feature_mean_test1, 1, 0)})


pred_basic_feature_logit_test1 <-predict(logit_basic_feature_train,newdata= basic_feature_logit_test1,se.fit=TRUE)
MSE = mse(pred_basic_feature_logit_test1$fit, basic_feature_logit_test1$A281)
print (MSE)


# For Testdata2

basic_feature_mean_test2 <- mean(basic_feature_test2$A281, na.rm = TRUE)
basic_feature_mean_test2
basic_feature_logit_test2 <- basic_feature_test2

basic_feature_logit_test2$A281 <- sapply(basic_feature_logit_test2$A281, function(x) {ifelse(x >= basic_feature_mean_test2, 1, 0)})


pred_basic_feature_logit_test2 <-predict(logit_basic_feature_train,newdata= basic_feature_logit_test2,se.fit=TRUE)
MSE = mse(pred_basic_feature_logit_test2$fit, basic_feature_logit_test2$A281)
print (MSE)


#For Testdata3

basic_feature_mean_test3 <- mean(basic_feature_test3$A281, na.rm = TRUE)
basic_feature_mean_test3
basic_feature_logit_test3 <- basic_feature_test3

basic_feature_logit_test3$A281 <- sapply(basic_feature_logit_test3$A281, function(x) {ifelse(x >= basic_feature_mean_test3, 1, 0)})


pred_basic_feature_logit_test3 <-predict(logit_basic_feature_train,newdata= basic_feature_logit_test3,se.fit=TRUE)
MSE = mse(pred_basic_feature_logit_test3$fit, basic_feature_logit_test3$A281)
print (MSE)


#For Testdata 4

basic_feature_mean_test4 <- mean(basic_feature_test4$A281, na.rm = TRUE)
basic_feature_mean_test4
basic_feature_logit_test4 <- basic_feature_test4

basic_feature_logit_test4$A281 <- sapply(basic_feature_logit_test4$A281, function(x) {ifelse(x >= basic_feature_mean_test4, 1, 0)})


pred_basic_feature_logit_test4 <-predict(logit_basic_feature_train,newdata= basic_feature_logit_test4,se.fit=TRUE)
MSE = mse(pred_basic_feature_logit_test4$fit, basic_feature_logit_test4$A281)
print (MSE)

############Experiment 2 : Textual features


######1. Linear regression
#Training a model 

fit_textual_feature_train <- lm(textual_feature$A263 ~ A63:A262, data = textual_feature)

summary(fit_textual_feature_train)

##Predicting values for Training data and 4 tests
#Train data prediction
pred_textual_feature <-predict(fit_textual_feature_train,textual_feature,se.fit=TRUE)
MSE = mse(pred_textual_feature$fit, textual_feature$A263)
print(MSE)

#Test data 1

pred_textual_feature_test1 <-predict(fit_textual_feature_train,textual_feature_test1,se.fit=TRUE)
MSE = mse(pred_textual_feature_test1$fit, textual_feature_test1$A263)
print(MSE)


#Test data 2
pred_textual_feature_test2 <-predict(fit_textual_feature_train,textual_feature_test2,se.fit=TRUE)
MSE = mse(pred_textual_feature_test2$fit, textual_feature_test2$A263)
print(MSE)


#Test data 3
pred_textual_feature_test3 <-predict(fit_textual_feature_train,textual_feature_test3,se.fit=TRUE)
MSE = mse(pred_textual_feature_test3$fit, textual_feature_test3$A263)
print(MSE)


#Test data 4
pred_textual_feature_test4 <-predict(fit_textual_feature_train,textual_feature_test4,se.fit=TRUE)
MSE = mse(pred_textual_feature_test4$fit, textual_feature_test4$A263)
print(MSE)


####### 2. Logistic Regression

# For Training data

textual_feature_mean <- mean(textual_feature$A263, na.rm = TRUE)
textual_feature_logit <- textual_feature
textual_feature_logit$A263 <- sapply(textual_feature_logit$A263, function(x) {ifelse(x >= textual_feature_mean, 1, 0)})
logit_textual_feature_train <- glm(formula = textual_feature_logit$A263 ~ A63:A262, data = textual_feature_logit, family=binomial())

summary(logit_textual_feature_train)


##Predicting values for training data and test data
#Training data
pred_textual_feature_logit <-predict(logit_textual_feature_train,newdata= textual_feature_logit,se.fit=TRUE)
#pred_textual_feature_logit
MSE = mse(pred_textual_feature_logit$fit, textual_feature_logit$A263)
print (MSE)

# For Testdata1


textual_feature_mean_test1 <- mean(textual_feature_test1$A263, na.rm = TRUE)
textual_feature_mean_test1
textual_feature_logit_test1 <- textual_feature_test1

textual_feature_logit_test1$A263 <- sapply(textual_feature_logit_test1$A263, function(x) {ifelse(x >= textual_feature_mean_test1, 1, 0)})


pred_textual_feature_logit_test1 <-predict(logit_textual_feature_train,newdata= textual_feature_logit_test1,se.fit=TRUE)
MSE = mse(pred_textual_feature_logit_test1$fit, textual_feature_logit_test1$A263)
print (MSE)


# For Testdata2

textual_feature_mean_test2 <- mean(textual_feature_test2$A263, na.rm = TRUE)
textual_feature_mean_test2
textual_feature_logit_test2 <- textual_feature_test2

textual_feature_logit_test2$A263 <- sapply(textual_feature_logit_test2$A263, function(x) {ifelse(x >= textual_feature_mean_test2, 1, 0)})


pred_textual_feature_logit_test2 <-predict(logit_textual_feature_train,newdata= textual_feature_logit_test2,se.fit=TRUE)
MSE = mse(pred_textual_feature_logit_test2$fit, textual_feature_logit_test2$A263)
print (MSE)


#For Testdata3

textual_feature_mean_test3 <- mean(textual_feature_test3$A263, na.rm = TRUE)
textual_feature_mean_test3
textual_feature_logit_test3 <- textual_feature_test3

textual_feature_logit_test3$A263 <- sapply(textual_feature_logit_test3$A263, function(x) {ifelse(x >= textual_feature_mean_test3, 1, 0)})


pred_textual_feature_logit_test3 <-predict(logit_textual_feature_train,newdata= textual_feature_logit_test3,se.fit=TRUE)
MSE = mse(pred_textual_feature_logit_test3$fit, textual_feature_logit_test3$A263)
print (MSE)


#For Testdata 4

textual_feature_mean_test4 <- mean(textual_feature_test4$A263, na.rm = TRUE)
textual_feature_mean_test4
textual_feature_logit_test4 <- textual_feature_test4

textual_feature_logit_test4$A263 <- sapply(textual_feature_logit_test4$A263, function(x) {ifelse(x >= textual_feature_mean_test4, 1, 0)})


pred_textual_feature_logit_test4 <-predict(logit_textual_feature_train,newdata= textual_feature_logit_test4,se.fit=TRUE)
MSE = mse(pred_textual_feature_logit_test4$fit, textual_feature_logit_test4$A263)
print (MSE)