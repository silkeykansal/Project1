library(caret)

#train set
train.set<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project1/BlogFeedback/blogData_train.csv',stringsAsFactors = F,header=FALSE, sep=",",)


#test set
test.feb.set1<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project1/BlogFeedback/blogData_test-2012.02.17.00_00.csv',stringsAsFactors = F,header=FALSE, sep=",",)
test.feb.set2<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project1/BlogFeedback/blogData_test-2012.02.08.00_00.csv',stringsAsFactors = F,header=FALSE, sep=",",)
test.mar.set1<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project1/BlogFeedback/blogData_test-2012.03.17.00_00.csv',stringsAsFactors = F,header=FALSE, sep=",",)
test.mar.set2<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project1/BlogFeedback/blogData_test-2012.03.08.00_00.csv',stringsAsFactors = F,header=FALSE, sep=",",)

#create bascif train and test
myvars <- paste("V", 51:60, sep="")
basic.train <- train.set[myvars]
basic.test.feb.set1 <- test.feb.set1[myvars]
basic.test.feb.set2 <- test.feb.set2[myvars]
basic.test.mar.set1 <- test.mar.set1[myvars]
basic.test.mar.set2 <- test.mar.set2[myvars]

#create textualf train
myvars <- paste("V", 63:262, sep="")
textual.train <- train.set[myvars]
textual.test.feb.set1 <- test.feb.set1[myvars]
textual.test.feb.set2 <- test.feb.set2[myvars]
textual.test.mar.set1 <- test.mar.set1[myvars]
textual.test.mar.set2 <- test.mar.set2[myvars]

#create output
y = train.set["V281"]

#########Linear regression#########
#Linear fit
basic.lm.fit <- lm(y$V281 ~ ., data=basic.train)
textual.lm.fit <- lm(y$V281 ~ ., data=textual.train)

#predict train
basic.lm.pred.train <- predict(basic.lm.fit, basic.train, se.fit = TRUE)
textual.lm.pred.train <- predict(textual.lm.fit, textual.train, se.fit = TRUE)

#Mean square error
basic.lm.mse <- mean ((basic.lm.pred.train$fit-y)^2)
textual.lm.mse <- mean ((textual.lm.pred.train$fit-y)^2)

#predict test
basic.lm.pred.feb.set1 <- predict(basic.lm.fit, basic.test.feb.set1, se.fit = TRUE)
basic.lm.pred.feb.set2 <- predict(basic.lm.fit, basic.test.feb.set2, se.fit = TRUE)
basic.lm.pred.mar.set1 <- predict(basic.lm.fit, basic.test.mar.set1, se.fit = TRUE)
basic.lm.pred.mar.set2 <- predict(basic.lm.fit, basic.test.mar.set2, se.fit = TRUE)

textual.lm.pred.feb.set1 <- predict(textual.lm.fit, textual.test.feb.set1, se.fit = TRUE)
textual.lm.pred.feb.set2 <- predict(textual.lm.fit, textual.test.feb.set2, se.fit = TRUE)
textual.lm.pred.mar.set1 <- predict(textual.lm.fit, textual.test.mar.set1, se.fit = TRUE)
textual.lm.pred.mar.set2 <- predict(textual.lm.fit, textual.test.mar.set2, se.fit = TRUE)

#Mean Square error for test
basic.lm.mse.feb.set1 <- mean ((basic.lm.pred.feb.set1$fit-test.feb.set1$V281)^2)
basic.lm.mse.feb.set2 <- mean ((basic.lm.pred.feb.set2$fit-test.feb.set2$V281)^2)
basic.lm.mse.mar.set1 <- mean ((basic.lm.pred.mar.set1$fit-test.mar.set1$V281)^2)
basic.lm.mse.mar.set2 <- mean ((basic.lm.pred.mar.set2$fit-test.mar.set2$V281)^2)

textual.lm.mse.feb.set1 <- mean ((textual.lm.pred.feb.set1$fit-test.feb.set1$V281)^2)
textual.lm.mse.feb.set2 <- mean ((textual.lm.pred.feb.set2$fit-test.feb.set2$V281)^2)
textual.lm.mse.mar.set1 <- mean ((textual.lm.pred.mar.set1$fit-test.mar.set1$V281)^2)
textual.lm.mse.mar.set2 <- mean ((textual.lm.pred.mar.set2$fit-test.mar.set2$V281)^2)

############Logistic Regression###########
#changing output to classified 
y.class = y
y.mean = mean(y$V281)
y.class$V281[y$V281 <= y.mean] = 0
y.class$V281[y$V281 > y.mean] = 1

#logiistic fit
basic.glm.fit <- glm(y.class$V281 ~ ., data=basic.train)
textual.glm.fit <- glm(y.class$V281 ~ ., data=textual.train)

#predict train
basic.glm.pred.train <- predict(basic.glm.fit, basic.train, se.fit = TRUE)
textual.glm.pred.train <- predict(textual.glm.fit, textual.train, se.fit = TRUE)

#update predicted value to call
basic.glm.pred.train$fit[basic.glm.pred.train$fit <= .5] = 0
basic.glm.pred.train$fit[basic.glm.pred.train$fit > .5] = 1
textual.glm.pred.train$fit[textual.glm.pred.train$fit <= .5] = 0
textual.glm.pred.train$fit[textual.glm.pred.train$fit > .5] = 1

basic.cm.train <- confusionMatrix(basic.glm.pred.train$fit, unlist(y.class))
textual.cm.train <- confusionMatrix(textual.glm.pred.train$fit, unlist(y.class))

#classify test data
test.feb.set1$V281[test.feb.set1$V281 <= y.mean] = 0
test.feb.set1$V281[test.feb.set1$V281 > y.mean] = 1
test.feb.set2$V281[test.feb.set2$V281 <= y.mean] = 0
test.feb.set2$V281[test.feb.set2$V281 > y.mean] = 1
test.mar.set1$V281[test.mar.set1$V281 <= y.mean] = 0
test.mar.set1$V281[test.mar.set1$V281 > y.mean] = 1
test.mar.set2$V281[test.mar.set2$V281 <= y.mean] = 0
test.mar.set2$V281[test.mar.set2$V281 > y.mean] = 1

#predict test data
basic.glm.pred.feb.set1 <- predict(basic.glm.fit, basic.test.feb.set1, se.fit = TRUE)
basic.glm.pred.feb.set2 <- predict(basic.glm.fit, basic.test.feb.set2, se.fit = TRUE)
basic.glm.pred.mar.set1 <- predict(basic.glm.fit, basic.test.mar.set1, se.fit = TRUE)
basic.glm.pred.mar.set2 <- predict(basic.glm.fit, basic.test.mar.set2, se.fit = TRUE)

textual.glm.pred.feb.set1 <- predict(textual.glm.fit, textual.test.feb.set1, se.fit = TRUE)
textual.glm.pred.feb.set2 <- predict(textual.glm.fit, textual.test.feb.set2, se.fit = TRUE)
textual.glm.pred.mar.set1 <- predict(textual.glm.fit, textual.test.mar.set1, se.fit = TRUE)
textual.glm.pred.mar.set2 <- predict(textual.glm.fit, textual.test.mar.set2, se.fit = TRUE)

#update predicted value to class
basic.glm.pred.feb.set1$fit[basic.glm.pred.feb.set1$fit <= .5] = 0
basic.glm.pred.feb.set1$fit[basic.glm.pred.feb.set1$fit > .5] = 1
basic.glm.pred.feb.set2$fit[basic.glm.pred.feb.set2$fit <= .5] = 0
basic.glm.pred.feb.set2$fit[basic.glm.pred.feb.set2$fit > .5] = 1
basic.glm.pred.mar.set1$fit[basic.glm.pred.mar.set1$fit <= .5] = 0
basic.glm.pred.mar.set1$fit[basic.glm.pred.mar.set1$fit > .5] = 1
basic.glm.pred.mar.set2$fit[basic.glm.pred.mar.set2$fit <= .5] = 0
basic.glm.pred.mar.set2$fit[basic.glm.pred.mar.set2$fit > .5] = 1

textual.glm.pred.feb.set1$fit[textual.glm.pred.feb.set1$fit <= .5] = 0
textual.glm.pred.feb.set1$fit[textual.glm.pred.feb.set1$fit > .5] = 1
textual.glm.pred.feb.set2$fit[textual.glm.pred.feb.set2$fit <= .5] = 0
textual.glm.pred.feb.set2$fit[textual.glm.pred.feb.set2$fit > .5] = 1
textual.glm.pred.mar.set1$fit[textual.glm.pred.mar.set1$fit <= .5] = 0
textual.glm.pred.mar.set1$fit[textual.glm.pred.mar.set1$fit > .5] = 1
textual.glm.pred.mar.set2$fit[textual.glm.pred.mar.set2$fit <= .5] = 0
textual.glm.pred.mar.set2$fit[textual.glm.pred.mar.set2$fit > .5] = 1



#conflusion matrix for test
basic.cm.test.feb.set1 <- confusionMatrix(basic.glm.pred.feb.set1$fit, test.feb.set1$V281)
basic.cm.test.feb.set2 <- confusionMatrix(basic.glm.pred.feb.set2$fit, test.feb.set2$V281)
basic.cm.test.mar.set1 <- confusionMatrix(basic.glm.pred.mar.set1$fit, test.mar.set1$V281)
basic.cm.test.mar.set2 <- confusionMatrix(basic.glm.pred.mar.set2$fit, test.mar.set2$V281)

textual.cm.test.feb.set1 <- confusionMatrix(textual.glm.pred.feb.set1$fit, test.feb.set1$V281)
textual.cm.test.feb.set2 <- confusionMatrix(textual.glm.pred.feb.set2$fit, test.feb.set2$V281)
textual.cm.test.mar.set1 <- confusionMatrix(textual.glm.pred.mar.set1$fit, test.mar.set1$V281)
textual.cm.test.mar.set2 <- confusionMatrix(textual.glm.pred.mar.set2$fit, test.mar.set2$V281)

