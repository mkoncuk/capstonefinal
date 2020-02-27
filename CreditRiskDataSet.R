options(scipen=999)
library(readxl)
library(tidyverse)
library(reshape2)
CreditRisk <- read_excel("D:/İTÜ SEM/Dataset/Müşteri Analitiği/CreditRiskDataSet.xlsx")
melted.df <- melt(na.omit(CreditRisk[,c(1,6,7,9,10,11,12)]),id.vars = "Credit Risk")
df <- data.frame(CreditRisk[1:425,])
str(df)
df$Gender <- as.factor(df$Gender)
df$Marital.Status  <- as.factor(df$Marital.Status)
df$Loan.Purpose <- as.factor(df$Loan.Purpose)

df$Credit.Risk <- as.factor(df$Credit.Risk)
df$Housing <- as.factor(df$Housing)
df$Job <- as.factor(df$Job)
summary(df)
library(Amelia)
missmap(df)
df <- na.omit(df)
library(funModeling)
library(dplyr)
library(Hmisc)
library(caret)

glimpse(df)
profiling_num(df)
a <- (describe(df))

plot_num(df)
dev.off()
par(mfrow=c(3,2))
plot(Credit.Risk ~., data =df, col=c("green","red"))
par(mfrow=c(1,1))
freq(df[,-12])

sutunlar <- c(2,3,4,5,8)
sutunlar2 <- c(1,6,7,9,10,11)
par(mfrow=c(2,3))
for (n in 1:5) {
  
  boxplot(df[,sutunlar[n]] ~ df$Credit.Risk , 
          col= c("green","red"),
          ylab=colnames(df)[sutunlar[n]] , xlab="Credit Risk")
  
}


for (n in 1:6) {

  p <- ggplot(df, aes(y=df[,sutunlar2[n]], x=df[,sutunlar2[n]], color=Credit.Risk, fill=Credit.Risk)) + 
     geom_bar( stat="identity")+
    labs(x=colnames(df)[sutunlar2[n]],y="Amount") +facet_grid(facets = Credit.Risk ~ .)
  
  print(p)
  readline("Tuşa bas...")
}

par(mfrow=c(1,1))






#Splitting test and train data
library(caTools)
set.seed(123)
split = sample.split(df$Credit.Risk, SplitRatio = 0.75)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Fitting K-NN to the Training set and Predicting the Test set results


library(class)
y_pred = knn(train =  training_set[, -12],
             test = test_set[, -12],
             cl = training_set[, 12],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm  = table(test_set[, 12], y_pred)
accuracy_rate_knn <- sum(diag(cm))/sum(cm)

con_matrix <- confusionMatrix(y_pred, test_set[, 12])
attributes(con_matrix)
con_matrix$overall
Result.Control <- data.frame(knn =con_matrix$overall)

# Decision Tree Model For Classifications
library(party)
classifier = ctree(formula = Credit.Risk ~ 
                     .,
                   data = training_set)

y_pred = predict(classifier, newdata = test_set[-12])
y_pred

cm= table(test_set[, 12], y_pred)

plot(classifier)

accuracy_rate_Tree<- sum(diag(cm))/sum(cm)
Result.Control$DecisionTree <- confusionMatrix(y_pred, test_set[, 12])$overall




#SVM ile sınıflama
library(e1071)
classifier = svm(formula = Credit.Risk~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-12])

# Making the Confusion Matrix
cm = table(test_set[, 12], y_pred)
accuracy_rate_SVM<-sum(diag(cm))/sum(cm)

confusionMatrix(y_pred, test_set[, 12])
Result.Control$SVM <- confusionMatrix(y_pred, test_set[, 12])$overall



# Logistic Regression to the Training set
classifier = glm(formula = Credit.Risk ~ .,
                 family = "binomial",
                 data = training_set)
 
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-12])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 12], y_pred > 0.5)


accuracy_rate_Log.Reg<- (sum(diag(cm)))/sum(cm)
confusionMatrix(as.factor(y_pred), test_set[, 12])
Result.Control$LogisticReg <- confusionMatrix(as.factor(y_pred), test_set[, 12])$overall

## ROC Egrisi
library(pROC)
testy <- as.numeric(as.character(test_set[,12]))
y_pred <- as.numeric(y_pred)
a <- roc(testy ~ y_pred, plot = TRUE, print.auc = TRUE)
 



# Fitting Navie Bayes to the Training set
library(e1071)
classifier = naiveBayes(x = training_set[-12],
                        y = training_set$Credit.Risk)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-12])

# Making the Confusion Matrix
cm = table(test_set[, 12], y_pred)
accuracy_rate_Navie<-sum(diag(cm))/sum(cm)

confusionMatrix(y_pred, test_set[, 12])
Result.Control$NavieBayes <- confusionMatrix(y_pred, test_set[, 12])$overall


# Fitting Random Forest Classification to the Training set
library( randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-12],
                          y = training_set$Credit.Risk,
                          ntree =1000)
 plot(classifier)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-12])

# Making the Confusion Matrix
cm = table(test_set[, 12], y_pred)
accuracy_rate_RandomForest <- sum(diag(cm))/sum(cm)
confusionMatrix(y_pred, test_set[, 12])
Result.Control$RandomForest <- confusionMatrix(y_pred, test_set[, 12])$overall




#Yapay Sinir Ağları

olcekleme <- function(x) {
  (x - min(x))/(max(x) - min(x))
}


training_set.scaled <-data.frame(apply(training_set,2,function(x) as.numeric(as.character(x))))
test_set.scaled <- data.frame(apply(test_set,2,function(x) as.numeric(as.character(x))))
for (n in 1:11){
  training_set.scaled[,n] <- olcekleme(training_set.scaled[,n]) 
  test_set.scaled[,n] <- olcekleme(test_set.scaled[,n])
}

training_set.scaled$Credit.Risk <- as.factor(training_set.scaled$Credit.Risk)
test_set.scaled$Credit.Risk <- as.factor(test_set.scaled$Credit.Risk)

#Neural Network Model
library(nnet)
set.seed(123)
nnet_fit <- nnet(Credit.Risk ~., training_set.scaled, size = 3, decay = 0.1)


pred <- predict(nnet_fit, test_set.scaled[,-12], type = "class")
pred 

confusionMatrix(factor(pred), test_set.scaled[,12], positive = "0")
Result.Control$nnet <-confusionMatrix(as.factor(pred), test_set.scaled[,12], positive = "0")$overall



#XGBOOST Model
set.seed(123)

library(xgboost)
train_y <- as.numeric(as.character(training_set[,12]))
train_x <- data.frame(apply(training_set[,-12],2,function(x) as.numeric(as.character(x))))
test_x <- data.frame(apply(test_set[,-12],2,function(x) as.numeric(as.character(x))))
test_y <- as.numeric(as.character(test_set[,12]))
 
dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)


xgb_fit <- xgboost(data = dtrain, 
                   max.depth = 2,
                   eta = 1,
                   ntread = 2,
                   nrounds = 5,
                   objective = "binary:logistic",
                   verbose = 1)


pred <- predict(xgb_fit, as.matrix(test_x),type = "class")
pred <- sapply(pred,function(x) ifelse(x>0.5,1,0))

confusionMatrix(factor(pred), as.factor(test_y), positive = "0")
Result.Control$XGBoost <-confusionMatrix(as.factor(pred), as.factor(test_y), positive = "0")$overall



#Gradient Boosting Machines Model Fit
set.seed(123)
library(gbm)
train <- cbind(train_x,Credit.Risk =train_y)
 
train_ <- transform(train, Credit.Risk=Credit.Risk)

gbm_fit <- gbm(Credit.Risk~., data = train, 
               shrinkage = 0.01,
               distribution = "bernoulli",
               cv.folds = 5,
               n.trees = 3000,
               verbose = F)

summary(gbm_fit)

gbm.perf(gbm_fit, method = "cv")
plot.gbm(gbm_fit,  2, gbm.perf(gbm_fit, method = "cv"))

pred <- predict.gbm(gbm_fit, test_x, type = "response")
pred <- sapply(pred,function(x) ifelse(x>0.5,1,0))

confusionMatrix(factor(pred), as.factor(test_y), positive = "0")
Result.Control$gbm <-confusionMatrix(as.factor(pred), as.factor(test_y), positive = "0")$overall


#Linear Discriminat Analysis
library(MASS)
fit.lda <- lda(Credit.Risk~Checking+Savings+Months.Customer+
                 Months.Employed+Age, data =training_set)
y_pred <- predict(fit.lda,test_set[,-12]) 

confusionMatrix(y_pred$class, test_set[,12], positive = "0")
Result.Control$LDA <-confusionMatrix(y_pred$class, test_set[,12], positive = "0")$overall

Result.Control <- Result.Control[c(1,2,6),]


#Tuning Model NNET
ctrl <- trainControl(method="cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


nnetGrid <- expand.grid(size = 1:10,
                        decay = c(0, 0.1, 1, 2))

maxSize <- max(nnetGrid$size)

numWts <- 1*(maxSize * (length(training_set.scaled[,-12]) + 1) + maxSize + 1)

training_set.scaled$Credit.Risk <- as.factor(training_set.scaled$Credit.Risk)
levels(training_set.scaled$Credit.Risk) <- c("X1", "X2") 

nnet_tune <- train(
  training_set.scaled[,-12], training_set.scaled[,12],
  method = "nnet",
  metric = "ROC",
  tuneGrid = nnetGrid,
  trace = FALSE, 
  maxit = 2000,
  MaxNWts = numWts,
  trControl = ctrl
  
)

nnet_tune$bestTune


plot(nnet_tune)
pred <- predict(nnet_tune, test_set.scaled[,-12])
pred <- ifelse(pred == "X1", 0,1)

confusionMatrix(factor(pred), test_set.scaled[,12], positive = "0")







#Tuning Random Forest

#RANDOM SEARCH
control <- trainControl(method='repeatedcv', 
                        number = 10,
                        search = 'random')

#tunelenght ile 15 tane mtry degeri rastgele uretilecek 
set.seed(123)
rf_random <- train(Credit.Risk ~ .,
                   data = training_set,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)

plot(rf_random)
confusionMatrix(predict(rf_random, test_set[,-12]), test_set[,12], positive = "0")


#GRID SEARCH
control <- trainControl(method='cv', 
                        number=10, 
                        search='grid')

tunegrid <- expand.grid(mtry = (1:20)) 

rf_gridsearch <- train(Credit.Risk ~ ., 
                       data = training_set,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid)

plot(rf_gridsearch)

confusionMatrix(predict(rf_gridsearch, test_set[,-12]), test_set[,12], positive = "0")
rf_gridsearch$bestTune


#
