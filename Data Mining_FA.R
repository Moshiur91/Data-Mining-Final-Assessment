bankdata = read.csv('bank-full.csv',sep=',',header = T)
summary(bankdata)

## Explorartory Data Analysis
sapply(bankdata, function(x) sum(is.na(x)))
sum(duplicated(bankdata))


hist(bankdata$age)
hist(bankdata$duration)
hist(bankdata$balance)
hist(bankdata$campaign)
hist(bankdata$previous)
hist(bankdata$pdays)
hist(bankdata$day)

library(ggplot2)
ggplot(data=bankdata, aes(x=marital, fill=marital)) + geom_bar(stat="Count")
ggplot(data=bankdata, aes(x=y, fill=y)) + geom_bar(stat="Count")
ggplot(data=bankdata, aes(x=education, fill=education)) + geom_bar(stat="Count")
ggplot(data=bankdata, aes(x=job, fill=job)) + geom_bar(stat="Count")
ggplot(data=bankdata, aes(x=poutcome, fill=poutcome)) + geom_bar(stat="Count")
ggplot(data=bankdata, aes(x=month, fill=month)) + geom_bar(stat="Count")

pairs(bankdata[,12:15])

ggplot(bankdata, aes(marital)) + geom_bar(fill="skyblue") +  facet_wrap(~job)
ggplot(bankdata, aes(y)) + geom_bar(fill="skyblue") +  facet_wrap(~job)
ggplot(bankdata, aes(y)) + geom_bar(fill="skyblue") +  facet_wrap(~marital)
ggplot(bankdata, aes(y)) + geom_bar(fill="skyblue") +  facet_wrap(~age)
ggplot(bankdata, aes(x = duration, y = duration, color = y)) + geom_boxplot() 
ggplot(bankdata, aes(x = '', y = age, color = y)) + geom_boxplot() 

## Decision Tree
library(rpart.plot)
library(rpart)
library(e1071)
library(caret)
set.seed(0)
intrain <- createDataPartition(y = bankdata$y, p= 0.70, list = FALSE)
training <- bankdata[intrain,]
testing <- bankdata[-intrain,]
dim(training) 
dim(testing)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(0)
dtree_fit <- train(y ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
print(dtree_fit)

## CART 
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
dt_pred <- predict(dtree_fit, newdata = testing)
conf_mat=confusionMatrix(dt_pred,as.factor(testing$y))
print(conf_mat)

## PreProcessing
bankdataP <- data.frame(bankdata$age,as.numeric(as.factor(bankdata$job)),as.numeric(as.factor(bankdata$marital)),as.numeric(as.factor(bankdata$education)),as.numeric(as.factor(bankdata$default)),bankdata$balance,as.numeric(as.factor(bankdata$housing)),as.numeric(as.factor(bankdata$loan)),as.numeric(as.factor(bankdata$contact)),bankdata$day,as.numeric(as.factor(bankdata$month)),bankdata$duration,bankdata$campaign,bankdata$pdays,bankdata$previous,as.numeric(as.factor(bankdata$poutcome)),as.numeric(as.factor(bankdata$y)))
colnames(bankdataP)=c("Age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")                                                             
##KNN
library(class)
library(e1071)
library(caret)
library(rpart.plot)
set.seed(0)

intrain <- createDataPartition(y = bankdataP$y, p= 0.70, list = FALSE)
training <- bankdataP[intrain,]
testing <- bankdataP[-intrain,]

knn_fit <- knn(train = training, test = testing,   cl = training$y, k = 5)
conf_mat=confusionMatrix(knn_fit,as.factor(testing$y))
print(conf_mat)

print(cm)
acc=array()
for (i in 1:20)
{
  knn_fit <- knn(train = training, test = testing,   cl = training$y, k = i)
  accuracy <- mean(knn_fit == testing$y)
  acc[i]=accuracy
}
plot(acc)
x=c(1:20)
y=acc
plot(acc)

##SVM
library(randomForest)
training$y <- as.factor(training$y)
rf_fit <-randomForest(y ~ .,  
                      data = training,  
                      importance = TRUE) 
rf_pred=predict(rf_fit,testing)
conf_mat=confusionMatrix(rf_pred,as.factor(testing$y))
print(conf_mat)

## adaBoost
library(ada)
ada_fit=ada(x = training[-17],y = training$y,iter=20, loss="logistic")
print(ada_fit)
ada_pred=predict(ada_fit,testing)
conf_mat=confusionMatrix(ada_pred,as.factor(testing$y))
print(conf_mat)
