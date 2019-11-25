titanic_train<-read.csv("C:/Users/Somenath Banerjee/Desktop/projects/Titanic/train.csv")
titanic_test<-read.csv("C:/Users/Somenath Banerjee/Desktop/projects/Titanic/test.csv")
str(titanic_train)
View(titanic_train)
dim(titanic_train)
dim(titanic_test)
summary(titanic_train)
sapply(data, function(df)
  {
  sum(is.na(df))
})
sapply(titanic_train, function(df)
{
  sum(is.na(df))
})

titanic_test$Survived<-NA
data<-rbind(titanic_train,titanic_test)
library(dplyr)
glimpse(data)
library("Amelia")
missmap(data,main = 'Mapping missing data')
#continuous variable : impute using mean value
table(data$Embarked, useNA="always")
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm = TRUE)
sum(is.na(data$Age))
data$Fare[is.na(data$Fare)]<-mean(data$Fare,na.rm = TRUE)
sum(is.na(data$Fare))
#categorical variable : impute using mode
data$Embarked[is.na(data$Embarked)]<-'S'
sum(is.na(data$Embarked))
data$Survived<-as.factor(data$Survived)
table(data$Survived)
data$Survived[is.na(data$Survived)]<-0

#Data Partition
library(caret)
index<-createDataPartition(data$Survived, p=0.75, list = FALSE)
train<-data[index,]
test<-data[-index,]

#Univariate analysis
#1.Categorical
library(ggplot2)
xtabs(~Survived,train)
ggplot(train)+geom_bar(aes(x=Survived))
ggplot(train)+geom_bar(aes(x=Sex))
ggplot(train)+geom_bar(aes(x=Pclass))
#2.Continuous
ggplot(train)+geom_histogram(aes(x=Age), fill="yellow", color='red')
ggplot(train)+geom_boxplot(aes(x=factor(0), y=Fare))+coord_flip()

ggplot(train)+geom_histogram(aes(x=Pclass), fill="green", color='maroon')
ggplot(train)+geom_boxplot(aes(x=factor(0), y=Age))+coord_flip()

#Bivariate analysis
#Categorical-categorical
xtabs(~Survived+Sex,train)
ggplot(train)+geom_bar(aes(x=Sex,fill=factor(Survived)))
ggplot(train)+geom_bar(aes(x=Pclass,fill=factor(Survived)))
ggplot(train)+geom_bar(aes(x=Embarked,fill=factor(Survived)))
#Categorical-Continuous
ggplot(train)+geom_boxplot(aes(factor(Survived),Age))
ggplot(train)+geom_boxplot(aes(factor(Survived),Fare))

#Multivariate Analysis
xtabs(~Survived+Sex+Pclass,train)
ggplot(train)+geom_bar(aes(Sex,fill=factor(Survived)))+facet_grid(Pclass~.)
xtabs(~Survived+Embarked+Sex,train)
ggplot(train)+geom_bar(aes(Sex,fill=factor(Survived)))+facet_grid(Embarked~.)

#Feature Engineering
#Creating a feature called "Child" and convert into factor
data$Child<-NA
data$Child[data$Age<18]<-1
data$Child[data$Age>=18]<-0
data$Child<-as.factor(data$Child)
#Cretaing a fetaure called Title and convert it into factor
data$Name<-as.character(data$Name)
data$Title<-sapply(data$Name,FUN = function(x){
  strsplit(x,split='[,.]')[[1]][2]
})
data$Title<-sub(' ','',data$Title)
barplot(table(data$Title))
data$Title<-as.factor(data$Title)

#Combine small title groups
data$Title[data$Title %in% c("Don","Major","Capt","Sir")]<-"Sir"
data$Title[data$Title %in% c("Mlle","Mme")]<-"Mlle"

#Building a random forest model
library(caret)
library(randomForest)
set.seed(999)
#rf.fit2<-randomForest(as.factor(Survived)~.,data=train,importance=TRUE,ntree=200)
#rf.fit2<-randomForest(train[,-1],train$Survived,importance=TRUE,ntree=500,keep.Forest=FALSE)
#varImpPlot(rf.fit2)
control<-trainControl(method = "repeatedcv", repeats = 3,number = 10)
glimpse(train)
train<-train[,-c(1,4,9,11,12)]
rf.fit<-train(Survived~.,data = train, method="rf",trControl=control,preProc=c("center","scale"))
varImp(rf.fit2)
#Discarding Cabin column
train<-train[,-8]
#Keeping significant variables
train<-train[,-8]
rf.fit<-train(Survived~.,data = train, method="rf",trControl=control,preProc=c("center","scale"))
#Removing insignificant columns from test data
test<-test[,-c(1,4,9,11,12)]
pred<-predict(rf.fit,test)
confusionMatrix(pred,test$Survived)
#Tuning model parameter
grid<-expand.grid(.mtry=sqrt(ncol(train)))
rf.fit<-train(Survived~.,data = train, method="rf",trControl=control,preProc=c("center","scale","BoxCox"),tuneGrid=grid)
rf.pred<-predict(rf.fit2,test)
confusionMatrix(test$Survived,rf.pred)
#Finally came up with a model of 77.9% accuracy

