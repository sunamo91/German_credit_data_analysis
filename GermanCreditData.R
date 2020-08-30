library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(psych)

#Exploratory Data analysis
setwd("File_location")
GermanCreditData<-read.csv("GermanCredit_data.csv",header = TRUE,sep=",")
GermanData<-GermanCreditData[,-c(1,5:10,18)]
#GermanData$PERSONAL_STATUS<-apply(GermanData[,c(8)], 1, function(x){replace(x, is.na(x), 1)})
#GermanData$AGE<-apply(GermanData[,c(13)], 1, function(x){replace(x, is.na(x), 35)})
GermanData$AGE[is.na(GermanData$AGE)]<-mean(GermanData$AGE,na.rm = T)
GermanData$PERSONAL_STATUS[is.na(GermanData$PERSONAL_STATUS)]<-1

View(GermanCreditData)
View(GermanData)
str(GermanData)





########################################################

#Modelling

set.seed(123)
TRG_PCT<-0.5
nr<-nrow(GermanData)
trnIndex50 <- sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50%sample of row-indices
GermanTrain5050<-GermanData[trnIndex50,]   #training data with the randomly selected row-indices
GermanTest5050 <- GermanData[-trnIndex50,]  #test data with the other row-indices

#develop a tree on the training data
GermanSplit5050<-rpart(RESPONSE ~ ., data=GermanTrain5050, method="class")

#plot the tree
library(rpart.plot)
rpart.plot::prp(GermanSplit5050, type=2, extra=1)


#Obtain the model's predictions on the training data
predTrain5050<-predict(GermanSplit5050, GermanTrain5050, type='class')
#Confusion table
ConfusionMatrix=table(prediction = predTrain5050, true=GermanTrain5050$RESPONSE)
ConfusionMatrix
#Accuracy
mean(predTrain5050==GermanTrain5050$RESPONSE) #0.81


#Obtain the model's predictions on the test data
predTest5050<-predict(GermanSplit5050, GermanTest5050, type='class')
#Confusion table
table(prediction = predTest5050, true=GermanTest5050$RESPONSE)
#Accuracy
mean(predTest5050==GermanTest5050$RESPONSE) #0.736

#Performance Measures

#Number of instances
N = sum(ConfusionMatrix) 
#True Positives
TP=ConfusionMatrix[2,2]
#True Negatives
TN=ConfusionMatrix[1,1]
#False Positives
FP=ConfusionMatrix[2,1]
#False Negatives
FN=ConfusionMatrix[1,2]
#Precision
precision = TP/(TP+FP)
#Sensitivity or Recall
sensitivity = TP/(TP+FN)
#Specifity
specificity= TN/(TN+FP)
#Prevalence
prevalence= (TP+FN)/(TP+TN+FP+FN)
#False Negative Rate (1-sensitivity)
FNR= FN/(TP+FN)
#False Positive Rate (1-specificity)
FPR= FP/(FP+TN)

PerfMeasure<-data.frame(precision,sensitivity,specificity,prevalence,FNR,FPR)
str(PerfMeasure) #'data.frame':	1 obs. of  6 variables:
#$ precision  : num 0.835
#$ sensitivity: num 0.909
#$ specificity: num 0.577
#$ prevalence : num 0.702
#$ FNR        : num 0.0912
#$ FPR        : num 0.423

#Lift Curve
#get the 'scores' from applying the model to the data
predTrainProb5050=predict(GermanSplit5050, GermanTrain5050, type='prob')
head(predTrainProb5050)
#0         1
#288 0.7333333 0.2666667
#788 0.1399177 0.8600823
#409 0.1399177 0.8600823
#881 0.1399177 0.8600823
#937 0.1399177 0.8600823
#46  0.1399177 0.8600823

#we need the score and actual class (OUTCOME) values
TrainSc5050 <- subset(GermanTrain5050, select=c("RESPONSE"))  # selects the RESPONSE column into TrainSc5050
TrainSc5050["score"]<-predTrainProb5050[, 1]  #add a column named 'score' with prob(default) values in the first column of predTrainProb5050
#sort by score
TrainSc5050<-TrainSc5050[order(TrainSc5050$score, decreasing=TRUE),]
str(TrainSc5050) #'data.frame':	500 obs. of  2 variables:
#$ RESPONSE: int  1 0 0 0 0 0 0 0 0 0 ...
#$ score   : num  0.875 0.875 0.875 0.875 0.875 ...

levels(TrainSc5050$RESPONSE)
levels(TrainSc5050$RESPONSE)[1]<-1
levels(TrainSc5050$RESPONSE)[2]<-0
# this has not changed OUTCOME -- it will now have factor levels '1' and '0'
TrainSc5050$RESPONSE<-as.numeric(as.character(TrainSc5050$RESPONSE))
str(TrainSc5050) #'data.frame':	500 obs. of  2 variables:
#$ RESPONSE: num  1 0 0 0 0 0 0 0 0 0 ...
#$ score   : num  0.875 0.875 0.875 0.875 0.875 ...

#obtain the cumulative sum of default cases captured
TrainSc5050$cumDefault<-cumsum(TrainSc5050$RESPONSE)
head(TrainSc5050)
# RESPONSE score cumDefault
#462        1 0.875          1
#121        0 0.875          1
#290        0 0.875          1
#546        0 0.875          1
#776        0 0.875          1
#810        0 0.875          1

#Plot the cumDefault values (y-axis) by numCases (x-axis)
plot(seq(nrow(TrainSc5050)), TrainSc5050$cumDefault,type = "l", xlab='#cases', ylab='#default')

#ROCR

library('ROCR')

predTestProb5050=predict(GermanSplit5050,GermanTest5050, type="prob")

testScore<-predTestProb5050[,2]

head(predTestProb5050)
# head(predTestProb5050)
#0         1
#2  0.7333333 0.2666667
#3  0.1399177 0.8600823
#4  0.7333333 0.2666667
#8  0.7333333 0.2666667
#10 0.2500000 0.7500000
#11 0.6904762 0.3095238

head(testScore)
# 2         3         4         8        10        11 
#0.2666667 0.8600823 0.2666667 0.2666667 0.7500000 0.3095238 

#prediction function from ROCR to get a prediction object

rocPredTest5050 = prediction(testScore,GermanTest5050$RESPONSE,label.ordering = c(0,1))

#performance using the function from ROCR, then plot

rocPerfTest5050 = performance(rocPredTest5050,"tpr", "fpr")
plot(rocPerfTest5050)

#AUC value
aucPerf5050 = performance(rocPredTest5050, measure = "auc")
aucPerf5050@y.values #0.6978501


#Pruning

GermanPrePrune = rpart(RESPONSE ~ ., data=GermanTrain5050, 
                                     method="class", control = rpart.control(cp = 0.0))

printcp(GermanPrePrune)
plotcp(GermanPrePrune)

cp =GermanPrePrune$cptable[which.min(GermanPrePrune$cptable[,"xerror"]),"CP"]
print(cp) #cp=0.02908277

GermanDataPruned=prune(GermanPrePrune,cp=0.02908277)

printcp(GermanDataPruned)
plotcp(GermanDataPruned)

rpart.plot::prp(GermanDataPruned, type=2, extra = 1)

#Obtain model's prediction on Training data
predTrainPrune5050=predict(GermanDataPruned, GermanTrain5050, type='class')
#Confusion table
ConfusionMatrix1=table(prediction = predTrainPrune5050, true=GermanTrain5050$RESPONSE)
#Accuracy
mean(predTrainPrune5050==GermanTrain5050$RESPONSE)#0.79

#Prediction on Test data
predTestPrune5050=predict(GermanDataPruned,GermanTest5050, type = 'class')
#Confusion table
table(prediction = predTestPrune5050, true=GermanTest5050$RESPONSE)
#Accuracy
mean(predTestPrune5050==GermanTest5050$RESPONSE)#0.75
 #######

#3.d

#70-30 split

set.seed(121)
trnIndex70<-sample(1:nrow(GermanData),size = round(0.70*nrow(GermanData)))
GermanTrain7030<-GermanData[trnIndex70,]
GermanTest7030<-GermanData[-trnIndex70,]

dim(GermanTrain7030) #700 23

#Developing a Tree on Training data

GermanSplit7030<-rpart(RESPONSE ~ ., data=GermanTrain7030,method='class')
rpart.plot::prp(GermanSplit7030, type=2, extra = 1)
text(GermanSplit7030)
#Prediction of model on training data
predTrain7030=predict(GermanSplit7030, GermanTrain7030, type='class')
#Confusion table
table(prediction = predTrain7030, true=GermanTrain7030$RESPONSE)
#          true
#prediction   0   1
#         0 109  28
#         1  99 464

#Accuracy
mean(predTrain7030==GermanTrain7030$RESPONSE) #0.8185714

#Model's prediction on Test data
predTest7030=predict(GermanSplit7030 ,GermanTest7030 , type = 'class')
#Confusion table
table(prediction = predTest7030, true=GermanTest7030$RESPONSE)
#Accuracy
mean(predTest7030==GermanTest7030$RESPONSE) #0.7366667

########
#Lets consider at 80:20 split for training and test data

set.seed(153)
trnIndex80<-sample(1:nrow(GermanData),size = round(0.80*nrow(GermanData)))
GermanTrain8020<-GermanData[trnIndex80,]
GermanTest8020<-GermanData[-trnIndex80,]

dim(GermanTest8020) #200 23

#Developing a Tree on Training data

GermanSplit8020<-rpart(RESPONSE ~ ., data=GermanTrain8020,method='class')
rpart.plot::prp(GermanSplit8020, type=2, extra = 1)

#Model's prediction on Training data
predTrain8020=predict(GermanSplit8020, GermanTrain8020, type='class')
#Confusion table
table(prediction = predTrain8020, true=GermanTrain8020$RESPONSE)
#          true
#prediction   0   1
#         0 106  30
#         1 135 529


#Accuracy
mean(predTrain8020==GermanTrain8020$RESPONSE) #0.79375

#Model's prediction on Test data
predTest8020=predict(GermanSplit8020 ,GermanTest8020 , type = 'class')
#Confusion table
table(prediction = predTest8020, true=GermanTest8020$RESPONSE)
#Accuracy
mean(predTest8020==GermanTest8020$RESPONSE) #0.725

#6
#PROFITS AND CUMULATIVE PROFITS 
scoreTest=predict(GermanSplit8020,GermanTest8020, type="prob")[,'1'] 
prLifts=data.frame(scoreTest)
prLifts=cbind(prLifts, GermanTest8020$RESPONSE)
head(prLifts)

#Sort by decreasing order of score
prLifts=prLifts[order(-scoreTest) ,]  

#Add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanTest8020$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)
#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Index = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Index]
print(c(maxProfit = maxProfit, scoreTest = maxProfit_score))
View(GermanTest8020)
View(prLifts)
View(scoreTest)








