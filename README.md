# German_credit_data_analysis
DECISION TREE ANALYSIS - GERMAN CREDIT DATA

1.	 Explore the data: What is the proportion of “Good” to “Bad” cases? 
The proportion of Good vs Bad Case is 70:30 from the Response Proportion Barplot below. 
 t<-table(data$RESPONSE)
> ptab<-prop.table(t)
> ptab<-ptab*100
> barplot(ptab, main = "Proportion of Good vs Bad Cases", xlab = "RESPONSE", ylab = "PROPORTION",col=c("orange", "steelblue"), names.arg = c("Bad Case", "Good Case"))
> box(lwd=2)
 

2.	Missing Values
str(data)
summary(data$X)

1)	766 NAs in NEW_CAR 
apply(data[,c(5)], 1, function(x){replace(x, is.na(x), 0)})

2)	896 NAs in USED_CAR
3)	819 NAs in FURNITURE
4)	720 NAs in RADIO/TV
5)	950 NAs in EDUCATION
6)	903 NAs in RETRAINING – Delete the column
7)	310 NA s in PERSONAL_STATUS
GermanData$PERSONAL_STATUS<-apply(GermanData[,c(8)], 1, function(x){replace(x, is.na(x), 1)})
8)	9 NA s in AGE
GermanData$AGE<-apply(GermanData[,c(13)], 1, function(x){replace(x, is.na(x), 35)})

ATTRIBUTE	NA PERCENTAGE (Out of 1000)	Strategy 
NEW_CAR	76.6%	Default to 0
USED_CAR	89.6%	Default to 0
FURNITURE	81.9%	Default to 0
RADIO/TV	72%	Default to 0
EDUCATION	95%	Default to 0
RETRAINING	90.3%	Default to 0
PERSONAL_STATUS	31%	Categorical Variable, Hence Mode i.e 1
AGE	0.9%	Replace NA with Mean i.e =35.48








3.	Exploratory Data Analysis


> chk<- data.frame(table(data$CHK_ACCT,data$RESPONSE))
> names(chk) <- c("CHK_ACCT", "RESPONSE", "Count")
> p2<-ggplot(data=chk, aes(x=CHK_ACCT, y=Count, fill= RESPONSE),color='grey') + geom_bar(stat = "identity",colour="yellow")+
+     scale_fill_manual(values=c("#CC6666","#000000"))
> 


 
          

VARIABLES OF SIGNIFICANCE: -
Categorical Variables of significance from perspective: -
1)	Checking Account
2)	History
3)	Savings Account
4)	Employment
5)	Job
Interesting Relationships –
1)	Checking Account – Majority of people ~ 130 cases- with no checking account have a better credit score than ones who do have one and this was an interesting observation. 
2)	History – Category 4 consists of around 230-250 critical accounts who have had immense delay in paying off their debt and yet credit score for this category is maximum which is very unusual from real world finance statistics. 

3)	Savings Account – The category 1 consists of 380 people with savings account balance <100DM and there seems to be many “good credit” cases for these as compared to category 2/3/4. 

Numeric Variables of significance from perspective: -
1)	Duration
2)	Install Rate
3)	Age
4)	Num_Credits
Interesting Relationships –
1)	Duration – Although it doesn’t necessarily correlate with Good/Bad Credit score – but the categorization and prediction via “Duration” is a unreliable factor to calculate response since it’s a very unpredictable variable. 

Hence, checking account, History and Savings account have a high correlation with Good/Bad Credit Scores. 
Ex – If one has a critical history/Category 4 history – he has a “GOOD CREDIT”.
Ex – If one has <100DM amount in his savings account- it is very likely that he falls under “GOOD CREDIT” Case. 
Ex – If One has no checking account – he has higher chances of falling under “GOOD” Credit than compared to ones who do have saving account. 
4.	Modelling – Decision Tree
Solution: 
         Data model with information gain
Seed	Accuracy training data	Accuracy test data
150	64.14%	60.0%
420	59.42%	57.33%
800	65.14%	62.33
 
         Data model with Gini Index
Seed	Accuracy training data	Accuracy test data
150	69.71%	62.66%
420	70.85%	63%
800	71.85%	63%

•	Yes, decisions tree models are considered unstable because minor change in the data can make very different trees. To counter this, we can use a variety of ensemble methods like bagging, boosting and stacking. 
•	On our data model with information gain, we observed that changing the seed resulted in a difference of about 5% among the training and test data. 
•	However, the model using Gini Index was relatively stable with fluctuation between 1-2 %.

CONFUSION MATRIX
          true
prediction   0   1
         0  96  21
         1  53 330
> precision
[1] 0.8616188
> sensitivity
[1] 0.9401709
> specificity
[1] 0.6442953
> prevalence
[1] 0.702
$ precision  : num 0.862
 $ sensitivity: num 0.94
 $ specificity: num 0.644
 $ prevalence : num 0.702
 $ FNR        : num 0.0598
 $ FPR        : num 0.356
GermanTrain5050<-GermanData[trnIndex50,]
> GermanTrain5050$RESPONSE<-as.factor(GermanTrain5050$RESPONSE)
> cModel1=C5.0(RESPONSE ~ ., data=GermanTrain5050, method="class")
> summary(cModel1)


LIFT 

4. Consider the net profit (on average) of credit decisions as: Accept applicant decision for an Actual 
 

THRESHOLD VALUE	ACCURACY OF DATASET
0.5	0.8275
0.4	0.82375
0.6	0.8175
0.3	0.81125
0.8	0.7475

Hence clearly from the table and ROC curve – the curve increases and starts streamlining at a threshold of around 0.4. Theoretically we can see that accuracy of the model is highest when the threshold is 0.5 and is decreases with threshold increasing/decreasing. 
Hence for the C5.0 model – the best model is one with Threshold value =0.5.


