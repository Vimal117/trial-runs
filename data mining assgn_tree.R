library(rpart)
library(rpart.plot)
library(dplyr)

#install.packages('tidyverse')
main_data=read.csv(file.choose(),header=1)
data_use=main_data[,1:32]
View(data_use)
attach(data_use)
summary(data_use)
#filling in missing values and cleaning the data
data_use$NEW_CAR[is.na(data_use$NEW_CAR)]<-0
data_use$USED_CAR[is.na(data_use$USED_CAR)]<-0
data_use$FURNITURE[is.na(data_use$FURNITURE)]<-0
data_use$RADIO.TV[is.na(data_use$RADIO.TV)]<-0
data_use$EDUCATION[is.na(data_use$EDUCATION)]<-0
data_use$RETRAINING[is.na(data_use$RETRAINING)]<-0
data_use$AGE[is.na(data_use$AGE)]<-35.48
summary(data_use)
#making a decison tree on full data
tree_full=rpart(RESPONSE~.,data_use[,-1],method="class",model = 1)
rpart.plot::prp(tree_full,type=2,extra=3)
plot(tree_full, uniform = 2, main="Decision tree using all parameters by using minsplit=20")
text(tree_full,pretty=0,cex=0.7)
summary(tree_full)
printcp(tree_full)
plotcp(tree_full)
pred_whole=predict(tree_full,data_use,type="class")
table(prediction=pred_whole,True=RESPONSE)
summary(pred_whole)
mean(pred_whole==RESPONSE)
install.packages("bitops")
library('ROCR')
library(gplots)
#score test data set
pred=predict(tree_full,data_use)
summary(pred)
data_use$score<-predict(tree_full,type='prob',data_use) #LIFT chart
pred<-prediction(data_use$score[,1],data_use$RESPONSE)
perf <- performance(pred,"lift","rpp")
plot(perf)
pred2<-prediction(data_use$score[,2],data_use$RESPONSE) # ROC
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2)
View(perf2)
abline(a=0,b=1)
View(pred_whole)
View(data_use)
#lift.chart(tree_full,data_use,"yes",type="cumulative")
tab=table(pred,RESPONSE)
pred=prediction(tree_full,data_use)
perf=performance(pred_whole,"lift","rpp")
View(perf)
plot(perf)
#****************************************
#plotting trees now using training and test datasets 50:50
set.seed(125)
nrw=nrow(data_use)
index1=sample(x=1:nrw,size =round(0.5*nrw),replace = 0 )
dat_train1=data_use[index1,-33]
dat_test1=data_use[-index1,-33]
prune_model1=rpart(dat_train1$RESPONSE~.,dat_train1,method = "class")
pred1=predict(prune_model1,dat_test1,type="class")
rpart.plot::prp(prune_model1,type=2,extra=3)
mean(pred1==dat_test1$RESPONSE) #accuracy
mat1=table(pred1,dat_test1$RESPONSE)
accuracy1=sum(diag(mat1))/sum(mat1)
accuracy1
mat1
rowsum=apply(mat1,1,sum)
colsum=apply(mat1,2,sum)
precision_model1=mat1[1,1]/rowsum[1]
precision_model1
recall_model1=mat1[1,1]/colsum[1]
recall_model1
F1_model1=(2*precision_model1*recall_model1)/(precision_model1+recall_model1)
F1_model1
#Lift chart
dat_test1$score<-predict(prune_model1,type='prob',dat_test1) #LIFT chart
pred<-prediction(dat_test1$score[,1],dat_test1$RESPONSE)
perf <- performance(pred,"lift","rpp")
plot(perf)
abline(a=0,b=1)
#ROC
install.packages("pROC")
library(pROC)
View(dat_test1)
pred2<-prediction(dat_test1$score[,2],dat_test1$RESPONSE)#ROC
######


#optimal cost with different costs for fp and fn
cost.perf = performance(rocPredTst, "cost", cost.fp = 2, cost.fn = 1)
rocPredTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#other performance measures with the performance function
acc.perf = performance(rocPredTst, measure = "acc")
plot(acc.perf)

#AUC vaue
auc.perf = performance(rocPredTst, measure = "auc")
auc.perf@y.values
##
#####

perf2 <- performance(pred2,"tpr","fpr",print.auc=1)
cost.perf = performance(pred2, "cost")
pred2@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
##for different costs involved
cost.perf = performance(pred2, "cost",cost.fp=2,cost.fn=1)
plot(cost.perf)
pred2@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
######
acc.perf = performance(pred2, measure = "acc")
plot(acc.perf)

auc.perf = performance(pred2, measure = "auc")   ## AUC
auc.perf@y.values

plot(perf2)
printcp(prune_model1)
plotcp(prune_model1)
prune_model1=rpart(dat_train1$RESPONSE~.,dat_train1,method ="class",cp=0.03,minsplit=40)
pred2=predict(prune_model1,dat_test1,type="class")
rpart.plot::prp(prune_model1,type=3,extra=3)
mean(pred2==dat_test1$RESPONSE)
#*************************************************************************************
#*************************************************************************************
install.packages("C50")
library(C50)
library(rpart.plot)

ctrl=C5.0Control(subset=TRUE,bands=0,
              winnow=FALSE,noGlobalPruning=FALSE,
              CF=0.25,minCases=5,fuzzyThreshold=FALSE,
              sample=0.7,seed=1234,earlyStopping=TRUE,label="outcome")

moedlC5=C5.0(as.factor(data_use$RESPONSE)~.,data_use,method="class",control=ctrl)
pred_whole=predict(moedlC5,data_use)
#table(pred_whole,data_use$RESPONSE)
#printcp(moedlC5)
#dev.off()
mean(pred_whole==data_use$RESPONSE)
plot(moedlC5,main="Decision Tree")
summary(moedlC5)
C5imp(moedlC5)
#text(moedlC5, use.n=TRUE, all=TRUE, cex=.7)
C5.0Control()


