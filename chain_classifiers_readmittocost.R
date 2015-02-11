library(e1071)
library(rpart)

#computes the average accuracy for multilabel classification (2 labels)
averageAccuracy <- function(x.truth, x.preds, y.truth, y.preds) {
  score <- 0
  for(i in 1:length(x.truth)) {
    intersect.labels<-union.labels<-2
    if(x.truth[i] != x.preds[i]) {
      intersect.labels<-intersect.labels-1
      union.labels<-union.labels+1
    }
    if(y.truth[i] != y.preds[i]) {
      intersect.labels<-intersect.labels-1
      union.labels<-union.labels+1
    }
    score<-score+(intersect.labels/union.labels)
  }
  score/length(x.truth)
}

#computes accuracy for a single label
accuracy <- function(truth, preds) {
  sum(truth == preds)/length(truth)
}

#computes the hamming loss for multilabel classification (2 labels)
hammingLoss <- function(x.truth, x.preds, y.truth, y.preds) {
  losses<-0
  for(i in 1:length(x.truth)) {
    losses<-losses + length(sym_diff(c(paste("x",x.truth[i],sep="."),paste("y",y.truth[i],sep=".")),c(paste("x",x.preds[i],sep="."),paste("y",y.preds[i],sep="."))))/2
  }
  losses/length(x.truth)
}

#symmetric difference of two sets (for xor in hamming difference)
sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))


x<-readRDS("~/KDD2015/Data/siddata.rds")
x$KEY<-NULL
x$ReadmitAndCostBucket <- paste(x$ReadmitBucket,x$CostBucket,sep=" ")
x$VisitLink<-NULL
x$NextAdmitCost<-NULL
x$DaysBetweenVisits<-NULL
x$NextAdmitDate<-NULL
x$ReadmitBucket<-as.factor(x$ReadmitBucket)
x$ReadmitAndCostBucket<-as.factor(x$ReadmitAndCostBucket)
x$CostBucket<-as.factor(x$CostBucket)
x$FEMALE<-as.logical(x$FEMALE)
x$RACE<-as.factor(x$RACE)

for(i in c(grep("^CM_",colnames(x)),grep("^REVCD_",colnames(x)),grep("^DXCCS_",colnames(x)),grep("^PRCCS_",colnames(x)))) {
  x[,i]<-as.logical(x[,i])
}
for(i in grep("^U_",colnames(x))) {
  x[,i]<-as.factor(x[,i])
}

x.shuffle = x[sample(1:nrow(x)), ]
x.shuffle = x[sample(1:nrow(x),500),]

cost.preds<-c()
readmit.preds<-c()
cost.truths<-c()
readmit.truths<-c()

folds <- 2

for(i in 1:folds) {
  print(i)
  
  #separate train and test
  x.train = x.shuffle[which(1:nrow(x.shuffle)%%folds != i%%folds), ]
  x.test = x.shuffle[which(1:nrow(x.shuffle)%%folds == i%%folds), ]
  
  cost.model = rpart(CostBucket~., data=x.train[,-which(names(x.shuffle) %in% c("ReadmitAndCostBucket"))], control=rpart.control(cp=0.01))
  readmit.model = rpart(ReadmitBucket~., data=x.train[,-which(names(x.shuffle) %in% c("CostBucket","ReadmitAndCostBucket"))], control=rpart.control(cp=0.01))
 
  print("making predictions")

  readmit.p = predict(readmit.model, x.test, type = 'class')
  readmit.preds<-c(readmit.preds,readmit.p)
  readmit.preds<-factor(readmit.preds, levels=1:nlevels(x$ReadmitBucket), labels=levels(x$ReadmitBucket))
  readmit.truths<-c(readmit.truths,x.test$ReadmitBucket)
  readmit.truths<-factor(readmit.truths, levels=1:nlevels(x$ReadmitBucket), labels=levels(x$ReadmitBucket))
   
  cost.predict.data = x.test
  cost.predict.data$ReadmitBucket = readmit.p
  cost.p = predict(cost.model, cost.predict.data, type = 'class')  
  cost.preds<-c(cost.preds,cost.p)
  cost.preds<-factor(cost.preds, levels=1:nlevels(x$CostBucket), labels=levels(x$CostBucket))
  cost.truths<-c(cost.truths,x.test$CostBucket)
  cost.truths<-factor(cost.truths, levels=1:nlevels(x$CostBucket), labels=levels(x$CostBucket))
  
}

print(paste("Average accuracy:",averageAccuracy(cost.truths, cost.preds, readmit.truths, readmit.preds),sep=" "))
print(paste("Hamming loss:",hammingLoss(cost.truths, cost.preds, readmit.truths, readmit.preds),sep=" "))
