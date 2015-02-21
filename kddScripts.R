library(e1071)
library(rpart)
library(kernlab)
library(caret)

#computes the average accuracy for multilabel classification (2 labels)
#averageAccuracy <- function(x.truth, x.preds, y.truth, y.preds) {
averageAccuracy <- function(x.truth, x.preds) {
	scores <- c()
  	for(i in 1:length(x.truth)) {
    		score <- 0
    		if(x.truth[i] == x.preds[i]) {
      			#score = score + 0.5
      			score = score + 1
    		}
    		#if(y.truth[i] == y.preds[i]) {
    		#	score = score + 0.5
    		#}
    		scores <- c(scores, score)
  	}
	sum(scores)/length(x.truth)
}

#computes the hamming loss for multilabel classification (2 labels)
#hammingLoss <- function(x.truth, x.preds, y.truth, y.preds) {
hammingLoss <- function(x.truth, x.preds) {
  	scores <- c()
  	for(i in 1:length(x.truth)) {
  		score <- 0
    		if(x.truth[i] != x.preds[i]) {
      			score = score + 1
    		}
    		#if(y.truth[i] != y.preds[i]) {
    		#  	score = score + 1
    		#}
    		scores <- c(scores, score)
  	}
  	#sum(scores)/(length(x.truth)*2)
  	sum(scores)/(length(x.truth))
}


x<-readRDS("../data/siddata_filtered.rds")
#x$KEY<-NULL
#x.low<-x[x$ReadmitBucket=='Low',]
#x.low<-x.low[sample(nrow(x.low), 5000),]
#x.other<-x[x$ReadmitBucket!='Low',]
#x.other$ReadmitBucket<-'Other'
#x.other<-x.other[sample(nrow(x.other),5000),]
#x<-rbind(x.low,x.other)
x$ReadmitAndCostBucket <- paste(x$ReadmitBucket,x$CostBucket,sep=" ")
#x$VisitLink<-NULL
#x$NextAdmitCost<-NULL
#x$DaysBetweenVisits<-NULL
#x$NextAdmitDate<-NULL
#x$ReadmitBucket<-as.factor(x$ReadmitBucket)
x$ReadmitAndCostBucket<-as.factor(x$ReadmitAndCostBucket)
#x$CostBucket<-as.factor(x$CostBucket)
#x$FEMALE<-as.logical(x$FEMALE)
#x$RACE<-as.factor(x$RACE)

x$ReadmitBucket<-NULL
x$CostBucket<-NULL

message('nullified :)')

for(i in c(grep("^CM_",colnames(x)),grep("^REVCD_",colnames(x)),grep("^DXCCS_",colnames(x)),grep("^PRCCS_",colnames(x)))) {
  	x[,i]<-as.logical(x[,i])
}

for(i in grep("^U_",colnames(x))) {
  	x[,i]<-as.factor(x[,i])
}
message('factor-ified')

x.shuffle = x[sample(1:nrow(x)), ]
message('shuffled')
message(nrow(x))


#x.shuffle<-x.shuffle[sample(nrow(x.shuffle), 50000),]
#message('sample')

#cost.preds<-c()
#readmit.preds<-c()
#cost.truths<-c()
#readmit.truths<-c()
lp.preds<-c()
lp.truths<-c()

folds <- 10

print(x.shuffle$ReadmitAndCostBucket[1:10])

#x.shuffle$ReadmitAndCostBucket<-as.character(x.shuffle$ReadmitAndCostBucket)

#df.highhigh<-x.shuffle[x.shuffle$ReadmitAndCostBucket=="High High",]
#df.other<-x.shuffle[x.shuffle$ReadmitAndCostBucket!="High High",]
#df.highhigh<-df.highhigh[sample(1:nrow(df.highhigh),5000),]
#df.other<-df.other[sample(1:nrow(df.other),5000),]
#df.highhigh$CLASS<-'high'
#df.other$CLASS<-'other'
#x.shuffle<-rbind(df.highhigh,df.other)
#x.shuffle<-x.shuffle[sample(1:nrow(x.shuffle)),]
#x.shuffle$ReadmitAndCostBucket<-NULL
#x.shuffle$CLASS<-as.factor(x.shuffle$CLASS)

x.small<-x.shuffle[,sample(1:ncol(x.shuffle),10)]
x.small$ReadmitAndCostBucket<-x.shuffle$ReadmitAndCostBucket
x.shuffle<-x.small

#x.small$ReadmitAndCostBucket<-x.shuffle<-ReadmitAndCostBucket
#x.shuffle<-x.small
print(colnames(x.shuffle))

for(i in 1:folds) {
  	print(i)
  
  	#separate train and test
  	x.train = x.shuffle[which(1:nrow(x.shuffle)%%folds != i%%folds), ]
  	x.test = x.shuffle[which(1:nrow(x.shuffle)%%folds == i%%folds), ]
  	message('divided')
  
  	#cost.model = rpart(CostBucket~., data=x.train[,-which(names(x.shuffle) %in% c("ReadmitBucket","ReadmitAndCostBucket"))], control=rpart.control(cp=0.01))
  	#readmit.model = rpart(ReadmitBucket~., data=x.train[,-which(names(x.shuffle) %in% c("CostBucket","ReadmitAndCostBucket"))], control=rpart.control(cp=0.01))
  	lp.model = rpart(ReadmitAndCostBucket~.,data=x.train,control=rpart.control(cp=0.01))
	#lp.model = ksvm(formula('ReadmitAndCostBucket~.'),data=x.train)
	#lp.model = rpart(CLASS~.,data=x.train,control=rpart.control(cp=0.01))
 
  	print("making predictions")

  	#cost.p = predict(cost.model, x.test, type = 'class')
  	#readmit.p = predict(readmit.model, x.test, type = 'class')
  	lp.p = predict(lp.model, x.test, type='class')
	#lp.p = predict(lp.model, x.test)
  
  	#cost.preds<-c(cost.preds,cost.p)
  	#cost.preds<-factor(cost.preds, levels=1:nlevels(x$CostBucket), labels=levels(x$CostBucket))
  	#readmit.preds<-c(readmit.preds,readmit.p)
  	#readmit.preds<-factor(readmit.preds, levels=1:nlevels(x$ReadmitBucket), labels=levels(x$ReadmitBucket))
  	lp.preds<-c(lp.preds,lp.p)
	if(i==1) {print(lp.preds)}
  	lp.preds<-factor(lp.preds, levels=1:nlevels(x$ReadmitAndCostBucket), labels=levels(x$ReadmitAndCostBucket))
	#lp.preds<-factor(lp.preds, levels=1:nlevels(x.shuffle$CLASS), labels=levels(x.shuffle$CLASS))  

  	#cost.truths<-c(cost.truths,x.test$CostBucket)
  	#cost.truths<-factor(cost.truths, levels=1:nlevels(x$CostBucket), labels=levels(x$CostBucket))
  	#readmit.truths<-c(readmit.truths,x.test$ReadmitBucket)
  	#readmit.truths<-factor(readmit.truths, levels=1:nlevels(x$ReadmitBucket), labels=levels(x$ReadmitBucket))
  	lp.truths<-c(lp.truths,x.test$ReadmitAndCostBucket)
  	lp.truths<-factor(lp.truths, levels=1:nlevels(x$ReadmitAndCostBucket), labels=levels(x$ReadmitAndCostBucket))
	#lp.truths<-factor(lp.truths, levels=1:nlevels(x.shuffle$CLASS), labels=levels(x.shuffle$CLASS)) 

  	message('done-zo')
  
}

#print(paste("Average accuracy:",averageAccuracy(cost.truth, cost.preds, readmit.truth, readmit.preds),sep=" "))
#print(paste("Hamming loss:",hammingLoss(cost.truth, cost.preds, readmit.truth, readmit.preds),sep=" "))
#print(paste("Average accuracy:",averageAccuracy(lp.truths, lp.preds),sep=" "))
#print(paste("Hamming loss:",hammingLoss(lp.truths, lp.preds),sep=" "))

print(confusionMatrix(lp.preds,lp.truths))

saveRDS(lp.preds,file="lowcost-preds-bin.rds")
saveRDS(lp.truths,file="lowcost-truths-bin.rds")
