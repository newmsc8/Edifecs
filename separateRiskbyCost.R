#load necessary packages
library(rpart)
library(e1071)
library(caret)
#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
model.type <- args[2] #base model type (tree, svm, nb)
sample.size <- as.numeric(args[3]) #use 0 for full file
message('file name: ',file.name,' -- model type: ',model.type,' -- sample size: ',sample.size)
#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[4]
	message('cp: ',cp)
}
df <- readRDS(file.name) #read in data
df$KEY<-NULL #get rid of unnecessary column
df$VisitLink<-NULL #get rid of unnecessary column
df$ReadmitAndCostBucket <- paste(df$ReadmitBucket,df$CostBucket,sep=" ") #create LP label
df$DaysBetweenVisits<-NULL
df$NextAdmitDate<-NULL
df$NextAdmitCost<-NULL
df$ReadmitBucket<-as.factor(df$ReadmitBucket) #ensure readmit response variable is a factor
df$ReadmitAndCostBucket<-as.factor(df$ReadmitAndCostBucket) #ensure LP response variable is a factor
df$CostBucket<-as.factor(df$CostBucket) #ensure cost response variable is a factor
df$FEMALE<-as.logical(df$FEMALE) #ensure gender is a boolean
df$RACE<-as.factor(df$RACE) #ensure race is a factor
for(i in c(grep("^CM_",colnames(df)),grep("^REVCD_",colnames(df)),grep("^DXCCS_",colnames(df)),grep("^PRCCS_",colnames(df)))) {
  df[,i]<-as.logical(df[,i])
}
#ensure that all U-codes are factors
for(i in grep("^U_",colnames(df))) {
	df[,i]<-as.factor(df[,i])
}
k = 10 #number of folds for cross validation
cost.preds<-c() #where we will hold predictions for cost
cost.truths<-c() #where we will hold ground truth for cost
risk.preds<-c() #where we will hold predictions for risk
risk.truths<-c() #where we will hold ground truth for risk
#remove ReadmitAndCostBucket
df$ReadmitAndCostBucket<-NULL
message('remove unnecessary buckets')
message('ncol: ',ncol(df))
#if a sample size argument was provided, take a sample of the data of the specified size
if(sample.size>0) {
	df<-df[sample(nrow(df), sample.size),]
	message('sample')
}
#shuffle the data
#df.shuffle = df[1:nrow(df),]
#message('shuffle')
#if we are building a naive bayes model, make sure all predictors are of consumable form (numeric or factor)
if(model.type == 'nb') {
	df[, -which(sapply(df, is.numeric))]<-as.data.frame(lapply((df[,-which(sapply(df, is.numeric))]), as.factor))
}
df.low = df[df$CostBucket=='Low',]
df.med = df[df$CostBucket=='Medium',]
df.high = df[df$CostBucket=='High',]
df.shuffle.low = df.low[1:nrow(df.low),]
df.shuffle.med = df.med[1:nrow(df.med),]
df.shuffle.high = df.high[1:nrow(df.high),]
for(i in 1:k) { #10 fold cross validation, modify k for fewer folds
	#cur<-vector()
	message(i)
	#separate train and test data
	df.train.low = df.shuffle.low[which(1:nrow(df.shuffle.low)%%k != i%%k),]
	df.test.low = df.shuffle.low[which(1:nrow(df.shuffle.low)%%k == i%%k),]
	df.train.med = df.shuffle.med[which(1:nrow(df.shuffle.med)%%k != i%%k),]
	df.test.med = df.shuffle.med[which(1:nrow(df.shuffle.med)%%k == i%%k),]
	df.train.high = df.shuffle.high[which(1:nrow(df.shuffle.high)%%k != i%%k),]
	df.test.high = df.shuffle.high[which(1:nrow(df.shuffle.high)%%k == i%%k),]
	message('making models')
	#train model
	#train a Risk model without the CostBucket
	#r.model = switch(model.type,
	#	tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
	#	svm = svm(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]),
	#	nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]))
	#message('risk model made')
	#train a model that classifies cost for those readmitted within 30 days
	rlow.model = switch(model.type,
		tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train.low[,-which(names(df.shuffle.low) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('ReadmitBucket~.'), data=df.train.low[,-which(names(df.shuffle.low) %in% c('CostBucket'))]),
		nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train.low[,-which(names(df.shuffle.low) %in% c('CostBucket'))]))
	message('low cost risk model made')
	#train a model that classifies cost for those readmitted within 60 days
	rmed.model = switch(model.type,
		tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train.med[,-which(names(df.shuffle.med) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('ReadmitBucket~.'), data=df.train.med[,-which(names(df.shuffle.med) %in% c('CostBucket'))]),
		nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train.med[,-which(names(df.shuffle.med) %in% c('CostBucket'))]))
	message('medium cost risk model made')
	#train a model that classifies cost for those readmitted after 60 days
	rhigh.model = switch(model.type,
		tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train.high[,-which(names(df.shuffle.high) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('ReadmitBucket~.'), data=df.train.high[,-which(names(df.shuffle.high) %in% c('CostBucket'))]),
		nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train.high[,-which(names(df.shuffle.high) %in% c('CostBucket'))]))
	message('high cost risk model made')
  	print("making predictions")
	#use risk model to predict risk bucket
  	#r.p = switch(model.type,
	#	tree = predict(r.model, newdata=df.test,type='class'),
	#	svm = predict(r.model, newdata=df.test,type='class'),
	#	nb = predict(r.model, newdata=df.test,type='class'))
	#message('prediction done for risk')
	#set the predictions as the ReadmitBucket label for the cost test data frame
	#c.df.test<-df.test
	#c.df.test$ReadmitBucket<-r.p
	#use cost model trained on 30 day readmits to predict cost for those predicted as 30 day readmits
	#c30.df.test<-c.df.test[c.df.test$ReadmitBucket=='Low',]
	rlow.p = switch(model.type,
		tree = predict(rlow.model, newdata=df.test.low,type='class'),
		svm = predict(rlow.model, newdata=df.test.low,type='class'),
		nb = predict(rlow.model, newdata=df.test.low,type='class'))
	message('prediction done for risk of low cost')
	#use cost model trained on 60 day readmits to predict cost for those predicted as 60 day readmits
	#c60.df.test<-c.df.test[c.df.test$ReadmitBucket=='Medium',]
	rmed.p = switch(model.type,
		tree = predict(rmed.model, newdata=df.test.med,type='class'),
		svm = predict(rmed.model, newdata=df.test.med,type='class'),
		nb = predict(rmed.model, newdata=df.test.med,type='class'))
	message('prediction done for risk of med cost')
	#use cost model trained on above 60 day readmits to predict cost for those predicted as above 60 day readmits
	#c90.df.test<-c.df.test[c.df.test$ReadmitBucket=='High',]
	rhigh.p = switch(model.type,
		tree = predict(rhigh.model, newdata=df.test.high,type='class'),
		svm = predict(rhigh.model, newdata=df.test.high,type='class'),
		nb = predict(rhigh.model, newdata=df.test.high,type='class'))
	message('prediction done for risk of high cost')		
	#store predictions in cost.preds and risk.preds and truths in cost.truths and risk.truths
	risk.preds = c(risk.preds,as.character(rlow.p))
	risk.truths = c(risk.truths,as.character(df.test.low$ReadmitBucket))
	cost.truths = c(cost.truths,as.character(df.test.low$CostBucket))
	risk.preds = c(risk.preds,as.character(rmed.p))
	risk.truths = c(risk.truths,as.character(df.test.med$ReadmitBucket))
	cost.truths = c(cost.truths,as.character(df.test.med$CostBucket))
	risk.preds = c(risk.preds,as.character(rhigh.p))
	risk.truths = c(risk.truths,as.character(df.test.high$ReadmitBucket))
	cost.truths = c(cost.truths,as.character(df.test.high$CostBucket))
	#risk.preds = c(risk.preds,as.character(r.p))
	#risk.truths = c(risk.truths,as.character(df.test$ReadmitBucket))
	message('stored in preds and truths')
}

ret = cbind(risk.preds,risk.truths)
message('risk accuracy: ',(nrow(ret[ret[,1]==ret[,2],])/nrow(ret)))

#save predictions/truths for future reference if needed
risk.preds.file<-paste('sepriskbyrealcost',model.type,'risk preds svd.rds',sep=" ")
risk.truths.file<-paste('sepriskbyrealcost',model.type,'risk truths svd.rds',sep=" ")
#risk.preds.file<-paste('sepcostbyrisk',model.type,'risk preds.rds',sep=" ")
cost.truths.file<-paste('sepriskbyrealcost',model.type,'cost truths svd.rds',sep=" ")
if(model.type=='tree') {
	risk.preds.file<-paste(cp,risk.preds.file,sep=" ")
	risk.truths.file<-paste(cp,risk.truths.file,sep=" ")
	#risk.preds.file<-paste(cp,risk.preds.file,sep=" ")
	cost.truths.file<-paste(cp,cost.truths.file,sep=" ")
}
saveRDS(risk.preds,file=risk.preds.file)
saveRDS(risk.truths,file=risk.truths.file)
#saveRDS(risk.preds,file=risk.preds.file)
saveRDS(cost.truths,file=cost.truths.file)


