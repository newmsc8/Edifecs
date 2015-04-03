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
df.30 = df[df$ReadmitBucket=='Low',]
df.60 = df[df$ReadmitBucket=='Medium',]
df.90 = df[df$ReadmitBucket=='High',]
#df.30 = df.30[with(df.30, order(NextAdmitCost)),]
#df.60 = df.60[with(df.60, order(NextAdmitCost)),]
#df.90 = df.90[with(df.90, order(NextAdmitCost)),]
#df.30$CostBucket[1:(nrow(df.30)/3)]<-'Low'
#df.30$CostBucket[((nrow(df.30)/3)+1):(2*nrow(df.30)/3)]<-'Medium'
#df.30$CostBucket[((2*nrow(df.30)/3)+1):nrow(df.30)]<-'High'
#df.60$CostBucket[1:(nrow(df.60)/3)]<-'Low'
#df.60$CostBucket[((nrow(df.60)/3)+1):(2*nrow(df.60)/3)]<-'Medium'
#df.60$CostBucket[((2*nrow(df.60)/3)+1):nrow(df.60)]<-'High'
#df.90$CostBucket[1:(nrow(df.90)/3)]<-'Low'
#df.90$CostBucket[((nrow(df.90)/3)+1):(2*nrow(df.90)/3)]<-'Medium'
#df.90$CostBucket[((2*nrow(df.90)/3)+1):nrow(df.90)]<-'High'
df.shuffle.30 = df.30[1:nrow(df.30),]
df.shuffle.60 = df.60[1:nrow(df.60),]
df.shuffle.90 = df.90[1:nrow(df.90),]
#df.shuffle.30$NextAdmitCost<-NULL
#df.shuffle.60$NextAdmitCost<-NULL
#df.shuffle.90$NextAdmitCost<-NULL
for(i in 1:k) { #10 fold cross validation, modify k for fewer folds
	#cur<-vector()
	message(i)
	#separate train and test data
	df.train.30 = df.shuffle.30[which(1:nrow(df.shuffle.30)%%k != i%%k),]
	df.test.30 = df.shuffle.30[which(1:nrow(df.shuffle.30)%%k == i%%k),]
	df.train.60 = df.shuffle.60[which(1:nrow(df.shuffle.60)%%k != i%%k),]
	df.test.60 = df.shuffle.60[which(1:nrow(df.shuffle.60)%%k == i%%k),]
	df.train.90 = df.shuffle.90[which(1:nrow(df.shuffle.90)%%k != i%%k),]
	df.test.90 = df.shuffle.90[which(1:nrow(df.shuffle.90)%%k == i%%k),]
	message('making models')
	#train model
	#train a Risk model without the CostBucket
	#r.model = switch(model.type,
	#	tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
	#	svm = svm(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]),
	#	nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]))
	#message('risk model made')
	#train a model that classifies cost for those readmitted within 30 days
	c30.model = switch(model.type,
		tree = rpart(formula=formula('CostBucket~.'), data=df.train.30[,-which(names(df.shuffle.30) %in% c('ReadmitBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('CostBucket~.'), data=df.train.30[,-which(names(df.shuffle.30) %in% c('ReadmitBucket'))]),
		nb = naiveBayes(formula=formula('CostBucket~.'), data=df.train.30[,-which(names(df.shuffle.30) %in% c('ReadmitBucket'))]))
	message('30 day risk cost model made')
	#train a model that classifies cost for those readmitted within 60 days
	c60.model = switch(model.type,
		tree = rpart(formula=formula('CostBucket~.'), data=df.train.60[,-which(names(df.shuffle.60) %in% c('ReadmitBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('CostBucket~.'), data=df.train.60[,-which(names(df.shuffle.60) %in% c('ReadmitBucket'))]),
		nb = naiveBayes(formula=formula('CostBucket~.'), data=df.train.60[,-which(names(df.shuffle.60) %in% c('ReadmitBucket'))]))
	message('60 day risk cost model made')
	#train a model that classifies cost for those readmitted after 60 days
	c90.model = switch(model.type,
		tree = rpart(formula=formula('CostBucket~.'), data=df.train.90[,-which(names(df.shuffle.90) %in% c('ReadmitBucket'))], control=rpart.control(cp=cp)),
		svm = svm(formula=formula('CostBucket~.'), data=df.train.90[,-which(names(df.shuffle.90) %in% c('ReadmitBucket'))]),
		nb = naiveBayes(formula=formula('CostBucket~.'), data=df.train.90[,-which(names(df.shuffle.90) %in% c('ReadmitBucket'))]))
	message('60+ day risk cost model made')
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
	c30.p = switch(model.type,
		tree = predict(c30.model, newdata=df.test.30,type='class'),
		svm = predict(c30.model, newdata=df.test.30,type='class'),
		nb = predict(c30.model, newdata=df.test.30,type='class'))
	message('prediction done for cost of 30day risk')
	#use cost model trained on 60 day readmits to predict cost for those predicted as 60 day readmits
	#c60.df.test<-c.df.test[c.df.test$ReadmitBucket=='Medium',]
	c60.p = switch(model.type,
		tree = predict(c60.model, newdata=df.test.60,type='class'),
		svm = predict(c60.model, newdata=df.test.60,type='class'),
		nb = predict(c60.model, newdata=df.test.60,type='class'))
	message('prediction done for cost of 60day risk')
	#use cost model trained on above 60 day readmits to predict cost for those predicted as above 60 day readmits
	#c90.df.test<-c.df.test[c.df.test$ReadmitBucket=='High',]
	c90.p = switch(model.type,
		tree = predict(c90.model, newdata=df.test.90,type='class'),
		svm = predict(c90.model, newdata=df.test.90,type='class'),
		nb = predict(c90.model, newdata=df.test.90,type='class'))
	message('prediction done for cost of 60day+ risk')		
	#store predictions in cost.preds and risk.preds and truths in cost.truths and risk.truths
	cost.preds = c(cost.preds,as.character(c30.p))
	cost.truths = c(cost.truths,as.character(df.test.30$CostBucket))
	risk.truths = c(risk.truths,as.character(df.test.30$ReadmitBucket))
	cost.preds = c(cost.preds,as.character(c60.p))
	cost.truths = c(cost.truths,as.character(df.test.60$CostBucket))
	risk.truths = c(risk.truths,as.character(df.test.60$ReadmitBucket))
	cost.preds = c(cost.preds,as.character(c90.p))
	cost.truths = c(cost.truths,as.character(df.test.90$CostBucket))
	risk.truths = c(risk.truths,as.character(df.test.90$ReadmitBucket))
	#risk.preds = c(risk.preds,as.character(r.p))
	#risk.truths = c(risk.truths,as.character(df.test$ReadmitBucket))
	message('stored in preds and truths')
}

#save predictions/truths for future reference if needed
cost.preds.file<-paste('sepcostbyrealrisk',model.type,'cost preds svd.rds',sep=" ")
cost.truths.file<-paste('sepcostbyrealrisk',model.type,'cost truths svd.rds',sep=" ")
#risk.preds.file<-paste('sepcostbyrisk',model.type,'risk preds.rds',sep=" ")
risk.truths.file<-paste('sepcostbyrealrisk',model.type,'risk truths svd.rds',sep=" ")
if(model.type=='tree') {
	cost.preds.file<-paste(cp,cost.preds.file,sep=" ")
	cost.truths.file<-paste(cp,cost.truths.file,sep=" ")
	#risk.preds.file<-paste(cp,risk.preds.file,sep=" ")
	risk.truths.file<-paste(cp,risk.truths.file,sep=" ")
}
saveRDS(cost.preds,file=cost.preds.file)
saveRDS(cost.truths,file=cost.truths.file)
#saveRDS(risk.preds,file=risk.preds.file)
saveRDS(risk.truths,file=risk.truths.file)

#report risk accuracy
#ret = cbind(risk.preds,risk.truths)
#message('risk accuracy: ',(nrow(ret[ret[,1]==ret[,2],])/nrow(ret)))
ret = cbind(cost.preds,cost.truths)
message('cost accuracy: ',(nrow(ret[ret[,1]==ret[,2],])/nrow(ret)))	

