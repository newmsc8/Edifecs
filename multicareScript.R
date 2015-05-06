#load necessary packages
library(rpart)
library(e1071)
#library(caret)

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
approach <- args[2] #what approach are we using (BR,LP,CC,BL)
model.type <- args[3] #base model type (tree, svm, nb, random, majority)
sample.size <- as.numeric(args[4]) #use 0 for full file
save.preface<- args[5]

message('file name: ',file.name,' -- appraoch: ',approach,' -- model type: ',model.type,' -- sample size: ',sample.size)
#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
  cp <- args[6]
  message('cp: ',cp)
}


df <- readRDS(file.name) #read in data
df$DaysSinceLastVisit<-NULL
df$EpicPatientID<-NULL
df$DischargeDTS<-NULL
df$DischargeDT<-NULL
df$ActualNextAdmitDate<-NULL
df$ActualNextAdmitCost<-NULL
for(i in 1:ncol(df)) {
  levels(df[,i])[is.na(levels(df[,i]))]<-'NA'
}

k = 10 #number of folds for cross validation
preds<-c() #where we will hold predictions
truths<-c() #where we will hold ground truth

#remove unnecessary response variable depending on approach
if((approach == "LP") || (approach=='BL') ) {
  df$ReadmitAndCost <- paste(df$ActualNextAdmitDateBucket, df$ActualNextAdmitCostBucket,sep=' ')
  df$ReadmitAndCost <-as.factor(df$ReadmitAndCost)
  df$ActualNextAdmitCostBucket<-NULL
  df$ActualNextAdmitDateBucket<-NULL
  message('added readmitandcost column, removed separate columns')
}
message('ncol: ',ncol(df))

#if a sample size argument was provided, take a sample of the data of the specified size
if(sample.size>0) {
  df<-df[sample(nrow(df), sample.size),]
  message('sample')
}

#shuffle the data
df.shuffle = df[1:nrow(df),]
message('shuffle')

#if we are building a naive bayes model, make sure all predictors are of consumable form (numeric or factor)
if(model.type == 'nb') {
  df.shuffle[, -which(sapply(df.shuffle, is.numeric))]<-as.data.frame(lapply((df.shuffle[,-which(sapply(df.shuffle, is.numeric))]), as.factor))
}

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds
  
  #cur<-vector()
  message(i)
  
  #separate train and test data
  df.train = df.shuffle[which(1:nrow(df.shuffle)%%k != i%%k),]
  df.test = df.shuffle[which(1:nrow(df.shuffle)%%k == i%%k),]
  
  #train model
  if(approach == 'LP') {
    #train an LP model
    message('in LP')
    formula = formula('ReadmitAndCost~.')
    model = switch(model.type,
                   tree = rpart(formula=formula, data=df.train, control=rpart.control(cp=cp)),
                   svm = svm(formula=formula, data=df.train),
                   nb = naiveBayes(formula=formula, data=df.train))
    message('model made')
    #use model to predict readmit and cost
    p = switch(model.type,
               tree = predict(model, newdata=df.test,type='class'),
               svm = predict(model, newdata=df.test,type='class'),
               nb = predict(model, newdata=df.test[,-which(names(df.shuffle) %in% c('ReadmitAndCost'))]))
    message('prediction done')
  } else if(approach == 'CC') {
    message('in CC')
    #train a model that classifies cost and uses readmit bucket as a predictor
    c.model = switch(model.type,
                     tree = rpart(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train, control=rpart.control(cp=cp)),
                     svm = svm(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train),
                     nb = naiveBayes(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train))
    message('model made 1')
    #train a model that classifies risk but does NOT use cost bucket as a predictor
    r.model = switch(model.type,
                     tree = rpart(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))], control=rpart.control(cp=cp)),
                     svm = svm(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))]),
                     nb = naiveBayes(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))]))
    message('model made 2')
    
    print("making predictions")
    
    #use risk model to predict risk bucket
    r.p = switch(model.type,
                 tree = predict(r.model, newdata=df.test,type='class'),
                 svm = predict(r.model, newdata=df.test,type='class'),
                 nb = predict(r.model, newdata=df.test,type='class'))
    message('prediction done 1')
    
    #set risk predictions as readmit bucket and then use cost model to predict cost bucket
    c.df.test<-df.test
    c.df.test$ActualNextAdmitDateBucket<-r.p
    c.p = switch(model.type,
                 tree = predict(c.model, newdata=c.df.test,type='class'),
                 svm = predict(c.model, newdata=c.df.test,type='class'),
                 nb = predict(c.model, newdata=c.df.test,type='class'))
    message('prediction done 2')
    
    #join risk and cost prediction as overall prediction
    p = paste(r.p,c.p,sep=' ')
  } else if(approach=='BR') {
    message('in BR')
    #build a model to predict cost WITHOUT using readmit bucket as a predictor
    c.model = switch(model.type,
                     tree = rpart(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitDateBucket'))], control=rpart.control(cp=cp)),
                     svm = svm(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitDateBucket'))]),
                     nb = naiveBayes(formula=formula('ActualNextAdmitCostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitDateBucket'))]))
    message('model made 1')
    #build a model to predict risk WITHOUT using cost bucket as a predictor
    r.model = switch(model.type,
                     tree = rpart(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))], control=rpart.control(cp=cp)),
                     svm = svm(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))]),
                     nb = naiveBayes(formula=formula('ActualNextAdmitDateBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ActualNextAdmitCostBucket'))]))
    message('model made 2')
    
    print("making predictions")
    
    #use risk model to predict readmit bucket
    r.p = switch(model.type,
                 tree = predict(r.model, newdata=df.test,type='class'),
                 svm = predict(r.model, newdata=df.test,type='class'),
                 nb = predict(r.model, newdata=df.test,type='class'))
    message('prediction done 1')
    
    #use cost model to predict cost bucket
    c.p = switch(model.type,
                 tree = predict(c.model, newdata=df.test,type='class'),
                 svm = predict(c.model, newdata=df.test,type='class'),
                 nb = predict(c.model, newdata=df.test,type='class'))
    message('prediction done 2')
    
    #join risk and cost prediction as overall prediction
    p = paste(r.p,c.p,sep=' ')
    
  } else if(model.type=='majority') {
    majority.value = tail(names(sort(table(as.character(df.train$ReadmitAndCost)))), 1)
    p = rep(majority.value,nrow(df.test))
  } else {
    p<-sample(levels(df.train$ReadmitAndCost),nrow(df.test),replace=TRUE)
  }
  
  #add new predictions to our aggregate vector of predictions
  preds<-c(preds,as.character(p))
  #add appropriate truth values for response variable(s) to our aggregate vector of true values
  if((approach == 'LP') || (approach=='BL')) {
    truths<-c(truths,as.character(df.test$ReadmitAndCost))
  } else {
    truths<-c(truths,as.character(paste(df.test$ActualNextAdmitDateBucket,df.test$ActualNextAdmitCostBucket,sep=' ')))
  }
  
  message('stored in preds and truths')
}
readmit.truths=unlist(strsplit(truths,' '))[seq(1,(2*length(truths)),2)]
cost.truths=unlist(strsplit(truths,' '))[seq(2,(2*length(truths)),2)]
readmit.preds=unlist(strsplit(preds,' '))[seq(1,(2*length(preds)),2)]
cost.preds=unlist(strsplit(preds,' '))[seq(2,(2*length(preds)),2)]

#readmit accuracy
ret = cbind(readmit.truths,readmit.preds)
message('readmit accuracy: ',(nrow(ret[ret[,1]==ret[,2],]))/nrow(ret))
#cost accuracy
ret = cbind(cost.truths,cost.preds)
message('cost accuracy: ',(nrow(ret[ret[,1]==ret[,2],]))/nrow(ret))

#readmit confusion
#message('readmit confusion matrix')
#full=c(readmit.preds,readmit.truths)
#full=as.factor(full)
#confusionMatrix(full[1:length(readmit.preds)],full[(length(readmit.preds)+1):length(full)])
#cost confusion
#message('cost confusion matrix')
#full=c(cost.preds,cost.truths)
#full=as.factor(full)
#confusionMatrix(full[1:length(cost.preds)],full[(length(cost.preds)+1):length(full)])


#save predictions/truths for future reference if needed
preds.file<-paste(save.preface,approach,model.type,'preds.rds',sep='')
truths.file<-paste(save.preface,approach,model.type,'truths.rds',sep='')

if(model.type=='tree') {
  preds.file<-paste(cp,preds.file,sep='')
  truths.file<-paste(cp,truths.file,sep='')
  model.file<-paste(cp,model.file,sep='')
}
saveRDS(preds,file=preds.file)
saveRDS(truths,file=truths.file)
if((approach=='BR') || (approach=='CC')) {
  risk.model.file<-paste(save.preface,approach,model.type,'lastriskmodel.rds',sep='')
  cost.model.file<-paste(save.preface,approach,model.type,'lastcostmodel.rds',sep='')
  if(model.type=='tree') {
    risk.model.file<-paste(cp,risk.model.file,sep='')
    cost.model.file<-paste(cp,cost.model.file,sep='')
  }
  saveRDS(r.model,file=risk.model.file)
  saveRDS(c.model,file=cost.model.file)
} else if(approach == 'LP') {
  model.file<-paste(save.preface,approach,model.type,'lastmodel.rds',sep='')
  if(model.type == 'tree') {
    model.file<-paste(cp,model.file,sep='')
  }
  saveRDS(model,file=model.file)
}
