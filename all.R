library(Cubist)
library(rpart)
library(randomForest)
library(Metrics)

args <- commandArgs(trailingOnly = TRUE)
#where the data is located
file.name <- args[1]
#number of beneficiaries from file.name to be evaluated
sample.size <- as.numeric(args[2])
message('predicting cost for ', sample.size, ' beneficiaries from data file ',file.name) 

#read in all data
df <- read.csv(file.name)
message('read in data')

df<-df[complete.cases(df),]
message('complete cases extracted')

#extract appropriate cohort
df = df[rowSums(df[,c('DX_108','CM_CHF','DX_49','DX_50','CM_DM','CM_DMCX','DX_127','CM_CHRNLUNG','DX_128','DX_101')]) == 0,]
message('nrows1: ',nrow(df))
df = df[df$AGE<=65,]
message('nrows2: ',nrow(df))
message('extracted cohort')

#if a sample size argument was provided, take a sample of the data of the specified size
if((sample.size > 0) && (nrow(df) > sample.size)) {
	df<-df[sample(nrow(df), sample.size),]
}
message('took sample: ',nrow(df))

#shuffle the data
df = df[1:nrow(df),]
message('shuffle data')

#NULL visit link
df$VisitLink<-NULL	
message('null visit link')

ret = data.frame()

for(i in 1:10) { #10 fold cross validation, modify k for fewer folds
	message(i)

	cur<-vector()

	#separate train and test data
	df.train = df[which(1:nrow(df)%%10 != i%%10),]
	df.test = df[which(1:nrow(df)%%10 == i%%10),]
	message('fold extracted')

	#response variable
	formula = formula("future_cost~.")

	#train model
	model = cubist(subset(df.train, select = -c(future_cost)),df.train$future_cost)
	message('trained model')

	#use model to predict values for sample
	p = predict(model,subset(df, select = -c(future_cost)))
	message('prediction complete')

	#results matrix has actual values in column 1 and predicted values in column 2
	cur = cbind(df.test$future_cost, p)
	ret = rbind(ret, cur)
}

#get rmse, mae, and quartiles for prediction
rmse = vector()
mae = vector()
rmse = rmse(ret[,1], ret[,2])
message('rmse: ',rmse)
mae = mae(ret[,1], ret[,2])
message('mae: ',mae)
differences = abs(ret[,1] - ret[,2])
message(nrow(differences))
quantiles = quantile(differences)

#print out errors
message('\n')
message('0% ----- ',quantiles[1])
message('25% ----- ',quantiles[2])
message('50% ----- ',quantiles[3])
message('75% ----- ',quantiles[4])
message('100% ----- ',quantiles[5])
message('\n')
message('\n')
message('rmse ----- ',rmse)
message('mae ----- ',mae)
message('\n')



