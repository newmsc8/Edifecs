library(Cubist)
library(rpart)
library(randomForest)
library(Metrics)

args <- commandArgs(trailingOnly = TRUE)
#where the data is located
file.name <- args[1]
#where the model is located
model.loc <- args[2]
#number of beneficiaries from file.name to be evaluated
sample.size <- as.numeric(args[3])
#type of model in model.loc
model.type <- args[4]
#which cohort are we modeling
cohort <- args[5]
message('predicting cost for ', sample.size, ' beneficiaries from data file ',file.name,' using model from ',model.loc,' of type ',model.type) 

#read in all data
df <- read.csv(file.name)
message('read in data')

df<-df[complete.cases(df),]
message('complete cases extracted')

#extract appropriate cohort (18,635-36,172-30,813-13,836-28,941-60,722)
df = switch(cohort,
	chf = df[rowSums(df[,c('DX_108','CM_CHF')]) > 0,],
	diabetes = df[rowSums(df[,c('DX_49','DX_50','CM_DM','CM_DMCX')]) > 0,],
	copd = df[rowSums(df[,c('DX_127','CM_CHRNLUNG')]) > 0,],
	asthma = df[df$DX_128 > 0,],
	coronary = df[df$DX_101 > 0,],
	age = df[df$AGE>65,],
	all = df[rowSums(df[,c('DX_108','CM_CHF','DX_49','DX_50','CM_DM','CM_DMCX','DX_127','CM_CHRNLUNG','DX_128','DX_101')]) == 0,])
message('extracted cohort for: ',cohort)
if(cohort=='all') {
	df = df[df$AGE<=65,]
	message('extracted age under 65')
}

message('nrow: ',nrow(df))
message('sample.size: ',sample.size)
message(sample.size >0)
message(nrow(df) > sample.size)

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

#read in model
model<- readRDS(model.loc)
message('stored model')

#use model to predict values for sample
prediction = switch(model.type,
	tree = predict(model, df, type='vector'),
	forest = predict(model, df),
	lm = predict(model, df, type='response'),
	glm = predict(model,df),
	mtrees = predict(model,subset(df, select = -c(future_cost))),
	individual = df$previous_cost*ratio,
	population = rep(mean(df$previous_cost)*ratio, nrow(df)),
	previous = predict(model, subset(df, select = c(previous_cost, future_cost)), type='response'))
message('prediction complete: ',nrow(prediction))

#get rmse, mae, and quartiles for prediction
rmse = vector()
mae = vector()
rmse = rmse(df$future_cost, prediction)
message('rmse: ',rmse)
mae = mae(df$future_cost, prediction)
message('mae: ',mae)
differences = abs(df$future_cost - prediction)
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



