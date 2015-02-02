library(Cubist)
library(rpart)
library(randomForest)
library(Metrics)
#library(bigmemory)
#library(biganalytics)

#read arguments from command line
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
sample.size <- args[2] #sample size, 0 means entire data file will be used
bucket.start <- args[3] #used for cost bucketing, 0 if no cost bucketing is being used
bucket.end <- args[4] #used for cost bucketing, 0 if no cost bucketing is being used
model.type <- args[5] #model type (lm, tree, individual, population, forest, previous, glm, mtrees)
file.text <- args[6] #file location for errors to report to
file.rds <- args[7] #dfile location to print out last cross validation's model

#if the regression tree model is being run, the cp needs to be provided as a final argument
if(model.type == "tree") {
	cp <- args[8]
} else if(model.type == "individual" || model.type == "population") {
	ratio <- as.numeric(args[8]) #if individual or population baselines are being run this argument should be 3 for q1, 1 for q1q2, and .33 for q1q2q3
} else if(model.type == "glm") {
	link.function = args[8]
}

message('starting ', model.type, ' starting at ', bucket.start, ' ending at ', bucket.end)

df.full <- read.csv(file.name)
k = 10
rmse = vector()
mae = vector()
ret = data.frame()
r2 = vector()
#df.bucket = data.frame()

#df.bucket = beneficiaries with previous year's cost between arguments bucket.start and bucket.end
#all beneficiaries with a previous cost above 0 are put into df.bucket if 0s were provided as the arguments
df.bucket <- df.full[df.full$previous_cost >= bucket.start,]
if(bucket.end>0) {
	df.bucket <- df.bucket[df.bucket$previous_cost < bucket.end,]
}

#remove rows with NA values
df.bucket = df.bucket[complete.cases(df.bucket),]

if(model.type == "glm") {
	df.bucket = df.bucket[df.bucket$future_cost > 0,]
}

#if a sample size argument was provided, take a sample of the data of the specified size
if(sample.size>0) {
	df.bucket<-df.bucket[sample(nrow(df.bucket), sample.size),]

}

#shuffle the data
df.shuffle = df.bucket[1:nrow(df.bucket),]

df.shuffle$VisitLink<-NULL

message(nrow(df.shuffle))

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds
	message(i)

	cur<-vector()

	#separate train and test data
	df.train = df.shuffle[which(1:nrow(df.shuffle)%%k != i%%k),]
	df.test = df.shuffle[which(1:nrow(df.shuffle)%%k == i%%k),]

	#response variable
	formula = formula("future_cost~.")

	#train model
	model = switch(model.type,
		tree = rpart(formula, df.train, control=rpart.control(cp=cp)),
		forest = randomForest(formula, df.train, ntrees=50),
		lm = lm(formula, data=df.train),
		glm = glm(formula, family=poisson(link=link.function),data=df.train),
		mtrees = cubist(subset(df.train, select = -c(future_cost)),df.train$future_cost),
		individual = NA,
		population = NA,
		previous = lm(formula, data=subset(df.test, select = c(previous_cost, future_cost))))
	#use model to predict values for test data
	p = switch(model.type,
		tree = predict(model, df.test, type='vector'),
		forest = predict(model, df.test),
		lm = predict(model, df.test, type='response'),
		glm = predict(model,df.test),
		mtrees = predict(model,subset(df.test, select = -c(future_cost))),
		individual = df.test$previous_cost*ratio,
		population = rep(mean(df.train$previous_cost)*ratio, nrow(df.test)),
		previous = predict(model, subset(df.test, select = c(previous_cost, future_cost)), type='response'))

	#results matrix has actual values in column 1 and predicted values in column 2
	cur = cbind(df.test$future_cost, p)
	ret = rbind(ret, cur)
}

#calculage rmse, mae, and r-squared
rmse = rmse(ret[,1], ret[,2])
mae = mae(ret[,1], ret[,2])
r2 = cor(ret[,1], ret[,2])^2
stats = round(c(rmse, mae, r2), digits=4)

differences = abs(ret[,1] - ret[,2])
quantiles = quantile(differences)
#max_diff = max(differences)
#min_diff = min(differences)
#max = ceiling(max_diff/5000) * 5000
#min = floor(min_diff/5000) * 5000
#if(min == min_diff) {
#	min = min + 5000
#}
#error_buckets = vector()
message('\n')
message(quantiles[1])
message(quantiles[2])
message(quantiles[3])
message(quantiles[4])
message(quantiles[5])
message('\n')

#thresh = max
#while (thresh > min) {
#	message(thresh)
#	error_buckets = cbind(c(length(which(differences <= thresh & differences > thresh-5000))))
#	message(length(which(differences <= thresh & differences > thresh-5000)))
#	thresh = thresh - 5000
#}

message(rmse)
message(mae)
message(r2)

#if the model was not a baseline, print off the final fold's trained model
if(!is.na(file.rds) && model.type!="individual" && model.type!="population" && model.type != "previous") {
	saveRDS(model, file=file.rds)
}

#print metrics into text file
sink(file.text)
cat(c('rmse', 'mae', 'r^2'))
cat('\n')
cat(stats)
cat(quantiles)
#thresh = max
#while(thresh > min) {
#	cat(thresh)
#	thresh = thresh - 5000
#}
#cat(error_buckets)
sink()

file.show(file.text)




