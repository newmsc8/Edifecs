library(Cubist)
library(rpart)
library(randomForest)
library(Metrics)
library(rattle)

#read arguments from command line
args <- commandArgs(trailingOnly = TRUE)
data.file <- args[1] #where the data is located
model.type <- args[2] #model type (lm, tree, individual, population, forest, previous, glm, mtrees)
model.rds <- args[3] #file location to save model

message('starting ', model.type, ' for data in ', data.file, ', saving in ', model.rds)

#if the regression tree model is being run, the cp needs to be provided as a final argument
if(model.type == "tree") {
	cp <- as.numeric(args[4]) #complexity parameter to use for decision tree
	message('using cp ',cp)
} else if(model.type == "individual" || model.type == "population") {
	ratio <- as.numeric(args[4]) #if individual or population baselines are being run this argument should be 3 for q1, 1 for q1q2, and .33 for q1q2q3
	message('using ratio: ',ratio)
} else if(model.type == "glm") {
	link.function = args[4] #if glm model is being made provide link function
	message('using link function: ',link.function)
}

df <- read.csv(data.file) #data frame holding data
rmse = vector() #vector where rmse will be held
mae = vector() #vector where mae will be held
r2 = vector() #vector where r2 will be held
message('original number of rows in df: ',nrow(df))

#remove rows with NA values
df = df[complete.cases(df),]
message('number of rows with complete cases: ',nrow(df))

if(model.type == "glm") {
	df = df[df$future_cost > 0,]
	message('number of rows with future cost > 0: ',nrow(df))
}

#shuffle the data
df = df[1:nrow(df),]
#delete visit link from data
df$VisitLink<-NULL
#response variable
formula = formula("future_cost~.")

#train model
model = switch(model.type,
	tree = rpart(formula, df, control=rpart.control(cp=cp)),
	forest = randomForest(formula, df, ntrees=50),
	lm = lm(formula, data=df),
	glm = glm(formula, family=poisson(link=link.function),data=df),
	mtrees = cubist(subset(df, select = -c(future_cost)),df$future_cost),
	individual = NA,
	population = NA,
	previous = lm(formula, data=subset(df, select = c(previous_cost, future_cost))))
message('model trained')
#use model to predict values for test data
predictions = switch(model.type,
	tree = predict(model, df, type='vector'),
	forest = predict(model, df),
	lm = predict(model, df, type='response'),
	glm = predict(model,df),
	mtrees = predict(model,subset(df, select = -c(future_cost))),
	individual = df$previous_cost*ratio,
	population = rep(mean(df$previous_cost)*ratio, nrow(df)),
	previous = predict(model, subset(df, select = c(previous_cost, future_cost)), type='response'))
message('predictions made')
#results matrix has actual values in column 1 and predicted values in column 2
true.values = df$future_cost
message('true values saved')

#calculate rmse, mae, and r-squared
rmse = rmse(true.values, predictions)
mae = mae(true.values, predictions)
r2 = cor(true.values, predictions)^2
#print errors
message('rmse: ',rmse)
message('mae: ',mae)
message('r2: ',r2)

#calculate quantiles and print
differences = abs(true.values - predictions)
quantiles = quantile(differences)
message('\n')
message('quantile 0%: ',quantiles[1])
message('quantile 25%: ',quantiles[2])
message('quantile 50%: ',quantiles[3])
message('quantile 75%: ',quantiles[4])
message('quantile 100%: ',quantiles[5])
message('\n')

#save model trained on full dataset
saveRDS(model,file=model.rds)

#if the model was a tree, print and save rules
if(model.type == 'tree') {
	message(asRules(model))
}

