library(e1071)
library(kernlab)
library(rpart)
library(randomForest)
library(caret)
library(gbm)


averageAccuracyReadmit <- function(x.truths, x.preds) {
  score <- 0
  for(i in 1:length(x.truths)) {
    if(x.truths[i] == x.preds[i]) {
      score = score + 1
    }
  }
  sum(score)/length(x.truths)
}

args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1]
r.t <- args[2]
r.p <- args[3]

x<-readRDS(file.name)
x$EpicPatientID <- NULL
x$DischargeDTS <- NULL
x$ActualNextAdmitDateBucket <- NULL
x$LACE <- NULL

message('without LACE')


print(ncol(x))
print(nrow(x))
message("read")

x.shuffle = x[complete.cases(x), ]

svm.preds<-c()
rf.preds <-c()
gbm.preds <- c()
log.preds <- c()
readmit.truths <-c()
readmit.preds <- c()

message("start fold")
print(nrow(x.shuffle))
print(ncol(x.shuffle))

paste(table(x.shuffle$risk))

folds <- 10

for(i in 1:folds) {
  
  print(i)
  
  #separate train and test
  x.train = x.shuffle[which(1:nrow(x.shuffle)%%folds != i%%folds), ]
  x.test = x.shuffle[which(1:nrow(x.shuffle)%%folds == i%%folds), ]
  print(paste(table(x.test$risk)))
  x.train$risk -> label
  
  message("training readmit")  
  #print(summary(x.train))
  rf.model = randomForest(risk~., data=x.train,ntree=40,type="prob")
  svm.model = svm(risk~.,data=x.train,probability=TRUE)
  gbm.model = gbm.fit(x=x.train[,-15], y = as.vector(x.train[,15]),  distribution = "bernoulli", n.trees = 316, interaction.depth = 2, n.minobsinnode = 10,  shrinkage = 0.01, verbose = FALSE)
  log.model = glm(risk~., data = x.train, family = binomial)
  #print(summary(x.test))
  
  message("making predictions")
  rf.preds = predict(rf.model,x.test,type="prob")
  svm.preds = predict(svm.model,x.test,decision.values = TRUE,probability=TRUE)
  gbm.preds = predict(gbm.model,newdata = x.test[,-15], n.trees = 316, type ='response')
  log.preds = predict(log.model, x.test, type="response")
    
  readmit.truths<-c(readmit.truths,x.test$risk)
  readmit.truths<-factor(readmit.truths, levels=1:nlevels(x$risk), labels=levels(x$risk))
  # saving all the prediction in dataframe for further calculation
  readmit.p <- cbind(rf.preds,attr(svm.preds, "probabilities"),gbm.preds,log.preds,x.test$risk)
  readmit.preds<-rbind(readmit.preds,readmit.p)
  
}

saveRDS(readmit.truths,r.t)
saveRDS(readmit.preds,r.p)
