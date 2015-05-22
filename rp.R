args <- commandArgs(trailingOnly = TRUE)
r.t <- args[1] # truth values
r.p <- args[2] # pred probability values

library(caret)
#index <- args[3]


readRDS(r.t) -> readmit.truths
readRDS(r.p) -> readmit.preds

#as.vector(readmit.preds) -> readmit.preds
final_vector <- c()
print(table(readmit.truths))
for(j in seq(0.04,0.20,0.001))   # loop for different thresholds - start from 0.04, goes till 0.2 with gap 0.001
{
  readmit.p <- c()
  message(paste('**********'),j,paste('***********'))
  for(i in 1:length(readmit.preds))
  {
    if(readmit.preds[i] > j)
    {
      readmit.p <- c(readmit.p, 1)
    }
    else
      readmit.p <- c(readmit.p, 0)
  }

  readmit.p <- as.factor(readmit.p)
  readmit.p <- factor(readmit.p,c("0","1"))
  readmit.truths <- factor(readmit.truths, c("0","1"))
  
  
  table(readmit.truths,readmit.p) -> m   # confusion matrix
  
  print(m)
  
  if(ncol(m) == 1 & colnames(m)[1] == 1)  
  {
    print(ncol(m))
    print(colnames(m)[1])
    tn <- 0
    fn <- 0
    fp <- m[1,1]
    tp <- m[2,1]
    
  } else if(ncol(m) == 1 & colnames(m)[1] == 0) {
      print(colnames(m)[1])
      tn <- m[1,1]
      fn <- m[2,1]
      fp <- 0
      tp <- 0
    }  else    {
      tn <- m[1,1]
      fn <- m[2,1]
      fp <- m[1,2]   
      tp <- m[2,2]
    }
  
  recall <- tp/(tp+fn)
  precision <- tp/(tp+fp)
  spc <- tn / (tn + fp)
  FPR <- 1 - spc
  
  n <- table(readmit.truths == readmit.p)
  accuracy <- n[2]/(n[1]+n[2])
  a <- cbind(j,round(recall,4)*100,round(spc,4)*100, round(accuracy,4)*100,round(precision,4)*100,round(FPR,4)*100)
  print(a)
  #print(confusionMatrix(readmit.p,readmit.truths))
  final_vector <- rbind(final_vector,a)
}


print(final_vector)

#write.csv(final_vector,finalvector)
print("end")

