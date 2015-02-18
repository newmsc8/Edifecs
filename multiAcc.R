#multiAcc.R -- R Script to report Accuracy for MultiLabel Prediction

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name.p <- args[1] #where the data is located
file.name.t <- args[2] #where the data is located

#get data, make truth charater strings for comparison
x.preds<-readRDS(file.name.p)
x.preds<-as.character(x.preds)
x.truth<-readRDS(file.name.t)
x.truth<-as.character(x.truth)

score <- 0
message('score')
for(i in 1:length(x.truth)) {

	intersect.labels<-union.labels<-2

	truth.risk<-unlist(strsplit(x.truth[i],' '))[1]
	truth.cost<-unlist(strsplit(x.truth[i],' '))[2]
	pred.risk<-unlist(strsplit(x.preds[i],' '))[1]
	pred.cost<-unlist(strsplit(x.preds[i],' '))[2]

	if(truth.risk!=pred.risk) {
		#give credit for correct risk prediction
		intersect.labels<-intersect.labels-1
		union.labels<-union.labels+1
	}
    	if(truth.cost!=pred.cost) {
		#give credit for correct cost prediction
      		intersect.labels<-intersect.labels-1
      		union.labels<-union.labels+1
    	}

	#increase score
    	score<-score+(intersect.labels/union.labels)
}
#final accuracy
message(score/length(x.truth))


