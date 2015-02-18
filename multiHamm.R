#multiHamm.R -- R Script to report Hamming Loss for MultiLabel Prediction

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name.p <- args[1] #where the data is located
file.name.t <- args[2] #where the data is located

x.preds<-readRDS(file.name.p)
x.preds<-as.character(x.preds)
x.truth<-readRDS(file.name.t)
x.truth<-as.character(x.truth)

score <- 0
message('score')
for(i in 1:length(x.truth)) {
	first<-FALSE

	truth.risk<-unlist(strsplit(x.truth[i],' '))[1]
	truth.cost<-unlist(strsplit(x.truth[i],' '))[2]
	pred.risk<-unlist(strsplit(x.preds[i],' '))[1]
	pred.cost<-unlist(strsplit(x.preds[i],' '))[2]

	if(truth.risk==pred.risk) {
		first<-TRUE
	}
    	if(truth.cost==pred.cost) {
		if(first!=TRUE) {
			score<-score+0.5
		}
    	} else {
		if(first==TRUE) {
			score<-score+0.5
		} else {
			score<-score+1
		}
	}
}
message(score/length(x.truth))
