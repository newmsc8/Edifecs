#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name.p <- args[1] #where the data is located
file.name.t <- args[2] #where the data is located

x.preds<-readRDS(file.name.p)
x.truth<-readRDS(file.name.t)

score <- 0
message('score')
for(i in 1:length(x.truth)) {
	#message('start for')
	#message('intersect')
	#message(x.truth[i])
	#message(as.numeric(x.truth[i]))
	#message(x.preds[i])
	#message(as.numeric(x.preds[i]))
	first<-FALSE
	if((as.numeric(x.truth[i])/3) <= 1 ) {
		#message('t<=1')
		if((as.numeric(x.preds[i])/3) <= 1) {
			#message('p<=1')
			first<-TRUE
		}
	} else if((as.numeric(x.truth[i])/3) <= 2) {
		if((as.numeric(x.preds[i])/3) <= 2 && (as.numeric(x.preds[i])/3) > 1) {
			first<-TRUE
		}
     	} else { 
		if((as.numeric(x.preds[i])/3) > 2) {
      			first<-TRUE
		}
     	}
	#message('out')
	#message(as.numeric(x.truth[i])%%3)
	#message(as.numeric(x.preds[i])%%3)
    	if((as.numeric(x.truth[i])%%3) == (as.numeric(x.preds[i])%%3)) {
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
