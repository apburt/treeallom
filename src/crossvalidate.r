#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(caret))

crossValidateOLS <- function(data,func,kfolds=10)
{
	flds <- createFolds(data$agb,k=kfolds,list=FALSE)
	lar <- list()
	results <- matrix(nrow=kfolds,ncol=2)
	for(i in 1:kfolds)
	{
		training <- data[flds != i,]
		test <- data[flds == i,]
		v <- matrix(nrow=nrow(test),ncol=1)
		model <- fitOLS(training,func)
		yhats <- yhatOLSArithmetic(test,model,corrected=TRUE)
		accuracy_ratio <- yhats / test$agb
		log_accuracy_ratio <- log(accuracy_ratio)
		median_symmetric_accuracy <- 100*(exp(median(abs(log_accuracy_ratio)))-1)
		symmetric_signed_percentage_bias <- 100*sign(median(log_accuracy_ratio))*(exp(abs(median(log_accuracy_ratio)))-1)
		#
		lar[[i]] <- log_accuracy_ratio	
		results[i,1] <- median_symmetric_accuracy
		results[i,2] <- symmetric_signed_percentage_bias
	}
	return(list(lar,results))
}
