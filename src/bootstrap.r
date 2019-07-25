#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(boot))

boot_OLS_func <- function(data,indices,func,pred)
{
	dat <- data[indices,]
	model <- fitOLS(dat,func)
	if(is.null(pred)) return(coefficients(model))
	else
	{
		yhat <- yhatOLSArithmetic(pred,model)
		return(c(coefficients(model),yhat))
	}
}

bootOLS <- function(caldata,func,runs,ncpus,pred=NULL)
{
	results <- boot(data=caldata,statistic=boot_OLS_func,R=runs,parallel="multicore",ncpus=ncpus,func=func,pred=pred)
	return(results)
}
