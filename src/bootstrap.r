#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(boot))

boot_OLS_func <- function(data,indices,func)
{
	dat <- data[indices,]
	model <- fitOLS(dat,func)
	return(coefficients(model))
}

bootOLS <- function(caldata,func,runs,ncpus,pred=NULL)
{
	results <- boot(data=caldata,statistic=boot_OLS_func,R=runs,parallel="multicore",ncpus=ncpus,func=func)
	return(results)
}
