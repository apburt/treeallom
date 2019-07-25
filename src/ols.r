#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(lmtest))
suppressMessages(library(nortest))

fitOLS <- function(data,func)
{
	return(lm(func,data))
}

yhatOLSLog <- function(data,model)
{
	return(predict.lm(model,data))
}

yhatOLSArithmetic <- function(data,model,corrected=TRUE)
{
	if(corrected == TRUE) return(exp(summary(model)$sigma**2/2) * exp(predict.lm(model,data)))
	else return(exp(predict.lm(model,data)))
}

rainbow_test <- function(model)
{
	print(raintest(model))
}

studentised_breusch_pagan_test <- function(data,func,model)
{
	print(bptest(model,func,data=data))
}

studentised_white_test <- function(data,func,model)
{
	funcw <- toString(c(func))
	params <- strsplit(strsplit((toString(c(func))),"~")[[1]][2],"+",fixed=TRUE)	
	for(i in 1:length(params[[1]]))
	{
		funcw <- paste(funcw," + I(",gsub(" ","",params[[1]][i], fixed = TRUE),"**2)",sep="")
	}
	print(bptest(model,formula(funcw),data=data))
}

anderson_darling_test <- function(model)
{
	print(ad.test(resid(model)))
}

shapiro_wilk_test <- function(model)
{
	print(shapiro.test(resid(model)))
}
