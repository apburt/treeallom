#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(nlreg))

fitNLS <- function(data,multiplicative=TRUE)
{
	if(multiplicative == FALSE)
	{
		func <- y ~ a * X ^ b
		model <- nlreg(func,data=data,start=list(a=0.1,b=1),control=list(x.tol=1e-7,rel.tol=1e-7,step.min=1/5120,maxit=250))
		return(model)
	}
	if(multiplicative == TRUE)
	{
		func <- y ~ a * X ^ b
		model <- nlreg(func,data=data,start=list(a=0.1,b=1,k=1),weights=~X**k,control=list(x.tol=1e-10,rel.tol=1e-10,step.min=1/5120,maxit=250))
		return(model)
	}
}

isMultiplicative <- function(model)
{
	if(length(param(model)) == 3)
	{
		return(FALSE)
	}
	if(length(param(model)) == 4)
	{
		return(TRUE)
	}	
}

yhat <- function(model,data)
{
	a <- coefficients(model)[1]
	b <- coefficients(model)[2]
	y <- a * data$X ^ b
	return(y)
}

residuals <- function(model,data)
{
	yp <- yhat(model,data)
	resids <- data$y - yp
	return(resids)
}

getModelExpForGgplot <- function(model)
{
	a <- toString(round(coefficients(model)[1],3))
	b <- toString(round(coefficients(model)[2],3))		
	return(as.expression(bquote(.(a)~X^{.(b)})))
}
