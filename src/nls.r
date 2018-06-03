#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(nlreg))

fitNLS <- function(data,multiplicative=TRUE,threeparam=FALSE)
{
	if(multiplicative == FALSE && threeparam == FALSE)
	{
		func <- agb ~ a * d2hrho ^ b
		model <- nls(func,data=data,start=list(a=0.1,b=1))
		return(model)
	}
	if(multiplicative == FALSE && threeparam == TRUE)
	{
		func <- agb ~ a * d2hrho ^ b + c
		model <- nls(func,data=data,start=list(a=0.1,b=1,c=10))
		return(model)
	}

	if(multiplicative == TRUE && threeparam == FALSE)
	{
		func <- agb ~ a * d2hrho ^ b
		model <- nlreg(func,data=data,start=list(a=0.1,b=1,k=1),weights=~d2hrho**k)
		return(model)
	}
	if(multiplicative == TRUE && threeparam == TRUE)
	{
		func <- agb ~ a * d2hrho ^ b + c
		model <- nlreg(func,data=data,start=list(a=0.1,b=1,c=10,k=1),weights=~d2hrho**k)
		return(model)
	}
}

isMultiplicative <- function(model)
{
	multiplicative <- TRUE
	tryCatch(param(model),error=function(e) multiplicative <<- FALSE)
	return(multiplicative)
}

isThreeParam <- function(model)
{
	if(length(coefficients(model)) == 3) return(TRUE)
	else return(FALSE)
}

yhat <- function(model,data)
{
	if(isThreeParam(model) == FALSE)
	{
		a <- coefficients(model)[1]
		b <- coefficients(model)[2]
		y <- a * data$d2hrho ^ b
		return(y)
	}
	if(isThreeParam(model) == TRUE)
	{
		a <- coefficients(model)[1]
		b <- coefficients(model)[2]
		c <- coefficients(model)[3]
		y <- a * data$d2hrho ^ b + c
		return(y)
	}
}

residuals <- function(model,data)
{
	yp <- yhat(model,data)
	resids <- data$agb - yp
	return(resids)
}

getModelExpForGgplot <- function(model)
{
	a <- toString(round(coefficients(model)[1],3))
	b <- toString(round(coefficients(model)[2],3))
	if(isThreeParam(model) == FALSE)
	{		
		return(as.expression(bquote(.(a)~D^{2}*H*rho^{.(b)})))
	}
	if(isThreeParam(model) == TRUE)
	{
		c <- toString(round(coefficients(model)[3],3))
		return(as.expression(bquote(.(a)~D^{2}*H*rho^{.(b)}~"+"~.(c))))
	}
}
