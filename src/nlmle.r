#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(nlreg))

fitNLMLE <- function(data,tol=1e-10,reltol=1e-10,stepmin=1/5120,maxitr=250)
{
	func <- y ~ a * X ^ b
	model <- nlreg(func,data=data,start=list(a=0.1,b=1,k=1),weights=~X**k,control=list(x.tol=tol,rel.tol=reltol,step.min=stepmin,maxit=maxitr))
	return(model)
}

yhatNLMLE <- function(data,model)
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
        b0 <- toString(round(param(model)[1],3))
        b1 <- toString(round(param(model)[2],3))
        k <- toString(round(param(model)[3],3))
        sigma <- toString(max(round(exp(param(model)[4]),3),0.001))
	#return(as.expression(bquote(.(b0)~X^{.(b1)}*","~sigma^{2}==.(sigma)*","~k==.(k))))
        return(as.expression(bquote(beta["0"]==.(b0)*","~beta["1"]==.(b1)*","~sigma^{2}==.(sigma)*","~k==.(k))))
}
