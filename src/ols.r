#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

fitOLS <- function(data)
{
	model <- lm(formula=log(agb)~log(d2hrho),data=data)
	return(model)
}

yhatOLS <- function(model,data)
{
	a <- coefficients(model)[1]
	b <- coefficients(model)[2]
	y <- exp(a) * exp(summary(model)$sigma^2 / 2) * data$d2hrho ^ b
	return(y)
}

measUncertOLS <- function()
{
	du <- 0.05
	hu <- 0.1
	rhou <- 0.1
	ccu <- 0.8
	###
	alpha <- 2
	bravo <- 1
	delta <- 1
	t1 = alpha^2 * du^2
	t2 = bravo^2 * hu^2
	t3 = delta^2 * rhou^2
	t4 = 2 * alpha * bravo * (1-ccu) * (t1 + t2)
	mu <- sqrt(t1 + t2 + t3 + t4)
	return(mu) 
}
