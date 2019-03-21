#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

measSigma <- function(model,data,sigmad=0.05,sigmah=0.1,sigmar=0.1,runs=10000)
{
	sigma_m <- matrix(0,nrow=nrow(data),ncol=1)
	for(i in 1:nrow(data))
	{
		ddist <- rnorm(runs,data[i,'d'],data[i,'d']*sigmad)
		hdist <- rnorm(runs,data[i,'h'],data[i,'h']*sigmah)
		rdist <- rnorm(runs,data[i,'rho'],data[i,'rho']*sigmar)
		regressor <- data.frame(ddist*ddist*hdist*rdist)
		colnames(regressor) <- c("X")
		ystar <- yhat(model,regressor)
		tmp <- data.frame(data[i,'d']*data[i,'d']*data[i,'h']*data[i,'rho'])
		colnames(tmp) <- c("X")
		yhat <- yhat(model,tmp)
		residuals <- (ystar-yhat)^2
		sigma <- sqrt(sum(residuals)/runs)
		sigma_m[[i]] <- sigma
	}
	return(sigma_m)
}
