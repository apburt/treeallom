#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

simulate <- function(data,runs)
{
	func <- log(agb) ~ log(d) + log(h) + log(rho)
	results <- matrix(nrow=runs,ncol=5)
	ddata <- matrix(nrow=nrow(data),ncol=runs)
	hdata <- matrix(nrow=nrow(data),ncol=runs)
	rhodata <- matrix(nrow=nrow(data),ncol=runs)
	for(i in 1:nrow(data))
	{
		ddata[i,] <- rnorm(runs,caldata[i,'d'],0)
		hdata[i,] <- rnorm(runs,caldata[i,'h'],2)
		rhodata[i,] <- rnorm(runs,caldata[i,'rho'],100)
	}
	for(i in 1:runs)
	{
		d <- ddata[,i]
		h <- hdata[,i]
		rho <- rhodata[,i]
		df <- data.frame(d,h,data$agb,rho)
		colnames(df) <- c("d","h","agb","rho")
		model <- fitOLS(df,func)
		results[i,1] <- coefficients(model)[1]
		results[i,2] <- coefficients(model)[2]
		results[i,3] <- coefficients(model)[3]
		results[i,4] <- coefficients(model)[4]	
		results[i,5] <- summary(model)$sigma
	}
#	print(results)
	print(format(round(mean(results[,1]),3),nsmall=3))
	print(format(round(mean(results[,2]),3),nsmall=3))
	print(format(round(mean(results[,3]),3),nsmall=3))
	print(format(round(mean(results[,4]),3),nsmall=3))
	print(format(round(mean(results[,5]),3),nsmall=3))
}
