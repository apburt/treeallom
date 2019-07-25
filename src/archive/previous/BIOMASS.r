#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

library(BIOMASS)

yhatBIOMASS <- function(data)
{
	y <- computeAGB(D=data$d*100,H=data$h,WD=data$rho/1000)
	return(y)
}

uncertaintyIntervalsBIOMASS <- function(data,alpha)
{
	intervals <- list()
	uncertainties <- AGBmonteCarlo(D=data$d*100,WD=data$rho/1000,H=data$h,Dpropag=0.05,errWD=0.1,errH=0.1)
	treeintervals <- matrix(0,nrow(data),ncol=2)
	for(i in 1:nrow(data))
	{
		treeintervals[i,1] <- quantile(uncertainties$AGB_simu[i,],alpha/2)
		treeintervals[i,2] <- quantile(uncertainties$AGB_simu[i,],1-(alpha/2))
	}
	intervals[[1]] <- treeintervals
	plotintervals <- matrix(0,1,ncol=2)
	plotintervals[1,1] <- uncertainties$credibilityAGB[[1]]
	plotintervals[1,2] <- uncertainties$credibilityAGB[[2]]
	intervals[[2]] <- plotintervals
	return(intervals)
}
