#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

library(BIOMASS)

yhatBIOMASS <- function(data)
{
	y <- computeAGB(D=data$d*100,H=data$h,WD=data$rho/1000)
	return(y)
}

treeIntervalsBIOMASS <- function(data,alpha)
{
	intervals <- matrix(0,nrow(data),ncol=2)
	uncertainties <- AGBmonteCarlo(D=data$d*100,WD=data$rho/1000,H=data$h,errWD=0.05,errH=0.1,Dpropag=0.1)
	for(i in 1:nrow(data))
	{
	        intervals[i,1] <- quantile(uncertainties$AGB_simu[i,],alpha/2)
	        intervals[i,2] <- quantile(uncertainties$AGB_simu[i,],1-(alpha/2))
	}
	return(intervals)
}

plotIntervalBIOMASS <- function(data)
{
	intervals <- matrix(0,1,ncol=2)
	uncertainties <- AGBmonteCarlo(D=data$d*100,WD=data$rho/1000,H=data$h,errWD=0.05,errH=0.1,Dpropag=0.1)
	intervals[1,1] <- uncertainties$credibilityAGB[[1]]
	intervals[1,2] <- uncertainties$credibilityAGB[[2]]
	return(intervals)
}
