#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

bootstrap <- function(model,data,runs=10000)
{
	bmodels <- vector(mode="list")
	count <- 1
	for(i in 1:runs)
	{
		rademacher <- sample(0:1,nrow(data),replace=TRUE) * 2 -1
		mammen <- sample(c(-(sqrt(5)-1)/2,(sqrt(5)+1)/2),nrow(data),prob=c((sqrt(5) + 1)/(2*sqrt(5)),(sqrt(5)-1)/(2*sqrt(5))),replace=TRUE)
		ystar <- yhat(model,data) + (residuals(model,data) * mammen)
		obs <- sample(5:nrow(data),1)
		idx <- sample(1:nrow(data),obs,replace=TRUE)
		rdata <- matrix(0,nrow=obs,ncol=2)
		for(j in 1:obs)
		{
			#rdata[j,] = c(data[idx[j],1],data[idx[j],2])
			rdata[j,] = c(data[idx[j],1],ystar[idx[j]])
		}
		rdata <- data.frame(rdata)
		colnames(rdata) <- c("X","y")
		#this is probably not a great idea ...
		tryCatch(
				{
					bmodel <- fitNLS(rdata)
					bmodels[[count]] <- bmodel
					count <- count + 1
				},
				error=function(e)
				{
					print("Error in generating bootstrap sample, but continuing anyway...")
				}
			)
	}
	return(bmodels)
}

getConfidenceIntervals <- function(bmodels,data,alpha=0.05)
{
	ystars <- matrix(0,ncol=length(bmodels),nrow=nrow(data))
	for(i in 1:length(bmodels))
	{
		y <- yhat(bmodels[[i]],data)
		for(j in 1:nrow(data))
		{
			ystars[j,i] <- y[j]
		}
	}
	intervals <- matrix(0,ncol=2,nrow=nrow(data))
	for(k in 1:nrow(ystars))
	{
		intervals[k,1] <- quantile(ystars[k,],alpha/2)
		intervals[k,2] <- quantile(ystars[k,],1-(alpha/2))
	}
	return(intervals)
}
