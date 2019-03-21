#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"olsBIOMASS.r",sep=""))
source(paste(path_to_src,"nls.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"measuncert.r",sep=""))
source(paste(path_to_src,"fieldresults.r",sep=""))

cdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
caldata <- data.frame(cdata$d*cdata$d*cdata$h*cdata$rho,cdata$agb)
colnames(caldata) <- c("X","y")

alpha <- as.numeric(args[3])
runs <- as.numeric(args[4])
nn <- as.integer(length(caldata$X) * as.numeric(args[5]))

model <- fitNLS(caldata)
nlqrmodels <- nlQuantileRegression(model,caldata,alpha)
bootmodels <- bootstrap(model,caldata,runs)

results <- list()
n <- 1
for(i in 6:length(args))
{
	fielddata <- read.table(args[i],col.names=c("id","x","y","d","h","rho"))
	tmp1 <- strsplit(args[i],'/')
	tmp2 <- tmp1[[1]][length(tmp1[[1]])]
	tmp3 <- strsplit(tmp2,'\\.')
	pname <- tmp3[[1]][1]
	#
	fdata <- data.frame(fielddata$d*fielddata$d*fielddata$h*fielddata$rho)
	colnames(fdata) <- c("X")
	stem_count <- nrow(fielddata) 
	basal_area <- sum((fielddata$d / 2) ^ 2 * pi)
	lorey_height <- sum(((fielddata$d / 2) ^ 2 * pi) * fielddata$h ) / sum((fielddata$d / 2) ^ 2 * pi)
	wd_baw <- sum((fielddata$d / 2) ^ 2 * pi * fielddata$rho ) / sum((fielddata$d / 2) ^ 2 * pi)
	###ols
	ols_t_agb <- yhatBIOMASS(fielddata) * 1000
	ols_t_agb_li <- treeIntervalsBIOMASS(fielddata,alpha)[,1] * 1000
	ols_t_agb_ui <- treeIntervalsBIOMASS(fielddata,alpha)[,2] * 1000
	ols_t_u <- 0.5 * (ols_t_agb_ui - ols_t_agb_li) / ols_t_agb 
	ols_p_agb <- sum(ols_t_agb)
	ols_p_agb_li <- plotIntervalBIOMASS(fielddata)[[1]] * 1000
	ols_p_agb_ui <- plotIntervalBIOMASS(fielddata)[[2]] * 1000
	ols_p_u <- 0.5 * (ols_p_agb_ui - ols_p_agb_li) / ols_p_agb 
	#
	nls1_sigma_meas <- measSigma(model,fielddata)
	nls1_t_agb <- yhat(model,fdata)
	nls1_t_agb_ali <- getPredictionIntervals(nlqrmodels,fdata)[,1]
	nls1_t_agb_aui <- getPredictionIntervals(nlqrmodels,fdata)[,2]
	nls1_t_agb_mli <- nls1_t_agb - nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
	nls1_t_agb_mui <- nls1_t_agb + nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
	nls1_t_agb_li <- nls1_t_agb_ali - nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
	nls1_t_agb_ui <- nls1_t_agb_aui + nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
	nls1_t_u_a <- 0.5 * (nls1_t_agb_aui - nls1_t_agb_ali) / nls1_t_agb
	nls1_t_u_m <- 0.5 * (nls1_t_agb_mui - nls1_t_agb_mli) / nls1_t_agb
	nls1_t_u <- 0.5 * (nls1_t_agb_ui - nls1_t_agb_li) / nls1_t_agb
	#
	nls1_p_agb <- sum(nls1_t_agb)
	nls1_p_agb_ali <- sum(getConfidenceIntervals(bootmodels,fdata,alpha)[,1])
	nls1_p_agb_aui <- sum(getConfidenceIntervals(bootmodels,fdata,alpha)[,2])
	nls1_p_agb_mli <- nls1_p_agb - (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
	nls1_p_agb_mui <- nls1_p_agb + (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
	nls1_p_agb_li <- nls1_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
	nls1_p_agb_ui <- nls1_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
	nls1_p_u_a <- 0.5 * (nls1_p_agb_aui - nls1_p_agb_ali) / nls1_p_agb
	nls1_p_u_m <- 0.5 * (nls1_p_agb_mui - nls1_p_agb_mli) / nls1_p_agb
	nls1_p_u <- 0.5 * (nls1_p_agb_ui - nls1_p_agb_li) / nls1_p_agb
	#
	nls2_t_agb <- numeric(nrow(fdata))
	nls2_t_agb_ali <- numeric(nrow(fdata))
	nls2_t_agb_aui <- numeric(nrow(fdata))
	nls2_t_agb_mli <- numeric(nrow(fdata))
	nls2_t_agb_mui <- numeric(nrow(fdata))
	nls2_t_agb_li <- numeric(nrow(fdata))
	nls2_t_agb_ui <- numeric(nrow(fdata))
	nls2_t_u_a <- numeric(nrow(fdata))
	nls2_t_u_m <- numeric(nrow(fdata))
	nls2_t_u <- numeric(nrow(fdata))
	#
	nls2_p_agb <- 0
	nls2_p_agb_ali <- 0       
	nls2_p_agb_aui <- 0
	nls2_p_agb_mli <- 0       
	nls2_p_agb_mui <- 0
	nls2_p_agb_li <- 0
	nls2_p_agb_ui <- 0
	#
	for(j in 1:nrow(fdata))
	{
		segcaldata <- head(caldata[order(abs(caldata$X-fdata$X[j])),],nn)
		data <- data.frame(fdata$X[j])
		colnames(data) <- c("X")
		meas <- data.frame(fielddata[j,])
		colnames(meas) <- c("id","x","y","d","h","rho")
		#
		segmodel <- fitNLS(segcaldata)
		segnlqrmodels <- nlQuantileRegression(segmodel,segcaldata,alpha)
		segbootmodels <- bootstrap(model,segcaldata,runs)
		#
		nls2_sigma_meas <- measSigma(segmodel,meas)
		nls2_t_agb[j] <- yhat(segmodel,data)
		nls2_t_agb_ali[j] <- getPredictionIntervals(segnlqrmodels,data)[,1]
		nls2_t_agb_aui[j] <- getPredictionIntervals(segnlqrmodels,data)[,2]
		nls2_t_agb_mli[j] <- nls2_t_agb[j] - nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_mui[j] <- nls2_t_agb[j] + nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_li[j] <- nls2_t_agb_ali[j] - nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_ui[j] <- nls2_t_agb_aui[j] + nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_u_a[j] <- 0.5 * (nls2_t_agb_aui[j] - nls2_t_agb_ali[j]) / nls2_t_agb[j]
		nls2_t_u_m[j] <- 0.5 * (nls2_t_agb_mui[j] - nls2_t_agb_mli[j]) / nls2_t_agb[j]
		nls2_t_u[j] <- 0.5 * (nls2_t_agb_ui[j] - nls2_t_agb_li[j]) / nls2_t_agb[j]
		#
		nls2_p_agb <- nls2_p_agb + nls2_t_agb[j]
		nls2_p_agb_ali <- nls2_p_agb_ali + getConfidenceIntervals(segbootmodels,data,alpha)[,1] 
		nls2_p_agb_aui <- nls2_p_agb_aui + getConfidenceIntervals(segbootmodels,data,alpha)[,2]
		nls2_p_agb_mli <- nls2_p_agb_mli + nls2_t_agb[j] - (1/sqrt(nrow(fdata)) * nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE))
		nls2_p_agb_mui <- nls2_p_agb_mui + nls2_t_agb[j] + (1/sqrt(nrow(fdata)) * nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE))
		nls2_p_agb_li <- nls2_p_agb_li + getConfidenceIntervals(segbootmodels,data,alpha)[,1] - (1/sqrt(nrow(fdata)) * nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE))
		nls2_p_agb_ui <- nls2_p_agb_ui + getConfidenceIntervals(segbootmodels,data,alpha)[,2] + (1/sqrt(nrow(fdata)) * nls2_sigma_meas[[1]] * qnorm(alpha/2,lower.tail=FALSE))
	}
	nls2_p_u_a <- 0.5 * (nls2_p_agb_aui - nls2_p_agb_ali) / nls2_p_agb
	nls2_p_u_m <- 0.5 * (nls2_p_agb_mui - nls2_p_agb_mli) / nls2_p_agb
	nls2_p_u <- 0.5 * (nls2_p_agb_ui - nls2_p_agb_li) / nls2_p_agb
	#
	###sort tree results	
	tr <- data.frame(pname,fielddata$id,fielddata$x,fielddata$y,round(fielddata$d,3),round(fielddata$h,3),round(fielddata$rho,3),
	round(ols_t_agb,3),round(ols_t_agb_li,3),round(ols_t_agb_ui,3),round(ols_t_u,3),
	round(nls1_t_agb,3),round(nls1_t_agb_ali,3),round(nls1_t_agb_aui,3),round(nls1_t_agb_mli,3),round(nls1_t_agb_mui,3),round(nls1_t_agb_li,3),round(nls1_t_agb_ui,3),round(nls1_t_u_a,3),round(nls1_t_u_m,3),round(nls1_t_u,3),
	round(nls2_t_agb,3),round(nls2_t_agb_ali,3),round(nls2_t_agb_aui,3),round(nls2_t_agb_mli,3),round(nls2_t_agb_mui,3),round(nls2_t_agb_li,3),round(nls2_t_agb_ui,3),round(nls2_t_u_a,3),round(nls2_t_u_m,3),round(nls2_t_u,3))
	colnames(tr) <- c("pid","tid","x","y","d","h","rho",
	"ols_t_agb","ols_t_agb_li","ols_t_agb_ui","ols_t_u",
	"nls1_t_agb","nls1_t_agb_ali","nls1_t_agb_aui","nls1_t_agb_mli","nls1_t_agb_mui","nls1_t_agb_li","nls1_t_agb_ui","nls1_t_u_a","nls1_t_u_m","nls1_t_u",
	"nls2_t_agb","nls2_t_agb_ali","nls2_t_agb_aui","nls2_t_agb_mli","nls2_t_agb_mui","nls2_t_agb_li","nls2_t_agb_ui","nls2_t_u_a","nls2_t_u_m","nls2_t_u")
	###sort plot results
	pr <- data.frame(pname,round(stem_count,0),round(basal_area,3),round(lorey_height,3),round(wd_baw,3),
	round(ols_p_agb,3),round(ols_p_agb_li,3),round(ols_p_agb_ui,3),round(ols_p_u,3),
	round(nls1_p_agb,3),round(nls1_p_agb_ali,3),round(nls1_p_agb_aui,3),round(nls1_p_agb_mli,3),round(nls1_p_agb_mui,3),round(nls1_p_agb_li,3),round(nls1_p_agb_ui,3),round(nls1_p_u_a,3),round(nls1_p_u_m,3),round(nls1_p_u,3),
	round(nls2_p_agb,3),round(nls2_p_agb_ali,3),round(nls2_p_agb_aui,3),round(nls2_p_agb_mli,3),round(nls2_p_agb_mui,3),round(nls2_p_agb_li,3),round(nls2_p_agb_ui,3),round(nls2_p_u_a,3),round(nls2_p_u_m,3),round(nls2_p_u,3))
	colnames(pr) <- c("pid","sc","ba","lh","wd_baw",
	"ols_p_agb","ols_p_agb_li","ols_p_agb_ui","ols_p_u",
	"nls1_p_agb","nls1_p_agb_ali","nls1_p_agb_aui","nls1_p_agb_mli","nls1_p_agb_mui","nls1_p_agb_li","nls1_p_agb_ui","nls1_p_u_a","nls1_p_u_m","nls1_p_u",
	"nls2_p_agb","nls2_p_agb_ali","nls2_p_agb_aui","nls2_p_agb_mli","nls2_p_agb_mui","nls2_p_agb_li","nls2_p_agb_ui","nls2_p_u_a","nls2_p_u_m","nls2_p_u")
	###
	results[[n]] <- tr
	n <- n+1
	results[[n]] <- pr
	n <- n+1
}
writeResults(results)
