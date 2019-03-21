#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

getFieldResults <- function(caldata,ffname,alpha,runs,localreg,nn=200)
{
	model <- fitNLS(caldata)
	nlqrmodels <- nlQuantileRegression(model,caldata,alpha)
	bmodels <- bootstrap(model,caldata,runs)
	results <- list()
	j <- 1
	for(i in 1:length(ffname))
	{
		###field data
		fielddata <- read.table(ffname[i],col.names=c("id","x","y","d","h","rho"))
		tmp1 <- strsplit(ffname[i],'/')
		tmp2 <- tmp1[[1]][length(tmp1[[1]])]
		tmp3 <- strsplit(tmp2,'\\.')
		pname <- tmp3[[1]][1]
		##filter	
		#fielddata <- fielddata[fielddata$d >= 0.9,]
		##
		fdata <- data.frame(fielddata$d*fielddata$d*fielddata$h*fielddata$rho)
		colnames(fdata) <- c("X")
		stem_count <- nrow(fielddata) 
		basal_area <- sum((fielddata$d / 2) ^ 2 * pi)
		lorey_height <- sum(((fielddata$d / 2) ^ 2 * pi) * fielddata$h ) / sum((fielddata$d / 2) ^ 2 * pi)
		wd_baw <- sum((fielddata$d / 2) ^ 2 * pi * fielddata$rho ) / sum((fielddata$d / 2) ^ 2 * pi)
		###ols
		ols_t_agb <- yhatBIOMASS(fielddata) * 1000
		ols_intervals <- uncertaintyIntervalsBIOMASS(fielddata,0.05)
		ols_t_agb_li <- ols_intervals[[1]][,1] * 1000
		ols_t_agb_ui <- ols_intervals[[1]][,2] * 1000
		ols_t_u <- 0.5 * (ols_t_agb_ui - ols_t_agb_li) / ols_t_agb 
		ols_p_agb <- sum(ols_t_agb)
		ols_p_agb_li <- ols_intervals[[2]][1] * 1000
		ols_p_agb_ui <- ols_intervals[[2]][2] * 1000
		ols_p_u <- 0.5 * (ols_p_agb_ui - ols_p_agb_li) / ols_p_agb 
		###nls
		nlmle_sigma_meas <- measSigma(model,fielddata)
		nlmle_t_agb <- yhat(model,fdata)
		nlmle_t_agb_ali <- getPredictionIntervals(nlqrmodels,fdata)[,1]
		nlmle_t_agb_aui <- getPredictionIntervals(nlqrmodels,fdata)[,2]
		nlmle_t_agb_mli <- nlmle_t_agb - (nlmle_sigma_meas * qnorm(alpha/2,lower.tail=FALSE))
		nlmle_t_agb_mui <- nlmle_t_agb + (nlmle_sigma_meas * qnorm(alpha/2,lower.tail=FALSE))
		nlmle_t_agb_li <- nlmle_t_agb_ali - (nlmle_sigma_meas * qnorm(alpha/2,lower.tail=FALSE))
		nlmle_t_agb_ui <- nlmle_t_agb_aui + (nlmle_sigma_meas * qnorm(alpha/2,lower.tail=FALSE))
		nlmle_t_u_a <- 0.5 * (nlmle_t_agb_aui - nlmle_t_agb_ali) / nlmle_t_agb
		nlmle_t_u_m <- 0.5 * (nlmle_t_agb_mui - nlmle_t_agb_mli) / nlmle_t_agb
		nlmle_t_u <- 0.5 * (nlmle_t_agb_ui - nlmle_t_agb_li) / nlmle_t_agb
		#
		nlmle_p_agb <- sum(nlmle_t_agb)
		nlmle_p_agb_ali <- sum(getConfidenceIntervals(bmodels,fdata,alpha)[,1])
		nlmle_p_agb_aui <- sum(getConfidenceIntervals(bmodels,fdata,alpha)[,2])
		nlmle_p_agb_mli <- nlmle_p_agb - (1/sqrt(nrow(fdata)) * sum(nlmle_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nlmle_p_agb_mui <- nlmle_p_agb + (1/sqrt(nrow(fdata)) * sum(nlmle_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nlmle_p_agb_li <- nlmle_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nlmle_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nlmle_p_agb_ui <- nlmle_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nlmle_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nlmle_p_u_a <- 0.5 * (nlmle_p_agb_aui - nlmle_p_agb_ali) / nlmle_p_agb
		nlmle_p_u_m <- 0.5 * (nlmle_p_agb_mui - nlmle_p_agb_mli) / nlmle_p_agb
		nlmle_p_u <- 0.5 * (nlmle_p_agb_ui - nlmle_p_agb_li) / nlmle_p_agb
		#	
		if(localreg == FALSE)
		{
			###sort tree results	
			tr <- data.frame(pname,fielddata$id,fielddata$x,fielddata$y,round(fielddata$d,3),round(fielddata$h,3),round(fielddata$rho,3),
			round(ols_t_agb,3),round(ols_t_agb_li,3),round(ols_t_agb_ui,3),round(ols_t_u,3),
			round(nlmle_t_agb,3),round(nlmle_t_agb_ali,3),round(nlmle_t_agb_aui,3),round(nlmle_t_agb_mli,3),round(nlmle_t_agb_mui,3),round(nlmle_t_agb_li,3),round(nlmle_t_agb_ui,3),round(nlmle_t_u_a,3),round(nlmle_t_u_m,3),round(nlmle_t_u,3))
			colnames(tr) <- c("pid","tid","x","y","d","h","rho",
			"ols_t_agb","ols_t_agb_li","ols_t_agb_ui","ols_t_u",
			"nlmle_t_agb","nlmle_t_agb_ali","nlmle_t_agb_aui","nlmle_t_agb_mli","nlmle_t_agb_mui","nlmle_t_agb_li","nlmle_t_agb_ui","nlmle_t_u_a","nlmle_t_u_m","nlmle_t_u")
			###sort plot results
			pr <- data.frame(pname,round(stem_count,0),round(basal_area,3),round(lorey_height,3),round(wd_baw,3),
			round(ols_p_agb,3),round(ols_p_agb_li,3),round(ols_p_agb_ui,3),round(ols_p_u,3),
			round(nlmle_p_agb,3),round(nlmle_p_agb_ali,3),round(nlmle_p_agb_aui,3),round(nlmle_p_agb_mli,3),round(nlmle_p_agb_mui,3),round(nlmle_p_agb_li,3),round(nlmle_p_agb_ui,3),round(nlmle_p_u_a,3),round(nlmle_p_u_m,3),round(nlmle_p_u,3))
			colnames(pr) <- c("pid","sc","ba","lh","wd_baw",
			"ols_p_agb","ols_p_agb_li","ols_p_agb_ui","ols_p_u",
			"nlmle_p_agb","nlmle_p_agb_ali","nlmle_p_agb_aui","nlmle_p_agb_mli","nlmle_p_agb_mui","nlmle_p_agb_li","nlmle_p_agb_ui","nlmle_p_u_a","nlmle_p_u_m","nlmle_p_u")
			###
			results[[j]] <- tr
			j <- j+1
			results[[j]] <- pr
			j <- j+1
		}
		if(localreg == TRUE)
		{
			lnlmle_t_agb <- numeric(nrow(fdata))
			lnlmle_t_agb_ali <- numeric(nrow(fdata))
			lnlmle_t_agb_aui <- numeric(nrow(fdata))
			lnlmle_t_agb_mli <- numeric(nrow(fdata))
			lnlmle_t_agb_mui <- numeric(nrow(fdata))
			lnlmle_t_agb_li <- numeric(nrow(fdata))
			lnlmle_t_agb_ui <- numeric(nrow(fdata))
			lnlmle_t_u_a <- numeric(nrow(fdata))
			lnlmle_t_u_m <- numeric(nrow(fdata))
			lnlmle_t_u <- numeric(nrow(fdata))
			lnlmle_p_agb <- 0
			lnlmle_p_agb_ali <- 0
			lnlmle_p_agb_aui <- 0
			lnlmle_p_agb_mli <- 0
			lnlmle_p_agb_mui <- 0
			lnlmle_p_agb_li <- 0
			lnlmle_p_agb_ui <- 0
			for(k in 1:nrow(fielddata))
			{
				lcaldata <- head(caldata[order(abs(caldata$X-fdata$X[k])),],nn)
				lfielddata <- data.frame(fielddata[k,])
				lfdata <- data.frame(fdata$X[k])
				colnames(lfdata) <- c("X")
				lmodel <- fitNLS(lcaldata)
				lnlqrmodels <- nlQuantileRegression(lmodel,lcaldata,alpha)
				lbmodels <- bootstrap(lmodel,lcaldata,runs)
				lnlmle_t_agb[k] <- yhat(lmodel,lfdata)
				lnlmle_t_agb_ali[k] <- getPredictionIntervals(lnlqrmodels,lfdata)[,1]
				lnlmle_t_agb_aui[k] <- getPredictionIntervals(lnlqrmodels,lfdata)[,2]
				lnlmle_t_agb_mli[k] <- lnlmle_t_agb[k] - (nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_t_agb_mui[k] <- lnlmle_t_agb[k] + (nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_t_agb_li[k] <- lnlmle_t_agb_ali[k] - (nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_t_agb_ui[k] <- lnlmle_t_agb_aui[k] + (nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_t_u_a[k] <- 0.5 * (lnlmle_t_agb_aui[k] - lnlmle_t_agb_ali[k]) / lnlmle_t_agb[k]
				lnlmle_t_u_m[k] <- 0.5 * (lnlmle_t_agb_mui[k] - lnlmle_t_agb_mli[k]) / lnlmle_t_agb[k]
				lnlmle_t_u[k] <- 0.5 * (lnlmle_t_agb_ui[k] - lnlmle_t_agb_li[k]) / lnlmle_t_agb[k]
				#
				lnlmle_p_agb <- lnlmle_p_agb + lnlmle_t_agb[k]
				lnlmle_p_agb_ali <- lnlmle_p_agb_ali + getConfidenceIntervals(lbmodels,lfdata,alpha)[,1]
				lnlmle_p_agb_aui <- lnlmle_p_agb_aui + getConfidenceIntervals(lbmodels,lfdata,alpha)[,2]
				lnlmle_p_agb_mli <- lnlmle_p_agb_mli + lnlmle_t_agb[k] - (1/sqrt(nrow(fdata)) * nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_p_agb_mui <- lnlmle_p_agb_mui + lnlmle_t_agb[k] + (1/sqrt(nrow(fdata)) * nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_p_agb_li <- lnlmle_p_agb_li + getConfidenceIntervals(lbmodels,lfdata,alpha)[,1] - (1/sqrt(nrow(fdata)) * nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
				lnlmle_p_agb_ui <- lnlmle_p_agb_ui + getConfidenceIntervals(lbmodels,lfdata,alpha)[,2] + (1/sqrt(nrow(fdata)) * nlmle_sigma_meas[k] * qnorm(alpha/2,lower.tail=FALSE))
			}
			lnlmle_p_u_a <- 0.5 * (lnlmle_p_agb_aui - lnlmle_p_agb_ali) / lnlmle_p_agb
			lnlmle_p_u_m <- 0.5 * (lnlmle_p_agb_mui - lnlmle_p_agb_mli) / lnlmle_p_agb
			lnlmle_p_u <- 0.5 * (lnlmle_p_agb_ui - lnlmle_p_agb_li) / lnlmle_p_agb
			###sort tree results    
			tr <- data.frame(pname,fielddata$id,fielddata$x,fielddata$y,round(fielddata$d,3),round(fielddata$h,3),round(fielddata$rho,3),
			round(ols_t_agb,3),round(ols_t_agb_li,3),round(ols_t_agb_ui,3),round(ols_t_u,3),
			round(nlmle_t_agb,3),round(nlmle_t_agb_ali,3),round(nlmle_t_agb_aui,3),round(nlmle_t_agb_mli,3),round(nlmle_t_agb_mui,3),round(nlmle_t_agb_li,3),round(nlmle_t_agb_ui,3),round(nlmle_t_u_a,3),round(nlmle_t_u_m,3),round(nlmle_t_u,3),
			round(lnlmle_t_agb,3),round(lnlmle_t_agb_ali,3),round(lnlmle_t_agb_aui,3),round(lnlmle_t_agb_mli,3),round(lnlmle_t_agb_mui,3),round(lnlmle_t_agb_li,3),round(lnlmle_t_agb_ui,3),round(lnlmle_t_u_a,3),round(lnlmle_t_u_m,3),round(lnlmle_t_u,3))
			colnames(tr) <- c("pid","tid","x","y","d","h","rho",
			"ols_t_agb","ols_t_agb_li","ols_t_agb_ui","ols_t_u",
			"nlmle_t_agb","nlmle_t_agb_ali","nlmle_t_agb_aui","nlmle_t_agb_mli","nlmle_t_agb_mui","nlmle_t_agb_li","nlmle_t_agb_ui","nlmle_t_u_a","nlmle_t_u_m","nlmle_t_u",
			"lnlmle_t_agb","lnlmle_t_agb_ali","lnlmle_t_agb_aui","lnlmle_t_agb_mli","lnlmle_t_agb_mui","lnlmle_t_agb_li","lnlmle_t_agb_ui","lnlmle_t_u_a","lnlmle_t_u_m","lnlmle_t_u")
			###sort plot results
			pr <- data.frame(pname,round(stem_count,0),round(basal_area,3),round(lorey_height,3),round(wd_baw,3),
			round(ols_p_agb,3),round(ols_p_agb_li,3),round(ols_p_agb_ui,3),round(ols_p_u,3),
			round(nlmle_p_agb,3),round(nlmle_p_agb_ali,3),round(nlmle_p_agb_aui,3),round(nlmle_p_agb_mli,3),round(nlmle_p_agb_mui,3),round(nlmle_p_agb_li,3),round(nlmle_p_agb_ui,3),round(nlmle_p_u_a,3),round(nlmle_p_u_m,3),round(nlmle_p_u,3),
			round(lnlmle_p_agb,3),round(lnlmle_p_agb_ali,3),round(lnlmle_p_agb_aui,3),round(lnlmle_p_agb_mli,3),round(lnlmle_p_agb_mui,3),round(lnlmle_p_agb_li,3),round(lnlmle_p_agb_ui,3),round(lnlmle_p_u_a,3),round(lnlmle_p_u_m,3),round(lnlmle_p_u,3))
			colnames(pr) <- c("pid","sc","ba","lh","wd_baw",
			"ols_p_agb","ols_p_agb_li","ols_p_agb_ui","ols_p_u",
			"nlmle_p_agb","nlmle_p_agb_ali","nlmle_p_agb_aui","nlmle_p_agb_mli","nlmle_p_agb_mui","nlmle_p_agb_li","nlmle_p_agb_ui","nlmle_p_u_a","nlmle_p_u_m","nlmle_p_u",
			"lnlmle_p_agb","lnlmle_p_agb_ali","lnlmle_p_agb_aui","lnlmle_p_agb_mli","lnlmle_p_agb_mui","lnlmle_p_agb_li","lnlmle_p_agb_ui","lnlmle_p_u_a","lnlmle_p_u_m","lnlmle_p_u")
			###     
			results[[j]] <- tr
			j <- j+1
			results[[j]] <- pr
			j <- j+1
		}
	}
	return(results)
}

writeResults <- function(results)
{
	i <- 1
	while(i < length(results))
	{
		pname <- toString(results[[i+1]][1,"pid"])
		tfile <- paste(pname,'_tree.txt',sep='')
		pfile <- paste(pname,'_plot.txt',sep='')
		write.table(results[[i]],file=tfile,col.names=TRUE,row.names=FALSE,quote=FALSE)
		write.table(results[[i+1]],file=pfile,col.names=TRUE,row.names=FALSE,quote=FALSE)
		i <- i + 2
	}
	tsum <- data.frame()
	psum <- data.frame()
	j <- 1
	while(j < length(results))
	{
		tsum <- rbind(tsum,results[[j]])
		psum <- rbind(psum,results[[j+1]])
		j <- j + 2
	}
	write.table(tsum,file="tree_results.txt",col.names=TRUE,row.name=FALSE,quote=FALSE)
	write.table(psum,file="plot_results.txt",col.names=TRUE,row.name=FALSE,quote=FALSE)
}
