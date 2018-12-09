#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

getFieldResults <- function(models,args,alpha,runs)
{
	results <- list()
	j <- 1
	for(i in 1:length(args))
	{
		###field data
		fielddata <- read.table(args[i],col.names=c("id","x","y","d","h","rho"))
		tmp1 <- strsplit(args[i],'/')
		tmp2 <- tmp1[[1]][length(tmp1[[1]])]
		tmp3 <- strsplit(tmp2,'\\.')
		pname <- tmp3[[1]][1]
		##filter	
		#fielddata <- fielddata[fielddata$d >= 0.9,]
		##
		fdata <- data.frame(fielddata$d*fielddata$d*fielddata$h*fielddata$rho)
		colnames(fdata) <- c("d2hrho")
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
		###nls1
		nls1_sigma_meas <- measSigma(models[[1]],fielddata)
		nls1_t_agb <- yhat(models[[1]],fdata)
		nls1_t_agb_ali <- getPredictionIntervals(models[[2]],fdata)[,1]
		nls1_t_agb_aui <- getPredictionIntervals(models[[2]],fdata)[,2]
		nls1_t_agb_mli <- nls1_t_agb - nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls1_t_agb_mui <- nls1_t_agb + nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls1_t_agb_li <- nls1_t_agb_ali - nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls1_t_agb_ui <- nls1_t_agb_aui + nls1_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls1_t_u_a <- 0.5 * (nls1_t_agb_aui - nls1_t_agb_ali) / nls1_t_agb
		nls1_t_u_m <- 0.5 * (nls1_t_agb_mui - nls1_t_agb_mli) / nls1_t_agb
		nls1_t_u <- 0.5 * (nls1_t_agb_ui - nls1_t_agb_li) / nls1_t_agb
		#
		nls1_p_agb <- sum(nls1_t_agb)
		nls1_p_agb_ali <- sum(getConfidenceIntervals(models[[3]],fdata,alpha)[,1])
		nls1_p_agb_aui <- sum(getConfidenceIntervals(models[[3]],fdata,alpha)[,2])
		nls1_p_agb_mli <- nls1_p_agb - (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls1_p_agb_mui <- nls1_p_agb + (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls1_p_agb_li <- nls1_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls1_p_agb_ui <- nls1_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls1_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls1_p_u_a <- 0.5 * (nls1_p_agb_aui - nls1_p_agb_ali) / nls1_p_agb
		nls1_p_u_m <- 0.5 * (nls1_p_agb_mui - nls1_p_agb_mli) / nls1_p_agb
		nls1_p_u <- 0.5 * (nls1_p_agb_ui - nls1_p_agb_li) / nls1_p_agb
		###nls2
		nls2_sigma_meas <- measSigma(models[[4]],fielddata)
		nls2_t_agb <- yhat(models[[4]],fdata)
		nls2_t_agb_ali <- getPredictionIntervals(models[[5]],fdata)[,1]
		nls2_t_agb_aui <- getPredictionIntervals(models[[5]],fdata)[,2]
		nls2_t_agb_mli <- nls2_t_agb - nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_mui <- nls2_t_agb + nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_li <- nls2_t_agb_ali - nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_ui <- nls2_t_agb_aui + nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_u_a <- 0.5 * (nls2_t_agb_aui - nls2_t_agb_ali) / nls2_t_agb
		nls2_t_u_m <- 0.5 * (nls2_t_agb_mui - nls2_t_agb_mli) / nls2_t_agb
		nls2_t_u <- 0.5 * (nls2_t_agb_ui - nls2_t_agb_li) / nls2_t_agb
		#
		nls2_p_agb <- sum(nls2_t_agb)
		nls2_p_agb_ali <- sum(getConfidenceIntervals(models[[6]],fdata,alpha)[,1])
		nls2_p_agb_aui <- sum(getConfidenceIntervals(models[[6]],fdata,alpha)[,2])
		nls2_p_agb_mli <- nls2_p_agb - (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_mui <- nls2_p_agb + (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_li <- nls2_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_ui <- nls2_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_u_a <- 0.5 * (nls2_p_agb_aui - nls2_p_agb_ali) / nls2_p_agb
		nls2_p_u_m <- 0.5 * (nls2_p_agb_mui - nls2_p_agb_mli) / nls2_p_agb
		nls2_p_u <- 0.5 * (nls2_p_agb_ui - nls2_p_agb_li) / nls2_p_agb
		###nls3
		nls3_sigma_meas <- measSigma(models[[7]],fielddata)
		nls3_t_agb <- yhat(models[[7]],fdata)
		nls3_t_agb_ali <- getPredictionIntervals(models[[8]],fdata)[,1]
		nls3_t_agb_aui <- getPredictionIntervals(models[[8]],fdata)[,2]
		nls3_t_agb_mli <- nls3_t_agb - nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_mui <- nls3_t_agb + nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_li <- nls3_t_agb_ali - nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_ui <- nls3_t_agb_aui + nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_u_a <- 0.5 * (nls3_t_agb_aui - nls3_t_agb_ali) / nls3_t_agb
		nls3_t_u_m <- 0.5 * (nls3_t_agb_mui - nls3_t_agb_mli) / nls3_t_agb
		nls3_t_u <- 0.5 * (nls3_t_agb_ui - nls3_t_agb_li) / nls3_t_agb
		#
		nls3_p_agb <- sum(nls3_t_agb)
		nls3_p_agb_ali <- sum(getConfidenceIntervals(models[[9]],fdata,alpha)[,1])
		nls3_p_agb_aui <- sum(getConfidenceIntervals(models[[9]],fdata,alpha)[,2])
		nls3_p_agb_mli <- nls3_p_agb - (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_mui <- nls3_p_agb + (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_li <- nls3_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_ui <- nls3_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_u_a <- 0.5 * (nls3_p_agb_aui - nls3_p_agb_ali) / nls3_p_agb
		nls3_p_u_m <- 0.5 * (nls3_p_agb_mui - nls3_p_agb_mli) / nls3_p_agb
		nls3_p_u <- 0.5 * (nls3_p_agb_ui - nls3_p_agb_li) / nls3_p_agb
		###nls4
		nls4_sigma_meas <- measSigma(models[[10]],fielddata)
		nls4_t_agb <- yhat(models[[10]],fdata)
		nls4_t_agb_ali <- getPredictionIntervals(models[[11]],fdata)[,1]
		nls4_t_agb_aui <- getPredictionIntervals(models[[11]],fdata)[,2]
		nls4_t_agb_mli <- nls4_t_agb - nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_mui <- nls4_t_agb + nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_li <- nls4_t_agb_ali - nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_ui <- nls4_t_agb_aui + nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_u_a <- 0.5 * (nls4_t_agb_aui - nls4_t_agb_ali) / nls4_t_agb
		nls4_t_u_m <- 0.5 * (nls4_t_agb_mui - nls4_t_agb_mli) / nls4_t_agb
		nls4_t_u <- 0.5 * (nls4_t_agb_ui - nls4_t_agb_li) / nls4_t_agb
		#
		nls4_p_agb <- sum(nls4_t_agb)
		nls4_p_agb_ali <- sum(getConfidenceIntervals(models[[12]],fdata,alpha)[,1])
		nls4_p_agb_aui <- sum(getConfidenceIntervals(models[[12]],fdata,alpha)[,2])
		nls4_p_agb_mli <- nls4_p_agb - (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_mui <- nls4_p_agb + (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_li <- nls4_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_ui <- nls4_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_u_a <- 0.5 * (nls4_p_agb_aui - nls4_p_agb_ali) / nls4_p_agb
		nls4_p_u_m <- 0.5 * (nls4_p_agb_mui - nls4_p_agb_mli) / nls4_p_agb
		nls4_p_u <- 0.5 * (nls4_p_agb_ui - nls4_p_agb_li) / nls4_p_agb
		###sort tree results	
		tr <- data.frame(pname,fielddata$id,fielddata$x,fielddata$y,round(fielddata$d,3),round(fielddata$h,3),round(fielddata$rho,3),
			round(ols_t_agb,3),round(ols_t_agb_li,3),round(ols_t_agb_ui,3),round(ols_t_u,3),
			round(nls1_t_agb,3),round(nls1_t_agb_ali,3),round(nls1_t_agb_aui,3),round(nls1_t_agb_mli,3),round(nls1_t_agb_mui,3),round(nls1_t_agb_li,3),round(nls1_t_agb_ui,3),round(nls1_t_u_a,3),round(nls1_t_u_m,3),round(nls1_t_u,3),
			round(nls2_t_agb,3),round(nls2_t_agb_ali,3),round(nls2_t_agb_aui,3),round(nls2_t_agb_mli,3),round(nls2_t_agb_mui,3),round(nls2_t_agb_li,3),round(nls2_t_agb_ui,3),round(nls2_t_u_a,3),round(nls2_t_u_m,3),round(nls2_t_u,3),
			round(nls3_t_agb,3),round(nls3_t_agb_ali,3),round(nls3_t_agb_aui,3),round(nls3_t_agb_mli,3),round(nls3_t_agb_mui,3),round(nls3_t_agb_li,3),round(nls3_t_agb_ui,3),round(nls3_t_u_a,3),round(nls3_t_u_m,3),round(nls3_t_u,3),
			round(nls4_t_agb,3),round(nls4_t_agb_ali,3),round(nls4_t_agb_aui,3),round(nls4_t_agb_mli,3),round(nls4_t_agb_mui,3),round(nls4_t_agb_li,3),round(nls4_t_agb_ui,3),round(nls4_t_u_a,3),round(nls4_t_u_m,3),round(nls4_t_u,3))
		colnames(tr) <- c("pid","tid","x","y","d","h","rho",
			"ols_t_agb","ols_t_agb_li","ols_t_agb_ui","ols_t_u",
			"nls1_t_agb","nls1_t_agb_ali","nls1_t_agb_aui","nls1_t_agb_mli","nls1_t_agb_mui","nls1_t_agb_li","nls1_t_agb_ui","nls1_t_u_a","nls1_t_u_m","nls1_t_u",
			"nls2_t_agb","nls2_t_agb_ali","nls2_t_agb_aui","nls2_t_agb_mli","nls2_t_agb_mui","nls2_t_agb_li","nls2_t_agb_ui","nls2_t_u_a","nls2_t_u_m","nls2_t_u",
			"nls3_t_agb","nls3_t_agb_ali","nls3_t_agb_aui","nls3_t_agb_mli","nls3_t_agb_mui","nls3_t_agb_li","nls3_t_agb_ui","nls3_t_u_a","nls3_t_u_m","nls3_t_u",
			"nls4_t_agb","nls4_t_agb_ali","nls4_t_agb_aui","nls4_t_agb_mli","nls4_t_agb_mui","nls4_t_agb_li","nls4_t_agb_ui","nls4_t_u_a","nls4_t_u_m","nls4_t_u")
		###sort plot results
		pr <- data.frame(pname,round(stem_count,0),round(basal_area,3),round(lorey_height,3),round(wd_baw,3),
			round(ols_p_agb,3),round(ols_p_agb_li,3),round(ols_p_agb_ui,3),round(ols_p_u,3),
			round(nls1_p_agb,3),round(nls1_p_agb_ali,3),round(nls1_p_agb_aui,3),round(nls1_p_agb_mli,3),round(nls1_p_agb_mui,3),round(nls1_p_agb_li,3),round(nls1_p_agb_ui,3),round(nls1_p_u_a,3),round(nls1_p_u_m,3),round(nls1_p_u,3),
			round(nls2_p_agb,3),round(nls2_p_agb_ali,3),round(nls2_p_agb_aui,3),round(nls2_p_agb_mli,3),round(nls2_p_agb_mui,3),round(nls2_p_agb_li,3),round(nls2_p_agb_ui,3),round(nls2_p_u_a,3),round(nls2_p_u_m,3),round(nls2_p_u,3),
			round(nls3_p_agb,3),round(nls3_p_agb_ali,3),round(nls3_p_agb_aui,3),round(nls3_p_agb_mli,3),round(nls3_p_agb_mui,3),round(nls3_p_agb_li,3),round(nls3_p_agb_ui,3),round(nls3_p_u_a,3),round(nls3_p_u_m,3),round(nls3_p_u,3),
			round(nls4_p_agb,3),round(nls4_p_agb_ali,3),round(nls4_p_agb_aui,3),round(nls4_p_agb_mli,3),round(nls4_p_agb_mui,3),round(nls4_p_agb_li,3),round(nls4_p_agb_ui,3),round(nls4_p_u_a,3),round(nls4_p_u_m,3),round(nls4_p_u,3))
		colnames(pr) <- c("pid","sc","ba","lh","wd_baw",
			"ols_p_agb","ols_p_agb_li","ols_p_agb_ui","ols_p_u",
			"nls1_p_agb","nls1_p_agb_ali","nls1_p_agb_aui","nls1_p_agb_mli","nls1_p_agb_mui","nls1_p_agb_li","nls1_p_agb_ui","nls1_p_u_a","nls2_p_u_m","nls1_p_u",
			"nls2_p_agb","nls2_p_agb_ali","nls2_p_agb_aui","nls2_p_agb_mli","nls2_p_agb_mui","nls2_p_agb_li","nls2_p_agb_ui","nls2_p_u_a","nls3_p_u_m","nls2_p_u",
			"nls3_p_agb","nls3_p_agb_ali","nls3_p_agb_aui","nls3_p_agb_mli","nls3_p_agb_mui","nls3_p_agb_li","nls3_p_agb_ui","nls3_p_u_a","nls4_p_u_m","nls3_p_u",
			"nls4_p_agb","nls4_p_agb_ali","nls4_p_agb_aui","nls4_p_agb_mli","nls4_p_agb_mui","nls4_p_agb_li","nls4_p_agb_ui","nls4_p_u_a","nls5_p_u_m","nls4_p_u")
		###
		results[[j]] <- tr
		j <- j+1
		results[[j]] <- pr
		j <- j+1
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
