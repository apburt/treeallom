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
		#fielddata <- fielddata[fielddata$d >= 0.3,]
		##
		fdata <- data.frame(fielddata$d*fielddata$d*fielddata$h*fielddata$rho)
		colnames(fdata) <- c("d2hrho")
		basal_area <- sum((fielddata$d / 2) ^ 2 * pi)
		lorey_height <- sum(((fielddata$d / 2) ^ 2 * pi) * fielddata$h ) / sum((fielddata$d / 2) ^ 2 * pi)
		###ols1
		ols1_t_agb <- yhatOLS(models[[1]],fdata)
		ols1_t_u_a <- summary(models[[1]])$sigma
		ols1_t_u_m <- measUncertOLS()
		ols1_t_u <- ols1_t_u_a + ols1_t_u_m
		##
		ols1_p_agb <- sum(ols1_t_agb)
		ols1_p_u <- ols1_t_u * sqrt(sum(ols1_t_agb^2)) / sum(ols1_t_agb)
		###nls2
		#nls2
		#
		nls2_sigma_meas <- measSigma(models[[2]],fielddata)
		nls2_t_agb <- yhat(models[[2]],fdata)
		nls2_t_agb_ali <- getPredictionIntervals(models[[3]],fdata)[,1]
		nls2_t_agb_aui <- getPredictionIntervals(models[[3]],fdata)[,2]
		nls2_t_agb_mli <- nls2_t_agb - nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_mui <- nls2_t_agb + nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_li <- nls2_t_agb_ali - nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_agb_ui <- nls2_t_agb_aui + nls2_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls2_t_u_a <- 0.5 * (nls2_t_agb_aui - nls2_t_agb_ali) / nls2_t_agb
		nls2_t_u_m <- 0.5 * (nls2_t_agb_mui - nls2_t_agb_mli) / nls2_t_agb
		nls2_t_u <- 0.5 * (nls2_t_agb_ui - nls2_t_agb_li) / nls2_t_agb
		#
		nls2_p_agb <- sum(nls2_t_agb)
		nls2_p_agb_ali <- sum(getConfidenceIntervals(models[[4]],fdata,alpha)[,1])
		nls2_p_agb_aui <- sum(getConfidenceIntervals(models[[4]],fdata,alpha)[,2])
		nls2_p_agb_mli <- nls2_p_agb - (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_mui <- nls2_p_agb + (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_li <- nls2_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_agb_ui <- nls2_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls2_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls2_p_u_a <- 0.5 * (nls2_p_agb_aui - nls2_p_agb_ali) / nls2_p_agb
		nls2_p_u_m <- 0.5 * (nls2_p_agb_mui - nls2_p_agb_mli) / nls2_p_agb
		nls2_p_u <- 0.5 * (nls2_p_agb_ui - nls2_p_agb_li) / nls2_p_agb
		###nls3
		#nls3
		#
		nls3_sigma_meas <- measSigma(models[[5]],fielddata)
		nls3_t_agb <- yhat(models[[5]],fdata)
		nls3_t_agb_ali <- getPredictionIntervals(models[[6]],fdata)[,1]
		nls3_t_agb_aui <- getPredictionIntervals(models[[6]],fdata)[,2]
		nls3_t_agb_mli <- nls3_t_agb - nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_mui <- nls3_t_agb + nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_li <- nls3_t_agb_ali - nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_agb_ui <- nls3_t_agb_aui + nls3_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls3_t_u_a <- 0.5 * (nls3_t_agb_aui - nls3_t_agb_ali) / nls3_t_agb
		nls3_t_u_m <- 0.5 * (nls3_t_agb_mui - nls3_t_agb_mli) / nls3_t_agb
		nls3_t_u <- 0.5 * (nls3_t_agb_ui - nls3_t_agb_li) / nls3_t_agb
		#
		nls3_p_agb <- sum(nls3_t_agb)
		nls3_p_agb_ali <- sum(getConfidenceIntervals(models[[7]],fdata,alpha)[,1])
		nls3_p_agb_aui <- sum(getConfidenceIntervals(models[[7]],fdata,alpha)[,2])
		nls3_p_agb_mli <- nls3_p_agb - (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_mui <- nls3_p_agb + (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_li <- nls3_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_agb_ui <- nls3_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls3_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls3_p_u_a <- 0.5 * (nls3_p_agb_aui - nls3_p_agb_ali) / nls3_p_agb
		nls3_p_u_m <- 0.5 * (nls3_p_agb_mui - nls3_p_agb_mli) / nls3_p_agb
		nls3_p_u <- 0.5 * (nls3_p_agb_ui - nls3_p_agb_li) / nls3_p_agb
		###nls4
		#nls4
		#
		nls4_sigma_meas <- measSigma(models[[8]],fielddata)
		nls4_t_agb <- yhat(models[[8]],fdata)
		nls4_t_agb_ali <- getPredictionIntervals(models[[9]],fdata)[,1]
		nls4_t_agb_aui <- getPredictionIntervals(models[[9]],fdata)[,2]
		nls4_t_agb_mli <- nls4_t_agb - nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_mui <- nls4_t_agb + nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_li <- nls4_t_agb_ali - nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_agb_ui <- nls4_t_agb_aui + nls4_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls4_t_u_a <- 0.5 * (nls4_t_agb_aui - nls4_t_agb_ali) / nls4_t_agb
		nls4_t_u_m <- 0.5 * (nls4_t_agb_mui - nls4_t_agb_mli) / nls4_t_agb
		nls4_t_u <- 0.5 * (nls4_t_agb_ui - nls4_t_agb_li) / nls4_t_agb
		#
		nls4_p_agb <- sum(nls4_t_agb)
		nls4_p_agb_ali <- sum(getConfidenceIntervals(models[[10]],fdata,alpha)[,1])
		nls4_p_agb_aui <- sum(getConfidenceIntervals(models[[10]],fdata,alpha)[,2])
		nls4_p_agb_mli <- nls4_p_agb - (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_mui <- nls4_p_agb + (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_li <- nls4_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_agb_ui <- nls4_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls4_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls4_p_u_a <- 0.5 * (nls4_p_agb_aui - nls4_p_agb_ali) / nls4_p_agb
		nls4_p_u_m <- 0.5 * (nls4_p_agb_mui - nls4_p_agb_mli) / nls4_p_agb
		nls4_p_u <- 0.5 * (nls4_p_agb_ui - nls4_p_agb_li) / nls4_p_agb
		###nls5
		#nls5
		#
		nls5_sigma_meas <- measSigma(models[[11]],fielddata)
		nls5_t_agb <- yhat(models[[11]],fdata)
		nls5_t_agb_ali <- getPredictionIntervals(models[[12]],fdata)[,1]
		nls5_t_agb_aui <- getPredictionIntervals(models[[12]],fdata)[,2]
		nls5_t_agb_mli <- nls5_t_agb - nls5_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls5_t_agb_mui <- nls5_t_agb + nls5_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls5_t_agb_li <- nls5_t_agb_ali - nls5_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls5_t_agb_ui <- nls5_t_agb_aui + nls5_sigma_meas * qnorm(alpha/2,lower.tail=FALSE)
		nls5_t_u_a <- 0.5 * (nls5_t_agb_aui - nls5_t_agb_ali) / nls5_t_agb
		nls5_t_u_m <- 0.5 * (nls5_t_agb_mui - nls5_t_agb_mli) / nls5_t_agb
		nls5_t_u <- 0.5 * (nls5_t_agb_ui - nls5_t_agb_li) / nls5_t_agb
		#
		nls5_p_agb <- sum(nls5_t_agb)
		nls5_p_agb_ali <- sum(getConfidenceIntervals(models[[13]],fdata,alpha)[,1])
		nls5_p_agb_aui <- sum(getConfidenceIntervals(models[[13]],fdata,alpha)[,2])
		nls5_p_agb_mli <- nls5_p_agb - (1/sqrt(nrow(fdata)) * sum(nls5_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls5_p_agb_mui <- nls5_p_agb + (1/sqrt(nrow(fdata)) * sum(nls5_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls5_p_agb_li <- nls5_p_agb_ali - (1/sqrt(nrow(fdata)) * sum(nls5_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls5_p_agb_ui <- nls5_p_agb_aui + (1/sqrt(nrow(fdata)) * sum(nls5_sigma_meas*qnorm(alpha/2,lower.tail=FALSE)))
		nls5_p_u_a <- 0.5 * (nls5_p_agb_aui - nls5_p_agb_ali) / nls5_p_agb
		nls5_p_u_m <- 0.5 * (nls5_p_agb_mui - nls5_p_agb_mli) / nls5_p_agb
		nls5_p_u <- 0.5 * (nls5_p_agb_ui - nls5_p_agb_li) / nls5_p_agb
		###sort tree results	
		tr <- data.frame(fielddata$id,fielddata$x,fielddata$y,round(fielddata$d,3),round(fielddata$h,3),round(fielddata$rho,3),
			round(ols1_t_agb,3),round(ols1_t_u_a,3),round(ols1_t_u_m,3),round(ols1_t_u,3),
			round(nls2_t_agb,3),round(nls2_t_agb_ali,3),round(nls2_t_agb_aui,3),round(nls2_t_agb_mli,3),round(nls2_t_agb_mui,3),round(nls2_t_agb_li,3),round(nls2_t_agb_ui,3),round(nls2_t_u_a,3),round(nls2_t_u_m,3),round(nls2_t_u,3),
			round(nls3_t_agb,3),round(nls3_t_agb_ali,3),round(nls3_t_agb_aui,3),round(nls3_t_agb_mli,3),round(nls3_t_agb_mui,3),round(nls3_t_agb_li,3),round(nls3_t_agb_ui,3),round(nls3_t_u_a,3),round(nls3_t_u_m,3),round(nls3_t_u,3),
			round(nls4_t_agb,3),round(nls4_t_agb_ali,3),round(nls4_t_agb_aui,3),round(nls4_t_agb_mli,3),round(nls4_t_agb_mui,3),round(nls4_t_agb_li,3),round(nls4_t_agb_ui,3),round(nls4_t_u_a,3),round(nls4_t_u_m,3),round(nls4_t_u,3),
			round(nls5_t_agb,3),round(nls5_t_agb_ali,3),round(nls5_t_agb_aui,3),round(nls5_t_agb_mli,3),round(nls5_t_agb_mui,3),round(nls5_t_agb_li,3),round(nls5_t_agb_ui,3),round(nls5_t_u_a,3),round(nls5_t_u_m,3),round(nls5_t_u,3))
		colnames(tr) <- c("tid","x","y","d","h","rho",
			"ols1_t_agb","ols1_t_u_a","ols1_t_u_m","ols1_t_u",
			"nls2_t_agb","nls2_t_agb_ali","nls2_t_agb_aui","nls2_t_agb_mli","nls2_t_agb_mui","nls2_t_agb_li","nls2_t_agb_ui","nls2_t_u_a","nls2_t_u_m","nls2_t_u",
			"nls3_t_agb","nls3_t_agb_ali","nls3_t_agb_aui","nls3_t_agb_mli","nls3_t_agb_mui","nls3_t_agb_li","nls3_t_agb_ui","nls3_t_u_a","nls3_t_u_m","nls3_t_u",
			"nls4_t_agb","nls4_t_agb_ali","nls4_t_agb_aui","nls4_t_agb_mli","nls4_t_agb_mui","nls4_t_agb_li","nls4_t_agb_ui","nls4_t_u_a","nls4_t_u_m","nls4_t_u",
			"nls5_t_agb","nls5_t_agb_ali","nls5_t_agb_aui","nls5_t_agb_mli","nls5_t_agb_mui","nls5_t_agb_li","nls5_t_agb_ui","nls5_t_u_a","nls5_t_u_m","nls5_t_u")
		###sort plot results
		pr <- data.frame(pname,round(basal_area,3),round(lorey_height,3),
			round(ols1_p_agb,3),round(ols1_p_u,3),
			round(nls2_p_agb,3),round(nls2_p_agb_ali,3),round(nls2_p_agb_aui,3),round(nls2_p_agb_mli,3),round(nls2_p_agb_mui,3),round(nls2_p_agb_li,3),round(nls2_p_agb_ui,3),round(nls2_p_u_a,3),round(nls2_p_u_m,3),round(nls2_p_u,3),
			round(nls3_p_agb,3),round(nls3_p_agb_ali,3),round(nls3_p_agb_aui,3),round(nls3_p_agb_mli,3),round(nls3_p_agb_mui,3),round(nls3_p_agb_li,3),round(nls3_p_agb_ui,3),round(nls3_p_u_a,3),round(nls3_p_u_m,3),round(nls3_p_u,3),
			round(nls4_p_agb,3),round(nls4_p_agb_ali,3),round(nls4_p_agb_aui,3),round(nls4_p_agb_mli,3),round(nls4_p_agb_mui,3),round(nls4_p_agb_li,3),round(nls4_p_agb_ui,3),round(nls4_p_u_a,3),round(nls4_p_u_m,3),round(nls4_p_u,3),
			round(nls5_p_agb,3),round(nls5_p_agb_ali,3),round(nls5_p_agb_aui,3),round(nls5_p_agb_mli,3),round(nls5_p_agb_mui,3),round(nls5_p_agb_li,3),round(nls5_p_agb_ui,3),round(nls5_p_u_a,3),round(nls5_p_u_m,3),round(nls5_p_u,3))
		colnames(pr) <- c("pid","ba","lh",
			"ols1_p_agb","ols1_p_u",
			"nls2_p_agb","nls2_p_agb_ali","nls2_p_agb_aui","nls2_p_agb_mli","nls2_p_agb_mui","nls2_p_agb_li","nls2_p_agb_ui","nls2_p_u_a","nls2_p_u_m","nls2_p_u",
			"nls3_p_agb","nls3_p_agb_ali","nls3_p_agb_aui","nls3_p_agb_mli","nls3_p_agb_mui","nls3_p_agb_li","nls3_p_agb_ui","nls3_p_u_a","nls3_p_u_m","nls3_p_u",
			"nls4_p_agb","nls4_p_agb_ali","nls4_p_agb_aui","nls4_p_agb_mli","nls4_p_agb_mui","nls4_p_agb_li","nls4_p_agb_ui","nls4_p_u_a","nls4_p_u_m","nls4_p_u",
			"nls5_p_agb","nls5_p_agb_ali","nls5_p_agb_aui","nls5_p_agb_mli","nls5_p_agb_mui","nls5_p_agb_li","nls5_p_agb_ui","nls5_p_u_a","nls5_p_u_m","nls5_p_u")
		###
#		tresults[[i]] <- tr
#		presults[[i]] <- pr
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
	tsum = data.frame()
	psum = data.frame()
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
