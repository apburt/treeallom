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
source(paste(path_to_src,"archive/paperfigures.r",sep=""))
cdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
caldata <- data.frame(cdata$d*cdata$d*cdata$h*cdata$rho,cdata$agb)
colnames(caldata) <- c("X","y")
plotFig1(cdata)
plotSuppFig1(caldata)
models <- list()
######
nls1_model <- fitNLS(caldata,multiplicative=FALSE)
models[[1]] <- nls1_model
nls1_nlqrmodels <- nlQuantileRegression(nls1_model,caldata,0.05)
models[[2]] <- nls1_nlqrmodels
nls1_bootmodels <- bootstrap(nls1_model,caldata,20000)
models[[3]] <- nls1_bootmodels
###
nls2_model <- fitNLS(caldata,multiplicative=TRUE)
models[[4]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,caldata,0.05)
models[[5]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,caldata,20000)
models[[6]] <- nls2_bootmodels
###
results <- getFieldResults(models,args[3:length(args)],0.05)
writeResults(results)
plotFig4(models,results)
######
nls1_model <- fitNLS(caldata,multiplicative=FALSE)
models[[1]] <- nls1_model
nls1_nlqrmodels <- nlQuantileRegression(nls1_model,caldata,0.05)
models[[2]] <- nls1_nlqrmodels
nls1_bootmodels <- bootstrap(nls1_model,caldata,4000)
models[[3]] <- nls1_bootmodels
###
nls2_model <- fitNLS(caldata,multiplicative=TRUE)
models[[4]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,caldata,0.05)
models[[5]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,caldata,4000)
models[[6]] <- nls2_bootmodels
###
plotFig2(caldata,models,0.05)
######
models[[1]] <- nls2_model
models[[2]] <- nls2_nlqrmodels
models[[3]] <- nls2_bootmodels
####
dcaldata <- data.frame(cdata$d,cdata$agb)
colnames(dcaldata) <- c("X","y")
nls2_model <- fitNLS(dcaldata,multiplicative=TRUE)
models[[4]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,dcaldata,0.05)
models[[5]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,dcaldata,4000)
models[[6]] <- nls2_bootmodels
####
plotSuppFig2(caldata,dcaldata,models,0.05)
#######
caldata <- caldata[caldata$X >= 10000,]
nls1_model <- fitNLS(caldata,multiplicative=FALSE)
models[[1]] <- nls1_model
nls1_nlqrmodels <- nlQuantileRegression(nls1_model,caldata,0.05)
models[[2]] <- nls1_nlqrmodels
nls1_bootmodels <- bootstrap(nls1_model,caldata,4000)
models[[3]] <- nls1_bootmodels
###
nls2_model <- fitNLS(caldata,multiplicative=TRUE)
models[[4]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,caldata,0.05)
models[[5]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,caldata,4000)
models[[6]] <- nls2_bootmodels
###
plotFig3(caldata,models,0.05)
######
