#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

options(expressions=500000)
args = commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
allomdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
alpha <- as.numeric(args[3])
runs <- as.numeric(args[4])
source(paste(path_to_src,"ols.r",sep=""))
source(paste(path_to_src,"nls.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"measuncert.r",sep=""))
source(paste(path_to_src,"fieldresults.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))
adata = data.frame(allomdata$d*allomdata$d*allomdata$h*allomdata$rho,allomdata$agb)
colnames(adata) <- c("d2hrho","agb")
###ols1
models <- list()
ols1_model <- fitOLS(adata)
models[[1]] <- ols1_model
###nls2
nls2_model <- fitNLS(adata,multiplicative=FALSE,threeparam=FALSE)
models[[2]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,adata,alpha)
models[[3]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,adata,runs)
models[[4]] <- nls2_bootmodels
###nls3
nls3_model <- fitNLS(adata,multiplicative=TRUE,threeparam=FALSE)
models[[5]] <- nls3_model
nls3_nlqrmodels <- nlQuantileRegression(nls3_model,adata,alpha)
models[[6]] <- nls3_nlqrmodels
nls3_bootmodels <- bootstrap(nls3_model,adata,runs)
models[[7]] <- nls3_bootmodels
###nls4
nls4_model <- fitNLS(adata,multiplicative=FALSE,threeparam=TRUE)
models[[8]] <- nls4_model
nls4_nlqrmodels <- nlQuantileRegression(nls4_model,adata,alpha)
models[[9]] <- nls4_nlqrmodels
nls4_bootmodels <- bootstrap(nls4_model,adata,runs)
models[[10]] <- nls4_bootmodels
###nls5
nls5_model <- fitNLS(adata,multiplicative=TRUE,threeparam=TRUE)
models[[11]] <- nls5_model
nls5_nlqrmodels <- nlQuantileRegression(nls5_model,adata,alpha)
models[[12]] <- nls5_nlqrmodels
nls5_bootmodels <- bootstrap(nls5_model,adata,runs)
models[[13]] <- nls5_bootmodels
###
results <- getFieldResults(models,args[5:length(args)],alpha,runs)
writeResults(results)
#plotModels(models,adata)
#plotTreeResults(results)
#plotPlotResults(results)
