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
models <- list()
###nls1 - 2 param additive
nls1_model <- fitNLS(caldata,multiplicative=FALSE)
models[[1]] <- nls1_model
nls1_nlqrmodels <- nlQuantileRegression(nls1_model,caldata,alpha)
models[[2]] <- nls1_nlqrmodels
nls1_bootmodels <- bootstrap(nls1_model,caldata,runs)
models[[3]] <- nls1_bootmodels
###nls2 - 2 param multiplicative
nls2_model <- fitNLS(caldata,multiplicative=TRUE)
models[[4]] <- nls2_model
nls2_nlqrmodels <- nlQuantileRegression(nls2_model,caldata,alpha)
models[[5]] <- nls2_nlqrmodels
nls2_bootmodels <- bootstrap(nls2_model,caldata,runs)
models[[6]] <- nls2_bootmodels
###
results <- getFieldResults(models,args[5:length(args)],alpha)
writeResults(results)
