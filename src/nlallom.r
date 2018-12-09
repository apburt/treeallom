#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

options(expression = 500000)
args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
allomdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
multiplicative <- args[3]
threeparam <- args[4]
alpha <- as.numeric(args[5])
runs <- as.numeric(args[6])
source(paste(path_to_src,"nls.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))
adata <- data.frame(allomdata$d*allomdata$d*allomdata$h*allomdata$rho,allomdata$agb)
colnames(adata) <- c("d2hrho","agb")
amodel <- fitNLS(adata,multiplicative,threeparam)
nlqrmodels <- nlQuantileRegression(amodel,adata,alpha)
bmodels <- bootstrap(amodel,adata,runs)
plotModel(amodel,adata,nlqrmodels,bmodels,alpha)
