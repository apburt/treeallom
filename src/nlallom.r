#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"nls.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))
cdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
caldata <- data.frame(cdata$d*cdata$d*cdata$h*cdata$rho,cdata$agb)
colnames(caldata) <- c("X","y")
multiplicative <- args[3]
model <- fitNLS(caldata,multiplicative)
alpha <- as.numeric(args[4])
runs <- as.numeric(args[5])
nlqrmodels <- nlQuantileRegression(model,caldata,alpha)
bmodels <- bootstrap(model,caldata,runs)
plotModel(model,caldata,nlqrmodels,bmodels,alpha)
