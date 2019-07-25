#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"ols.r",sep=""))
source(paste(path_to_src,"nlmle.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"crossvalidate.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))
caldata <- read.table(args[2],col.names=c("study","d","h","agb","rho"))
model_form <- as.logical(args[3])
alpha <- as.numeric(args[4])
runs <- as.numeric(args[5])
ncpus <- as.numeric(args[6])
#
func <- log(agb) ~ log(d) + log(h) + log(rho) 
fitOLS(caldata,func)
crossValidateOLS(caldata,func)
bootOLS(caldata,func,runs,ncpus)
