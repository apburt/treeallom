#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"ols.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"crossvalidate.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))

caldata <- read.table(args[2],col.names=c("study","d","h","agb","rho"))
runs <- as.numeric(args[3])
ncpus <- as.numeric(args[4])
alpha <- as.numeric(args[5])
func <- log(agb) ~ log(d)
model <- fitOLS(caldata,func)
bresults <- bootOLS(caldata,func,runs,ncpus)
print(summary(model))
for(i in 1:length(coefficients(model)))
{
	b0_ci <- boot.ci(bresults,index=i,conf=0.95,type="perc")
	print(b0_ci)
} 
cresults <- crossValidateOLS(caldata,func)
print("CROSS VALIDATION")
print("MSA, SSPBB")
print(cresults[[2]])
