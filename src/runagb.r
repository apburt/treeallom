#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"nlmle.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"measuncert.r",sep=""))
source(paste(path_to_src,"olsBIOMASS.r",sep=""))
source(paste(path_to_src,"fieldresults.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))
cdata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))
caldata <- data.frame(cdata$d*cdata$d*cdata$h*cdata$rho,cdata$agb)
colnames(caldata) <- c("X","y")
alpha <- as.numeric(args[3])
runs <- as.numeric(args[4])
localreg <- args[5]
results <- getFieldResults(caldata,args[6:length(args)],alpha,runs,localreg)
writeResults(results)
