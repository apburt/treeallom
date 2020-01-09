#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"ols.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"crossvalidate.r",sep=""))
source(paste(path_to_src,"archive/paper/paperfigures.r",sep=""))
source(paste(path_to_src,"archive/paper/paperstats.r",sep=""))
source(paste(path_to_src,"archive/paper/papersimulation.r",sep=""))
caldata <- read.table(args[2],col.names=c("study","d","h","agb","rho"))
alpha <- 0.05
runs <- 10000
ncpus <- 4
####
plotFig0()
plotFig1(caldata)
plotFig2(caldata,alpha,runs,ncpus)
plotFig3(caldata,alpha,runs,ncpus)
plotFig4(caldata)
plotFig5(caldata,alpha,runs,ncpus)
###
#bivariateStats(caldata)
#covariateStats(caldata)
###
#crossvalidateStats(caldata)
#simulate(caldata,runs)
