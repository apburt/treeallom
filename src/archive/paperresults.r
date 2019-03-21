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
results <- read.table(args[3],header=TRUE)
alpha <- 0.05
runs <- 1000
#runs <- 4000
#####
models <- list()
models[[1]] <- fitNLS(caldata)
models[[2]] <- nlQuantileRegression(models[[1]],caldata,alpha)
models[[3]] <- bootstrap(models[[1]],caldata,runs)
limitedcaldata <- caldata[caldata$X > 10000,]
colnames(limitedcaldata) <- c("X","y")
print(nrow(limitedcaldata))
models[[4]] <- fitNLS(limitedcaldata)
models[[5]] <- nlQuantileRegression(models[[4]],limitedcaldata,alpha)
models[[6]] <- bootstrap(models[[4]],limitedcaldata,runs)
#####
lmodels <- list()
lmodels[[1]] <- models[[1]]
lmodels[[2]] <- models[[3]]
tmp <- caldata[caldata$X >= 5000,]
lmodels[[3]] <- fitNLS(tmp)
tmp <- caldata[caldata$X >= 10000,]
lmodels[[4]] <- fitNLS(tmp)
tmp <- caldata[caldata$X >= 20000,]
lmodels[[5]] <- fitNLS(tmp)
#####
supp2models <- list()
supp2models[[1]] <- fitNLS(caldata)
supp2models[[2]] <- nlQuantileRegression(supp2models[[1]],caldata,alpha)
dcaldata <- data.frame(cdata$d,cdata$agb)
colnames(dcaldata) <- c("X","y")
supp2models[[3]]<- fitNLS(dcaldata)
supp2models[[4]] <- nlQuantileRegression(supp2models[[3]],dcaldata,alpha)
#####
plotFig1(cdata)
plotFig2(caldata,limitedcaldata,models,alpha)
plotFig3(caldata,lmodels,alpha)
plotFig4(results)
plotSuppFig1(caldata)
plotSuppFig2(caldata,dcaldata,supp2models)
plotSuppFig3(caldata)
#####
