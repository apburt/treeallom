#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

args <- commandArgs(trailingOnly=TRUE)
path_to_src <- args[1]
source(paste(path_to_src,"ols.r",sep=""))
source(paste(path_to_src,"nlmle.r",sep=""))
source(paste(path_to_src,"quantileregression.r",sep=""))
source(paste(path_to_src,"bootstrap.r",sep=""))
source(paste(path_to_src,"generateplots.r",sep=""))

caldata <- read.table(args[2],col.names=c("country","d","h","agb","rho"))

tmp <- caldata[caldata$d > 1,]
print(nrow(tmp))


#func <- log(agb) ~ log(d)
#model <- fitOLS(caldata,func)
#print(summary(model))

#sum(sign(resid(model))==1)


#studentised_breusch_pagan_test(caldata,func,model)
#studentised_white_test(model,func,caldata)
#anderson_darling_test(model)
#shapiro_wilk_test(model)




#caldata['agb'] <- caldata['agb']* (1+(caldata['agb'] * 0.000001333))
#caldata['agb'] <- caldata['agb'] * 1.05

#for (i in 1:nrow(caldata))
#{
#	caldata[i,'d'] <- rnorm(1,caldata[i,'d'],caldata[i,'d']*0.1)
#}
#




#alpha <- as.numeric(args[3])
#runs <- as.numeric(args[4])
#ncpus <- as.numeric(args[5])
#func <- log(agb) ~ log(d)
#func <- log(agb) ~ log(d**2*h*rho)
#func <- log(agb) ~ log(d) + log(h) + log(rho)
#model <- fitOLS(caldata,func)
#bresults <- bootOLS(caldata,func,runs,ncpus)
