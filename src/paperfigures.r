#!/usr/bin/env Rscript

suppressMessages(library(ggplot2)) 
suppressMessages(library(ggrepel)) 

source("nls.r")
source("quantileregression.r")
source("bootstrap.r")

options(expressions=500000)
args = commandArgs(trailingOnly=TRUE)

plotFig3 <- function(afname)
{
	adata <- read.table(afname,col.names=c("country","d","h","agb","rho"))
	data = data.frame(adata$d*adata$d*adata$h*adata$rho,adata$agb)
	colnames(data) <- c("d2hrho","agb")
	M2_model <- fitNLS(data,FALSE,FALSE)
	M3_model <- fitNLS(data,TRUE,FALSE)
	M4_model <- fitNLS(data,TRUE,TRUE)
	x <- seq(0,200000,length.out = 10)
	x <- data.frame(x)
	colnames(x) <- c("d2hrho")
	M2_yhats <- yhat(M2_model,x)
	M3_yhats <- yhat(M3_model,x)
	M4_yhats <- yhat(M4_model,x)
	M3_nlqrmodels <- nlQuantileRegression(M3_model,data,0.05)
	M3_bmodels <- bootstrap(M3_model,data,10000)
	M3_pis <- getPredictionIntervals(M3_nlqrmodels,x)
	M3_cis <- getConfidenceIntervals(M3_bmodels,x,0.05)
	df <- data.frame(x,M2_yhats,M3_yhats,M4_yhats,M3_pis[,1],M3_pis[,2],M3_cis[,1],M3_cis[,2])
	colnames(df) <- c("x","y_M2","y_M3","y_M4","pil_M3","piu_M3","cil_M3","ciu_M3")
	p <- ggplot()
	for(i in 1:length(M3_bmodels))
	{
		yboot <- yhat(M3_bmodels[[i]],x)
		dfm <- data.frame(x,yboot)
		colnames(dfm) <- c("x","y")
		p <- p + geom_line(data=dfm,aes(x,y,color='BS',linetype='BS',alpha='BS'))
	}
	M2_name <- as.expression(bquote("M2:"~.(toString(round(coefficients(M2_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M2_model)[2],3)))}))
	M4_name <- as.expression(bquote("M4:"~.(toString(round(coefficients(M4_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M4_model)[2],3)))}~"+"~.(toString(round(coefficients(M4_model)[3],3)))))
	M3_name <- as.expression(bquote("M3:"~.(toString(round(coefficients(M3_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M3_model)[2],3)))}))
	plabels = c('M2'=M2_name,'M4'=M4_name,'M3'=M3_name,'PI'='M3 95% prediction intervals','CI'='M3 95% confidence intervals','BS'='M3 bootstrap models')
	pbreaks = c('M2','M4','M3','PI','CI','BS') 
	p <- p + geom_line(data=df,aes(x,y_M2,color='M2',linetype='M2',alpha='M2'))
	p <- p + geom_line(data=df,aes(x,y_M3,color='M3',linetype='M3',alpha='M3'))
	p <- p + geom_line(data=df,aes(x,y_M4,color='M4',linetype='M4',alpha='M4'))
	p <- p + geom_line(data=df,aes(x,pil_M3,color='PI',linetype='PI',alpha='PI'))
	p <- p + geom_line(data=df,aes(x,piu_M3,color='PI',linetype='PI',alpha='PI'))
	p <- p + geom_line(data=df,aes(x,cil_M3,color='CI',linetype='CI',alpha='CI'))
	p <- p + geom_line(data=df,aes(x,ciu_M3,color='CI',linetype='CI',alpha='CI'))
	p <- p + geom_point(data=data,aes(d2hrho,agb),size=1,alpha=0.75)
	p <- p + scale_color_manual(name='',values= c('M2'='red','M3'='black','M4'='blue','PI'='black','CI'='black','BS'='grey'),labels=plabels,breaks=pbreaks)
	p <- p + scale_linetype_manual(name='',values= c('M2'='twodash','M3'='solid','M4'='twodash','PI'='dotted','CI'='dashed','BS'='solid'),labels=plabels,breaks=pbreaks)
	p <- p + scale_alpha_manual(name='',values= c('M2'=1,'M3'=1,'M4'=1,'PI'=1,'CI'=1,'BS'=0.5),labels=plabels,breaks=pbreaks)
	p <- p + theme(legend.position=c(0.1,0.9),legend.justification=c(0,1),legend.text.align=0,legend.title=element_blank())#legend.background=element_rect(fill="transparent"),legend.box.background=element_rect(fill="transparent"))
	p <- p + coord_cartesian(xlim=c(0,150000),ylim=c(0,100000))
	p <- p + labs(x=expression(paste("D"^{2},"H",rho," (kg)")),y=expression(paste("AGB"," (kg)")))	
	p
}

plotFig4 <- function(args)
{
	adata <- read.table(args[1],col.names=c("country","d","h","agb","rho"))
	data = data.frame(adata$d*adata$d*adata$h*adata$rho,adata$agb)
	colnames(data) <- c("d2hrho","agb")
	M2_model <- fitNLS(data,FALSE,FALSE)
	M3_model <- fitNLS(data,TRUE,FALSE)
	M4_model <- fitNLS(data,TRUE,TRUE)

	M2_name <- as.expression(bquote("M2:"~.(toString(round(coefficients(M2_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M2_model)[2],3)))}))
	M4_name <- as.expression(bquote("M4:"~.(toString(round(coefficients(M4_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M4_model)[2],3)))}~"+"~.(toString(round(coefficients(M4_model)[3],3)))))
	M3_name <- as.expression(bquote("M3:"~.(toString(round(coefficients(M3_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(M3_model)[2],3)))}))

	M2_results <- read.table(args[2],header=TRUE)
	M3_results <- read.table(args[3],header=TRUE)

	p <- ggplot(data=M3_results,aes(x=ba/sc,y=agb_p))


	p <- p + geom_label_repel(aes(M2_results$ba/M2_results$sc,M2_results$agb_p,label=M2_results$pid),direction=c("x"))#),direction=c("x"),point.padding = NA)

	p <- p + geom_pointrange(data=M2_results,aes(x=ba/sc,y=agb_p,ymin=agb_pl,ymax=agb_pu,color='M2'),alpha=0.8)
	p <- p + geom_pointrange(aes(x=ba/sc,y=agb_p,ymin=agb_pl,ymax=agb_pu,color='M3'))

#	for(i in 1:10)
#	{
#		d <- data.frame(x=c(M3_results$ba[i],M2_results$ba[i]),y=c(M3_results$agb_p[i],M2_results$agb_p[i]))
#		p <- p + geom_line(data=d,aes(x,y),color='black',alpha=0.75,linetype='dashed')
#	}
	p <- p + scale_color_manual(name='',values=c('M2'='red','M3'='black'),labels=c('M2'=M2_name,'M3'=M3_name),breaks=c('M3','M2'))
	p <- p + theme(legend.position=c(0.9,0.1),legend.justification=c(1,0),legend.text.align=0,legend.title=element_blank())
	p <- p + labs(x=expression(paste("Mean basal area (",m^2,")")),y=expression(paste("AGB"," (kg)")))
#	p <- p + labs(x=expression(paste("Basal area (",m^2, Ha^-1,")")),y=expression(paste("AGB"," (kg)")))
#	p <- p + coord_cartesian(xlim=c(25,50),ylim=c(100000,700000))
	p <- p + coord_cartesian(xlim=c(0.05,0.12),ylim=c(100000,700000))
	p <- p + scale_y_continuous(labels=scales::comma)
	p
}

plotFig4(args)
#plotFig3(args[1])
