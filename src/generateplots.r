#/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(ggplot2))
suppressMessages(library(ggrepel))

plotModel <- function(model,data,nlqrmodels,bmodels,alpha)
{
	x <- seq(0,200000,length.out=20)
	x <- data.frame(x)
	colnames(x) <- c("d2hrho")
	yhats <- yhat(model,x)
	pis <- getPredictionIntervals(nlqrmodels,x)
	cis <- getConfidenceIntervals(bmodels,x,alpha)
	df <- data.frame(x,yhats,pis[,1],pis[,2],cis[,1],cis[,2])
	colnames(df) <- c("x","y","pil","piu","cil","ciu")
	p <- ggplot(data=data,aes(d2hrho,agb))# + geom_point(size=0)
	for(i in 1:length(bmodels))
	{
		yboot <- yhat(bmodels[[i]],x)
		dfm <- data.frame(x,yboot)
		colnames(dfm) <- c("x","y")
		if(i == 1)
		{
			p <- p + geom_line(data=dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			p <- p + geom_line(data=dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	p <- p + geom_line(data=df,aes(x,y,color='model',linetype='model',alpha='model'))
	p <- p + geom_line(data=df,aes(x,pil,color='pinterval',linetype='pinterval',alpha='pinterval'))
	p <- p + geom_line(data=df,aes(x,piu),color='black',linetype='twodash',alpha=1)
	p <- p + geom_line(data=df,aes(x,cil,color='cinterval',linetype='cinterval',alpha='cinterval'))
	p <- p + geom_line(data=df,aes(x,ciu),color='black',linetype='dashed',alpha=1)
	p <- p + geom_point(data=data,aes(d2hrho,agb),size=1,alpha=0.75)
	p <- p + scale_color_manual(name='',values=c('model'='black','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p <- p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p <- p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p <- p + theme(legend.position=c(0.1,0.9),legend.justification=c(0,1),legend.text.align=0,legend.background=element_rect(fill="transparent"),legend.box.background=element_rect(fill="transparent"),legend.title=element_blank())
	p <- p + coord_cartesian(xlim=c(0,max(data$d2hrho)*1.25),ylim=c(0,max(data$agb)*1.25))
	p <- p + labs(x=expression(paste("D"^{2},"H",rho," (kg)")),y=expression(paste("AGB"," (kg)")))
	p
}

#plotTreeResults <- function(results)
#{
#	p <- ggplot()
#	for(i in 1:length(results[[1]]))
#	{
#		p <- p + geom_point(data=results[[1]][[i]],aes(x=agb_t,y=u_t))
#	}
#	p <- p + labs(x=expression(paste("AGB"," (kg)")),y=expression(paste("Relative uncertainty")))
#	p
#}

#plotPlotResults <- function(results)
#{
#	presults = data.frame()
#	i <- 1
#	while(i < length(results))
#	{
#		presults <- rbind(presults,results[[i+1]])
#		i <- i + 2
#	}
#	p <- ggplot(data=presults,aes(x=ba,y=agb_p)) + geom_point()
#	p <- p + geom_errorbar(aes(ymin=agb_pl,ymax=agb_pu))
#	p <- p + geom_label_repel(aes(label=r$pid),direction=c("x"))
#	p <- p + coord_cartesian(xlim=c(10*floor(min(r$ba)/10),10*ceiling(max(r$ba)/10)),ylim=c(min(r$agb_pl),max(r$agb_pu)))
#	p <- p + scale_y_continuous(labels=scales::comma)
#	p <- p + labs(x=expression(paste("Basal area (",m^2, Ha^-1,")")),y=expression(paste("AGB"," (kg)")))
#	p
#}
