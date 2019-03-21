#/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(ggplot2))
suppressMessages(library(ggrepel))
suppressMessages(library(gridExtra))

plotModel <- function(model,data,nlqrmodels,bmodels,alpha)
{
	x <- seq(0,max(data$X)*1.5,length.out=20)
	x <- data.frame(x)
	colnames(x) <- c("X")
	yhats <- yhat(model,x)
	pis <- getPredictionIntervals(nlqrmodels,x)
	cis <- getConfidenceIntervals(bmodels,x,alpha)
	df <- data.frame(x,yhats,pis[,1],pis[,2],cis[,1],cis[,2])
	colnames(df) <- c("x","y","pil","piu","cil","ciu")
	p1 <- ggplot(data=data,aes(X,y))# + geom_point(size=0)
	for(i in 1:length(bmodels))
	{
		yboot <- yhat(bmodels[[i]],x)
		dfm <- data.frame(x,yboot)
		colnames(dfm) <- c("x","y")
		if(i == 1)
		{
			p1 <- p1 + geom_line(data=dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			p1 <- p1 + geom_line(data=dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	p1 <- p1 + geom_line(data=df,aes(x,y,color='model',linetype='model',alpha='model'))
	p1 <- p1 + geom_line(data=df,aes(x,pil,color='pinterval',linetype='pinterval',alpha='pinterval'))
	p1 <- p1 + geom_line(data=df,aes(x,piu),color='black',linetype='twodash',alpha=1)
	p1 <- p1 + geom_line(data=df,aes(x,cil,color='cinterval',linetype='cinterval',alpha='cinterval'))
	p1 <- p1 + geom_line(data=df,aes(x,ciu),color='black',linetype='dashed',alpha=1)
	p1 <- p1 + geom_point(data=data,aes(X,y),size=1,alpha=0.75)
	p1 <- p1 + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p1 <- p1 + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p1 <- p1 + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplot(model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	p1 <- p1 + theme(legend.position=c(0.05,0.95),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	p1 <- p1 + coord_cartesian(xlim=c(0,max(data$X)*1.25),ylim=c(0,max(data$y)*1.25))
	p1 <- p1 + labs(x="X","y",title=expression(paste("y = ",beta[0],X^beta[1]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,X^k,")]")))
	ggsave("nlallom_model.pdf",plot=p1,scale=1)

	vf <- sqrt((exp(param(model)[4]) * caldata$X ** param(model)[3]))
	p2 <- ggplot()
	p2 <- p2 + geom_point(data=caldata,aes(log(fitted(model)),(caldata$y-fitted(model))/vf),size=1,alpha=0.33,stroke=0)
	p2 <- p2 + geom_hline(yintercept=0,linetype="dashed",color="red",alpha=0.75)
	p2 <- p2 + theme(aspect.ratio=1/((1+sqrt(5))/2))
	p2 <- p2 + labs(x=expression(paste("ln(",hat("y"),")")),y=expression(paste("(y"," - ",hat("y"),")","(",sigma^2,X^k,")"^-frac(1,2)))) 
	p3 <- ggplot(data=caldata,aes(sample=(caldata$y-fitted(model))/vf))
	p3 <- p3 + geom_qq(size=1,alpha=0.75,stroke=0)
	p3 <- p3 + labs(x="Theoretical quantile",y="Sample quantile")   
	p3 <- p3 + stat_qq_line(linetype="dashed",color="red",alpha=0.75)
	p3 <- p3 + theme(aspect.ratio=1)
	p3 <- p3 + scale_y_continuous(breaks=seq(-4,4,len = 5))
	p3 <- p3 + scale_x_continuous(breaks=seq(-4,4,len = 5))
	ggsave("nlallom_diagnostics.pdf",plot=grid.arrange(p2,p3,widths=c(1.51,1)),scale=1)
}
