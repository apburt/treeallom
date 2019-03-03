#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(ggplot2)) 
suppressMessages(library(ggrepel)) 
suppressMessages(library(gridExtra)) 

getModelExpForGgplotBem <- function(model)
{
	a <- toString(round(coefficients(model)[1],3))
	b <- toString(round(coefficients(model)[2],3))		
	return(as.expression(bquote(.(a)~(D^{2}*H*rho["b"])^{.(b)})))
}

getModelExpForGgplotBemD <- function(model)
{
	a <- toString(round(coefficients(model)[1],3))
	b <- toString(round(coefficients(model)[2],3))		
	return(as.expression(bquote(.(a)~(D)^{.(b)})))
}

plotFig1 <- function(cdata)
{
	data <- data.frame(cdata$d,cdata$agb)
	colnames(data) <- c("d","agb")
	p1 <- ggplot()
	p1 <- p1 + geom_point(data=data,aes(data$d,data$agb),size=1.5,alpha=0.75,stroke=0)
	p1 <- p1 + theme(aspect.ratio=1/((1+sqrt(5))/2))
	p1 <- p1 + coord_cartesian(xlim=c(0,2.2),ylim=c(0,80000))
	p1 <- p1 + labs(x=expression(paste("D"," (m)")),y=expression(paste("AGB"," (kg)")))
	ggsave("f1.pdf",plot=p1,scale=1,limitsize=FALSE)
}

plotFig2 <- function(caldata,models,alpha)
{	
	x <- seq(0,150000,length.out=25)
	x <- data.frame(x)
	colnames(x) <- c("X")
	#####
	bem1_model <- models[[1]]
	bem1_yhats <- yhat(bem1_model,x)
	bem1_nlqrmodels <- models[[2]]
	bem1_bmodels <- models[[3]]
	bem1_pis <- getPredictionIntervals(bem1_nlqrmodels,x)
	bem1_cis <- getConfidenceIntervals(bem1_bmodels,x,alpha)
	bem1_df <- data.frame(x,bem1_yhats,bem1_pis[,1],bem1_pis[,2],bem1_cis[,1],bem1_cis[,2])
	colnames(bem1_df) <- c("x","y_bem1","pil_bem1","piu_bem1","cil_bem1","ciu_bem1")
	bem1_p <- ggplot(data=caldata,aes(X,y))
	for(i in 1:length(bem1_bmodels))
	{
		bem1_yboot <- yhat(bem1_bmodels[[i]],x)
		bem1_dfm <- data.frame(x,bem1_yboot)
		colnames(bem1_dfm) <- c("x","y")
		if(i == 1)
		{
			bem1_p <- bem1_p + geom_line(data=bem1_dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			bem1_p <- bem1_p + geom_line(data=bem1_dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,pil_bem1,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,piu_bem1),color='black',linetype='twodash',alpha=1)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,cil_bem1,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,ciu_bem1),color='black',linetype='dashed',alpha=1)
	bem1_p <- bem1_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,y_bem1,color='model',linetype='model',alpha='model'))
	bem1_p <- bem1_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + theme(legend.position=c(0.045,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust = 0.5))
	bem1_p <- bem1_p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	bem1_p <- bem1_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[1],"(D"^{2},"H",rho["b"],")"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,")]  (additive)")))
	#####
	bem2_model <- models[[4]]
	bem2_yhats <- yhat(bem2_model,x)
	bem2_nlqrmodels <- models[[5]]
	bem2_bmodels <- models[[6]]
	bem2_pis <- getPredictionIntervals(bem2_nlqrmodels,x)
	bem2_cis <- getConfidenceIntervals(bem2_bmodels,x,alpha)
	bem2_df <- data.frame(x,bem2_yhats,bem2_pis[,1],bem2_pis[,2],bem2_cis[,1],bem2_cis[,2])
	colnames(bem2_df) <- c("x","y_bem2","pil_bem2","piu_bem2","cil_bem2","ciu_bem2")
	bem2_p <- ggplot(data=caldata,aes(X,y))
	for(i in 1:length(bem2_bmodels))
	{
		bem2_yboot <- yhat(bem2_bmodels[[i]],x)
		bem2_dfm <- data.frame(x,bem2_yboot)
		colnames(bem2_dfm) <- c("x","y")
		if(i == 1)
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,pil_bem2,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,piu_bem2),color='black',linetype='twodash',alpha=1)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,cil_bem2,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,ciu_bem2),color='black',linetype='dashed',alpha=1)
	bem2_p <- bem2_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,y_bem2,color='model',linetype='model',alpha='model'))
	bem2_p <- bem2_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + theme(legend.position=c(0.045,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem2_p <- bem2_p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	bem2_p <- bem2_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[1],"(D"^{2},"H",rho["b"],")"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]  (multiplicative)")))
	bem1e_p <- ggplot()
	bem1e_p <- bem1e_p + geom_point(data=caldata,aes(yhat(bem1_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem1e_p <- bem1e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1e_p <- bem1e_p + coord_cartesian(xlim=c(0,80000),ylim=c(0,80000))
	bem1e_p <- bem1e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- ggplot()
	bem2e_p <- bem2e_p + geom_point(data=caldata,aes(yhat(bem2_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem2e_p <- bem2e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2e_p <- bem2e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- bem2e_p + coord_cartesian(xlim=c(0,80000),ylim=c(0,80000))
	bem1el_p <- ggplot()
	bem1el_p <- bem1el_p + geom_point(data=caldata,aes(log(yhat(bem1_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem1el_p <- bem1el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1el_p <- bem1el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem1el_p <- bem1el_p + coord_cartesian(xlim=c(0,11.5),ylim=c(0,11.5))
	bem2el_p <- ggplot()
	bem2el_p <- bem2el_p + geom_point(data=caldata,aes(log(yhat(bem2_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem2el_p <- bem2el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2el_p <- bem2el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem2el_p <- bem2el_p + coord_cartesian(xlim=c(0,11.5),ylim=c(0,11.5))
	p <- grid.arrange(bem1_p,bem2_p,bem1e_p,bem2e_p,bem1el_p,bem2el_p,nrow=3,ncol=2)
	ggsave("f2.pdf",plot=p,scale=1.57,limitsize=FALSE)
}

plotFig3 <- function(caldata,models,alpha)
{	
	x <- seq(10000,150000,length.out=25)
	x <- data.frame(x)
	colnames(x) <- c("X")
	#####
	bem1_model <- models[[1]]
	bem1_yhats <- yhat(bem1_model,x)
	bem1_nlqrmodels <- models[[2]]
	bem1_bmodels <- models[[3]]
	bem1_pis <- getPredictionIntervals(bem1_nlqrmodels,x)
	bem1_cis <- getConfidenceIntervals(bem1_bmodels,x,alpha)
	bem1_df <- data.frame(x,bem1_yhats,bem1_pis[,1],bem1_pis[,2],bem1_cis[,1],bem1_cis[,2])
	colnames(bem1_df) <- c("x","y_bem1","pil_bem1","piu_bem1","cil_bem1","ciu_bem1")
	bem1_p <- ggplot(data=caldata,aes(X,y))
	for(i in 1:length(bem1_bmodels))
	{
		bem1_yboot <- yhat(bem1_bmodels[[i]],x)
		bem1_dfm <- data.frame(x,bem1_yboot)
		colnames(bem1_dfm) <- c("x","y")
		if(i == 1)
		{
			bem1_p <- bem1_p + geom_line(data=bem1_dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			bem1_p <- bem1_p + geom_line(data=bem1_dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,pil_bem1,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,piu_bem1),color='black',linetype='twodash',alpha=1)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,cil_bem1,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,ciu_bem1),color='black',linetype='dashed',alpha=1)
	bem1_p <- bem1_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,y_bem1,color='model',linetype='model',alpha='model'))
	bem1_p <- bem1_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + theme(legend.position=c(0.065,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust = 0.5))
	bem1_p <- bem1_p + coord_cartesian(xlim=c(10000,125000),ylim=c(0,80000))
	symbol<-"\u2265"
	bem1_p <- bem1_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste(beta[1],"(D"^{2},"H",rho["b"],")"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,")]  ","(D"^{2},"H",rho["b"]," > 10000kg)")))
	#####
	bem2_model <- models[[4]]
	bem2_yhats <- yhat(bem2_model,x)
	bem2_nlqrmodels <- models[[5]]
	bem2_bmodels <- models[[6]]
	bem2_pis <- getPredictionIntervals(bem2_nlqrmodels,x)
	bem2_cis <- getConfidenceIntervals(bem2_bmodels,x,alpha)
	bem2_df <- data.frame(x,bem2_yhats,bem2_pis[,1],bem2_pis[,2],bem2_cis[,1],bem2_cis[,2])
	colnames(bem2_df) <- c("x","y_bem2","pil_bem2","piu_bem2","cil_bem2","ciu_bem2")
	bem2_p <- ggplot(data=caldata,aes(X,y))
	for(i in 1:length(bem2_bmodels))
	{
		bem2_yboot <- yhat(bem2_bmodels[[i]],x)
		bem2_dfm <- data.frame(x,bem2_yboot)
		colnames(bem2_dfm) <- c("x","y")
		if(i == 1)
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(x,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(x,y),color='grey',alpha=0.5)
		}
	}
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,pil_bem2,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,piu_bem2),color='black',linetype='twodash',alpha=1)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,cil_bem2,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,ciu_bem2),color='black',linetype='dashed',alpha=1)
	bem2_p <- bem2_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,y_bem2,color='model',linetype='model',alpha='model'))
	bem2_p <- bem2_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + theme(legend.position=c(0.065,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem2_p <- bem2_p + coord_cartesian(xlim=c(10000,125000),ylim=c(0,80000))
	bem2_p <- bem2_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste(beta[1],"(D"^{2},"H",rho["b"],")"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]  ","(D"^{2},"H",rho["b"]," > 10000kg)")))
	bem1e_p <- ggplot()
	bem1e_p <- bem1e_p + geom_point(data=caldata,aes(yhat(bem1_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem1e_p <- bem1e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1e_p <- bem1e_p + coord_cartesian(xlim=c(1000,80000),ylim=c(1000,80000))
	bem1e_p <- bem1e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- ggplot()
	bem2e_p <- bem2e_p + geom_point(data=caldata,aes(yhat(bem2_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem2e_p <- bem2e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2e_p <- bem2e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- bem2e_p + coord_cartesian(xlim=c(1000,80000),ylim=c(1000,80000))
	bem1el_p <- ggplot()
	bem1el_p <- bem1el_p + geom_point(data=caldata,aes(log(yhat(bem1_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem1el_p <- bem1el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1el_p <- bem1el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem1el_p <- bem1el_p + coord_cartesian(xlim=c(7.75,11.5),ylim=c(7.75,11.5))
	bem2el_p <- ggplot()
	bem2el_p <- bem2el_p + geom_point(data=caldata,aes(log(yhat(bem2_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem2el_p <- bem2el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2el_p <- bem2el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem2el_p <- bem2el_p + coord_cartesian(xlim=c(7.75,11.5),ylim=c(7.75,11.5))
	p <- grid.arrange(bem1_p,bem2_p,bem1e_p,bem2e_p,bem1el_p,bem2el_p,nrow=3,ncol=2)
	ggsave("f3.pdf",plot=p,scale=1.57,limitsize=FALSE)
}

plotFig4 <- function(models,results)
{
	psum <- data.frame()
	i <- 1
	while(i < length(results))
	{
		psum <- rbind(psum,results[[i+1]])
		i <- i + 2
	}
	results <- psum
	bem1_model <- models[[1]]
	bem2_model <- models[[4]]
	bem1_name <- as.expression(bquote("BEM-1: "~.(toString(round(coefficients(bem1_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(bem1_model)[2],3)))}~" (additive)"))
	bem2_name <- as.expression(bquote("BEM-2: "~.(toString(round(coefficients(bem2_model)[1],3)))~D^{2}*H*rho^{.(toString(round(coefficients(bem2_model)[2],3)))}~" (multiplicative)"))
	xseq <- seq(1,nrow(results),1)
	p <- ggplot(data=results)
	p <- p + geom_pointrange(data=results,aes(x=xseq,y=nls1_p_agb,ymin=nls1_p_agb_li,ymax=nls1_p_agb_ui,color='BEM1',shape='BEM1'))
	p <- p + geom_pointrange(data=results,aes(x=xseq,y=nls2_p_agb,ymin=nls2_p_agb_li,ymax=nls2_p_agb_ui,color='BEM2',shape='BEM2'))
	p <- p + geom_text_repel(data=results,aes(xseq,nls1_p_agb,label=pid),direction=c("x"),max.iter=1000000)#),direction=c("x"),point.padding = NA)
	p <- p + scale_color_manual(name='',values=c('BEM1'='red','BEM2'='black'),labels=c('BEM1'=bem1_name,'BEM2'=bem2_name),breaks=c('BEM1','BEM2'))
	p <- p + scale_shape_manual(name='',values=c('BEM1'=20,'BEM2'=18),labels=c('BEM1'=bem1_name,'BEM2'=bem2_name),breaks=c('BEM1','BEM2'))
	p <- p + coord_cartesian(xlim=c(1,9),ylim=c(100000,700000))
	p <- p + labs(x=expression(paste("Mean basal area (",m^2,")")),y=expression(paste("AGB"," (kg)")))
	p <- p + scale_y_continuous(labels=scales::comma)
	p <- p + scale_x_continuous(breaks = NULL)
	p <- p + theme(legend.position=c(0.95,0.03),legend.justification=c(1,0),legend.text.align=0,legend.title=element_blank(),legend.key=element_blank(),aspect.ratio=1/((1+sqrt(5))/2),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#	p <- p + guides(colour = guide_legend(reverse=T)) + guides(shape = guide_legend(reverse=T))
	ggsave("f4.pdf",plot=p,scale=1,limitsize=FALSE)
}

plotSuppFig1 <- function(caldata)
{
	ols_model <- lm(formula=log(y)~log(X),data=caldata)
	p1 <- ggplot()
	p1 <- p1 + geom_point(data=caldata,aes(ols_model$fitted.values,resid(ols_model)),size=1,alpha=0.33,stroke=0)
	p1 <- p1 + geom_hline(yintercept=0,linetype="dashed",color="red",alpha=0.75)
	p1 <- p1 + coord_cartesian(xlim=c(0,11.5),ylim = c(-1.5,1.5))
	p1 <- p1 + labs(x=expression(paste("Estimated [ln(AGB)]")),y=expression(paste("Residuals [","ln(AGB)"," - ","ln(",hat("AGB"),")]"))) 
	p1 <- p1 + theme(aspect.ratio=1/((1+sqrt(5))/2))
	p2 <- ggplot(ols_model, aes(sample = rstandard(ols_model))) 
	p2 <- p2 + geom_qq(size=1,alpha=0.75,stroke=0)
	p2 <- p2 + labs(x="Theoretical quantile",y="Sample quantile")	
	p2 <- p2 + stat_qq_line(linetype="dashed",color="red",alpha=0.75)
	p2 <- p2 + coord_cartesian(xlim=c(-4.5,4.5), ylim = c(-4.5,4.5))
	p2 <- p2 + scale_y_continuous(breaks=seq(-4,4,len = 5))
	p2 <- p2 + scale_x_continuous(breaks=seq(-4,4,len = 5))
	p2 <- p2 + theme(aspect.ratio=1)
	p <- grid.arrange(p1,p2,widths=c(1.51,1))
	ggsave("suppf1.pdf",plot=p,scale=1,limitsize=FALSE)
}

plotSuppFig2 <- function(caldata,dcaldata,models,alpha)
{	
	x <- seq(0,150000,length.out=25)
	x <- data.frame(x)
	colnames(x) <- c("X")
	#####
	bem1_model <- models[[1]]
	bem1_yhats <- yhat(bem1_model,x)
	bem1_nlqrmodels <- models[[2]]
	bem1_bmodels <- models[[3]]
	bem1_pis <- getPredictionIntervals(bem1_nlqrmodels,x)
	bem1_cis <- getConfidenceIntervals(bem1_bmodels,x,alpha)
	bem1_df <- data.frame(x,bem1_yhats,bem1_pis[,1],bem1_pis[,2],bem1_cis[,1],bem1_cis[,2])
	colnames(bem1_df) <- c("x","y_bem1","pil_bem1","piu_bem1","cil_bem1","ciu_bem1")
	bem1_p <- ggplot(data=caldata,aes(X,y))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,pil_bem1,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,piu_bem1),color='black',linetype='twodash',alpha=1)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,cil_bem1,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,ciu_bem1),color='black',linetype='dashed',alpha=1)
	bem1_p <- bem1_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,y_bem1,color='model',linetype='model',alpha='model'))
	bem1_p <- bem1_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + theme(legend.position=c(0.045,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust = 0.5))
	bem1_p <- bem1_p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	bem1_p <- bem1_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[1],"(D"^{2},"H",rho["b"],")"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]  (multiplicative)")))
	#####
	dx <- seq(0,2.5,length.out=25)
	dx <- data.frame(dx)
	colnames(dx) <- c("X")
	bem2_model <- models[[4]]
	bem2_nlqrmodels <- models[[5]]
	bem2_bmodels <- models[[6]]
	bem2_yhats <- yhat(bem2_model,dx)
	bem2_pis <- getPredictionIntervals(bem2_nlqrmodels,dx)
	bem2_cis <- getConfidenceIntervals(bem2_bmodels,dx,alpha)
	bem2_df <- data.frame(dx,bem2_yhats,bem2_pis[,1],bem2_pis[,2],bem2_cis[,1],bem2_cis[,2])
	colnames(bem2_df) <- c("x","y_bem2","pil_bem2","piu_bem2","cil_bem2","ciu_bem2")
	bem2_p <- ggplot(data=dcaldata,aes(X,y))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,pil_bem2,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,piu_bem2),color='black',linetype='twodash',alpha=1)	
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,cil_bem2,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,ciu_bem2),color='black',linetype='dashed',alpha=1)
	bem2_p <- bem2_p + geom_point(data=dcaldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,y_bem2,color='model',linetype='model',alpha='model'))
	bem2_p <- bem2_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + theme(legend.position=c(0.045,0.955),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem2_p <- bem2_p + coord_cartesian(xlim=c(0,2.25),ylim=c(0,80000))
	bem2_p <- bem2_p + labs(x=expression(paste("D (m)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[1],"(D)"^beta[2]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D)"^"k",")]  (multiplicative)")))	
	bem1e_p <- ggplot()
	bem1e_p <- bem1e_p + geom_point(data=caldata,aes(yhat(bem1_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem1e_p <- bem1e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1e_p <- bem1e_p + coord_cartesian(xlim=c(0,80000),ylim=c(0,80000))
	bem1e_p <- bem1e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- ggplot()
	bem2e_p <- bem2e_p + geom_point(data=dcaldata,aes(yhat(bem2_model,dcaldata),dcaldata$y),size=1.5,alpha=0.75,stroke=0)
	bem2e_p <- bem2e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2e_p <- bem2e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- bem2e_p + coord_cartesian(xlim=c(0,80000),ylim=c(0,80000))
	bem1el_p <- ggplot()
	bem1el_p <- bem1el_p + geom_point(data=caldata,aes(log(yhat(bem1_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem1el_p <- bem1el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1el_p <- bem1el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem1el_p <- bem1el_p + coord_cartesian(xlim=c(0,11.5),ylim=c(0,11.5))
	bem2el_p <- ggplot()
	bem2el_p <- bem2el_p + geom_point(data=dcaldata,aes(log(yhat(bem2_model,dcaldata)),log(dcaldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem2el_p <- bem2el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2el_p <- bem2el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem2el_p <- bem2el_p + coord_cartesian(xlim=c(0,11.5),ylim=c(0,11.5))
	p <- grid.arrange(bem1_p,bem2_p,bem1e_p,bem2e_p,bem1el_p,bem2el_p,nrow=3,ncol=2)
	ggsave("suppf2.pdf",plot=p,scale=1.57,limitsize=FALSE)
}
