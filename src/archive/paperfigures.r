#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(ggplot2)) 
suppressMessages(library(ggrepel)) 
suppressMessages(library(gridExtra)) 

getModelExpForGgplotBem <- function(model)
{
	b0 <- toString(round(param(model)[1],3))
	b1 <- toString(round(param(model)[2],3))		
	k <- toString(round(param(model)[3],3))
	sigma <- toString(max(round(exp(param(model)[4]),3),0.001))
#	return(as.expression(bquote(.(b0)~(D^{2}*H*rho["b"])^{.(b1)}~sigma^{2}==.(sigma)*","~k==.(k))))
	return(as.expression(bquote(beta["0"]==.(b0)*","~beta["1"]==.(b1)*","~sigma^{2}==.(sigma)*","~k==.(k))))
}

getModelExpForGgplotBemLimit <- function(model,limit)
{
	b0 <- toString(round(param(model)[1],3))
	b1 <- toString(round(param(model)[2],3))		
	k <- toString(round(param(model)[3],3))
	sigma <- toString(max(round(exp(param(model)[4]),3),0.001))
	return(as.expression(bquote(beta["0"]==.(b0)*","~beta["1"]==.(b1)*","~sigma^{2}==.(sigma)*","~k==.(k)~~(D^{2}*H*rho["b"]~">"~.(toString(limit))*"kg"))))
}

getModelExpForGgplotBemD <- function(model)
{
	b0 <- toString(round(coefficients(model)[1],0))
	b1 <- toString(round(coefficients(model)[2],3))		
	k <- toString(round(param(model)[3],3))
	sigma <- toString(round(exp(param(model)[4]),0))
	return(as.expression(bquote(beta["0"]==.(b0)*","~beta["1"]==.(b1)*","~sigma^{2}==.(sigma)*","~k==.(k))))
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

plotFig2 <- function(caldata,limitedcaldata,models,alpha)
{	
	x <- seq(0,150000,length.out=25)
	x <- data.frame(x)
	colnames(x) <- c("X")
	xl <- seq(10000,150000,length.out=25)
	xl <- data.frame(xl)
	colnames(xl) <- c("X")
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
	bem1_p <- bem1_p + theme(legend.position=c(0,1),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem1_p <- bem1_p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	symbol<-"\u2265"
	bem1_p <- bem1_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("NL-MLEm: AGB = ",beta[0],"(D"^{2},"H",rho["b"],")"^beta[1]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]")))
	#####
	bem2_model <- models[[4]]
	bem2_yhats <- yhat(bem2_model,xl)
	bem2_nlqrmodels <- models[[5]]
	bem2_bmodels <- models[[6]]
	bem2_pis <- getPredictionIntervals(bem2_nlqrmodels,xl)
	bem2_cis <- getConfidenceIntervals(bem2_bmodels,xl,alpha)
	bem2_df <- data.frame(xl,bem2_yhats,bem2_pis[,1],bem2_pis[,2],bem2_cis[,1],bem2_cis[,2])
	colnames(bem2_df) <- c("xl","y_bem2","pil_bem2","piu_bem2","cil_bem2","ciu_bem2")
	bem2_p <- ggplot(data=limitedcaldata,aes(X,y))
	for(i in 1:length(bem2_bmodels))
	{
		bem2_yboot <- yhat(bem2_bmodels[[i]],xl)
		bem2_dfm <- data.frame(xl,bem2_yboot)
		colnames(bem2_dfm) <- c("xl","y")
		if(i == 1)
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(xl,y,color='bstrap',linetype='bstrap',alpha='bstrap'))
		}
		else
		{
			bem2_p <- bem2_p + geom_line(data=bem2_dfm,aes(xl,y),color='grey',alpha=0.5)
		}
	}
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(xl,pil_bem2,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(xl,piu_bem2),color='black',linetype='twodash',alpha=1)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(xl,cil_bem2,color='cinterval',linetype='cinterval',alpha='cinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(xl,ciu_bem2),color='black',linetype='dashed',alpha=1)
	bem2_p <- bem2_p + geom_point(data=limitedcaldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(xl,y_bem2,color='model',linetype='model',alpha='model'))
	bem2_p <- bem2_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + theme(legend.position=c(0,1),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem2_p <- bem2_p + coord_cartesian(xlim=c(10000,125000),ylim=c(0,80000))
	bem2_p <- bem2_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("NL-MLEm ","(D"^{2},"H",rho["b"]," > 10000kg)")))
	bem1e_p <- ggplot()
	bem1e_p <- bem1e_p + geom_point(data=caldata,aes(yhat(bem1_model,caldata),caldata$y),size=1.5,alpha=0.75,stroke=0)
	bem1e_p <- bem1e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1e_p <- bem1e_p + coord_cartesian(xlim=c(0,80000),ylim=c(0,80000))
	bem1e_p <- bem1e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- ggplot()
	bem2e_p <- bem2e_p + geom_point(data=limitedcaldata,aes(yhat(bem2_model,limitedcaldata),limitedcaldata$y),size=1.5,alpha=0.75,stroke=0)
	bem2e_p <- bem2e_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2e_p <- bem2e_p + labs(x=expression(paste("Estimated AGB"," (kg)")),y=expression(paste("Observed AGB"," (kg)")))
	bem2e_p <- bem2e_p + coord_cartesian(xlim=c(1000,80000),ylim=c(1000,80000))
	bem1el_p <- ggplot()
	bem1el_p <- bem1el_p + geom_point(data=caldata,aes(log(yhat(bem1_model,caldata)),log(caldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem1el_p <- bem1el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem1el_p <- bem1el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem1el_p <- bem1el_p + coord_cartesian(xlim=c(0,11.5),ylim=c(0,11.5))
	bem2el_p <- ggplot()
	bem2el_p <- bem2el_p + geom_point(data=limitedcaldata,aes(log(yhat(bem2_model,limitedcaldata)),log(limitedcaldata$y)),size=1.5,alpha=0.75,stroke=0)
	bem2el_p <- bem2el_p + geom_abline(intercept=0,slope=1,color='red',linetype='dashed')
	bem2el_p <- bem2el_p + labs(x=expression(paste("Estimated ln(AGB)")),y=expression(paste("Observed ln(AGB)")))
	bem2el_p <- bem2el_p + coord_cartesian(xlim=c(7.75,11.5),ylim=c(7.75,11.5))
	p <- grid.arrange(bem1_p,bem2_p,bem1e_p,bem2e_p,bem1el_p,bem2el_p,nrow=3,ncol=2)
	ggsave("f2.pdf",plot=p,scale=1.57,limitsize=FALSE)
}

plotFig3 <- function(caldata,models,alpha)
{
	x1 <- seq(0,150000,length.out=25)
	x1 <- data.frame(x1)
	colnames(x1) <- c("X")
	x2 <- seq(5000,150000,length.out=25)
	x2 <- data.frame(x2)
	colnames(x2) <- c("X")
	x3 <- seq(10000,150000,length.out=25)
	x3 <- data.frame(x3)
	colnames(x3) <- c("X")
	x4 <- seq(20000,150000,length.out=25)
	x4 <- data.frame(x4)
	colnames(x4) <- c("X")
	tmp1 <- yhat(models[[1]],x1)
	cis <- getConfidenceIntervals(models[[2]],x1,alpha)
	tmp2 <- yhat(models[[3]],x2)
	tmp3 <- yhat(models[[4]],x3)
	tmp4 <- yhat(models[[5]],x4)
	df <- data.frame(x1,x2,x3,x4,tmp1,cis[,1],cis[,2],tmp2,tmp3,tmp4)
	colnames(df) <- c("x1","x2","x3","x4","y1","cil","ciu","y2","y3","y4")
	p <- ggplot(data=caldata,aes(X,y))
	p <- p + geom_vline(xintercept = 0, linetype="solid",color = "black", size=0.5,alpha=1)
	p <- p + geom_vline(xintercept = 5000, linetype="dashed",color = "red", size=0.5,alpha=1)
	p <- p + geom_vline(xintercept = 10000, linetype="dotdash",color = "blue", size=0.5,alpha=1)
	p <- p + geom_vline(xintercept = 20000, linetype="twodash",color = "forestgreen", size=0.5,alpha=1)
	p <- p + geom_point(data=caldata,aes(X,y),size=2.5,color="black",alpha=0.75,stroke=0)
	p <- p + geom_line(data=df,aes(x1,cil,color='ci',linetype='ci',alpha='ci'))
	p <- p + geom_line(data=df,aes(x1,ciu),color='grey',linetype='solid',alpha=1)
	p <- p + geom_line(data=df,aes(x1,y1,color='m1',linetype='m1',alpha='m1'))
	p <- p + geom_line(data=df,aes(x2,y2,color='m2',linetype='m2',alpha='m2'))
	p <- p + geom_line(data=df,aes(x3,y3,color='m3',linetype='m3',alpha='m3'))
	p <- p + geom_line(data=df,aes(x4,y4,color='m4',linetype='m4',alpha='m4'))
	p <- p + scale_color_manual(name='',values=c('m1'='black','ci'='grey','m2'='red','m3'='blue','m4'='forestgreen'),labels=c('m1'=getModelExpForGgplotBemLimit(models[[1]],0),'ci'='95% confidence intervals','m2'=getModelExpForGgplotBemLimit(models[[3]],5000),'m3'=getModelExpForGgplotBemLimit(models[[4]],10000),'m4'=getModelExpForGgplotBemLimit(models[[5]],20000)),breaks=c('m1','ci','m2','m3','m4'))
	p <- p + scale_alpha_manual(name='',values=c('m1'=1,'ci'=1,'m2'=1,'m3'=1,'m4'=1),labels=c('m1'=getModelExpForGgplotBemLimit(models[[1]],0),'ci'='95% confidence intervals','m2'=getModelExpForGgplotBemLimit(models[[3]],5000),'m3'=getModelExpForGgplotBemLimit(models[[4]],10000),'m4'=getModelExpForGgplotBemLimit(models[[5]],20000)),breaks=c('m1','ci','m2','m3','m4'))
	p <- p + scale_linetype_manual(name='',values=c('m1'='solid','ci'='solid','m2'='dashed','m3'='dotdash','m4'='twodash'),labels=c('m1'=getModelExpForGgplotBemLimit(models[[1]],0),'ci'='95% confidence intervals','m2'=getModelExpForGgplotBemLimit(models[[3]],5000),'m3'=getModelExpForGgplotBemLimit(models[[4]],10000),'m4'=getModelExpForGgplotBemLimit(models[[5]],20000)),breaks=c('m1','ci','m2','m3','m4'))
	p <- p + theme(legend.position=c(0,1),legend.justification=c(0,1),legend.text.align=0,legend.key=element_blank(),legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	p <- p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	p <- p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[0],"(D"^{2},"H",rho["b"],")"^beta[1]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]")))
	ggsave("f3.pdf",plot=p,scale=1,limitsize=FALSE)
}

plotFig4 <- function(results)
{
	bem1_name <- 'NL-MLEm'
	bem2_name <- 'LNL-MLEm'
	xseq <- seq(1,nrow(results),1)
	p <- ggplot(data=results)
	p <- p + geom_pointrange(data=results,aes(x=xseq+0,y=nls2_p_agb,ymin=nls2_p_agb_li,ymax=nls2_p_agb_ui,color='BEM2',shape='BEM2'))
	p <- p + geom_pointrange(data=results,aes(x=xseq+0.1,y=nls1_p_agb,ymin=nls1_p_agb_li,ymax=nls1_p_agb_ui,color='BEM1',shape='BEM1'))
	p <- p + scale_color_manual(name='',values=c('BEM1'='red','BEM2'='black'),labels=c('BEM1'=bem1_name,'BEM2'=bem2_name),breaks=c('BEM1','BEM2'))
	p <- p + scale_shape_manual(name='',values=c('BEM1'=20,'BEM2'=18),labels=c('BEM1'=bem1_name,'BEM2'=bem2_name),breaks=c('BEM1','BEM2'))
	p <- p + coord_cartesian(xlim=c(1,9),ylim=c(100000,700000))
	p <- p + labs(x=expression(paste("Mean basal area (",m^2,")")),y=expression(paste("AGB"," (kg)")))
	p <- p + scale_y_continuous(labels=scales::comma)
	p <- p + scale_x_continuous(breaks = NULL)
	p <- p + theme(legend.position=c(0.95,0.04),legend.justification=c(1,0),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1/((1+sqrt(5))/2),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#	p <- p + theme(legend.position=c(0.95,0.03),legend.justification=c(1,0),legend.text.align=0,legend.title=element_blank(),legend.key=element_blank(),aspect.ratio=1/((1+sqrt(5))/2),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#	p <- p + guides(colour = guide_legend(reverse=T)) + guides(shape = guide_legend(reverse=T))
	ggsave("f4.pdf",plot=p,scale=1,limitsize=FALSE)
	p <- p + geom_text_repel(data=results,aes(xseq,nls1_p_agb,label=pid),direction=c("x"),max.iter=1000000)#),direction=c("x"),point.padding = NA)
	ggsave("tmp4.pdf",plot=p,scale=1,limitsize=FALSE)
}

plotSuppFig1 <- function(caldata)
{
	ols_model <- lm(formula=log(y)~log(X),data=caldata)
#	p1 <- ggplot()
#	p1 <- p1 + geom_point(data=caldata,aes(ols_model$fitted.values,resid(ols_model)),size=1,alpha=0.33,stroke=0)
#	p1 <- p1 + geom_hline(yintercept=0,linetype="dashed",color="red",alpha=0.75)
#	p1 <- p1 + coord_cartesian(xlim=c(0,11.5),ylim = c(-1.5,1.5))
#	p1 <- p1 + labs(x=expression(paste("ln(",hat("AGB"),")")),y=expression(paste("ln(AGB)"," - ","ln(",hat("AGB"),")"))) 
#	p1 <- p1 + theme(aspect.ratio=1/((1+sqrt(5))/2))
#	p2 <- ggplot(ols_model, aes(sample = rstandard(ols_model))) 
#	p2 <- p2 + geom_qq(size=1,alpha=0.75,stroke=0)
#	p2 <- p2 + labs(x="Theoretical quantile",y="Sample quantile")	
#	p2 <- p2 + stat_qq_line(linetype="dashed",color="red",alpha=0.75)
#	p2 <- p2 + coord_cartesian(xlim=c(-4.5,4.5), ylim = c(-4.5,4.5))
#	p2 <- p2 + scale_y_continuous(breaks=seq(-4,4,len = 5))
#	p2 <- p2 + scale_x_continuous(breaks=seq(-4,4,len = 5))
#	p2 <- p2 + theme(aspect.ratio=1)
#	ggsave("suppf1.pdf",plot=grid.arrange(p1,p2,widths=c(1.51,1)),scale=1,limitsize=FALSE)
}

plotSuppFig2 <- function(caldata,dcaldata,models)
{	
	alpha <- 0.05
	x <- seq(0,150000,length.out=25)
	x <- data.frame(x)
	colnames(x) <- c("X")
	#####
	bem1_model <- models[[1]]
	bem1_yhats <- yhat(bem1_model,x)
	bem1_nlqrmodels <- models[[2]]
	bem1_pis <- getPredictionIntervals(bem1_nlqrmodels,x)
	bem1_df <- data.frame(x,bem1_yhats,bem1_pis[,1],bem1_pis[,2])
	colnames(bem1_df) <- c("x","y_bem1","pil_bem1","piu_bem1")
	bem1_p <- ggplot(data=caldata,aes(X,y))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,pil_bem1,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,piu_bem1),color='black',linetype='twodash',alpha=1)
	bem1_p <- bem1_p + geom_point(data=caldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem1_p <- bem1_p + geom_line(data=bem1_df,aes(x,y_bem1,color='model',linetype='model',alpha='model'))
	bem1_p <- bem1_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBem(bem1_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem1_p <- bem1_p + theme(legend.position=c(0,1),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem1_p <- bem1_p + coord_cartesian(xlim=c(0,125000),ylim=c(0,80000))
	bem1_p <- bem1_p + labs(x=expression(paste("D"^{2},"H",rho["b"]," (kg)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[0],"(D"^{2},"H",rho["b"],")"^beta[1]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D"^{2},"H",,rho["b"],")"^"k",")]")))
	#####
	dx <- seq(0,2.5,length.out=25)
	dx <- data.frame(dx)
	colnames(dx) <- c("X")
	bem2_model <- models[[3]]
	bem2_nlqrmodels <- models[[4]]
	bem2_yhats <- yhat(bem2_model,dx)
	bem2_pis <- getPredictionIntervals(bem2_nlqrmodels,dx)
	bem2_df <- data.frame(dx,bem2_yhats,bem2_pis[,1],bem2_pis[,2])
	colnames(bem2_df) <- c("x","y_bem2","pil_bem2","piu_bem2")
	bem2_p <- ggplot(data=dcaldata,aes(X,y))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,pil_bem2,color='pinterval',linetype='pinterval',alpha='pinterval'))
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,piu_bem2),color='black',linetype='twodash',alpha=1)	
	bem2_p <- bem2_p + geom_point(data=dcaldata,aes(X,y),size=1.5,alpha=0.75,stroke=0)
	bem2_p <- bem2_p + geom_line(data=bem2_df,aes(x,y_bem2,color='model',linetype='model',alpha='model'))
	bem2_p <- bem2_p + scale_color_manual(name='',values=c('model'='red','pinterval'='black','cinterval'='black','bstrap'='grey'),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_linetype_manual(name='',values=c('model'='solid','pinterval'='twodash','cinterval'='dashed','bstrap'='solid'),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + scale_alpha_manual(name='',values=c('model'=1,'pinterval'=1,'cinterval'=1,'bstrap'=0.5),labels=c('model'=getModelExpForGgplotBemD(bem2_model),'pinterval'=paste(toString(100-(alpha*100)),'% ','prediction intervals',sep=''),'cinterval'=paste(toString(100-(alpha*100)),'% ','confidence intervals',sep=''),'bstrap'='Bootstrap samples'),breaks=c('model','pinterval','cinterval','bstrap'))
	bem2_p <- bem2_p + theme(legend.position=c(0,1),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5))
	bem2_p <- bem2_p + coord_cartesian(xlim=c(0,2.25),ylim=c(0,80000))
	bem2_p <- bem2_p + labs(x=expression(paste("D (m)")),y=expression(paste("AGB"," (kg)")),title=expression(paste("AGB = ",beta[0],"(D)"^beta[1]," + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,"(D)"^"k",")]")))	
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

plotSuppFig3 <- function(caldata)
{
	model <- nlreg(y ~ b0 * X ** b1,data=caldata,weights=~X**k,start=c(b0=0.1,b1=1,k=1),control=list(x.tol=1e-10,rel.tol=1e-10,step.min=5120,maxit=250))
	vf <- sqrt((exp(param(model)[4]) * caldata$X ** param(model)[3]))
	p1 <- ggplot()
	p1 <- p1 + geom_point(data=caldata,aes(log(fitted(model)),(caldata$y-fitted(model))/vf),size=1,alpha=0.33,stroke=0)
	p1 <- p1 + geom_hline(yintercept=0,linetype="dashed",color="red",alpha=0.75)
	p1 <- p1 + coord_cartesian(xlim=c(0,11.5),ylim = c(-3,7)) 
	p1 <- p1 + theme(aspect.ratio=1/((1+sqrt(5))/2))
	p1 <- p1 + labs(x=expression(paste("ln(",hat("AGB"),")")),y=expression(paste("(AGB"," - ",hat("AGB"),")","(",sigma^2,"(D"^2,"H",rho["b"],")"^k,")"^-frac(1,2)))) 
	p2 <- ggplot(data=caldata,aes(sample=(caldata$y-fitted(model))/vf))
	p2 <- p2 + geom_qq(size=1,alpha=0.75,stroke=0)
	p2 <- p2 + labs(x="Theoretical quantile",y="Sample quantile")	
	p2 <- p2 + stat_qq_line(linetype="dashed",color="red",alpha=0.75)
	p2 <- p2 + theme(aspect.ratio=1)
	p2 <- p2 + scale_y_continuous(breaks=seq(-4,4,len = 5))
	p2 <- p2 + scale_x_continuous(breaks=seq(-4,4,len = 5))
	ggsave("suppf3a.pdf",plot=grid.arrange(p1,p2,widths=c(1.51,1)),scale=1,limitsize=FALSE)
	X <- 10000
	nn <- 200
	segcaldata <- head(caldata[order(abs(caldata$X-X)),],nn)
	segmodel <- nlreg(y ~ b0 * X ** b1,data=segcaldata,weights=~X**k,start=c(b0=0.1,b1=1,k=1),control=list(x.tol=1e-10,rel.tol=1e-10,step.min=5120,maxit=250))
	segvf <- sqrt((exp(param(segmodel)[4]) * segcaldata$X ** param(segmodel)[3]))
	p3 <- ggplot()
	p3 <- p3 + geom_point(data=segcaldata,aes(fitted(segmodel),(segcaldata$y-fitted(segmodel))/segvf),size=1,alpha=0.33,stroke=0)
	p3 <- p3 + geom_hline(yintercept=0,linetype="dashed",color="red",alpha=0.75)
#	p3 <- p3 + coord_cartesian(xlim=c(0,11.5),ylim = c(-3,7)) 
	p3 <- p3 + theme(aspect.ratio=1/((1+sqrt(5))/2))
	p3 <- p3 + labs(x=expression(paste(hat("AGB"))),y=expression(paste("(AGB"," - ",hat("AGB"),")","(",sigma^2,"(D"^2,"H",rho["b"],")"^k,")"^-frac(1,2)))) 
	p4 <- ggplot(data=segcaldata,aes(sample=(segcaldata$y-fitted(segmodel))/segvf))
	p4 <- p4 + geom_qq(size=1,alpha=0.75,stroke=0)
	p4 <- p4 + labs(x="Theoretical quantile",y="Sample quantile")	
	p4 <- p4 + stat_qq_line(linetype="dashed",color="red",alpha=0.75)
	p4 <- p4 + theme(aspect.ratio=1)
	p4 <- p4 + scale_y_continuous(breaks=seq(-4,4,len = 5))
	p4 <- p4 + scale_x_continuous(breaks=seq(-4,4,len = 5))
	ggsave("suppf3b.pdf",plot=grid.arrange(p3,p4,widths=c(1.51,1)),scale=1,limitsize=FALSE)
}
