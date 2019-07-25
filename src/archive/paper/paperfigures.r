#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

suppressMessages(library(ggplot2)) 
suppressMessages(library(RColorBrewer))

plotFig1 <- function(data)
{
	f1 <- ggplot(data=data,aes(d,agb))
	f1 <- f1 + geom_point(size=1.5,alpha=0.75,stroke=0)
	f1 <- f1 + scale_y_continuous(breaks=c(0,20000,40000,60000,80000), labels = function(x){paste0(x/1000)})
	f1 <- f1 + coord_cartesian(ylim = c(0,80000))
	f1 <- f1 + theme(aspect.ratio=1/1.61803398875,axis.title.x=element_blank(),axis.title.y=element_blank())
	f1 <- f1 + labs(x=expression(paste("D"," (m)")),y=expression(paste("AGB"," (kg)")))
	ggsave("f1.pdf",plot=f1,scale=1)
}

plotFig2 <- function(data,alpha,runs,ncpus)
{
	func_d <- log(agb) ~ log(d)
	model_d <- fitOLS(data,func_d)
	d_bresults <- bootOLS(data,func_d,runs,ncpus)
	d_b0_cis <- boot.ci(d_bresults,index=1,conf=0.95,type="bca")
	d_b1_cis <- boot.ci(d_bresults,index=2,conf=0.95,type="bca")
	d_b0_lower <- d_b0_cis$bca[4]
	d_b0_upper <- d_b0_cis$bca[5]
	d_b1_lower <- d_b1_cis$bca[4]
	d_b1_upper <- d_b1_cis$bca[5]
	#
	func_h <- log(agb) ~ log(h)
	model_h <- fitOLS(data,func_h)
	h_bresults <- bootOLS(data,func_h,runs,ncpus)
	h_b0_cis <- boot.ci(h_bresults,index=1,conf=0.95,type="bca")
	h_b1_cis <- boot.ci(h_bresults,index=2,conf=0.95,type="bca")
	h_b0_lower <- h_b0_cis$bca[4]
	h_b0_upper <- h_b0_cis$bca[5]
	h_b1_lower <- h_b1_cis$bca[4]
	h_b1_upper <- h_b1_cis$bca[5]
	#
	colours <- colorRampPalette(brewer.pal(8,"Dark2"))(length(levels(data$study)))
	colScale <- scale_colour_manual(name="study",values="colours")
	#######
	##B ~ D
	#######
	x_d <- seq(-4,max(log(data$d))*2,length.out=50)
	x_d <- data.frame(x_d)
	colnames(x_d) <- c("d")
	yhat_d <- coefficients(model_d)[1] + coefficients(model_d)[2] * x_d
	df <- data.frame(x_d,yhat_d)
	colnames(df) <- c("x","y")
	f2a <- ggplot()
	f2a <- f2a + geom_point(aes(x=log(data$d),y=log(data$agb),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2a <- f2a + geom_line(data=df,aes(x,y),colour="black")
	f2a <- f2a + geom_segment(aes(x=-3.1,xend=-2.9,y=9.75,yend=9.75),colour="black")
	f2a <- f2a + scale_colour_manual(name="study",values=colours)
	f2a <- f2a + coord_cartesian(xlim = c(-3,0.75),ylim=c(0,11)) 
	f2a <- f2a + annotate("text", x=-2.005, y=10.12, label = deparse(bquote(hat(beta[0]) ==~ .(format(round(coefficients(model_d)[[1]],3),nsmall=3)) ~ "(" * .(format(round(d_b0_lower,3),nsmall=3)) * "," ~ .(format(round(d_b0_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f2a <- f2a + annotate("text", x=-2.005, y=9.40, label = deparse(bquote(hat(beta[1]) ==~ .(format(round(coefficients(model_d)[[2]],3),nsmall=3)) ~ "(" * .(format(round(d_b1_lower,3),nsmall=3)) * "," ~ .(format(round(d_b1_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f2a <- f2a + annotate("text", x=-2.449, y=8.68, label = deparse(bquote(s ==~ .(format(round(summary(model_d)$sigma,3),nsmall=3)))),parse=TRUE,size=6)
	f2a <- f2a + theme(legend.position=c(0.012,0.92),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1,text = element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2a.pdf",plot=f2a,scale=1)
	#	
	f2b <- ggplot()
	f2b <- f2b + geom_point(aes(model_d$fitted.values,resid(model_d),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2b <- f2b + geom_hline(yintercept=0,linetype="dashed",color="black",size=0.25,alpha=1)
	f2b <- f2b + scale_colour_manual(name="study",values=colours)
	f2b <- f2b + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2b.pdf",plot=f2b,scale=1)
	#
	f2c <- ggplot()
	f2c <- f2c + geom_point(aes(x=qqnorm((log(data$agb)-yhatOLSLog(data,model_d))/summary(model_d)$sigma,plot.it=FALSE)$x,y=qqnorm((log(data$agb)-yhatOLSLog(data,model_d))/summary(model_d)$sigma,plot.it=FALSE)$y,color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2c <- f2c + scale_colour_manual(name="study",values=colours)
	f2c <- f2c + geom_abline(linetype="dashed",colour="black",size=0.25,alpha=1)
	f2c <- f2c + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2c.pdf",plot=f2c)
	#######
	##B ~ H
	#######
	x_h <- seq(0,max(log(data$h))*2,length.out=10)
	x_h <- data.frame(x_h)
	colnames(x_h) <- c("h")
	yhat_h <- coefficients(model_h)[1] + coefficients(model_h)[2] * x_h
	df <- data.frame(x_h,yhat_h)
	colnames(df) <- c("x","y")
	f2d <- ggplot()
	f2d <- f2d + geom_point(aes(x=log(data$h),y=log(data$agb),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2d <- f2d + geom_line(data=df,aes(x,y),colour="black")
	f2d <- f2d + geom_segment(aes(x=0.4,xend=0.6,y=9.75,yend=9.75),colour="black")
	f2d <- f2d + scale_colour_manual(name="study",values=colours)
	f2d <- f2d + coord_cartesian(xlim = c(0.25,4.25),ylim=c(0,11)) 
	f2d <- f2d + annotate("text", x=1.641, y=10.12, label = deparse(bquote(hat(beta[0]) ==~ .(format(round(coefficients(model_h)[[1]],3),nsmall=3))~"(" * .(format(round(h_b0_lower,3),nsmall=3)) ~ .(format(round(h_b0_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f2d <- f2d + annotate("text", x=1.53, y=9.40, label = deparse(bquote(hat(beta[1]) ==~ .(format(round(coefficients(model_h)[[2]],3),nsmall=3)) ~ "(" * .(format(round(h_b1_lower,3),nsmall=3)) * "," ~ .(format(round(h_b1_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f2d <- f2d + annotate("text", x=1.048, y=8.68, label = deparse(bquote(s ==~ .(format(round(summary(model_h)$sigma,3),nsmall=3)))),parse=TRUE,size=6)
	f2d <- f2d + theme(legend.position=c(0.012,0.92),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1,text = element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2d.pdf",plot=f2d,scale=1)
	#
	f2e <- ggplot()
	f2e <- f2e + geom_point(aes(model_h$fitted.values,resid(model_h),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2e <- f2e + geom_hline(yintercept=0,linetype="dashed",color="black",size=0.25,alpha=1)
	f2e <- f2e + scale_colour_manual(name="study",values=colours)
	f2e <- f2e + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2e.pdf",plot=f2e,scale=1)
	#
	f2f <- ggplot()
	f2f <- f2f + geom_point(aes(x=qqnorm((log(data$agb)-yhatOLSLog(data,model_h))/summary(model_h)$sigma,plot.it=FALSE)$x,y=qqnorm((log(data$agb)-yhatOLSLog(data,model_h))/summary(model_h)$sigma,plot.it=FALSE)$y,color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f2f <- f2f + scale_colour_manual(name="study",values=colours)
	f2f <- f2f + geom_abline(linetype="dashed",colour="black",size=0.25,alpha=1)
	f2f <- f2f + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f2f.pdf",plot=f2f)
}

plotFig3 <- function(data,alpha,runs,ncpus)
{
	#
	func_drho <- log(agb) ~ log(d) + log(rho)
	model_drho <- fitOLS(data,func_drho)
	drho_bresults <- bootOLS(data,func_drho,runs,ncpus)
	drho_b0_cis <- boot.ci(drho_bresults,index=1,conf=0.95,type="bca")
	drho_b1_cis <- boot.ci(drho_bresults,index=2,conf=0.95,type="bca")
	drho_b2_cis <- boot.ci(drho_bresults,index=3,conf=0.95,type="bca")
	drho_b0_lower <- drho_b0_cis$bca[4]
	drho_b0_upper <- drho_b0_cis$bca[5]
	drho_b1_lower <- drho_b1_cis$bca[4]
	drho_b1_upper <- drho_b1_cis$bca[5]
	drho_b2_lower <- drho_b2_cis$bca[4]
	drho_b2_upper <- drho_b2_cis$bca[5]
	#
	func_dh <- log(agb) ~ log(d) + log(h)
	model_dh <- fitOLS(data,func_dh)
	dh_bresults <- bootOLS(data,func_dh,runs,ncpus)
	dh_b0_cis <- boot.ci(dh_bresults,index=1,conf=0.95,type="bca")
	dh_b1_cis <- boot.ci(dh_bresults,index=2,conf=0.95,type="bca")
	dh_b2_cis <- boot.ci(dh_bresults,index=3,conf=0.95,type="bca")
	dh_b0_lower <- dh_b0_cis$bca[4]
	dh_b0_upper <- dh_b0_cis$bca[5]
	dh_b1_lower <- dh_b1_cis$bca[4]
	dh_b1_upper <- dh_b1_cis$bca[5]
	dh_b2_lower <- dh_b2_cis$bca[4]
	dh_b2_upper <- dh_b2_cis$bca[5]
	#
	func_dhrho <- log(agb) ~ log(d) + log(h) + log(rho)
	model_dhrho <- fitOLS(data,func_dhrho)
	dhrho_bresults <- bootOLS(data,func_dhrho,runs,ncpus)
	dhrho_b0_cis <- boot.ci(dhrho_bresults,index=1,conf=0.95,type="bca")
	dhrho_b1_cis <- boot.ci(dhrho_bresults,index=2,conf=0.95,type="bca")
	dhrho_b2_cis <- boot.ci(dhrho_bresults,index=3,conf=0.95,type="bca")
	dhrho_b3_cis <- boot.ci(dhrho_bresults,index=4,conf=0.95,type="bca")
	dhrho_b0_lower <- dhrho_b0_cis$bca[4]
	dhrho_b0_upper <- dhrho_b0_cis$bca[5]
	dhrho_b1_lower <- dhrho_b1_cis$bca[4]
	dhrho_b1_upper <- dhrho_b1_cis$bca[5]
	dhrho_b2_lower <- dhrho_b2_cis$bca[4]
	dhrho_b2_upper <- dhrho_b2_cis$bca[5]
	dhrho_b3_lower <- dhrho_b3_cis$bca[4]
	dhrho_b3_upper <- dhrho_b3_cis$bca[5]
	#
	colours <- colorRampPalette(brewer.pal(8,"Dark2"))(length(levels(data$study)))
	colScale <- scale_colour_manual(name="study",values="colours")
	#############
	##B ~ D * rho
	#############
	f3a <- ggplot()
	f3a <- f3a + geom_point(aes(x=model_drho$fitted.values,y=log(data$agb),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3a <- f3a + geom_abline(aes(slope=1,intercept=0),colour="black",linetype="dashed",alpha=1)
	f3a <- f3a + geom_segment(aes(x=1.345,xend=1.745,y=10.7,yend=10.7),colour="black",linetype="dashed",alpha=1)
	f3a <- f3a + scale_colour_manual(name="study",values=colours)
	f3a <- f3a + annotate("text", x=2.245,y= 10.75, label="1:1",size=6)
	f3a <- f3a + annotate("text", x=4.345, y=10.12, label = deparse(bquote(hat(beta[0]) ==~ .(format(round(coefficients(model_drho)[[1]],3),nsmall=3)) ~ "(" * .(format(round(drho_b0_lower,3),nsmall=3)) * "," ~ .(format(round(drho_b0_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3a <- f3a + annotate("text", x=4.345, y=9.40, label = deparse(bquote(hat(beta[1]) ==~ .(format(round(coefficients(model_drho)[[2]],3),nsmall=3)) ~ "(" * .(format(round(drho_b1_lower,3),nsmall=3)) * "," ~ .(format(round(drho_b1_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3a <- f3a + annotate("text", x=4.345, y=8.68, label = deparse(bquote(hat(beta[2]) ==~ .(format(round(coefficients(model_drho)[[3]],3),nsmall=3)) ~ "(" * .(format(round(drho_b2_lower,3),nsmall=3)) * "," ~ .(format(round(drho_b2_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3a <- f3a + annotate("text", x=3.14, y=7.96, label = deparse(bquote(s ==~ .(format(round(summary(model_drho)$sigma,3),nsmall=3)))),parse=TRUE,size=6)
	f3a <- f3a + theme(legend.position=c(0.012,0.92),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1,text = element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3a.pdf",plot=f3a,scale=1)
	#	
	f3b <- ggplot()
	f3b <- f3b + geom_point(aes(model_drho$fitted.values,resid(model_drho),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3b <- f3b + geom_hline(yintercept=0,linetype="dashed",color="black",size=0.25,alpha=1)
	f3b <- f3b + scale_colour_manual(name="study",values=colours)
	f3b <- f3b + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3b.pdf",plot=f3b,scale=1)
	#
	f3c <- ggplot()
	f3c <- f3c + geom_point(aes(x=qqnorm((log(data$agb)-yhatOLSLog(data,model_drho))/summary(model_drho)$sigma,plot.it=FALSE)$x,y=qqnorm((log(data$agb)-yhatOLSLog(data,model_drho))/summary(model_drho)$sigma,plot.it=FALSE)$y,color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3c <- f3c + scale_colour_manual(name="study",values=colours)
	f3c <- f3c + geom_abline(linetype="dashed",colour="black",size=0.25,alpha=1)
	f3c <- f3c + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3c.pdf",plot=f3c)
	###########
	##B ~ D * H
	###########
	f3d <- ggplot()
	f3d <- f3d + geom_point(aes(x=model_dh$fitted.values,y=log(data$agb),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3d <- f3d + geom_abline(aes(slope=1,intercept=0),colour="black",linetype="dashed",alpha=1)
	f3d <- f3d + geom_segment(aes(x=1.345,xend=1.745,y=10.7,yend=10.7),colour="black",linetype="dashed",alpha=1)
	f3d <- f3d + scale_colour_manual(name="study",values=colours)
	f3d <- f3d + annotate("text", x=2.245,y= 10.75, label="1:1",size=6)
	f3d <- f3d + annotate("text", x=4.345, y=10.12, label = deparse(bquote(hat(beta[0]) ==~ .(format(round(coefficients(model_dh)[[1]],3),nsmall=3)) ~ "(" * .(format(round(dh_b0_lower,3),nsmall=3)) * "," ~ .(format(round(dh_b0_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3d <- f3d + annotate("text", x=4.345, y=9.40, label = deparse(bquote(hat(beta[1]) ==~ .(format(round(coefficients(model_dh)[[2]],3),nsmall=3)) ~ "(" * .(format(round(dh_b1_lower,3),nsmall=3)) * "," ~ .(format(round(dh_b1_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3d <- f3d + annotate("text", x=4.345, y=8.68, label = deparse(bquote(hat(beta[2]) ==~ .(format(round(coefficients(model_dh)[[3]],3),nsmall=3)) ~ "(" * .(format(round(dh_b2_lower,3),nsmall=3)) * "," ~ .(format(round(dh_b2_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3d <- f3d + annotate("text", x=3.14, y=7.96, label = deparse(bquote(s ==~ .(format(round(summary(model_dh)$sigma,3),nsmall=3)))),parse=TRUE,size=6)
	f3d <- f3d + theme(legend.position=c(0.012,0.92),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1,text = element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3d.pdf",plot=f3d,scale=1)
	#	
	f3e <- ggplot()
	f3e <- f3e + geom_point(aes(model_dh$fitted.values,resid(model_dh),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3e <- f3e + geom_hline(yintercept=0,linetype="dashed",color="black",size=0.25,alpha=1)
	f3e <- f3e + scale_colour_manual(name="study",values=colours)
	f3e <- f3e + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3e.pdf",plot=f3e,scale=1)
	#
	f3f <- ggplot()
	f3f <- f3f + geom_point(aes(x=qqnorm((log(data$agb)-yhatOLSLog(data,model_dh))/summary(model_dh)$sigma,plot.it=FALSE)$x,y=qqnorm((log(data$agb)-yhatOLSLog(data,model_dh))/summary(model_dh)$sigma,plot.it=FALSE)$y,color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3f <- f3f + scale_colour_manual(name="study",values=colours)
	f3f <- f3f + geom_abline(linetype="dashed",colour="black",size=0.25,alpha=1)
	f3f <- f3f + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3f.pdf",plot=f3f)
	#################
	##B ~ D * H * rho
	#################
	f3g <- ggplot()
	f3g <- f3g + geom_point(aes(x=model_dhrho$fitted.values,y=log(data$agb),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3g <- f3g + geom_abline(aes(slope=1,intercept=0),colour="black",linetype="dashed",alpha=1)
	f3g <- f3g + geom_segment(aes(x=1.345,xend=1.745,y=10.7,yend=10.7),colour="black",linetype="dashed",alpha=1)
	f3g <- f3g + scale_colour_manual(name="study",values=colours)
	f3g <- f3g + annotate("text", x=2.245,y= 10.75, label="1:1",size=6)
	f3g <- f3g + annotate("text", x=4.41, y=10.12, label = deparse(bquote(hat(beta[0]) ==~ .(format(round(coefficients(model_dhrho)[[1]],3),nsmall=3)) ~ "(" * .(format(round(dhrho_b0_lower,3),nsmall=3)) * "," ~ .(format(round(dhrho_b0_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3g <- f3g + annotate("text", x=4.41, y=9.40, label = deparse(bquote(hat(beta[1]) ==~ .(format(round(coefficients(model_dhrho)[[2]],3),nsmall=3)) ~ "(" * .(format(round(dhrho_b1_lower,3),nsmall=3)) * "," ~ .(format(round(dhrho_b1_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3g <- f3g + annotate("text", x=4.41, y=8.68, label = deparse(bquote(hat(beta[2]) ==~ .(format(round(coefficients(model_dhrho)[[3]],3),nsmall=3)) ~ "(" * .(format(round(dhrho_b2_lower,3),nsmall=3)) * "," ~ .(format(round(dhrho_b2_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3g <- f3g + annotate("text", x=4.41, y=7.96, label = deparse(bquote(hat(beta[3]) ==~ .(format(round(coefficients(model_dhrho)[[4]],3),nsmall=3)) ~ "(" * .(format(round(dhrho_b3_lower,3),nsmall=3)) * "," ~ .(format(round(dhrho_b3_upper,3),nsmall=3)) * ")" )),parse=TRUE,size=6)
	f3g <- f3g + annotate("text", x=3.15, y=7.24, label = deparse(bquote(s ==~ .(format(round(summary(model_dhrho)$sigma,3),nsmall=3)))),parse=TRUE,size=6)
	f3g <- f3g + theme(legend.position=c(0.012,0.92),legend.justification=c(0,1),legend.background = element_rect(fill=alpha('grey', 0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),plot.title=element_text(hjust=0.5),aspect.ratio=1,text = element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3g.pdf",plot=f3g,scale=1)
	#
	f3h <- ggplot()
	f3h <- f3h + geom_point(aes(model_dhrho$fitted.values,resid(model_dhrho),color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3h <- f3h + geom_hline(yintercept=0,linetype="dashed",color="black",size=0.25,alpha=1)
	f3h <- f3h + scale_colour_manual(name="study",values=colours)
	f3h <- f3h + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3h.pdf",plot=f3h,scale=1)
	#
	f3i <- ggplot()
	f3i <- f3i + geom_point(aes(x=qqnorm((log(data$agb)-yhatOLSLog(data,model_dhrho))/summary(model_dhrho)$sigma,plot.it=FALSE)$x,y=qqnorm((log(data$agb)-yhatOLSLog(data,model_dhrho))/summary(model_dhrho)$sigma,plot.it=FALSE)$y,color=data$study),size=1.2,alpha=0.8,stroke=0,show.legend=FALSE)
	f3i <- f3i + scale_colour_manual(name="study",values=colours)
	f3i <- f3i + geom_abline(linetype="dashed",colour="black",size=0.25,alpha=1)
	f3i <- f3i + theme(plot.title=element_text(hjust=0.5),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f3i.pdf",plot=f3i)
}

plotFig4 <- function(data)
{
	func_d <- log(agb) ~ log(d)
	func_dh <- log(agb) ~ log(d) + log(h)
	func_dhrho <- log(agb) ~ log(d) + log(h) + log(rho)
	#
	results_d <- crossValidateOLS(data,func_d)
	results_dh <- crossValidateOLS(data,func_dh)
	results_dhrho <- crossValidateOLS(data,func_dhrho)
	#
	f4 <- ggplot()
	f4 <- f4 + geom_hline(yintercept=0,linetype="solid",color="black",size=0.5,alpha=1)
	#
	f4 <- f4 + geom_boxplot(aes(rep(0.5+(2/8)),results_d[[1]][[1]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(0.5+(4/8)),results_dh[[1]][[1]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(0.5+(6/8)),results_dhrho[[1]][[1]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(1.5+(2/8)),results_d[[1]][[2]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(1.5+(4/8)),results_dh[[1]][[2]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(1.5+(6/8)),results_dhrho[[1]][[2]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(2.5+(2/8)),results_d[[1]][[3]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(2.5+(4/8)),results_dh[[1]][[3]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(2.5+(6/8)),results_dhrho[[1]][[3]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(3.5+(2/8)),results_d[[1]][[4]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(3.5+(4/8)),results_dh[[1]][[4]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(3.5+(6/8)),results_dhrho[[1]][[4]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(4.5+(2/8)),results_d[[1]][[5]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(4.5+(4/8)),results_dh[[1]][[5]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(4.5+(6/8)),results_dhrho[[1]][[5]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(5.5+(2/8)),results_d[[1]][[6]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(5.5+(4/8)),results_dh[[1]][[6]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(5.5+(6/8)),results_dhrho[[1]][[6]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(6.5+(2/8)),results_d[[1]][[7]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(6.5+(4/8)),results_dh[[1]][[7]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(6.5+(6/8)),results_dhrho[[1]][[7]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(7.5+(2/8)),results_d[[1]][[8]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(7.5+(4/8)),results_dh[[1]][[8]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(7.5+(6/8)),results_dhrho[[1]][[8]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(8.5+(2/8)),results_d[[1]][[9]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(8.5+(4/8)),results_dh[[1]][[9]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(8.5+(6/8)),results_dhrho[[1]][[9]],color="dhrho"),width=1/8)
	#
	f4 <- f4 + geom_boxplot(aes(rep(9.5+(2/8)),results_d[[1]][[10]],color="d"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(9.5+(4/8)),results_dh[[1]][[10]],color="dh"),width=1/8)
	f4 <- f4 + geom_boxplot(aes(rep(9.5+(6/8)),results_dhrho[[1]][[10]],color="dhrho"),width=1/8)
	#
	name1 <- as.expression(bquote(" B"=="f(D)  "))
	name2 <- as.expression(bquote(" B"=="f(D,H)  "))
	name3 <- as.expression(bquote(" B"=="f(D,H,"*rho["b"]*")  "))
	f4 <- f4 + scale_colour_manual(name="",values=c("d"="black","dh"="red","dhrho"="blue"),labels=c("d"=name1,"dh"=name2,"dhrho"=name3))
	f4 <- f4 + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))
	f4 <- f4 + theme(legend.position="top",legend.background = element_rect(fill=alpha("white",0)),legend.key=element_blank(),legend.text.align=0,legend.title=element_blank(),text=element_text(size = 17.5),legend.text=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_blank(),aspect.ratio=1/1.61803398875)
	ggsave("f4.pdf",plot=f4,scale=1)
}

plotFig5 <- function(data,alpha,runs,ncpus)
{
	func <- log(agb) ~ log(d) + log(h) + log(rho)
	#0
	d <- data[data$d >= 0,]
	m0 <- fitOLS(d,func)
	b0 <- bootOLS(d,func,runs,ncpus)
	b0_0_cis <- boot.ci(b0,index=1,conf=0.95,type='bca')
	b0_1_cis <- boot.ci(b0,index=2,conf=0.95,type='bca')
	b0_2_cis <- boot.ci(b0,index=3,conf=0.95,type='bca')
	b0_3_cis <- boot.ci(b0,index=4,conf=0.95,type='bca')
	#0.1
	d <- data[data$d >= 0.1,]
	m1 <- fitOLS(d,func)
	b1 <- bootOLS(d,func,runs,ncpus)
	b1_0_cis <- boot.ci(b1,index=1,conf=0.95,type='bca')
	b1_1_cis <- boot.ci(b1,index=2,conf=0.95,type='bca')
	b1_2_cis <- boot.ci(b1,index=3,conf=0.95,type='bca')
	b1_3_cis <- boot.ci(b1,index=4,conf=0.95,type='bca')
	#0.25
	d <- data[data$d >= 0.25,]
	m2 <- fitOLS(d,func)
	b2 <- bootOLS(d,func,runs,ncpus)
	b2_0_cis <- boot.ci(b2,index=1,conf=0.95,type='bca')
	b2_1_cis <- boot.ci(b2,index=2,conf=0.95,type='bca')
	b2_2_cis <- boot.ci(b2,index=3,conf=0.95,type='bca')
	b2_3_cis <- boot.ci(b2,index=4,conf=0.95,type='bca')
	#0.5
	d <- data[data$d >= 0.5,]
	m3 <- fitOLS(d,func)
	b3 <- bootOLS(d,func,runs,ncpus)
	b3_0_cis <- boot.ci(b3,index=1,conf=0.95,type='bca')
	b3_1_cis <- boot.ci(b3,index=2,conf=0.95,type='bca')
	b3_2_cis <- boot.ci(b3,index=3,conf=0.95,type='bca')
	b3_3_cis <- boot.ci(b3,index=4,conf=0.95,type='bca')
	#0.75
	d <- data[data$d >= 0.75,]
	m4 <- fitOLS(d,func)
	b4 <- bootOLS(d,func,runs,ncpus)
	b4_0_cis <- boot.ci(b4,index=1,conf=0.95,type='bca')
	b4_1_cis <- boot.ci(b4,index=2,conf=0.95,type='bca')
	b4_2_cis <- boot.ci(b4,index=3,conf=0.95,type='bca')
	b4_3_cis <- boot.ci(b4,index=4,conf=0.95,type='bca')
	#1
	d <- data[data$d >= 1,]
	m5 <- fitOLS(d,func)
	b5 <- bootOLS(d,func,runs,ncpus)
	b5_0_cis <- boot.ci(b5,index=1,conf=0.95,type='bca')
	b5_1_cis <- boot.ci(b5,index=2,conf=0.95,type='bca')
	b5_2_cis <- boot.ci(b5,index=3,conf=0.95,type='bca')
	b5_3_cis <- boot.ci(b5,index=4,conf=0.95,type='bca')
	###
	f5a <- ggplot()
	f5a <- f5a + geom_point(aes(x=0,y=coefficients(m0)[1]))
	f5a <- f5a + geom_point(aes(x=0.1,y=coefficients(m1)[1]))
	f5a <- f5a + geom_point(aes(x=0.25,y=coefficients(m2)[1]))
	f5a <- f5a + geom_point(aes(x=0.5,y=coefficients(m3)[1]))
	f5a <- f5a + geom_point(aes(x=0.75,y=coefficients(m4)[1]))
	f5a <- f5a + geom_point(aes(x=1,y=coefficients(m5)[1]))
	f5a <- f5a + geom_errorbar(aes(x=0,ymin=b0_0_cis$bca[4],ymax=b0_0_cis$bca[5]),width=0.09)
	f5a <- f5a + geom_errorbar(aes(x=0.1,ymin=b1_0_cis$bca[4],ymax=b1_0_cis$bca[5]),width=0.09)
	f5a <- f5a + geom_errorbar(aes(x=0.25,ymin=b2_0_cis$bca[4],ymax=b2_0_cis$bca[5]),width=0.09)
	f5a <- f5a + geom_errorbar(aes(x=0.5,ymin=b3_0_cis$bca[4],ymax=b3_0_cis$bca[5]),width=0.09)
	f5a <- f5a + geom_errorbar(aes(x=0.75,ymin=b4_0_cis$bca[4],ymax=b4_0_cis$bca[5]),width=0.09)
	f5a <- f5a + geom_errorbar(aes(x=1,ymin=b5_0_cis$bca[4],ymax=b5_0_cis$bca[5]),width=0.09)	
	f5a <- f5a + scale_x_continuous(breaks=c(0,0.1,0.25,0.5,0.75,1))
	f5a <- f5a + theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5),panel.grid.minor.x = element_blank(),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f5a.pdf",plot=f5a)
	###
	f5b <- ggplot()
	f5b <- f5b + geom_point(aes(x=0,y=coefficients(m0)[2]))
	f5b <- f5b + geom_point(aes(x=0.1,y=coefficients(m1)[2]))
	f5b <- f5b + geom_point(aes(x=0.25,y=coefficients(m2)[2]))
	f5b <- f5b + geom_point(aes(x=0.5,y=coefficients(m3)[2]))
	f5b <- f5b + geom_point(aes(x=0.75,y=coefficients(m4)[2]))
	f5b <- f5b + geom_point(aes(x=1,y=coefficients(m5)[2]))
	f5b <- f5b + geom_errorbar(aes(x=0,ymin=b0_1_cis$bca[4],ymax=b0_1_cis$bca[5]),width=0.09)
	f5b <- f5b + geom_errorbar(aes(x=0.1,ymin=b1_1_cis$bca[4],ymax=b1_1_cis$bca[5]),width=0.09)
	f5b <- f5b + geom_errorbar(aes(x=0.25,ymin=b2_1_cis$bca[4],ymax=b2_1_cis$bca[5]),width=0.09)
	f5b <- f5b + geom_errorbar(aes(x=0.5,ymin=b3_1_cis$bca[4],ymax=b3_1_cis$bca[5]),width=0.09)
	f5b <- f5b + geom_errorbar(aes(x=0.75,ymin=b4_1_cis$bca[4],ymax=b4_1_cis$bca[5]),width=0.09)
	f5b <- f5b + geom_errorbar(aes(x=1,ymin=b5_1_cis$bca[4],ymax=b5_1_cis$bca[5]),width=0.09)
	f5b <- f5b + scale_x_continuous(breaks=c(0,0.1,0.25,0.5,0.75,1))
	f5b <- f5b + theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5),panel.grid.minor.x = element_blank(),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f5b.pdf",plot=f5b)
	###
	f5c <- ggplot()
	f5c <- f5c + geom_point(aes(x=0,y=coefficients(m0)[3]))
	f5c <- f5c + geom_point(aes(x=0.1,y=coefficients(m1)[3]))
	f5c <- f5c + geom_point(aes(x=0.25,y=coefficients(m2)[3]))
	f5c <- f5c + geom_point(aes(x=0.5,y=coefficients(m3)[3]))
	f5c <- f5c + geom_point(aes(x=0.75,y=coefficients(m4)[3]))
	f5c <- f5c + geom_point(aes(x=1,y=coefficients(m5)[3]))
	f5c <- f5c + geom_errorbar(aes(x=0,ymin=b0_2_cis$bca[4],ymax=b0_2_cis$bca[5]),width=0.09)
	f5c <- f5c + geom_errorbar(aes(x=0.1,ymin=b1_2_cis$bca[4],ymax=b1_2_cis$bca[5]),width=0.09)
	f5c <- f5c + geom_errorbar(aes(x=0.25,ymin=b2_2_cis$bca[4],ymax=b2_2_cis$bca[5]),width=0.09)
	f5c <- f5c + geom_errorbar(aes(x=0.5,ymin=b3_2_cis$bca[4],ymax=b3_2_cis$bca[5]),width=0.09)
	f5c <- f5c + geom_errorbar(aes(x=0.75,ymin=b4_2_cis$bca[4],ymax=b4_2_cis$bca[5]),width=0.09)
	f5c <- f5c + geom_errorbar(aes(x=1,ymin=b5_2_cis$bca[4],ymax=b5_2_cis$bca[5]),width=0.09)
	f5c <- f5c + labs(x="D-threshold (m)",y=expression(paste(beta[2])))#,title=expression(paste("LLT-OLS: ln(AGB) = ",beta[0]," + ",beta[1]," ln(D)"," + ",beta[2]," ln(H)"," + ",beta[3]," ln(",rho["b"],") + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,")]")))
	f5c <- f5c + scale_x_continuous(breaks=c(0,0.1,0.25,0.5,0.75,1))
	f5c <- f5c + theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5),panel.grid.minor.x = element_blank(),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f5c.pdf",plot=f5c)
	###
	f5d <- ggplot()
	f5d <- f5d + geom_point(aes(x=0,y=coefficients(m0)[4]))
	f5d <- f5d + geom_point(aes(x=0.1,y=coefficients(m1)[4]))
	f5d <- f5d + geom_point(aes(x=0.25,y=coefficients(m2)[4]))
	f5d <- f5d + geom_point(aes(x=0.5,y=coefficients(m3)[4]))
	f5d <- f5d + geom_point(aes(x=0.75,y=coefficients(m4)[4]))
	f5d <- f5d + geom_point(aes(x=1,y=coefficients(m5)[4]))
	f5d <- f5d + geom_errorbar(aes(x=0,ymin=b0_3_cis$bca[4],ymax=b0_3_cis$bca[5]),width=0.09)
	f5d <- f5d + geom_errorbar(aes(x=0.1,ymin=b1_3_cis$bca[4],ymax=b1_3_cis$bca[5]),width=0.09)
	f5d <- f5d + geom_errorbar(aes(x=0.25,ymin=b2_3_cis$bca[4],ymax=b2_3_cis$bca[5]),width=0.09)
	f5d <- f5d + geom_errorbar(aes(x=0.5,ymin=b3_3_cis$bca[4],ymax=b3_3_cis$bca[5]),width=0.09)
	f5d <- f5d + geom_errorbar(aes(x=0.75,ymin=b4_3_cis$bca[4],ymax=b4_3_cis$bca[5]),width=0.09)
	f5d <- f5d + geom_errorbar(aes(x=1,ymin=b5_3_cis$bca[4],ymax=b5_3_cis$bca[5]),width=0.09)
	f5d <- f5d + labs(x="D-threshold (m)",y=expression(paste(beta[3])))#,title=expression(paste("LLT-OLS: ln(AGB) = ",beta[0]," + ",beta[1]," ln(D)"," + ",beta[2]," ln(H)"," + ",beta[3]," ln(",rho["b"],") + ",epsilon,"  [",epsilon," ~ N(0,",sigma^2,")]")))
	f5d <- f5d + scale_x_continuous(breaks=c(0,0.1,0.25,0.5,0.75,1))
	f5d <- f5d + theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5),panel.grid.minor.x=element_blank(),aspect.ratio=1,text=element_text(size = 17.5),axis.title.x=element_blank(),axis.title.y=element_blank())
	ggsave("f5d.pdf",plot=f5d)
}
