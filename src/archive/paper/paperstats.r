#!/usr/bin/env Rscript

#Andrew Burt - a.burt@ucl.ac.uk

bivariateStats <- function(data)
{
	func_d <- log(agb) ~ log(d)
	model_d <- fitOLS(data,func_d)
	print(summary(model_d))
	rainbow_test(model_d)
	studentised_breusch_pagan_test(data,func_d,model_d)
	studentised_white_test(data,func_d,model_d)
	shapiro_wilk_test(model_d)
	anderson_darling_test(model_d)
	#
	func_h <- log(agb) ~ log(h)
	model_h <- fitOLS(data,func_h)
	print(summary(model_h))
	rainbow_test(model_h)
	studentised_breusch_pagan_test(data,func_h,model_h)
	studentised_white_test(data,func_h,model_h)
	shapiro_wilk_test(model_h)
	anderson_darling_test(model_h)
}

covariateStats <- function(data)
{
	#
	func_drho <- log(agb) ~ log(d) + log(rho)
	model_drho <- fitOLS(data,func_drho)
	print(summary(model_drho))
	rainbow_test(model_drho)
	studentised_breusch_pagan_test(data,func_drho,model_drho)
	studentised_white_test(data,func_drho,model_drho)
	shapiro_wilk_test(model_drho)
	anderson_darling_test(model_drho)
	#
	func_dh <- log(agb) ~ log(d) + log(h)
	model_dh <- fitOLS(data,func_dh)
	print(summary(model_dh))
	rainbow_test(model_dh)
	studentised_breusch_pagan_test(data,func_dh,model_dh)
	studentised_white_test(data,func_dh,model_dh)
	shapiro_wilk_test(model_dh)
	anderson_darling_test(model_dh)
	#
	func_dhrho <- log(agb) ~ log(d) + log(h) + log(rho)
	model_dhrho <- fitOLS(data,func_dhrho)
	print(summary(model_dhrho))
	rainbow_test(model_dhrho)
	studentised_breusch_pagan_test(data,func_dhrho,model_dhrho)
	studentised_white_test(data,func_dhrho,model_dhrho)
	shapiro_wilk_test(model_dhrho)
	anderson_darling_test(model_dhrho)
}

crossvalidateStats <- function(data)
{
	####
	func_d <- log(agb) ~ log(d)
	results_d <- crossValidateOLS(data,func_d)
	print("b~fn(d)")
	#
	print("Median symmetric accuracy, mean, min, max")
	print(mean(results_d[[2]][,1]))
	print(min(results_d[[2]][,1]))
	print(max(results_d[[2]][,1]))
	#
	print("Symmetric signed percentage bias, mean, min, max")
	print(mean(results_d[[2]][,2]))
	print(min(results_d[[2]][,2]))
	print(max(results_d[[2]][,2]))
	####
	func_h <- log(agb) ~ log(h)
	results_h <- crossValidateOLS(data,func_h)
	print("b~fn(h)")
	#
	print("Median symmetric accuracy, mean, min, max")
	print(mean(results_h[[2]][,1]))
	print(min(results_h[[2]][,1]))
	print(max(results_h[[2]][,1]))
	#
	print("Symmetric signed percentage bias, mean, min, max")
	print(mean(results_h[[2]][,2]))
	print(min(results_h[[2]][,2]))
	print(max(results_h[[2]][,2]))
	####
	func_dh <- log(agb) ~ log(d) + log(h)
	results_dh <- crossValidateOLS(data,func_dh)
	print("b~fn(d,h)")
	#
	print("Median symmetric accuracy, mean, min, max")
	print(mean(results_dh[[2]][,1]))
	print(min(results_dh[[2]][,1]))
	print(max(results_dh[[2]][,1]))
	#
	print("Symmetric signed percentage bias, mean, min, max")
	print(mean(results_dh[[2]][,2]))
	print(min(results_dh[[2]][,2]))
	print(max(results_dh[[2]][,2]))
	####
	func_drho <- log(agb) ~ log(d) + log(rho)
	results_drho <- crossValidateOLS(data,func_drho)
	print("b~fn(d,rho)")
	#
	print("Median symmetric accuracy, mean, min, max")
	print(mean(results_drho[[2]][,1]))
	print(min(results_drho[[2]][,1]))
	print(max(results_drho[[2]][,1]))
	#
	print("Symmetric signed percentage bias, mean, min, max")
	print(mean(results_drho[[2]][,2]))
	print(min(results_drho[[2]][,2]))
	print(max(results_drho[[2]][,2]))
	####
	func_dhrho <- log(agb) ~ log(d) + log(h) + log(rho)
	results_dhrho <- crossValidateOLS(data,func_dhrho)
	print("b~fn(d,h,rho)")
	#
	print("Median symmetric accuracy, mean, min, max")
	print(mean(results_dhrho[[2]][,1]))
	print(min(results_dhrho[[2]][,1]))
	print(max(results_dhrho[[2]][,1]))
	#
	print("Symmetric signed percentage bias, mean, min, max")
	print(mean(results_dhrho[[2]][,2]))
	print(min(results_dhrho[[2]][,2]))
	print(max(results_dhrho[[2]][,2]))
}
