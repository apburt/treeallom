# treeallom

Construct linear and non-linear allometric models.

## Overview

treeallom has been developed to construct allometric models of either linear form (log-transformed ordinary least squares, ![equation](http://latex.codecogs.com/gif.latex?\text{ln}(y)&space;=&space;\beta_0+\beta_1\text{ln}(X)+\varepsilon)), with additive error ![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2)), or non-linear power law form (maximum likelihood estimation, ![equation](http://latex.codecogs.com/gif.latex?y&space;=&space;\beta_{0}X^{\beta_1}+\varepsilon)), with multiplicative error (![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2&space;X^k))).

The accuracy (both trueness and precision characteristics) of predictions from these models are assessed using stratified k-fold cross-validation: bias, variance and uncertainty metrics are derived from the log of the accuracy ratio.

Confidence intervals about each model parameter, and the mean response, are generated from a BCa bootstrap. 

Several diagnostics and statistical tests are performed on model residuals to assess goodness of fit (incl. tests of homoscedasticity and normality).

The scripts presented here were developed for constructing allometric models predicting tree- and plot-scale above-ground biomass, but should be readily modifiable to other applications and/or model forms. 

## Above-ground biomass

In the current form, an overview of treeallom and the various methods used is provided by the treeallom.r script.

This script generates a linear or non-linear model from some input calibration data, and outputs the model alongside the various fit diagnostics/tests, cross-validation and bootstrapping results.  

The installation and usage of these two scripts is described below.

## Dependencies

R (v3.2.1 or later)

R packages:
* stats (3.5.2)
* nlreg (1.2.2.1)
* boot (1.3.20)
* caret (6.0.84)
* lmtest (0.9.37)
* ggplot2 (v3.1.0)

On Ubuntu 19.04 these dependencies can be installed via:

```
apt install r-base
sudo R;
>install.packages(c("nlreg","boot","caret","lmtest","ggplot2"))
```

On macOS 10.14, they can be installed using Homebrew (https://brew.sh), as:

```
brew install R;
R;
>install.packages(c("nlreg","boot","caret","lmtest","ggplot2"))
```

## Installation

With the dependencies installed, treeallom can be installed:

```
cd [INSTALLATION_DIR];
git clone https://github.com/apburt/treeallom.git;
```

## Usage

treeallom.r is called as:

```
Rscript [INSTALLATION_DIR]/treeallom/src/treeallom.r [INSTALLATION_DIR]/treeallom/src/ [CAL_DATA_DIR]/caldata.txt [FORM_BOOL] [ALPHA] [RUNS]
```

Where the current expectation of the calibration data is a headerless ASCII text file of the form: 4 col x n-stems (tree-id, diameter-at-breast height, tree height, observed AGB, wood density).
[FORM_BOOL] defines a linear (TRUE) or non-linear (FALSE) model form.
[ALPHA] and [RUNS] denote the confidence interval level and the number of bootstrap iterations respectively (e.g., 0.05 and 10000).
Once run, this script will produce "treeallom_model.pdf" in the working directory, visualising and quantifying the model parameters and associated confidence intervals.
Also generated is "treeallom_crossvalidate.pdf" and "treeallom_diagnostics.pdf", presenting the results from the stratified k-fold cross-validation and model fit diagnostics/tests respectively. 

If [RUNS] is large (approx. >10000), various stack overflow errors may appear, possible solutions include:

```
Rscript --max-ppsize=500000
ulimit -s 65000
```

## Authors

* **Andrew Burt**

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

## Acknowledgements

treeallom uses the following R packages: stats, nlreg, boot, caret, lmtest and ggplot2
