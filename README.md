# treeallom

Construct linear and non-linear allometric models

## Overview

treeallom has been developed to construct allometric models of linear form, ![equation](http://latex.codecogs.com/gif.latex?y&space;=&space;\beta_0+\beta_1X+\varepsilon), with additive error, ![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2)).

Confidence intervals about each model parameter are generated from a bootstrap.

The accuracy of predictions from these models are assessed using stratified k-fold cross-validation.
Derivatives of the log of the accuracy ratio are used to estimate uncertainty (median symmetric accuracy, MSA) and bias (signed symmetric percent bias, SSPB). 

The scripts presented here were developed for constructing allometric models predicting tree- and plot-scale above-ground biomass, but should be readily modifiable to other applications and/or model forms. 

nlme.r is an attempt at constructing a non-linear model (maximum likelihood estimation) with multiplicative error: ![equation](http://latex.codecogs.com/gif.latex?y&space;=&space;\beta_{0}X^{\beta_1}+\varepsilon) (![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2&space;X^k)))

## Dependencies

R (v3.6.2 or later)

R packages:
* boot (v1.3.24)
* caret (v6.0.84)
* nlreg (v1.2.2.2)

On Ubuntu 19.04 these dependencies can be installed via:

```
apt install r-base
sudo R;
>install.packages(c("boot","caret","nlreg"))
```

On macOS 10.14, they can be installed using Homebrew (https://brew.sh), as:

```
brew install R;
R;
>install.packages(c("boot","caret","nlreg"))
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
Rscript [INSTALLATION_DIR]/treeallom/src/treeallom.r [INSTALLATION_DIR]/treeallom/src/ [CAL_DATA_DIR]/caldata.txt [RUNS] [NCPU] [ALPHA]
```

Where the current expectation of the calibration data is a headerless ASCII text file of the form: 4 col x n-stems (tree-id, diameter-at-breast height, tree height, observed AGB, wood density).
[RUNS] and [NCPU] are integers specifying the number of bootstrap replicates, and the number of threads to use for this computation (e.g., 10000 and 2).
[ALPHA] specifies the level at which to generate the subsequent confidence intervals (e.g., 0.95)

One run, this script will print the model summary and the bootstrapped confidence intervals for each model parameter. 
Also printed are the MSA and SSPB results per cross-validation fold.

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

treeallom uses the following R packages: boot, caret and nlreg
