# nlallom

Construct non-linear allometric models with uncertainties quantified via non-parametric methods

## Overview

nlallom has been developed to construct non-linear power law allometric models (of the form ![equation](http://latex.codecogs.com/gif.latex?Y&space;=&space;aM^b) or ![equation](http://latex.codecogs.com/gif.latex?Y&space;=&space;aM^b&plus;c)), with the error term treated as additive (![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2))), or multiplicative (![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2&space;M^k))).

Uncertainties in these non-linear models are quantified using non-parametric methods: prediction and confidence intervals are generated via non-linear quantile regression[1] and the wild bootstrap[2] respectively. 

The framework presented here is focused towards allometric prediction of pan-tropical above-ground biomass (AGB), but should be readily modifiable to other applications of allometry and/or model forms.

In its current form, nlallom can be used in two ways:

1) nlallom.r - construct allometric models of the above forms using some allometric dataset 
2) runallom.r - estimate tree- and plot-scale AGB and uncertainty using an allometric dataset and field data

The usage of these two scripts is described below, and a more full description of the methods themselves can be found at: http://discovery.ucl.ac.uk/1575534/ (pp. 14-33)

[1] Koenker, R. and Park, B. J. (1996). "An interior point algorithm for nonlinear quantile regression". In: Journal of Econometrics. DOI: 10.1016/0304-4076(96)84507-6.

[2] Wu, C. F. J. (1986). "Jackknife, bootstrap and other resampling methods in regression analysis". In: The Annals of Statistics. DOI: 10.1214/aos/1176350142.

## Prerequisites

R (v3.4.3 or later)

R packages:
* nlreg (v1.2-2)
* quantreg (v5.35)
* ggplot2 (v2.2.1)
* ggrepel (v0.8.0)

On RHEL 7 these dependencies can be installed via:

```
yum install R
R;
>install.packages(c("nlreg","quantreg","ggplot2","ggrepel"))
```

On macOS 10.13, they can be installed using Homebrew (https://brew.sh), as:

```
brew install R;
R;
>install.packages(c("nlreg","quantreg","ggplot2","ggrepel"))
```

## Installation

With dependancies installed, nlallom can be installed:

```
cd [INSTALLATION_DIR];
git clone https://github.com/apburt/nlallom.git;
```

## Usage

nlallom.r is called as:

```
Rscript [INSTALLATION_DIR]/nlallom/src/nlallom.r [INSTALLATION_DIR]/nlallom/src/ [ALLOM_DATA_DIR]/allomdata.txt [ERRORFORM_BOOL] [MODELFORM_BOOL] [ALPHA] [RUNS] 
```

Where the allometric dataset is a headerless ASCII text file of the form: 4 col x n-stems (tree-id, diameter-at-breast height, tree height, measured AGB, wood density (SI base units)).
[ERRORFORM_BOOL] defines whether moder error is multiplicative (TRUE) or additive (FALSE), and [MODELFORM_BOOL] definies either two-paramter (FALSE) or three-parameter (TRUE) model form.
[ALPHA] and [RUNS] denote the prediction/confidence interval level and the number of bootstrap iterations respectively (e.g., 0.05 and 10000).
Once run, this script will produce a graph describing the model and associated uncertainties, in the working directory.

runallom.r is called as:

```
Rscript [INSTALLATION_DIR]/nlallom/src/runallom.r [INSTALLATION_DIR]/nlallom/src/ [ALLOM_DATA_DIR]/allomdata.txt [ALPHA] [RUNS] [FIELD_DIR]/*.txt
```

Where the field data (wildcards allowed) are headerless ASCII text files of the form: 6 col x n-stems (tree-id, x-coordinate, y-coordinate, diameter-at-breast height, tree height, wood density (SI base units)).
Once complete, this script will produce the files tresults.txt and presults.txt containing tree- and plot-scale estimates of AGB and their uncertaintis for each of the aforementioned model forms.

For both scripts, if [RUNS] is large (~>10000), various stack overflow errors may appear, possible solutions include:

```
Rscript --max-ppsize=500000
ulimit -s 65000
```

## Authors

* **Andrew Burt**

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
