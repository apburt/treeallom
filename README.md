# nlallom

Construct non-linear allometric models with uncertainties quantified via non-parametric methods

## Overview

nlallom has been developed to construct non-linear maximum likelihood estimation power-law allometric models (of the form ![equation](http://latex.codecogs.com/gif.latex?y&space;=&space;aX^b+\varepsilon)), with a multiplicative (![equation](http://latex.codecogs.com/gif.latex?\varepsilon&space;\sim&space;\mathcal{N}&space;(0,\sigma^2&space;X^k))) error term, using the nlreg package[1].

Uncertainties in these non-linear models are quantified using non-parametric methods: prediction and confidence intervals are generated via non-linear quantile regression[2] and the wild bootstrap[3] respectively. 

The framework presented here is focused on the construction of biomass estimation models (BEMs), but should be readily modifiable to other applications and/or model forms. 

In its current form, nlallom can be used in two ways:

1) nlallom.r - construct allometric models of the above forms using some calibration data
2) runagb.r - estimate tree- and plot-scale above-ground biomass and its uncertainty using calibration and field data

The usage of these two scripts is described below, and a more full description of the methods themselves can be found at: http://discovery.ucl.ac.uk/1575534/ (pp. 14-33)

[1] Bellio, R. and Brazzale, A. R. “Higher Order Asymptotics Unleashed: Software Design for Nonlinear Heteroscedastic Regression”. In: Journal of Computational and Graphical Statistics 12.3 (2003), pp. 682–697. doi: 10.1198/1061860032003.

[2] Koenker, R. and Park, B. J. (1996). "An interior point algorithm for nonlinear quantile regression". In: Journal of Econometrics. DOI: 10.1016/0304-4076(96)84507-6.

[3] Wu, C. F. J. (1986). "Jackknife, bootstrap and other resampling methods in regression analysis". In: The Annals of Statistics. DOI: 10.1214/aos/1176350142.

## Dependencies

R (v3.5.1 or later)

R packages:
* nlreg (v1.2.2.1)
* quantreg (v5.36)
* BIOMASS (v1.2)
* ggplot2 (v3.1.0)
* ggrepel (v0.8.0)
* gridExtra (v2.3)

On Ubuntu 18.10 these dependencies can be installed via:

```
apt install r-base
sudo R;
>install.packages(c("nlreg","quantreg","BIOMASS","ggplot2","ggrepel","gridExtra"))
```

On macOS 10.14, they can be installed using Homebrew (https://brew.sh), as:

```
brew install R;
R;
>install.packages(c("nlreg","quantreg","BIOMASS","ggplot2","ggrepel","gridExtra"))
```

## Installation

With the dependencies installed, nlallom can be installed:

```
cd [INSTALLATION_DIR];
git clone https://github.com/apburt/nlallom.git;
```

## Usage

nlallom.r is called as:

```
Rscript [INSTALLATION_DIR]/nlallom/src/nlallom.r [INSTALLATION_DIR]/nlallom/src/ [CAL_DATA_DIR]/caldata.txt [ERRORFORM_BOOL] [ALPHA] [RUNS] 
```

Where the calibration data are in a headerless ASCII text file of the form: 4 col x n-stems (tree-id, diameter-at-breast height, tree height, observed AGB, wood density (SI base units)).
[ERRORFORM_BOOL] defines whether model error is additive (FALSE) or multiplicative (TRUE).
[ALPHA] and [RUNS] denote the prediction/confidence interval and the number of bootstrap iterations respectively (e.g., 0.05 and 10000).
Once run, this script will produce a graph, "nlallom_model.pdf", describing the model and associated uncertainties, in the working directory.
Also produced is "nlallom_diagnostics.pdf", showing the distribution of the residuals.

runbem.r is called as:

```
Rscript [INSTALLATION_DIR]/nlallom/src/runagb.r [INSTALLATION_DIR]/nlallom/src/ [CAL_DATA_DIR]/caldata.txt [ALPHA] [RUNS] [LOCAL_BOOL] [FIELD_DIR]/*.txt
```

Where the plot field data (wildcards permitted) are in a headerless ASCII text files of the form: 6 col x n-stems (tree-id, x-coordinate, y-coordinate, diameter-at-breast height, tree height, wood density (SI base units)).
[LOCAL_BOOL] defines whether a local regression method is also considered (documentation forthcoming).
Once complete, this script will produce two text files for each plot containing tree- and plots-scale estimates of AGB and their uncertainties for each of the aforementioned model forms.
Additionally, for comparison, estimates of AGB and uncertainty will be generated using the BIOMASS R package[4].
The variables in these text files are defined in [VARIABLES](VARIABLES).

[4] Réjou-Méchain, M., A. Tanguy, C. Piponiot, J. Chave, and B. Hérault (2017). “BIOMASS: an r package for estimating above-ground biomass and its uncertainty in tropical forests”. In: Methods in Ecology and Evolution 8.9, pp. 1163–1167. doi: 10.1111/2041-210X.12753.

For both scripts, if [RUNS] is large (ca. >10000), various stack overflow errors may appear, possible solutions include:

```
Rscript --max-ppsize=500000
ulimit -s 65000
```

## Authors

* **Andrew Burt**

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
