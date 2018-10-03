# mtr-toolkit: A multi-target regression (MTR) toolkit in R!

This project implements current state-of-the-art MTR solutions, as well as, new methods proposed by *Saulo Martiello Mastelini* (in conjunction with other researchers, e.g., *Sylvio Barbon Jr.* and *Everton Jose Santana*).

Some of the solutions are, currently, under tests. A documentation (possibly) will be created as the codes are improved and new publications are obtained.

Currently, only local approaches are supported. The currently implemented methods are (MTR methods proposed by *Mastelini* are marked with \*):

- ST: Single-target
- SST: Stacked Single-target (a.k.a. MTRS -- Multi-target Regressor Stacking)
- ERC: Ensemble of Regressor Chains
- DSTARS: Deep Structure for Tracking Asynchronous Regressor Stacking (\*, \*\*)
- DRS: Deep Regressor Stacking (\*)
- MTAS: Multi-target Augmented Stacking (\*)
- MOTC: Multi-output Tree Chaining (\*)
- ORC: Optimum Regressor Chains (\*)
- MTSG: Multi-target Stacked Generalization (\*)
- ESR: Ensemble of Stacked Regressors (\*)

Currently supported regressors are (all the regression techniques are performed with their default parameters):

- ranger (*ranger* package implementation of Random Forest)
- svm (Support Vector Machine -- using the rbf kernel)
- xgboost (Extreme Gradient Boosting)
- cart (Classification and Regression Tree)
- mlp (*RSNNS* implementation of Multi Layer Perceptron Artificial Neural Network)
- gbm (Gradient Boosting Machine)
- pls (Partial Least Squares)
- ridge (Ridge Regression)
- lr (Linear Regression)
- rf (*randomForest*'s implementation of Random Forest)

Both SST (MTRS) and ERC were proposed by Spyromitros-Xioufis et al. (2016) and can be found in:

*- Spyromitros-Xioufis, E., Tsoumakas, G., Groves, W. and Vlahavas, I., 2016. Multi-target regression via input space expansion: treating targets as inputs. Machine Learning, 104(1), pp.55-98.*

\*\*: Two versions of DSTARS are implemented:

- DSTARS -> This version uses a single bootstrap sample when searching for the best regressor layer disposition. Therefore, it only uses the hyperparameter *'epsilon'* for the minimum expected value of error decrease when adding a new regressor.
- DSTARST -> This version uses as internal k-fold Cross-validation for determining the best number of regressor layers. Hence, the hyperparameters *'phi'* and *'epsilon'* must be specified for selecting the regressor layers that contributed in at least *phi* percent of time, and the minimum amount of expected error decrease by adding a new regressor, respectively. Multiple values of *phi* and *epsilon* can be evaluated as it is shown next. We suggest using (*phi*, *epsilon*) = (0.4, 1e-4).

*I will add the corresponding papers for our methods ASAP (the ones that were already published).*

The required R packages to run the toolkit can be installed by running `utils_and_includes/installLibraries.R`

## Basic usage:

All the implemented MTR methods are under *local_methods/*. To run an experiment the employed command is:

```
Rscript run_exp.R configuration_file.R
```

where *configuration_file.R* is an input file containing configuration parameters for running the experiments (e.g., datasets, number of targets, methods, regressors, output folder/files, etc.). The toolkit only supports `csv` files as inputs.

An example of possible configuration file is presented below:


```R
###############################################################################
#############################General settings##################################
###############################################################################

# Determine whether to employ PLS as a feature extractor (PLS' scores are used as new input features)
use.pls <- FALSE

# Determines the normalization to be applied in the datasets:
# Supported values are: 'min-max' and 'z-score'
norm.method <- "min-max"

# Defines the datasets and their corresponding number of targets
bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6, 6, 16, 16, 8, 8, 16, 16, 2, 3, 3, 3, 14, 2, 3, 6, 12, 3)

# Defines values for tuning the DSTARS algorithm (only used by the DSTARST version)
dstars.phis <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
dstars.epsilons <- c(10^-2, 10^-3, 10^-4)

# Determines external testing sets. This option must be used along the setting of folds.num to 1
bases.test <- NULL

# Determines the regression techniques to be used
techs <- c("ranger", "svm")

# Determines the number of CV folds for evaluation. (Use this parameter as 1 in conjunction with input test sets for train then test evaluations)
folds.num <- 10

# Determines the folder in which the input datasets are
datasets.folder <- "~/mtr_datasets"
# Determines an output folder for results writing
output.prefix <- "~/output_mtr"

# Determines the MTR methods to be run
mt.techs <- c("ST", "MTRS", "ERC", "DSTARST")

# Whether to unify the output logs of a same MTR method and different regressors
must.compare <- TRUE
# Whether to create a final table, comprising all the obtained results
generate.final.table <- TRUE
# Whether to present the aRRMSE values of all methods in a table for easy comparison using statistical tests
generate.nemenyi.frame <- TRUE
###############################################################################
###############################################################################
###############################################################################

```

*The other folds in this repository have additional tools for MTR data analysis and plot. The corresponding documentation will be added in the future.*
