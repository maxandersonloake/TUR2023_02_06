# TUR2023_02_06 – Building Damage Modelling for the 2023 Türkiye/Syria Earthquake Sequence

This repository contains the companion R scripts to the paper 'Accounting for biases in the analysis of building damage data for the 2023 M7.8 Türkiye/Syria earthquake sequence' by Max Anderson Loake and Kishor Jaiswal. This paper was submitted to the journal Earthquake Spectra and is currently under peer review. 

The scripts use two building damage datasets to perform joint Bayesian inference over building inventory and fragility. The  repository structure is as follows:

- `Functions.R`: Loads all required R packages and defines shared utility functions. It must be sourced by the other scripts before use.
- `MinistrysAnalysis.R` : Used to load and manipulate the MEUCC data, and perform basic fragility inference over the MEUCC data with and without accounting for building missingness.
- `MinistrysExtractCities.R` : This script is used to reduce the size of the MEUCC dataset, filtering to buildings in the cities explored in the engineering survey data. 
- `FullModel.R`: Performs main infererence, including loading data, defining priors, and plotting posterior distributions. 
- `Posterior Summary` :  Contains the posterior mean, posterior standard deviation, quantiles, and convergence diagnostics from the full model fit perfomed in `FullModel.R` using `StanModels/FullModel.stan`.
-  StanModels:
    - `StanModels/FullModel.stan` : Stan model used for full Bayesian inference of building damage fragility.
    - `StanModels/MinistrysFragCurves.stan` : Stan model used for simple Bayesian infernece over MEUCC data, without accounting for building mislabelling
    - `StanModels/MinistrysFragCurves_MissingAccounted.stan` : Stan model used for simple Bayesian infernece over MEUCC data, accounting for building mislabelling
-  Data:
    - `Data/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data.xlsx` : Engineering Survey Data
    - `Data/ShakeMapUpd.xml.gz` : ShakeMap for the M7.8 mainshock

## Instructions to run scripts

1. At the beginning of the four R scripts, change the directory to your working directory
2. If not already installed, install the packages loaded in `Functions.R`
3. Run `MinistrysAnalysis.R` to prepare the MEUCC data for analysis.
4. Run `FullModel.R` for full analysis

## License


## Contact

For questions about this repository or the associated analysis, please contact the repository maintainer or refer to the associated publication.
