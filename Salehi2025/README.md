21.5.2025

# Gender Differences in Global Antimicrobial Resistance

This repository contains a cleaned demo dataset (`tse_AMRdemo.rds`) and supporting code used in the analysis of gender-related differences in global antimicrobial resistance (AMR).

## Study Information

-   Title: Gender differences in global antimicrobial resistance

-   Authors: Mahkameh Salehi, Ville Laitinen, Shivang Bhanushali, Johan Bengtsson-Palme, Peter Collignon, John J Beggs, Katariina Pärnänen, Leo Lahti

-   DOI (Zenodo): <https://doi.org/10.5281/zenodo.14909582>

### Reference

Salehi, M., Laitinen, V., Bhanushali, S. *et al.* (2025). *Gender differences in global antimicrobial resistance*. *npj Biofilms and Microbiomes, 11*, 79.  
[https://doi.org/10.1038/s41522-025-00715-9](https://doi.org/10.1038/s41522-025-00715-9)

## Contents

-   `tse_AMRdemo.rds` – Cleaned TreeSummarizedExperiment object containing AMR gene count and metadata.

-   `data.R` – Preprocessing script: filters metadata, calculates ARG loads, top antibiotic class abundances, and computes diversity indices.

-   `funcs.R` – Utility functions used for formatting and significance annotations.

-   `model.R` – Runs the Bayesian models for the manuscript (slow, runs brms).

-   `ComparisonModelLognormal.R` and `ComparisonModelGaussian.R` – Comparison of log-normal and log-transformed normal models.

-   `StratifiedModelARG.R` and `StratifiedModelShannon.R` – Stratified model fitting by subgroups such as age and gender.

-   `HierarchicalModelARG.R` and `HierarchicalModelShannon.R` – Hierarchical model with nested random effects.

-   `FrequentistCompareARG.R` and `FrequentistCompareShannon.R` – Simulation-based comparison of Bayesian and frequentist models for ARG and Shannon.

-   `EnrichmentModels.R`  – runs enrichment models using Bayesian regression to estimate high ARG prevalence across groups.

-   `Stancompare.R` – comparison between rtsan and cmdstanr.

-   `report.qmd` – Quarto document that loads precomputed model results and generates results.

-    `main.R` – Wrapper script to execute all of the above in correct order.

-    All fitted models and simulation results are saved as `.rds` files to avoid repeated computation.

## How to Use and Reproduce the report

To load the dataset in R:

``` r
# Load the TSE object
tse <- readRDS("tse_AMRdemo.rds")

# Check the structure of the dataset
tse
```

To reproduce the analysis and generate Figure 5 from the manuscript:

1.  [Download all necessary files](https://github.com/microbiome/data/blob/main/Salehi2025) from the repository:

    - `tse_AMRdemo.rds`
    - `data.R`
    - `funcs.R`
    - `model.R`
    - `ComparisonModelLognormal.R`
    - `ComparisonModelGaussian.R`
    - `StratifiedModelARG.R`
    - `StratifiedModelShannon.R`
    - `HierarchicalModelARG.R`
    - `HierarchicalModelShannon.R`
    - `FrequentistCompareARG.R`
    - `FrequentistCompareShannon.R`
    - `EnrichmentModels.R`
    - `Stancompare.R`
    - `report.qmd`
    - `main.R`

2.  Open R or RStudio in the same directory where these files are located.

3. If you want to re-run the full analysis from scratch (including model fitting), run:

```r
source("main.R")
```

**Tip:** If you do not want to re-fit the models, you can skip the long-running computations by downloading all `.rds` result files from the repository, and then running just the following:

```r
source("data.R")
source("funcs.R")
quarto::quarto_render("report.qmd")
```
