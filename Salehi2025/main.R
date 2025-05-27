# Data processing
source("data.R")

# Utility functions
source("funcs.R")

# Modeling (slow, only needs to be done once)
source("model.R")

# Model comparison: log-normal vs. log-transformed normal (slow, only needs to be done once)
source("ComparisonModelLognormal.R")
source("ComparisonModelGaussian.R")

# Stratified model fitting (by subgroups like Income/gender) (slow, only needs to be done once)
source("StratifiedModelARG.R")
source("StratifiedModelShannon.R")

# Hierarchical model with nested random effects (slow, only needs to be done once)
source("HierarchicalModelARG.R")
source("HierarchicalModelShannon.R")

# Simulation-based comparison: Bayesian vs. Frequentist under small samples (ARG + Shannon)
# (slow, only needs to be done once)
source("FrequentistCompareARG.R")
source("FrequentistCompareShannon.R")

# Enrichment models (very slow, only needs to be done once)
source("EnrichmentModels.R")

# Comparison between rtsan and cmdstanr (very slow, only needs to be done once)
source("Stancompare.R")

# Render the full Quarto report
quarto::quarto_render("report.qmd")
