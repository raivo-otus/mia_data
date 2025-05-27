# This R script includes the data processing steps required for the analysis of Figure 5.
# It prepares and cleans the input data using mia where appropriate.

# Load required libraries
library(scater)
library(ggpubr)
library(rstatix)
library(viridis)
library(ggsignif)
library(Matrix)
library(RColorBrewer)
library(caret)
library(randomForest)
library(xgboost)
library(brms)
library(glmnet)
library(tidyverse)
library(vegan)
library(TreeSummarizedExperiment)
library(mia)
library(dplyr)
library(loo)
library(cmdstanr)
library(broom)
library(tibble)
library(tictoc)


# Load TSE object
# The 'tse_AMRdemo.rds' file contains a pre-processed TreeSummarizedExperiment (TSE) object.
# The dataset focuses on gender-related differences in global antimicrobial resistance (AMR).
# It originates from the study: "Gender differences in global antimicrobial resistance" (Salehi et al., 2025).
tse <- readRDS("tse_AMRdemo.rds")

# Set sample names
colnames(tse) <- tse$acc

# Filter out samples with missing gender or age information
tse <- tse[, !is.na(tse$gender) & !is.na(tse$host_age_years)]

# Calculate Shannon diversity index directly using the original count data
tse <- addAlpha(
  tse,
  assay.type = "counts",
  index = "shannon",
  name = "shannon_diversity"
)

# Aggregate gene-level counts by antibiotic resistance class using rowData variable 'Class'
rowData(tse)$Class <- as.factor(rowData(tse)$Class)
tse_class <- agglomerateByVariable(tse, by = "rows", group = rowData(tse)$Class)

# Replace NA counts with 0 to avoid issues in downstream functions
assay(tse_class, "counts")[is.na(assay(tse_class, "counts"))] <- 0

# Identify the top 5 most abundant AB classes using total counts across samples
top_5_AB_classes <- getTop(tse_class, top = 5, method = "sum", assay.type = "counts")
tse_class_top5 <- tse_class[top_5_AB_classes, ]

# Log-transform count data to reduce skewness and stabilize variance
tse_class_top5 <- transformAssay(
  tse_class_top5,
  assay.type = "counts",
  method = "log",
  pseudocount = TRUE,
  name = "logcounts"
)

# Add total read count per sample to metadata
colData(tse_class_top5)$readcount_2 <- colSums(assay(tse_class_top5, "counts"))
adult_metadata <- as.data.frame(colData(tse_class_top5))

# Filter samples with complete GDP and antibiotic usage data
# and ensure continent is stored as a factor variable
adult_metadata <- adult_metadata %>%
  mutate(continent = factor(geo_loc_name_country_continent_calc)) %>%
  filter(!is.na(Usage), !is.na(GDP_per_head))

# Subset TSE to retain only samples present in filtered metadata
tse_class_top5 <- tse_class_top5[, colnames(tse_class_top5) %in% adult_metadata$acc]
colData(tse_class_top5) <- DataFrame(adult_metadata)

# Remove samples with zero total relative abundance across all features
tse_class_top5 <- tse_class_top5[, colSums(assay(tse_class_top5, "relabundance")) > 0]

# Update adult_metadata from the modified TSE object
adult_metadata <- as.data.frame(colData(tse_class_top5))

# Set age_category as a factor in natural age order
adult_metadata$age_category <- factor(adult_metadata$age_category,
                                      levels = c("Infant", "Toddler", "Children", "Teenager", 
                                                 "Young Adult", "Middle-Aged Adult", "Older Adult", "Oldest Adult")
)

# Create binary and factor variables used in modeling
adult_metadata$income_group_HIC <- ifelse(adult_metadata$World_Bank_Income_Group == "High income", 1, 0)
adult_metadata$Usage_high <- ifelse(adult_metadata$Usage < 10, 0, 1)

# Extract enrichment data from the metadata
enrichment_df <- as.data.frame(adult_metadata)

# Calculate the 90th percentile threshold for ARG load
threshold <- quantile(enrichment_df$ARG_load, probs = 0.90, na.rm = TRUE)

# Create a binary variable indicating high ARG load (above threshold = 1, else = 0)
enrichment_df <- enrichment_df %>%
  mutate(high_ARG = ifelse(ARG_load > threshold, 1, 0),
         income_group = if_else(income_group_HIC == 1, "HIC", "LMIC"))

saveRDS(enrichment_df, "enrichment_df.rds")

# Create a balanced subset of the metadata to speed up hierarchical modeling
subset_data <- adult_metadata %>%
  group_by(age_category, gender) %>%
  group_modify(~ slice_sample(.x, n = min(20, nrow(.x)))) %>%
  ungroup() %>%
  mutate(income_group = if_else(income_group_HIC == 1, "HIC", "LMIC"))

# Create separate modeling datasets with reference levels for factors
adult_df <- adult_metadata %>%
  mutate(
    age_category = relevel(age_category, ref = "Middle-Aged Adult"),
    income_group = if_else(income_group_HIC == 1, "HIC", "LMIC")
  )

# Create a modeling dataset for LMIC (Low- and Middle-Income Countries)
adult_df_LMIC <- adult_df %>%
  filter(income_group == "LMIC") %>%
  mutate(
    continent = relevel(factor(continent), ref = "Asia"),
    gender = relevel(factor(gender), ref = "Men"),
    Usage_high = factor(Usage_high)
  )

# Create a modeling dataset for HIC (High-Income Countries)
adult_df_HIC <- adult_df %>%
  filter(income_group == "HIC") %>%
  mutate(
    continent = relevel(factor(continent), ref = "Europe"),
    gender = relevel(factor(gender), ref = "Men"),
    Usage_high = factor(Usage_high)
  )
