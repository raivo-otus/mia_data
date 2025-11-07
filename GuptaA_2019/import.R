
dir <- "GuptaA_2019"

library(mia)
library(ape)

# Read taxonomy data into TreeSE
rd <- read.csv(file.path(dir, "taxonomy_table.csv"), row.names = 1L) |> DataFrame()
cd <- read.csv(file.path(dir, "sample_metadata.csv"), row.names = 1L) |> DataFrame()
assay <- read.csv(file.path(dir, "taxonomy_abundance.csv"), row.names = 1L) |> as.matrix()
tree <- read.tree(file.path(dir, "phylogeny.tree"))
tse1 <- TreeSummarizedExperiment(
    assays = SimpleList(counts = assay),
    rowData = rd,
    colData = cd,
    rowTree = tree
)

# Read pathway data into TreeSE
assay <- read.csv(file.path(dir, "pathway_abundance.csv"), row.names = 1L) |> as.matrix()
tse2 <- TreeSummarizedExperiment(
    assays = SimpleList(relative_abundance = assay),
    colData = cd
)

mae <- MultiAssayExperiment(
    experiments = ExperimentList(taxonomy = tse1, pathway = tse2),
    colData = cd
)

saveRDS(mae, file.path(dir, "mae.rds"))
