# This script benchmarks the computation time of Bayesian models fitted using two Stan backends:
# rstan and cmdstanr. Each model is fitted three times with both backends
set.seed(123123)
incomes <- c("LMIC", "HIC")

# One-time setup: install CmdStan if not already installed
if (!cmdstanr::cmdstan_version(TRUE) > "0.0.0") {
  cmdstanr::install_cmdstan()
}

# Ensure CmdStan toolchain is available
cmdstanr::check_cmdstan_toolchain()

# Define Stan backends to compare
backends <- c("rstan", "cmdstanr")

# Number of repetitions per model/backend
n_reps <- 5

# Store fitted models and timings
fit_results <- list()
timings <- data.frame(
  model = character(),
  income = character(),
  backend = character(),
  repetition = integer(),
  time_sec = numeric(),
  stringsAsFactors = FALSE
)

# MODEL 1: ARG_load model by income group

for (ic in incomes) {
  
  # Select appropriate dataset based on income level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define model formula with log(readcount) and random intercept
  my_formula <- ARG_load ~ continent + age_category + gender + Usage_high + log(readcount) + (1 | bioproject)
  
  # Loop over Stan backends
  for (backend in backends) {
    
    for (rep in 1:n_reps) {
      print(paste("Fitting ARG_load model for", ic, "using", backend, "- repetition", rep))
      
      # Start timer
      tic()
      fit <- brm(
        formula = my_formula,
        data = my_data,
        family = lognormal(),
        prior = c(
          set_prior("normal(0, 1)", class = "b"),
          set_prior("normal(0, 1)", class = "Intercept"),
          set_prior("normal(0, 1)", class = "sd", group = "bioproject")
        ),
        backend = backend,
        chains = 2,
        iter = 5000,
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        cores = parallel::detectCores(),
        silent = TRUE,
        refresh = 0
      )
      # Stop timer
      time_taken <- toc(quiet = TRUE)
      
      fit_results[[paste("ARG_load", ic, backend, rep, sep = "_")]] <- fit
      timings <- rbind(
        timings,
        data.frame(model = "ARG_load", income = ic, backend = backend, repetition = rep, time_sec = time_taken$toc - time_taken$tic)
      )
    }
  }
}

# MODEL 2: Enrichment model across gender

# Define model formula
my_formula2 <- high_ARG ~ gender + (gender | income_group)

# Loop over Stan backends
for (backend in backends) {
  for (rep in 1:n_reps) {
    print(paste("Fitting enrichment_gender_hier model using", backend, "- repetition", rep))
    
    # Start timer
    tic()
    fit <- brm(
      formula = my_formula2,
      data = enrichment_df,
      family = bernoulli(),
      prior = set_prior("normal(0, 1)", class = "b"),
      backend = backend,
      chains = 2,
      iter = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      cores = parallel::detectCores(),
      silent = TRUE,
      refresh = 0
    )
    # Stop timer
    time_taken <- toc(quiet = TRUE)
    
    fit_results[[paste("enrichment_gender_hier", backend, rep, sep = "_")]] <- fit
    timings <- rbind(
      timings,
      data.frame(model = "enrichment_gender_hier", income = "all", backend = backend, repetition = rep, time_sec = time_taken$toc - time_taken$tic)
    )
  }
}

# Save timing results
saveRDS(timings, file = "stan_timing_results.rds")

