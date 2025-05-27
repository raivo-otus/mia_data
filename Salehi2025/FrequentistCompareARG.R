# Simulation pipeline for comparing Bayesian and Frequentist models
# Function to run one simulation iteration:
# - Draws a random sample
# - Fits Bayesian and Frequentist models
# - Extracts estimates and 95% interval widths
simulate_once <- function(data, sample_size) {
  sample <- as.data.frame(data) %>%
    slice_sample(n = sample_size)
  
  # Fit Bayesian model using lognormal family
  fit_bayes <- brm(
    formula = ARG_load ~ age_category,
    data = sample,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept")
    ),
    iter = 2000,
    chains = 2,
    refresh = 0,
    control = list(adapt_delta = 0.95)
  )
  
  # Fit Frequentist model (log-linear regression)
  fit_lm <- lm(log(ARG_load) ~ age_category, data = sample)
  
  # Summarize Bayesian
  bayes_summary <- as.data.frame(posterior_summary(fit_bayes)) %>%
    rownames_to_column("term") %>%
    filter(grepl("age_category", term)) %>%
    mutate(
      Predictor = gsub("b_age_category", "", term),
      model = "Bayesian",
      width = Q97.5 - Q2.5,
      estimate = Estimate
    ) %>%
    select(Predictor, estimate, width, model)
  
  # Summarize Frequentist
  lm_summary <- broom::tidy(fit_lm, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Predictor = gsub("age_category", "", term),
      model = "Frequentist",
      width = conf.high - conf.low,
      estimate = estimate
    ) %>%
    select(Predictor, estimate, width, model)
  
  # Standardize and combine
  bayes_summary$Predictor <- standardize_age(bayes_summary$Predictor)
  lm_summary$Predictor <- standardize_age(lm_summary$Predictor)
  
  bind_rows(bayes_summary, lm_summary) %>%
    mutate(sample_size = sample_size)
}

# Simulation
sample_sizes <- c(30, 80, 150, 250, 500)
n_reps <- 5

# Run simulations with enforced minimum per age category
results_ARG <- expand.grid(sample_size = sample_sizes, rep = 1:n_reps) %>%
  pmap_dfr(function(sample_size, rep) {
    message(glue::glue("Running: sample_size = {sample_size}, repetition = {rep}"))
    
    # Sample 3 per age_category, then fill the rest randomly
    base <- adult_df %>% group_by(age_category) %>% slice_sample(n = 3) %>% ungroup()
    rest <- adult_df %>% filter(!acc %in% base$acc) %>% slice_sample(n = sample_size - nrow(base))
    sample <- bind_rows(base, rest)
    
    simulate_once(sample, sample_size = sample_size) %>%
      mutate(rep = rep)
  })

# Save output
saveRDS(results_ARG, "results_compare_ARG.rds")
