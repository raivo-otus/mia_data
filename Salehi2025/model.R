# First, we define the income groups (LMIC, HIC) used for stratified statistical modeling.
set.seed(123123)
incomes <- c("LMIC", "HIC")

## Model 1 for ARG_load **************************** ####

fit1_ARG_load <- list()

for(ic in incomes) {  # Loop over income levels for ARG_load
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define fixed-effects model formula
  my_formula <- ARG_load ~ continent + age_category + gender + Usage_high
  
  print(paste("Fitting ARG_load Model 1 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit1_ARG_load[[ic]] <- my_fit
}

# Save ARG_load model
saveRDS(fit1_ARG_load, "fit1_ARG_load.rds")



## Model 1 for shannon_diversity **************************** ####

fit1_shannon_diversity <- list()

for(ic in incomes) {  # Loop over income levels for shannon_diversity
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define fixed-effects model formula
  my_formula <- shannon_diversity ~ continent + age_category + gender + Usage_high
  
  print(paste("Fitting shannon_diversity Model 1 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = gaussian(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit1_shannon_diversity[[ic]] <- my_fit
}

# Save shannon_diversity model
saveRDS(fit1_shannon_diversity, "fit1_shannon_diversity.rds")





## Model 2 for ARG_load **************************** ####

fit2_ARG_load <- list()

for(ic in incomes) {  # Loop over income levels for ARG_load
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define formula with random intercept for bioproject
  my_formula <- ARG_load ~ continent + age_category + gender + Usage_high + (1 | bioproject)
  
  print(paste("Fitting ARG_load Model 2 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "sd", group = "bioproject")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit2_ARG_load[[ic]] <- my_fit
}

# Save ARG_load Model 2
saveRDS(fit2_ARG_load, "fit2_ARG_load.rds")



## Model 2 for shannon_diversity **************************** ####

fit2_shannon_diversity <- list()

for(ic in incomes) {  # Loop over income levels for shannon_diversity
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define formula with random intercept for bioproject
  my_formula <- shannon_diversity ~ continent + age_category + gender + Usage_high + (1 | bioproject)
  
  print(paste("Fitting shannon_diversity Model 2 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = gaussian(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "sd", group = "bioproject")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit2_shannon_diversity[[ic]] <- my_fit
}

# Save shannon_diversity Model 2
saveRDS(fit2_shannon_diversity, "fit2_shannon_diversity.rds")



## Model 3 for ARG_load **************************** ####

fit3_ARG_load <- list()

for(ic in incomes) {  # Loop over income levels for ARG_load
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define fixed-effects model formula with log(readcount)
  my_formula <- ARG_load ~ continent + age_category + gender + Usage_high + log(readcount)
  
  print(paste("Fitting ARG_load Model 3 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit3_ARG_load[[ic]] <- my_fit
}

# Save ARG_load Model 3
saveRDS(fit3_ARG_load, "fit3_ARG_load.rds")


## Model 3 for shannon_diversity **************************** ####

fit3_shannon_diversity <- list()

for(ic in incomes) {  # Loop over income levels for shannon_diversity
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define fixed-effects model formula with log(readcount)
  my_formula <- shannon_diversity ~ continent + age_category + gender + Usage_high + log(readcount)
  
  print(paste("Fitting shannon_diversity Model 3 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = gaussian(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    cores = parallel::detectCores()
  )
  
  fit3_shannon_diversity[[ic]] <- my_fit
}

# Save shannon_diversity Model 3
saveRDS(fit3_shannon_diversity, "fit3_shannon_diversity.rds")




## Model 4 for ARG_load **************************** ####

fit4_ARG_load <- list()

for(ic in incomes) {  # Loop over income levels for ARG_load
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define model formula with log(readcount) and random intercept
  my_formula <- ARG_load ~ continent + age_category + gender + Usage_high + log(readcount) + (1 | bioproject)
  
  print(paste("Fitting ARG_load Model 4 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = lognormal(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "sd", group = "bioproject")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 14),
    cores = parallel::detectCores()
  )
  
  fit4_ARG_load[[ic]] <- my_fit
}

# Save ARG_load Model 4
saveRDS(fit4_ARG_load, "fit4_ARG_load.rds")


## Model 4 for shannon_diversity **************************** ####

fit4_shannon_diversity <- list()

for(ic in incomes) {  # Loop over income levels for shannon_diversity
  
  # Select data with correct pre-set base level
  my_data <- if (ic == "LMIC") adult_df_LMIC else adult_df_HIC
  
  # Define model formula with log(readcount) and random intercept
  my_formula <- shannon_diversity ~ continent + age_category + gender + Usage_high + log(readcount) + (1 | bioproject)
  
  print(paste("Fitting shannon_diversity Model 4 for", ic))
  my_fit <- brm(
    formula = my_formula,
    data = my_data,
    family = gaussian(),
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "sd", group = "bioproject")
    ),
    chains = 2,
    iter = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 14),
    cores = parallel::detectCores()
  )
  
  fit4_shannon_diversity[[ic]] <- my_fit
}

# Save shannon_diversity Model 4
saveRDS(fit4_shannon_diversity, "fit4_shannon_diversity.rds")

