# Bayesian Inference of Conjoint Experiments

# 1. Housekeeping ----------

# Install 'librarian'
install.packages('librarian')

# Install/load packages
librarian::shelf(tidyverse/tidyverse,
                 tidyverse/magrittr,
                 sfirke/janitor,
                 leeper/cregg,
                 paul-buerkner/brms,
                 mbarnfield/cjpwr,
                 here,
                 ggridges)

# Load immigration dataset
data(immigration)

# Load functions
source(here("cjbae_formula.R"))

# 2. Analyse using cregg ----------

# Write formula
cregg_f <- ChosenImmigrant ~ Gender + Education + 
  LanguageSkills + CountryOfOrigin + Job + JobExperience + 
  JobPlans + ReasonForApplication + PriorEntry

# Calculate AMCEs
cregg_amce <- cj(data = immigration,
               formula = cregg_f,
               id = ~ CaseID,
               estimate = "amce")

# Calculate MMs
cregg_mm <- cj(data = immigration,
              formula = cregg_f,
              id = ~ CaseID,
              estimate = "mm",
              h0 = 0.5)

# Plot amces
plot(cregg_amce, vline = 0.0)

# Plot MMs
plot(cregg_mm, vline = 0.5)



# 3. Analyse using Bayesian methods ----------

# Convert cregg formula to bae formula
f1 <- cjbae_formula(immigration, cregg_f, CaseID)

# Specify priors
p1 <- c(prior(normal(0, .2), class = "Intercept"),
        prior(normal(0, .2), class = "b"),
        prior(exponential(10), class = "sd"),
        prior(exponential(10), class = "sigma"))

# Run model
m1 <- brm(formula = f1,
          family = gaussian(),
          prior = p1,
          data = immigration,
          iter = 2e3,
          chains = 2,
          cores = 2,
          refresh = 10)

# Summarise model output
summary(m1)


# Plot baemces
posterior_samples(m1) %>%
  data.frame() %>%
  select(contains("b_"), -"b_Intercept") %>%
  reshape2::melt() %>%
  mutate(variable = gsub(".*_",
                         "",
                         variable)) %>%
  mutate(feature = substr(variable, 1, 3)) %>%
  ggplot(aes(x = value,
             y = variable,
             fill = feature)) +
  geom_density_ridges(rel_min_height = 0.01, alpha = .6) +
  geom_vline(xintercept = 0.0, linetype = "dotted") +
  xlab("Average Marginal Component Effect") +
  ylab("") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        axis.title.y = element_blank())



#4. Functions ----------

# cjbaemce() 
# Function takes data, (standard) formula and id, converts
# formula to brms formula, runs amce model, then
# outputs predictor amces as dataframe

cjbaemce <- function(data, formula, id) {
  
  # Load tidyverse for magrittr and dplyr
  library(tidyverse)
  
  # Find id variable in data
  id_enq <- enquo(id)
  
  # Find lhs and rhs variables in formula
  outcome <- all.vars(stats::update(formula, . ~ 0))
  if (!length(outcome) || outcome == ".") {
    stop("'formula' is missing a left-hand outcome variable")
  }
  predictors <- all.vars(stats::update(formula, 0 ~ .))
  
  # Collapse predictors in rhs string
  predictors_string <- paste(predictors, collapse = " + ")  
  
  # Get id variable from data
  id <- data %>%
    dplyr::select(!!id_enq)
  
  # Create formula string
  bae_formula <- paste0(outcome,
                        " ~ 1 + ",
                        predictors_string,
                        " + (1 | ",
                        colnames(id),
                        ")")
  
  # Convert to formula
  bae_formula <- formula(bae_formula)
  
  # Run model 
  baemces <- brm(
    formula = bae_formula,
    data = data,
    iter = 2e3,
    chains = 2,
    cores = 2,
    refresh = 10
  )
  
  # Extract posterior samples and make dataframe
  posterior_samples(baemces) %>%
    data.frame() %>%
    select(contains("b_"), "sigma")
}

# cjbae_plot()
# Function takes cjbaemce() output and plots it

cjbae_plot <- function(posteriors, visuals = NULL) {
  reshape2::melt(posteriors) %>%
    mutate(variable = gsub(".*_",
                           "",
                           variable)) %>%
    mutate(feature = substr(variable, 1, 3)) %>%
    ggplot(aes(x = value,
               y = variable,
               fill = feature)) +
    geom_density_ridges(rel_min_height = 0.01) +
    geom_vline(xintercept = 0.0, linetype = "dotted")
}



