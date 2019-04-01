#' @rdname cjbae
#' @title Bayesian inference for conjoint experiments
#' @export

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

#' @rdname cjbae
#' @export
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
