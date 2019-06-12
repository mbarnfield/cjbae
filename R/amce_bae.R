#' @rdname amce_bae
#' @title Bayesian AMCEs for conjoint analysis
#' @description Calculate Bayesian AMCEs for conjoint experiments
#' @param data A tidy conjoint dataset
#' @param formula A standard formula of the form outcome ~ feature 1 + feature 2 ... + feature n
#' @param id Respondent ID variable
#' @param prior Priors for brms. Priors required for beta, intercept, sd and sigma. For clarification see examples below.
#' @param save_amce Logical vector telling R whether or not to save the AMCE brmsfit as an object in your working directory. If you want both AMCE and MM estimates, set this to TRUE then load the brmsfit object into your environment and pass it through `mm_bae()`. Alternatively, use `cjbae()`. Defaults to TRUE.
#' @param save_name The name you would like the AMCE brmsfit saved as. Defaults to 'baerms'.
#' @param iter The number of iterations in the brms model. Essentially, the number of times it will sample from the posterior probability distribution. Defaults to 2000.
#' @param chains The number of chains in the brms model. Defaults to 2.
#' @param cores The number of cores used in the brms model. Defaults to 2.
#' @param refresh The number of refreshes. Defaults to 10.
#' @return A dataframe of AMCEs. These take the form of samples from the posterior probability distribution and can be plotted as distributions, rather than point estimates.
#' @details \code{amce_bae()} is a Bayesian estimation function for a key quantity of interest in conjoint analysis (AMCEs), and is essentially a wrapper for [`brms`](https://github.com/paul-buerkner/brms), and borrows extensively from [`cregg`](https://github.com/leeper/cregg), R's foremost conjoint analysis package. The calculation in this function is computationally expensive, but exactly how long it takes is highly contingent on the size of the dataset.
#' @export
#' @import tidyverse brms magrittr tidybayes reshape2 stats
#' @examples
#' #' #load example dataset from {cregg} (Leeper 2019)
#' library(cregg)
#' data(taxes)
#' # formula
#' f <- chose_plan ~ taxrate1 + taxrate2 + taxrate3 + taxrate4 + taxrate5 + taxrate6 + taxrev
#' # prior - minimally informative
#' prior <- c(set_prior("normal(0, .2)", class = "Intercept"),
#' set_prior("normal(0, .2)", class = "b"),
#' set_prior("exponential(10)", class = "sd"),
#' set_prior("exponential(10)", class = "sigma"))
#' # run amce function with save specified, saves brmsfit to working directory - this will take a while
#' amce_bae(data = taxes, formula = f, id = ID, prior = prior, save_amce = TRUE)
#' # run mm function on the saved output
#' readRDS(baerms)
#' mm <- mm_bae(baerms, f, ID)
#' # plot MMs
#' cjbae_plot(mm, "ridge", "mm")

amce_bae <- function(data,
                     formula,
                     id,
                     prior,
                     save_amce = TRUE,
                     save_name = "baerms",
                     iter = 2e3,
                     chains = 2,
                     cores = 2,
                     refresh = 10) {


  ## convert formula --------

  # find lhs and rhs vars
  outcome <- all.vars(stats::update(formula, . ~ 0))
  if (!length(outcome) || outcome == ".") {
    stop("'formula' is missing a left-hand outcome variable")
  }
  predictors <- all.vars(stats::update(formula, 0 ~ .))

  # collapse predictors in rhs string
  predictors_string <- paste(predictors, collapse = " + ")

  # get id variable from data
  id_enq <- enquo(id)
  id <- data %>%
    dplyr::select(!!id_enq)

  # create
  bae_formula <- paste0(outcome,
                        " ~ 1 + ",
                        predictors_string,
                        " + (1 | ",
                        colnames(id),
                        ")")

  # output as formula for brms
  formula(bae_formula)

  ## amce ------------

  if (save_amce == TRUE) {
    baemces <- brms::brm(
      formula = bae_formula,
      family = gaussian(),
      prior = prior,
      data = data,
      file = save_name,
      iter = iter,
      chains = chains,
      cores = cores,
      refresh = refresh
    )

    baemces <- posterior_samples(baemces, "^b") %>%
      data.frame() %>%
      dplyr::select(-"b_Intercept") %>%
      reshape2::melt() %>%
      dplyr::mutate(variable = gsub(".*_",
                             "",
                             variable)) %>%
      dplyr::rename(estimate = value) %>%
      dplyr::rename(level = variable)

    # create feature variable - first have to work out no. of unique levels per feature
    features_df <- dplyr::select(data, dplyr::one_of(predictors))
    lengths <- vector("double", ncol(features_df))
    for (i in seq_along(features_df)) {
      lengths[[i]] <- length(unique(features_df[[i]]))
    }
    # -1 because of reference cats, then times by 2000 (no. of samples per level)
    reps <- (lengths-1)*iter
    # repeat each predictor the corresponding number of times
    baemces$feature <- rep(predictors, times = reps)

    return(baemces)
  }


  else if (save_amce == FALSE) {
    baemces <- brms::brm(
      formula = bae_formula,
      family = gaussian(),
      prior = prior,
      data = data,
      iter = iter,
      chains = chains,
      cores = cores,
      refresh = refresh
    )

    baemces <- posterior_samples(baemces, "^b") %>%
      data.frame() %>%
      dplyr::select(-"b_Intercept") %>%
      reshape2::melt() %>%
      dplyr::mutate(variable = gsub(".*_",
                             "",
                             variable)) %>%
      dplyr::rename(estimate = value) %>%
      dplyr::rename(level = variable)

    # create feature variable - first have to work out no. of unique levels per feature
    features_df <- dplyr::select(data, dplyr::one_of(predictors))
    lengths <- vector("double", ncol(features_df))
    for (i in seq_along(features_df)) {
      lengths[[i]] <- length(unique(features_df[[i]]))
    }
    # -1 because of reference cats, then times by iter (no. of samples per level)
    reps <- (lengths-1)*iter
    # repeat each predictor the corresponding number of times
    baemces$feature <- rep(predictors, times = reps)

    return(baemces)
  }
}
