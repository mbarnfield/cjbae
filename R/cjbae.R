#' @rdname cjbae
#' @title Bayesian inference for conjoint experiments
#' @description Calculate Bayesian AMCEs and marginal means for conjoint experiments
#' @param data A tidy conjoint dataset
#' @param formula A standard formula of the form outcome ~ feature 1 + feature 2 ... + feature n
#' @param id Respondent ID variable
#' @param estimate Either AMCE or MM. AMCE is always calculated within the function so if you want both write "mm".
#' @param prior Priors for brms. Note these are always priors for the AMCE calculation, regardless of which estimate is specified here. Priors required for beta, intercept, sd and sigma. For clarification see examples below.
#' @param save_amce Logical vector telling R whether or not to save the AMCE brmsfit as an object in your working directory. If you want both AMCE and MM estimates, set this to TRUE and set `estimate = "mm"`, then load the brmsfit object into your environment and pass it through `cjbae_df()`. Defaults to TRUE.
#' @param save_name The name you would like the AMCE brmsfit saved as. Defaults to 'baerms'.
#' @param iter The number of iterations in the brms model. Essentially, the number of times it will sample from the posterior probability distribution. Defaults to 2000.
#' @param chains The number of chains in the brms model. Defaults to 2.
#' @param cores The number of cores used in the brms model. Defaults to 2.
#' @param refresh The number of refreshes. Defaults to 10.
#' @return A dataframe of either AMCE estimates or marginal means. These take the form of samples from the posterior probability distribution and can be plotted as distributions, rather than point estimates.
#' @details \code{cjbae()} is a generic Bayesian estimation function for the two key quantities of interest in conjoint experiments (AMCEs and marginal means), and is essentially a wrapper for [`brms`](https://github.com/paul-buerkner/brms) and [`emmeans`](https://github.com/rvlenth/emmeans), and borrows extensively from [`cregg`](https://github.com/leeper/cregg), R's foremost conjoint analysis package.
#' @export
#' @import tidyverse brms emmeans magrittr tidybayes stats
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
#' # run generic function with MM and save specified, gives MMs as output and saves brmsfit to
#' # working directory - this will take a while
#' bae <- cjbae(data = taxes, formula = f, id = ID, estimate = "mm", prior = prior, save_amce = TRUE)
#' # AMCE brmsfit object can be loaded from working directory using readRDS()
#' # then converted into proper cjbae output
#' cjbae_df(taxes, f, baerms)



cjbae <-
  function(data,
           formula,
           id,
           estimate = c("amce", "mm"),
           prior,
           save_amce = TRUE,
           save_name = "baerms",
           iter = 2e3,
           chains = 2,
           cores = 2,
           refresh = 10) {


    estimate <- match.arg(estimate)

    # check estimate is amce or mm
    if (!estimate %in% c("amce", "mm")) {
      stop("Estimate must be either amce or mm")}

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

    if (estimate == "amce" & save_amce == TRUE) {
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
        select(-"b_Intercept") %>%
        reshape2::melt() %>%
        mutate(variable = gsub(".*_",
                               "",
                               variable)) %>%
        rename(estimate = value) %>%
        rename(level = variable)

      # create feature variable - first have to work out no. of unique levels per feature
      features_df <- dplyr::select(data, one_of(predictors))
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


    else if (estimate == "amce" & save_amce == FALSE) {
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
        select(-"b_Intercept") %>%
        reshape2::melt() %>%
        mutate(variable = gsub(".*_",
                               "",
                               variable)) %>%
        rename(estimate = value) %>%
        rename(level = variable)

      # create feature variable - first have to work out no. of unique levels per feature
      features_df <- dplyr::select(data, one_of(predictors))
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




    ## marginal mean ---------

    else if (estimate == "mm" & save_amce == TRUE) {
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

      # create empty output to fill
      marginal_mean <- vector("list", 1)

      # loop over each predictor and calculate its MM, store into output separately
      # much quicker than applying emmeans to whole object and doesn't crash this way
      for (i in seq_along(predictors)) {
        marginal_mean[[i]] <- emmeans::emmeans(baemces, predictors[i]) %>%
          tidybayes::gather_emmeans_draws() %>% # tidy tibble for plotting
          dplyr::mutate(
            feature = predictors[i] # generate feature variable for grouping
          )
      }

      # merge each marginal mean tibble into one big df
      marginal_mean <- lapply(marginal_mean, function(x) {colnames(x)[1] <- 'level'; x})
      marginal_mean <- lapply(marginal_mean, function(x) {colnames(x)[5] <- 'estimate'; x})
      marginal_mean <- lapply(marginal_mean, data.frame)
      marginal_mean <- do.call(rbind, marginal_mean)

      # make feature headers to appear on plot axis
      make_feature_headers <- function(x, fmt = "(%s)") {
        feature_levels <- rev(split(x$level, x$feature))
        for (i in seq_along(feature_levels)) {
          feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
          feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
        }
        factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
      }

      marginal_mean$level <- make_feature_headers(marginal_mean, fmt = "(%s)")
      to_merge <- data.frame(feature = unique(marginal_mean$feature),
                             level = sprintf("(%s)", unique(marginal_mean$feature)))
      marginal_mean <- merge(marginal_mean, to_merge, all = TRUE)
    }

    else if (estimate == "mm" & save_amce == FALSE) {
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

      # create empty output to fill
      marginal_mean <- vector("list", 1)

      # loop over each predictor and calculate its MM, store into output separately
      # much quicker than applying emmeans to whole object and doesn't crash this way
      for (i in seq_along(predictors)) {
        marginal_mean[[i]] <- emmeans::emmeans(baemces, predictors[i]) %>%
          tidybayes::gather_emmeans_draws() %>% # tidy tibble for plotting
          dplyr::mutate(
            feature = predictors[i] # generate feature variable for grouping
          )
      }

      # merge each marginal mean tibble into one big df
      marginal_mean <- lapply(marginal_mean, function(x) {colnames(x)[1] <- 'level'; x})
      marginal_mean <- lapply(marginal_mean, function(x) {colnames(x)[5] <- 'estimate'; x})
      marginal_mean <- lapply(marginal_mean, data.frame)
      marginal_mean <- do.call(rbind, marginal_mean)

      # make feature headers to appear on plot axis
      make_feature_headers <- function(x, fmt = "(%s)") {
        feature_levels <- rev(split(x$level, x$feature))
        for (i in seq_along(feature_levels)) {
          feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
          feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
        }
        factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
      }

      marginal_mean$level <- make_feature_headers(marginal_mean, fmt = "(%s)")
      to_merge <- data.frame(feature = unique(marginal_mean$feature),
                             level = sprintf("(%s)", unique(marginal_mean$feature)))
      marginal_mean <- merge(marginal_mean, to_merge, all = TRUE)
    }

  }
