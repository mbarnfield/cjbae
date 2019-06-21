#' @rdname cjbae_df
#' @title Tidy Bayesian AMCE dataframe
#' @description Generate a tidy dataframe of Bayesian AMCEs from brmsfit
#' @param data A tidy conjoint dataframe - the original dataset you used in the AMCE model.
#' @param formula A standard formula of the form outcome ~ feature 1 + feature 2 ... + feature n - the same as the one plugged into the AMCE model.
#' @param brmsfit The brmsfit object to convert.
#' @param iter The number of iterations in the brms model. Defaults to 2000, the same as the default in amce_bae and cjbae.
#' @return A dataframe of AMCEs. These take the form of samples from the posterior probability distribution and can be plotted as distributions, rather than point estimates.
#' @details \code{cjbae_df()} takes an AMCE brmsfit object and creates a dataframe of AMCE parameter estimates. The other arguments are needed in addition to the brmsfit argument in order to create the feature variable in the output. Also used as a utility function in other functions.
#' @export
#' @import tidyverse magrittr reshape2 stats
#' @importFrom magrittr "%>%"
#' @examples
#' #' #' #load example dataset from {cregg} (Leeper 2019)
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
#' # if the iter argument in cjbae() is altered from the default, the same is required in cjbae_df()
#' bae <- cjbae(data = taxes, formula = f, id = ID, estimate = "mm", prior = prior, save_amce = TRUE, iter = 1e3)
#' cjbae_df(taxes, f, baerms, iter = 1e3)
#'
cjbae_df <- function(data, formula, brmsfit, iter = 2e3) {

  baemces <- posterior_samples(brmsfit, "^b") %>%
    data.frame() %>%
    dplyr::select(-"b_Intercept") %>%
    reshape2::melt() %>%
    dplyr::mutate(variable = gsub(".*_",
                                  "",
                                  variable)) %>%
    dplyr::rename(estimate = value) %>%
    dplyr::rename(level = variable)

  # predictors var to help generate feature var
  predictors <- all.vars(stats::update(formula, 0 ~ .))

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

  # create reference category
  # function to create dataframe of only predictor variables
  predictor_df <- function(x, f) {
    pred <- all.vars(stats::update(formula, 0 ~ .))
    pred_df <- dplyr::select(data, dplyr::one_of(pred))
  }

  pred_df <- predictor_df(data, formula)

  # find base level of every predictor (used as reference category in model)
  ref <- vector("character", ncol(pred_df))
  for (i in seq_along(pred_df)){
    ref[i] <- levels(pred_df[,i])[1]
  }

  # dataframe of reference categories with estimate of 0, bind to original data
  baemces <- data.frame(
    level = ref,
    feature = colnames(pred_df),
    estimate = rep(0, length(ref))
  ) %>%
    dplyr::mutate(level = gsub(".*_",
                               "",
                               level)) %>%
    rbind(baemces) %>%
    dplyr::arrange(feature, level)

  # make sure no character vars
  baemces[sapply(baemces, is.character)] <-
    lapply(baemces[sapply(baemces, is.character)], as.factor)

  # create feature headers for plot
  make_feature_headers <- function(x, fmt = "(%s)") {
    feature_levels <- rev(split(x$level, x$feature))
    for (i in seq_along(feature_levels)) {
      feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
      feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
    }
    factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
  }

  baemces$level <- make_feature_headers(baemces, fmt = "(%s)")
  to_merge <- data.frame(feature = unique(baemces$feature),
                         level = sprintf("(%s)", unique(baemces$feature)))
  baemces <- merge(baemces, to_merge, all = TRUE)

  # sort for sensible plotting
  baemces <- baemces %>% dplyr::arrange(feature, level)

}
