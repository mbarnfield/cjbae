#' @rdname mm_bae
#' @title Bayesian marginal means for conjoint analysis
#' @description Calculate Bayesian marginal means for conjoint experiments
#' @param baemces An AMCE object of class `brmsfit`.
#' @param formula A standard formula of the form outcome ~ feature 1 + feature 2 ... + feature n
#' @param id Respondent ID variable
#' @return A dataframe of marginal means. These take the form of samples from the posterior probability distribution and can be plotted as distributions, rather than point estimates.
#' @details \code{mm_bae()} is a Bayesian estimation function for a key quantity of interest in conjoint analysis (marginal means), and is essentially a wrapper for [`emmeans`](https://github.com/rvlenth/emmeans), and borrows extensively from [`cregg`](https://github.com/leeper/cregg), R's foremost conjoint analysis package. The calculation in this function is computationally expensive. Attempting to calculate MMs naively for an entire AMCE brmsfit object will often crash R, so this function uses a loop to calculate each feature one-by-one. The results of this process are equivalent. Obviously this takes time, but exactly how long is highly contingent on the number of predictors in the model.
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
#' # run amce function with save specified, saves brmsfit to working directory - this will take a while
#' amce_bae(data = taxes, formula = f, id = ID, prior = prior, save_amce = TRUE)
#' # run mm function on the saved output
#' readRDS(baerms)
#' mm <- mm_bae(baerms, f, ID)
#' # plot MMs
#' cjbae_plot(mm, "ridge", "mm")

mm_bae <- function(baemces,
                   formula,
                   id) {

  # get vector of features/predictors
  features <- all.vars(stats::update(formula, 0 ~ .))

  # create empty output to fill
  marginal_mean <- vector("list", 1)

  # loop over each predictor and calculate its MM, store into output separately
  # much quicker than applying emmeans to whole object and doesn't crash this way
  for (i in seq_along(features)) {
    marginal_mean[[i]] <- emmeans::emmeans(baemces, features[i]) %>%
      tidybayes::gather_emmeans_draws() %>% # tidy tibble for plotting
      dplyr::mutate(
        feature = features[i] # generate feature variable for grouping
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
