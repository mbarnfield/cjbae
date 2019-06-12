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
    marginal_mean[[i]] <- emmeans::emmeans(baemce, features[i]) %>%
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
