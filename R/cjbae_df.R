cjbae_df <- function(brmsfit) {

  baemces <- posterior_samples(brmsfit, "^b") %>%
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
