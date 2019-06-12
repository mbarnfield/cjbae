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
}
