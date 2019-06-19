#' @rdname cjbae_plot
#' @title Simple plotting of cjbae outputs.
#' @description Visualise Bayesian marginal means and AMCEs
#' @param data A tidy dataframe of either AMCEs or MMs.
#' @param visual Currently either "ridge" or "halfeye" - two different takes on a distribution plot. ADDED "point interval" which uses `stat_pointintervalh()` from `tidybayes`.`
#' @param estimate Either "mm" or "amce".
#' @return A plot of parameter distributions.
#' @details \code{cjbae_plot()} plots AMCEs or marginal means of feature-levels as distributions, colour-coded by feature.
#' @export
#' @import ggplot2 ggridges tidybayes
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
#' # generate df of AMCEs
#' amce <- cjbae_df(baerms)
#' # plot AMCEs
#' cjbae_plot(amce, "halfeye", "amce")


cjbae_plot <- function(data,
                       visual = c("ridge", "halfeye", "point interval"),
                       estimate = c("amce", "mm")) {


  # amce ridge plot
  if (visual == "ridge" & estimate == "amce") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        fill = feature,
                        colour = feature)) +
      ggridges::geom_density_ridges(rel_min_height = 0.01, alpha = .6) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlab("Average Marginal Component Effect") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

  # mm ridge plot
  else if (visual == "ridge" & estimate == "mm") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        fill = feature,
                        colour = feature)) +
      ggridges::geom_density_ridges(rel_min_height = 0.01, alpha = .6) +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Marginal Mean") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

  # amce halfeye plot
  else if (visual == "halfeye" & estimate == "amce") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        fill = feature)) +
      tidybayes::geom_halfeyeh() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlab("Average Marginal Component Effect") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

  # mm halfeye plot
  else if (visual == "halfeye" & estimate == "mm") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        fill = feature)) +
      tidybayes::geom_halfeyeh() +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Marginal Mean") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

  # amce interval plot
  else if (visual == "point interval" & estimate == "amce") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        fill = feature,
                        colour = feature)) +
      tidybayes::stat_pointintervalh() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlab("Average Marginal Component Effect") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

  # mm interval plot
  else if (visual == "point interval" & estimate == "mm") {
    ggplot2::ggplot(data,
                    aes(x = estimate,
                        y = level,
                        colour = feature)) +
      tidybayes::stat_pointintervalh() +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Marginal Mean") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }

}
