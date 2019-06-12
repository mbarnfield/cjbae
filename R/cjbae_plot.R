



cjbae_plot <- function(x,
                       visual = c("ridge", "halfeye"),
                       estimate = c("amce", "mm")) {
  
  # amce ridge plot
  if (visual == "ridge" & estimate == "amce") {
    ggplot2::ggplot(x,
                    aes(x = estimate,
                        y = level,
                        fill = feature,
                        colour = feature)) +
      geom_density_ridges(rel_min_height = 0.01, alpha = .6) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlab("Average Marginal Component Effect") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }
  
  # mm ridge plot
  else if (visual == "ridge" & estimate == "mm") {
    ggplot2::ggplot(x,
                    aes(x = estimate,
                        y = level,
                        fill = feature,
                        colour = feature)) +
      geom_density_ridges(rel_min_height = 0.01, alpha = .6) +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Marginal Mean") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }
  
  # amce halfeye plot
  else if (visual == "halfeye" & estimate == "amce") {
    ggplot2::ggplot(x,
                    aes(x = estimate,
                        y = level,
                        fill = feature)) +
      geom_halfeyeh() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlab("Average Marginal Component Effect") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }
  
  # mm ridge plot
  else if (visual == "halfeye" & estimate == "mm") {
    ggplot2::ggplot(x,
                    aes(x = estimate,
                        y = level,
                        fill = feature)) +
      geom_halfeyeh() +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Marginal Mean") +
      ylab("Feature level") +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            axis.title.y = element_blank())
  }
}
