traceplot <- function(bayes_lm_distributions) {
  rlang::check_installed("ggplot2")
  rlang::check_installed("tidyr")
  bld <- as.data.frame(bayes_lm_distributions)
  bld <- cbind(bld, "Run" = seq_len(nrow(bayes_lm_distributions)))
  bld <- tidyr::pivot_longer(
    bld, -"Run", names_to = "Variable", values_to = "Value"
    )
  ggplot2::ggplot(bld, ggplot2::aes(.data$Run, .data$Value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~.data$Variable, scales = "free_y") +
    ggplot2::ggtitle("Traceplots for Variables from bayes_lm")
}
