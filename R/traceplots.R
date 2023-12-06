#' Create Trace Plots
#'
#' `traceplot()` creates the trace plots for the results from `bayes_lm()`. This
#' function requires the ggplot2 and tidyr packages from the tidyverse.
#'
#' @param bayes_lm_distributions The result from `bayes_lm`
#'
#' @returns A ggplot2 plot of trace plots
#' @export
#'
#' @examples
#' #' # Simulate data
#' X <- matrix(rnorm(1000, 5, 2), nrow = 100)
#' betas <- runif(10, -2, 2)
#' Y <- X %*% betas + rnorm(100, 0, 3)
#'
#' # Perform Bayesian Linear Regression
#' result <- bayes_lm(Y, X, iterations = 10)
#'
#' # Plot data
#' traceplot(result)
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
