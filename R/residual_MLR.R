#' Analyze Residuals from Multiple Linear Regression
#'
#' This function takes a data-set and constructs both a reduced linear model
#' (without interactions) and a full model (with all two-way interactions).
#' Residual plots and model summaries for both models are generated.
#'
#' @param df A data frame.
#' @param dep_var A string of the dependent variable name.
#' @param ind_vars A vector of strings of the independent variable names.
#'
#' @return A list containing:
#'   \item{reduced}{The reduced model summary}
#'   \item{full}{The full model summary}
#'   \item{reduced_AIC}{AIC for the reduced model}
#'   \item{full_AIC}{AIC for the full model}
#'   \item{reduced_plots}{A list of plots for the reduced model}
#'   \item{full_plots}{A list of plots for the full model}
#'
#' @export
#'
#' @import graphics
#' @import stats
#' @import grDevices
#'
#' @examples
#' data <- data.frame(Y = rnorm(50, mean = 50, sd = 10),
#' X1 = rnorm(50, mean = 5, sd = 2),
#' X2 = sample(c("A", "B", "C"), 50, replace = TRUE))
#' analyze_residuals(data, "Y", c("X1", "X2"))

analyze_residuals <- function(df, dep_var, ind_vars) {
  df <- df[, c(dep_var, ind_vars)]

  # Reduced model (without interactions)
  formula_reduced <- as.formula(paste(dep_var, "~ ."))
  model_reduced <- lm(formula_reduced, data = df)
  model_reduced_summary = summary(model_reduced)
  reduced_AIC = AIC(model_reduced)

  # Full model with two-way interactions
  formula_full <- as.formula(paste(dep_var, "~ .^2"))
  model_full <- lm(formula_full, data = df)
  model_full_summary= summary(model_full)
  full_AIC = AIC(model_full)

  # Residual plots for reduced model
  plot(model_reduced$fitted.values, model_reduced$residuals, main="Residuals vs Fitted for reduced model",
       xlab="Fitted values", ylab="Residuals", pch=19)
  abline(h=0, col="red")
  qqnorm(model_reduced$residuals, main="Normal Q-Q")
  qqline(model_reduced$residuals, col="red")
  hist(model_reduced$residuals, main="Histogram of Residuals for reduced model", xlab="Residuals", col="lightblue", border="black")

  reduced_plots <- list(
    fitted_vs_residuals = recordPlot(),
    qq = recordPlot(),
    histogram = recordPlot()
  )

  # Residual plots for full model
  plot(model_full$fitted.values, model_full$residuals, main="Residuals vs Fitted for full model",
       xlab="Fitted values", ylab="Residuals", pch=19)
  abline(h=0, col="orange")
  qqnorm(model_full$residuals, main="Normal Q-Q")
  qqline(model_full$residuals, col="orange")
  hist(model_full$residuals, main="Histogram of Residuals for full model", xlab="Residuals", col="navyblue", border="black")

  full_plots <- list(
    fitted_vs_residuals = recordPlot(),
    qq = recordPlot(),
    histogram = recordPlot()
  )

  list(reduced = model_reduced_summary, full = model_full_summary, reduced_AIC = reduced_AIC, full_AIC = full_AIC, reduced_plots = reduced_plots, full_plots = full_plots)
}
