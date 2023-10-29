#' Plot and Summarize Data for Multiple Linear Regression Analysis
#'
#' @param df A data frame containing the dependent and independent variables.
#' @param dep_var A string specifying the name of the dependent variable in the data frame.
#' @param ind_vars A vector of categorical independent variables in the data frame.
#'
#' @return A list containing:
#'   \item{Scatterplot}{A ggplot2 scatter plot visualizing the relationship between continuous independent variables and the dependent variable with a linear fit.}
#'   \item{Boxplot}{A ggplot2 boxplot showing the distribution of the dependent variable across different categories of a categorical independent variable.}
#'   \item{ANOVA}{Results of the ANOVA comparison between the reduced model (without interactions) and the full model (with interactions).}
#'   \item{Full model summary}{Summary statistics for the full regression model that includes interaction terms.}
#'   \item{Reduced model summary}{Summary statistics for the reduced regression model without interactions.}
#'   \item{ANOVA Message}{A message indicating the significance of the interaction terms based on the p-value from the ANOVA comparison. The message provides insights into whether the interaction terms offer a statistically significant improvement in model fit.}
#'
#'
#' @export
#' @import graphics
#' @import stats
#' @import grDevices
#'
#' @examples
#' df <- data.frame(
#' Y = rnorm(50, mean = 50, sd = 10),
#' X1 = rnorm(50, mean = 5, sd = 2),
#' X2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE))
#' )
#' plot_data_summary(df, "Y", c("X1", "X2"))
plot_data_summary <- function(df, dep_var, ind_vars) {
  df1 <- df[, c(dep_var, ind_vars)]
  # Reduced model (without interactions)
  formula_reduced <- as.formula(paste(dep_var, "~ ."))
  model_reduced <- lm(formula_reduced, data = df1)
  model_reduced_summary = summary(model_reduced)


  # Full model with interactions
  formula_full <- as.formula(paste(dep_var, "~ .^2"))
  model_full <- lm(formula_full, data = df1)
  model_full_summary= summary(model_full)

  anova_results = anova(model_reduced, model_full)


  p_value <- anova_results$`Pr(>F)`[2]
  if (p_value > 0.05) {
    p_value_message <- "The p-value from the ANOVA comparison between the reduced and full models is greater than 0.05, suggesting that adding interaction terms might not provide a statistically significant improvement in the model fit."
  } else {
    p_value_message <- "The p-value from the ANOVA comparison between the reduced and full models is less than or equal to 0.05, suggesting that the interaction terms provide a statistically significant improvement in the model fit."
  }

  for(i in seq_along(ind_vars)) {
    var <- ind_vars[i]

    if(is.numeric(df[[var]])) {
      # Scatter plot for continuous variables
      p <- ggplot2::ggplot(df, ggplot2::aes_string(x=var, y=dep_var)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
        ggplot2::labs(title = paste("Scatter plot of", dep_var, "vs", var))

    } else {
      # Boxplot for categorical variables
      p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x=var, y=dep_var)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = paste("Boxplot of", dep_var, "by", var))

    }
  }
  results <- list(
    "Scatterplot" = p,
    "Boxplot" = p1,
    "ANOVA" = anova_results,
    "Full model summary" = model_full_summary,
    "Reduced model summary" = model_reduced_summary,
    "ANOVA Message" = p_value_message

  )

  return(results)
}
