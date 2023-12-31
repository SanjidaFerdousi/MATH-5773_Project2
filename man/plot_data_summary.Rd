% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_summary.R
\name{plot_data_summary}
\alias{plot_data_summary}
\title{Plot and Summarize Data for Multiple Linear Regression Analysis}
\usage{
plot_data_summary(df, dep_var, ind_vars)
}
\arguments{
\item{df}{A data frame containing the dependent and independent variables.}

\item{dep_var}{A string specifying the name of the dependent variable in the data frame.}

\item{ind_vars}{A vector of categorical independent variables in the data frame.}
}
\value{
A list containing:
  \item{Scatterplot}{A ggplot2 scatter plot visualizing the relationship between continuous independent variables and the dependent variable with a linear fit.}
  \item{Boxplot}{A ggplot2 boxplot showing the distribution of the dependent variable across different categories of a categorical independent variable.}
  \item{ANOVA}{Results of the ANOVA comparison between the reduced model (without interactions) and the full model (with interactions).}
  \item{Full model summary}{Summary statistics for the full regression model that includes interaction terms.}
  \item{Reduced model summary}{Summary statistics for the reduced regression model without interactions.}
  \item{ANOVA Message}{A message indicating the significance of the interaction terms based on the p-value from the ANOVA comparison. The message provides insights into whether the interaction terms offer a statistically significant improvement in model fit.}
}
\description{
Plot and Summarize Data for Multiple Linear Regression Analysis
}
\examples{
df <- data.frame(
Y = rnorm(50, mean = 50, sd = 10),
X1 = rnorm(50, mean = 5, sd = 2),
X2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE))
)
plot_data_summary(df, "Y", c("X1", "X2"))
}
