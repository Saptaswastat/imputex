#' Impute Missing Data Using Multiple Methods and Visualize
#'
#' This function performs three imputation methods on a given dataset:
#' mean imputation, conditional (kNN) imputation, and MICE (multiple imputation via chained equations).
#' It also provides visual comparisons of the distributions before and after imputation.
#'
#' @param data A `data.frame` with missing values. Numeric variables are required for visualization.
#'
#' @return A list containing:
#' \describe{
#'   \item{mean_imputed_data}{A `data.frame` with missing values imputed using mean imputation.}
#'   \item{conditional_imputed_data}{A `data.frame` with missing values imputed using kNN (conditional) imputation.}
#'   \item{mice_imputed_data}{A `data.frame` with missing values imputed using the MICE method.}
#'   \item{plots}{A list of `ggplot2` objects showing density comparisons for each imputation method.}
#' }
#'
#' @importFrom utils globalVariables
#' @importFrom VIM kNN
#' @importFrom mice mice complete
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_density facet_wrap labs theme_minimal

utils::globalVariables(c("value", "type"))
#' @export
impute_all_methods <- function(data) {
  utils::globalVariables(c("value", "type"))

  if (!requireNamespace("mice", quietly = TRUE) ||
      !requireNamespace("VIM", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("reshape2", quietly = TRUE)) {
    stop("Required packages: mice, VIM, ggplot2, reshape2")
  }

  mean_imp <- data
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      mean_imp[[col]][is.na(mean_imp[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
  }

  cond_imp <- VIM::kNN(data, k = 5, imp_var = FALSE)
  mice_model <- mice::mice(data, m = 1, method = "pmm", seed = 123)
  mice_imp <- mice::complete(mice_model, 1)


  plot_distributions <- function(orig, imp, method_name) {
    df_orig <- reshape2::melt(orig)
    df_imp <- reshape2::melt(imp)
    df_combined <- rbind(
      data.frame(variable = df_orig$variable, value = df_orig$value, type = "Original"),
      data.frame(variable = df_imp$variable, value = df_imp$value, type = method_name)
    )

    ggplot2::ggplot(df_combined, ggplot2::aes(x = value, fill = type)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::facet_wrap(~ variable, scales = "free") +
      ggplot2::labs(title = paste("Distribution Comparison:", method_name), x = "Value", y = "Density") +
      ggplot2::theme_minimal()
  }

  # Diagnostic plots from MICE
  # These are lattice plots, so they need to be printed explicitly or returned
  mice_stripplot <- mice::stripplot(mice_model, pch = 20, cex = 1.2, col = c("blue", "red"),
                                    main = "MICE Stripplot: Observed vs Imputed")
  mice_densityplot <- mice::densityplot(mice_model, main = "MICE Imputation Density Plot")



  p1 <- plot_distributions(data, mean_imp, "Mean Imputation")
  p2 <- plot_distributions(data, cond_imp, "Conditional Imputation")
  p3 <- plot_distributions(data, mice_imp, "MICE Imputation")

  return(list(
    mean_imputed_data = mean_imp,
    conditional_imputed_data = cond_imp,
    mice_imputed_data = mice_imp,
    plots = list(
      mean_plot = p1,
      conditional_plot = p2,
      mice_plot = p3,
      mice_diagnostics = list(
        stripplot = mice_stripplot,
        densityplot = mice_densityplot
      )
    )
  ))
}
