# Forecast error and accuracy monitoring measures.

calc_forecast_errors <- function(actual, forecast) {
  actual <- as.numeric(actual)
  forecast <- as.numeric(forecast)
  actual - forecast
}

calc_accuracy_measures <- function(actual, forecast) {
  actual <- as.numeric(actual)
  forecast <- as.numeric(forecast)
  errors <- calc_forecast_errors(actual, forecast)
  abs_errors <- abs(errors)
  mad_value <- mean(abs_errors, na.rm = TRUE)

  data.frame(
    bias = mean(errors, na.rm = TRUE),
    mad = mad_value,
    mse = mean(errors^2, na.rm = TRUE),
    mape = mean(abs(errors / actual), na.rm = TRUE) * 100,
    rsfe = sum(errors, na.rm = TRUE),
    tracking_signal = ifelse(mad_value > 0, sum(errors, na.rm = TRUE) / mad_value, NA_real_),
    stringsAsFactors = FALSE
  )
}

make_comparison_table <- function(method_results, next_forecasts) {
  rows <- lapply(names(method_results), function(method_key) {
    result <- method_results[[method_key]]
    method_label <- if (!is.null(result$method)) result$method else method_key

    if (isTRUE(result$applicable) && !is.null(result$actual) && !is.null(result$forecast)) {
      measures <- calc_accuracy_measures(result$actual, result$forecast)
      measures$method_key <- method_key
      measures$method <- method_label
      measures$next_period_forecast <- next_forecasts[[method_key]]
      measures$status <- "Applied"
    } else {
      data.frame(
        method_key = method_key,
        method = method_label,
        bias = NA_real_,
        mad = NA_real_,
        mse = NA_real_,
        mape = NA_real_,
        rsfe = NA_real_,
        tracking_signal = NA_real_,
        next_period_forecast = next_forecasts[[method_key]],
        status = ifelse(isTRUE(result$applicable), "Applied", "Not applicable"),
        stringsAsFactors = FALSE
      )
    }

    measures
  })

  comparison <- do.call(rbind, rows)
  rownames(comparison) <- NULL

  comparison[
    ,
    c(
      "method_key", "method", "bias", "mad", "mse", "mape", "rsfe",
      "tracking_signal", "next_period_forecast", "status"
    )
  ]
}
