# Plot generation for the forecasting project.

ensure_figure_dir <- function(path = "outputs/figures") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  path
}

plot_actual_series <- function(series_tbl, save_path = "outputs/figures/actual_series_plot.png") {
  ensure_figure_dir(dirname(save_path))

  png(save_path, width = 1200, height = 700, res = 120)
  on.exit(dev.off(), add = TRUE)

  plot(
    series_tbl$period_date,
    series_tbl$value,
    type = "l",
    lwd = 2,
    col = "#1f4e79",
    xlab = "Period",
    ylab = "Non-Domestic Producer Price Index (2010=100)",
    main = "Actual Monthly Non-Domestic Producer Price Index"
  )
  grid()
}

plot_actual_vs_forecast <- function(series_tbl,
                                   method_result,
                                   save_path,
                                   title) {
  ensure_figure_dir(dirname(save_path))

  if (!isTRUE(method_result$applicable)) {
    png(save_path, width = 1200, height = 700, res = 120)
    on.exit(dev.off(), add = TRUE)
    plot.new()
    text(0.5, 0.5, paste(title, "\nMethod not applicable"), cex = 1.2)
    return(invisible(NULL))
  }

  dates <- series_tbl$period_date
  actual <- method_result$actual
  forecast <- method_result$forecast

  if (length(actual) == length(dates)) {
    plot_dates <- dates
  } else {
    plot_dates <- tail(dates, length(actual))
  }

  png(save_path, width = 1200, height = 700, res = 120)
  on.exit(dev.off(), add = TRUE)

  y_range <- range(c(actual, forecast), na.rm = TRUE)
  plot(
    plot_dates,
    actual,
    type = "l",
    lwd = 2,
    col = "#1f4e79",
    ylim = y_range,
    xlab = "Period",
    ylab = "Index Value",
    main = title
  )
  lines(plot_dates, forecast, col = "#c0504d", lwd = 2, lty = 2)
  legend(
    "topleft",
    legend = c("Actual", "Forecast / Fitted"),
    col = c("#1f4e79", "#c0504d"),
    lty = c(1, 2),
    lwd = 2,
    bty = "n"
  )
  grid()
}

plot_superior_method <- function(series_tbl,
                                 method_result,
                                 next_forecast,
                                 target_period,
                                 save_path = "outputs/figures/superior_method_plot.png") {
  ensure_figure_dir(dirname(save_path))

  png(save_path, width = 1200, height = 700, res = 120)
  on.exit(dev.off(), add = TRUE)

  hist_dates <- series_tbl$period_date
  hist_values <- series_tbl$value
  target_date <- seq(max(hist_dates), by = "month", length.out = 2)[2]

  plot(
    hist_dates,
    hist_values,
    type = "l",
    lwd = 2,
    col = "#1f4e79",
    xlim = c(min(hist_dates), target_date),
    xlab = "Period",
    ylab = "Index Value",
    main = paste("Superior Method Forecast for", target_period)
  )

  if (isTRUE(method_result$applicable) && !is.null(method_result$fitted)) {
    lines(hist_dates, method_result$fitted, col = "#8064a2", lty = 2, lwd = 2)
  }

  points(target_date, next_forecast, pch = 19, col = "#c0504d", cex = 1.4)
  text(target_date, next_forecast, labels = round(next_forecast, 2), pos = 3)

  legend(
    "topleft",
    legend = c("Actual", "Model fit", "Next-period forecast"),
    col = c("#1f4e79", "#8064a2", "#c0504d"),
    lty = c(1, 2, NA),
    pch = c(NA, NA, 19),
    lwd = 2,
    bty = "n"
  )
  grid()
}

save_all_method_plots <- function(series_tbl, method_results) {
  plot_actual_series(series_tbl)

  mapping <- list(
    naive = "outputs/figures/naive_forecast_plot.png",
    moving_average = "outputs/figures/moving_average_plot.png",
    weighted_moving_average = "outputs/figures/weighted_moving_average_plot.png",
    exponential_smoothing = "outputs/figures/exponential_smoothing_plot.png",
    trend_adjusted_smoothing = "outputs/figures/trend_adjusted_smoothing_plot.png",
    linear_trend = "outputs/figures/trend_projection_plot.png",
    seasonal_indices = "outputs/figures/seasonal_indices_plot.png",
    additive_decomposition = "outputs/figures/additive_decomposition_plot.png",
    multiplicative_decomposition = "outputs/figures/multiplicative_decomposition_plot.png",
    regression_seasonal = "outputs/figures/regression_seasonal_dummy_plot.png"
  )

  for (name in names(mapping)) {
    plot_actual_vs_forecast(
      series_tbl = series_tbl,
      method_result = method_results[[name]],
      save_path = mapping[[name]],
      title = method_results[[name]]$method
    )
  }
}
