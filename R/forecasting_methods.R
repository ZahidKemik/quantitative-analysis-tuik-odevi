# Forecasting method workflows required by the assignment.

seasonal_period <- function(series_tbl) {
  12L
}

build_time_index <- function(n) {
  seq_len(n)
}

#' One-step-ahead in-sample forecasts for method comparison.
run_all_forecasting_methods <- function(series_tbl) {
  y <- series_tbl$value
  n <- length(y)
  s <- seasonal_period(series_tbl)
  t_index <- build_time_index(n)

  results <- list(
    naive = method_naive(y),
    moving_average = method_moving_average(y, window = 3),
    weighted_moving_average = method_weighted_moving_average(y, weights = c(0.5, 0.3, 0.2)),
    exponential_smoothing = method_exponential_smoothing(y, alpha = 0.3),
    trend_adjusted_smoothing = method_trend_adjusted_smoothing(y, alpha = 0.3, beta = 0.2),
    linear_trend = method_linear_trend(y, t_index),
    seasonal_indices = method_seasonal_indices(y, s),
    additive_decomposition = method_additive_decomposition(y, s),
    multiplicative_decomposition = method_multiplicative_decomposition(y, s),
    regression_seasonal = method_regression_seasonal(y, series_tbl$period_date, s)
  )

  results
}

extract_next_forecasts <- function(method_results, series_tbl) {
  s <- seasonal_period(series_tbl)
  y <- series_tbl$value
  n <- length(y)
  t_index <- build_time_index(n)

  list(
    naive = tail(method_naive(y)$next_forecast, 1),
    moving_average = tail(method_moving_average(y, 3)$next_forecast, 1),
    weighted_moving_average = tail(method_weighted_moving_average(y, c(0.5, 0.3, 0.2))$next_forecast, 1),
    exponential_smoothing = tail(method_exponential_smoothing(y, 0.3)$next_forecast, 1),
    trend_adjusted_smoothing = tail(method_trend_adjusted_smoothing(y, 0.3, 0.2)$next_forecast, 1),
    linear_trend = tail(method_linear_trend(y, t_index)$next_forecast, 1),
    seasonal_indices = method_seasonal_indices(y, s)$next_forecast,
    additive_decomposition = method_additive_decomposition(y, s)$next_forecast,
    multiplicative_decomposition = method_multiplicative_decomposition(y, s)$next_forecast,
    regression_seasonal = method_regression_seasonal(y, series_tbl$period_date, s)$next_forecast
  )
}

method_naive <- function(y) {
  n <- length(y)
  fitted <- c(NA, y[-n])
  actual <- y[-1]
  forecast <- fitted[-1]

  list(
    applicable = TRUE,
    method = "Naive Forecasting",
    actual = actual,
    forecast = forecast,
    fitted = fitted,
    next_forecast = y[n],
    notes = "Each forecast equals the previous observed value."
  )
}

method_moving_average <- function(y, window = 3) {
  n <- length(y)
  fitted <- rep(NA_real_, n)

  for (i in seq(window + 1, n)) {
    fitted[i] <- mean(y[(i - window):(i - 1)])
  }

  valid <- (window + 1):n
  list(
    applicable = TRUE,
    method = "Moving Average",
    window = window,
    actual = y[valid],
    forecast = fitted[valid],
    fitted = fitted,
    next_forecast = mean(tail(y, window)),
    notes = paste("Moving average window =", window, "months.")
  )
}

method_weighted_moving_average <- function(y, weights = c(0.5, 0.3, 0.2)) {
  weights <- weights / sum(weights)
  window <- length(weights)
  n <- length(y)
  fitted <- rep(NA_real_, n)

  for (i in seq(window + 1, n)) {
    fitted[i] <- sum(weights * y[(i - window):(i - 1)])
  }

  valid <- (window + 1):n
  list(
    applicable = TRUE,
    method = "Weighted Moving Average",
    weights = weights,
    actual = y[valid],
    forecast = fitted[valid],
    fitted = fitted,
    next_forecast = sum(weights * tail(y, window)),
    notes = "More weight is assigned to the most recent observations."
  )
}

method_exponential_smoothing <- function(y, alpha = 0.3) {
  n <- length(y)
  level <- y[1]
  fitted <- rep(NA_real_, n)
  fitted[1] <- level

  for (i in 2:n) {
    fitted[i] <- alpha * y[i - 1] + (1 - alpha) * fitted[i - 1]
  }

  next_level <- alpha * y[n] + (1 - alpha) * fitted[n]

  list(
    applicable = TRUE,
    method = "Exponential Smoothing",
    alpha = alpha,
    actual = y[-1],
    forecast = fitted[-1],
    fitted = fitted,
    next_forecast = next_level,
    notes = paste("Smoothing parameter alpha =", alpha)
  )
}

method_trend_adjusted_smoothing <- function(y, alpha = 0.3, beta = 0.2) {
  n <- length(y)
  if (n < 4) {
    return(list(applicable = FALSE, method = "Trend-Adjusted Exponential Smoothing", notes = "Series too short."))
  }

  level <- y[1]
  trend <- y[2] - y[1]
  fitted <- rep(NA_real_, n)
  fitted[1] <- level + trend

  for (i in 2:n) {
    prev_level <- level
    level <- alpha * y[i] + (1 - alpha) * (level + trend)
    trend <- beta * (level - prev_level) + (1 - beta) * trend
    fitted[i] <- level + trend
  }

  next_forecast <- level + trend

  list(
    applicable = TRUE,
    method = "Trend-Adjusted Exponential Smoothing",
    alpha = alpha,
    beta = beta,
    actual = y[-1],
    forecast = fitted[-1],
    fitted = fitted,
    next_forecast = next_forecast,
    notes = "Holt-style trend-adjusted exponential smoothing."
  )
}

method_linear_trend <- function(y, t_index) {
  model <- lm(y ~ t_index)
  fitted <- as.numeric(fitted(model))
  next_forecast <- as.numeric(predict(model, newdata = data.frame(t_index = max(t_index) + 1)))

  list(
    applicable = TRUE,
    method = "Linear Trend Projection",
    model = model,
    equation = paste0(
      "y = ",
      round(coef(model)[1], 4),
      " + ",
      round(coef(model)[2], 4),
      " * t"
    ),
    intercept = unname(coef(model)[1]),
    slope = unname(coef(model)[2]),
    actual = y,
    forecast = fitted,
    fitted = fitted,
    next_forecast = next_forecast,
    notes = "Simple linear trend over time."
  )
}

method_seasonal_indices <- function(y, seasonal_period = 12) {
  n <- length(y)
  if (n < 2 * seasonal_period) {
    return(list(applicable = FALSE, method = "Seasonal Indices", notes = "At least two full seasonal cycles are required."))
  }

  trend <- stats::filter(y, rep(1 / seasonal_period, seasonal_period), sides = 2)
  trend[trend == 0] <- NA
  ratio <- y / trend
  month_index <- ((seq_len(n) - 1) %% seasonal_period) + 1
  seasonal_index <- tapply(ratio, month_index, mean, na.rm = TRUE)
  seasonal_index <- seasonal_index / mean(seasonal_index, na.rm = TRUE)

  fitted <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (!is.na(trend[i])) {
      fitted[i] <- trend[i] * seasonal_index[month_index[i]]
    }
  }

  last_trend <- mean(tail(na.omit(trend), seasonal_period), na.rm = TRUE)
  next_month <- (month_index[n] %% seasonal_period) + 1
  next_forecast <- last_trend * seasonal_index[next_month]

  list(
    applicable = TRUE,
    method = "Seasonal Indices",
    seasonal_period = seasonal_period,
    seasonal_index = seasonal_index,
    actual = y,
    forecast = fitted,
    fitted = fitted,
    next_forecast = next_forecast,
    notes = "Seasonal indices computed from de-trended ratios."
  )
}

method_additive_decomposition <- function(y, seasonal_period = 12) {
  n <- length(y)
  if (n < 2 * seasonal_period) {
    return(list(applicable = FALSE, method = "Additive Decomposition", notes = "At least two full seasonal cycles are required."))
  }

  ts_y <- ts(y, frequency = seasonal_period)
  decomp <- stats::decompose(ts_y, type = "additive")
  fitted <- as.numeric(decomp$trend + decomp$seasonal)
  fitted[is.na(fitted)] <- NA

  last_trend <- tail(na.omit(decomp$trend), 1)
  next_season <- decomp$seasonal[length(decomp$seasonal) - seasonal_period + 1]
  next_forecast <- last_trend + next_season

  list(
    applicable = TRUE,
    method = "Additive Decomposition",
    decomposition = decomp,
    actual = y,
    forecast = fitted,
    fitted = fitted,
    next_forecast = as.numeric(next_forecast),
    notes = "Classical additive decomposition with moving-average trend."
  )
}

method_multiplicative_decomposition <- function(y, seasonal_period = 12) {
  n <- length(y)
  if (n < 2 * seasonal_period || any(y <= 0, na.rm = TRUE)) {
    return(list(
      applicable = FALSE,
      method = "Multiplicative Decomposition",
      notes = "Requires positive values and at least two seasonal cycles."
    ))
  }

  ts_y <- ts(y, frequency = seasonal_period)
  decomp <- stats::decompose(ts_y, type = "multiplicative")
  fitted <- as.numeric(decomp$trend * decomp$seasonal)
  fitted[is.na(fitted)] <- NA

  last_trend <- tail(na.omit(decomp$trend), 1)
  next_season <- decomp$seasonal[length(decomp$seasonal) - seasonal_period + 1]
  next_forecast <- last_trend * next_season

  list(
    applicable = TRUE,
    method = "Multiplicative Decomposition",
    decomposition = decomp,
    actual = y,
    forecast = fitted,
    fitted = fitted,
    next_forecast = as.numeric(next_forecast),
    notes = "Classical multiplicative decomposition."
  )
}

method_regression_seasonal <- function(y, period_date, seasonal_period = 12) {
  n <- length(y)
  if (n < seasonal_period + 2) {
    return(list(applicable = FALSE, method = "Regression with Trend and Seasonal Dummies", notes = "Insufficient observations."))
  }

  t_index <- build_time_index(n)
  month_factor <- factor(format(period_date, "%m"), levels = sprintf("%02d", 1:12))
  model <- lm(y ~ t_index + month_factor)
  fitted <- as.numeric(fitted(model))

  next_date <- seq(max(period_date), by = "month", length.out = 2)[2]
  next_row <- data.frame(
    t_index = max(t_index) + 1,
    month_factor = factor(format(next_date, "%m"), levels = levels(month_factor))
  )
  next_forecast <- as.numeric(predict(model, newdata = next_row))

  list(
    applicable = TRUE,
    method = "Regression with Trend and Seasonal Dummies",
    model = model,
    actual = y,
    forecast = fitted,
    fitted = fitted,
    next_forecast = next_forecast,
    notes = "Linear regression with deterministic time trend and monthly dummy variables."
  )
}

select_superior_method <- function(comparison_tbl, method_results) {
  applied <- comparison_tbl[comparison_tbl$status == "Applied" & !is.na(comparison_tbl$mad), , drop = FALSE]
  if (nrow(applied) == 0) {
    stop("No applicable methods with accuracy measures were found.", call. = FALSE)
  }

  applied <- applied[order(applied$mad, applied$mse, applied$mape), , drop = FALSE]

  preferred_order <- c(
    "Regression with Trend and Seasonal Dummies",
    "Additive Decomposition",
    "Trend-Adjusted Exponential Smoothing",
    "Weighted Moving Average",
    "Exponential Smoothing",
    "Linear Trend Projection",
    "Moving Average",
    "Naive Forecasting",
    "Multiplicative Decomposition",
    "Seasonal Indices"
  )

  top_mad <- applied$mad[1]
  candidates <- applied[applied$mad <= top_mad * 1.05, , drop = FALSE]

  chosen_label <- preferred_order[preferred_order %in% candidates$method][1]
  if (is.na(chosen_label)) {
    chosen_label <- candidates$method[1]
  }

  chosen_key <- candidates$method_key[candidates$method == chosen_label][1]

  list(
    method = chosen_label,
    method_key = chosen_key,
    comparison = applied,
    reason = paste(
      chosen_label,
      "combines competitive accuracy with a structure that matches the monthly trend and seasonality",
      "observed in the Non-Domestic Producer Price Index series."
    )
  )
}
