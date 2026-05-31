# Data access and preparation for TÜİK series via tuikr.

PROJECT_META <- list(
  dataset_name = "Non-Domestic Producer Price Index and Rate of Change [2010=100]",
  theme = "Price Statistics",
  theme_id = "6",
  table_name = "Non-Domestic Producer Price Index and Rate of Change [2010=100]",
  dataflow_id = "TR,DF_YDUFE_EDO_V1,1.0",
  selected_variable = "Non-Domestic Producer Price Index (2010=100)",
  frequency = "Monthly",
  istab_table_name = "Non-Domestic Producer Price Index  (2010=100)"
)

#' Try SDMX first; if nsiws returns 401, use the portal spreadsheet path documented by tuikr.
fetch_tuik_raw <- function(dataflow_id = PROJECT_META$dataflow_id, lang = "en") {
  if (!requireNamespace("tuikr", quietly = TRUE)) {
    stop(
      "Package 'tuikr' is required. Install with ",
      "remotes::install_github('emraher/tuikr').",
      call. = FALSE
    )
  }

  sdmx_data <- tryCatch(
    tuikr::statistical_data(dataflow_id = dataflow_id, lang = lang),
    error = function(err) {
      message(
        "SDMX download failed (", conditionMessage(err), "). ",
        "Switching to TÜİK portal table download via tuikr::statistical_tables()."
      )
      NULL
    }
  )

  if (!is.null(sdmx_data)) {
    attr(sdmx_data, "access_method") <- "sdmx"
    return(sdmx_data)
  }

  portal_data <- fetch_tuik_raw_portal(
    dataflow_id = dataflow_id,
    lang = lang,
    istab_table_name = PROJECT_META$istab_table_name
  )
  attr(portal_data, "access_method") <- "portal_istab"
  portal_data
}

fetch_tuik_raw_portal <- function(dataflow_id, lang = "en", istab_table_name) {
  tables <- tuikr::statistical_tables(theme = PROJECT_META$theme_id, lang = lang)

  istab_row <- tables[
    tables$node_type == "istab" &
      tables$table_name == istab_table_name,
  , drop = FALSE]

  if (nrow(istab_row) == 0) {
    stop(
      "Could not find the istab table '", istab_table_name,
      "' through tuikr::statistical_tables().",
      call. = FALSE
    )
  }

  download_url <- istab_row$table_url[[1]]
  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file), add = TRUE)

  download_tuik_portal_file(download_url, destfile = temp_file, lang = lang)
  parse_tuik_spreadsheet(temp_file, selected_variable = PROJECT_META$selected_variable)
}

download_tuik_portal_file <- function(url, destfile, lang = "en") {
  if (!requireNamespace("crul", quietly = TRUE)) {
    stop("Package 'crul' is required for portal downloads.", call. = FALSE)
  }

  request_info <- list(
    page_url = paste0("https://veriportali.tuik.gov.tr/", lang, "/statistical-themes"),
    headers = list(
      Accept = "application/json, text/plain, */*",
      `Accept-Language` = if (lang == "tr") "tr-TR,tr;q=0.9" else "en-US,en;q=0.9",
      `User-Agent` = paste(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "AppleWebKit/537.36 (KHTML, like Gecko)",
        "Chrome/122.0.0.0 Safari/537.36"
      )
    )
  )

  cookie_file <- tempfile(fileext = ".txt")
  common_opts <- list(cookiefile = cookie_file, cookiejar = cookie_file)

  landing_cli <- crul::HttpClient$new(
    url = request_info$page_url,
    headers = request_info$headers,
    opts = common_opts
  )
  landing_cli$get()$raise_for_status()

  download_cli <- crul::HttpClient$new(
    url = url,
    headers = c(
      request_info$headers,
      list(
        Referer = request_info$page_url,
        Origin = "https://veriportali.tuik.gov.tr"
      )
    ),
    opts = common_opts
  )

  response <- download_cli$get()
  response$raise_for_status()
  writeBin(response$content, destfile)
  invisible(destfile)
}

parse_tuik_spreadsheet <- function(path, selected_variable) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read TÜİK portal spreadsheets.", call. = FALSE)
  }

  sheet_names <- readxl::excel_sheets(path)
  parsed <- NULL

  for (sheet in sheet_names) {
    raw <- tryCatch(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE),
      error = function(e) {
        alt_path <- sub("\\.xls$", ".xlsx", path, ignore.case = TRUE)
        if (alt_path != path && file.exists(alt_path)) {
          readxl::read_excel(alt_path, sheet = sheet, col_names = FALSE)
        } else {
          stop(conditionMessage(e), call. = FALSE)
        }
      }
    )

    parsed <- extract_monthly_index_vertical_sheet(raw)
    if (is.null(parsed)) {
      parsed <- extract_monthly_index_horizontal_sheet(raw)
    }
    if (!is.null(parsed) && nrow(parsed) > 0) {
      break
    }
  }

  if (is.null(parsed) || nrow(parsed) == 0) {
    stop("Could not parse a monthly index series from the downloaded TÜİK table.", call. = FALSE)
  }

  data.frame(
    INDICATOR = "NON_DOMESTIC_PPI",
    INDICATOR_label = selected_variable,
    obsTime = parsed$period_raw,
    obsValue = parsed$value,
    stringsAsFactors = FALSE
  )
}

month_name_to_number <- function(month_name) {
  key <- tolower(trimws(as.character(month_name)))
  key <- gsub("\\.", "", key, fixed = TRUE)
  key <- gsub("ı", "i", key, fixed = TRUE)
  key <- gsub("ş", "s", key, fixed = TRUE)
  key <- gsub("ğ", "g", key, fixed = TRUE)
  key <- gsub("ü", "u", key, fixed = TRUE)
  key <- gsub("ö", "o", key, fixed = TRUE)
  key <- gsub("ç", "c", key, fixed = TRUE)

  lookup <- c(
    "january" = 1, "february" = 2, "march" = 3, "april" = 4, "may" = 5, "june" = 6,
    "july" = 7, "august" = 8, "september" = 9, "october" = 10, "november" = 11, "december" = 12,
    "ocak" = 1, "subat" = 2, "mart" = 3, "nisan" = 4, "mayis" = 5, "haziran" = 6,
    "temmuz" = 7, "agustos" = 8, "eylul" = 9, "ekim" = 10, "kasim" = 11, "aralik" = 12
  )

  unname(lookup[key])
}

extract_monthly_index_vertical_sheet <- function(raw) {
  raw_chr <- as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)

  month_row <- NA_integer_
  for (i in seq_len(nrow(raw_chr))) {
    first_col <- tolower(trimws(raw_chr[i, 1]))
    if (first_col %in% c("yıl", "year")) {
      month_row <- i
      break
    }
  }

  if (is.na(month_row)) {
    return(NULL)
  }

  out <- list()
  for (i in (month_row + 1):nrow(raw_chr)) {
    year_val <- suppressWarnings(as.integer(raw_chr[i, 1]))
    if (is.na(year_val) || year_val < 1900 || year_val > 2100) {
      next
    }

    for (col in 2:min(13, ncol(raw_chr))) {
      month_val <- month_name_to_number(raw_chr[month_row, col])
      value <- suppressWarnings(as.numeric(raw_chr[i, col]))
      if (is.na(month_val) || is.na(value)) {
        next
      }

      period_raw <- sprintf("%04d-%02d", year_val, month_val)
      out[[length(out) + 1]] <- data.frame(
        period_raw = period_raw,
        value = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(NULL)
  }

  series <- do.call(rbind, out)
  series <- series[order(series$period_raw), ]
  series <- series[!duplicated(series$period_raw), , drop = FALSE]
  series
}

extract_monthly_index_horizontal_sheet <- function(raw) {
  raw_chr <- as.data.frame(lapply(raw, function(x) {
    if (inherits(x, "Date")) {
      format(x, "%Y-%m")
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)

  year_row <- NA_integer_
  for (i in seq_len(nrow(raw_chr))) {
    vals <- suppressWarnings(as.integer(raw_chr[i, ]))
    if (sum(!is.na(vals) & vals >= 2000 & vals <= 2100) >= 6) {
      year_row <- i
      break
    }
  }

  if (is.na(year_row)) {
    return(NULL)
  }

  month_row <- year_row + 1
  data_row <- NA_integer_

  for (i in seq(month_row + 1, nrow(raw_chr))) {
    label <- paste(raw_chr[i, 1], raw_chr[i, 2])
    label_norm <- tolower(label)
    if (grepl("tarim-gfe|tarım-gfe|agricultural input price index", label_norm, ignore.case = TRUE) &&
        grepl("gfe|girdi", label_norm, ignore.case = TRUE)) {
      numeric_vals <- suppressWarnings(as.numeric(unlist(raw_chr[i, ])))
      if (sum(!is.na(numeric_vals)) >= 12) {
        data_row <- i
        break
      }
    }
  }

  if (is.na(data_row)) {
    return(NULL)
  }

  out <- list()
  for (col in 3:ncol(raw_chr)) {
    year_val <- suppressWarnings(as.integer(raw_chr[year_row, col]))
    month_val <- month_name_to_number(raw_chr[month_row, col])
    value <- suppressWarnings(as.numeric(raw_chr[data_row, col]))

    if (is.na(year_val) || is.na(month_val) || is.na(value)) {
      next
    }

    period_raw <- sprintf("%04d-%02d", year_val, month_val)
    out[[length(out) + 1]] <- data.frame(
      period_raw = period_raw,
      value = value,
      stringsAsFactors = FALSE
    )
  }

  if (length(out) == 0) {
    return(NULL)
  }

  series <- do.call(rbind, out)
  series <- series[order(series$period_raw), ]
  series <- series[!duplicated(series$period_raw), , drop = FALSE]
  series
}

prepare_forecast_series <- function(raw_data) {
  data <- raw_data
  data <- data[!is.na(data$obsValue), , drop = FALSE]

  label_cols <- grep("_label$", names(data), value = TRUE)

  for (col in label_cols) {
    values <- tolower(as.character(data[[col]]))
    if (any(grepl("rate|change|percent|değişim|yıllık|monthly", values, ignore.case = TRUE))) {
      keep <- !grepl("rate|change|percent|değişim|yıllık|monthly", values, ignore.case = TRUE)
      if (sum(keep) >= 24) {
        data <- data[keep, , drop = FALSE]
      }
    }
  }

  for (col in label_cols) {
    values <- tolower(as.character(data[[col]]))
    if (any(grepl("non-domestic|yurt dışı|yd-ufe|yd ufe", values, ignore.case = TRUE))) {
      keep <- grepl("non-domestic|yurt dışı|yd-ufe|yd ufe|total", values, ignore.case = TRUE)
      if (sum(keep) >= 24) {
        data <- data[keep, , drop = FALSE]
      }
    }
  }

  if (nrow(data) == 0) {
    stop("No observations remained after filtering.", call. = FALSE)
  }

  series_tbl <- data.frame(
    period_raw = as.character(data$obsTime),
    value = as.numeric(data$obsValue),
    stringsAsFactors = FALSE
  )
  series_tbl <- series_tbl[order(series_tbl$period_raw), ]
  series_tbl <- series_tbl[!duplicated(series_tbl$period_raw), , drop = FALSE]
  series_tbl$period_date <- parse_tuik_period(series_tbl$period_raw)
  series_tbl <- series_tbl[!is.na(series_tbl$period_date), , drop = FALSE]
  series_tbl <- series_tbl[order(series_tbl$period_date), ]

  if (nrow(series_tbl) < 24) {
    warning("The filtered series has fewer than 24 monthly observations.")
  }

  series_tbl
}

parse_tuik_period <- function(period_raw) {
  period_raw <- trimws(as.character(period_raw))
  parsed <- suppressWarnings(as.Date(
    paste0(sub("^(\\d{4})[- ]?(\\d{2}).*$", "\\1-\\2-01", period_raw)),
    format = "%Y-%m-%d"
  ))

  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.Date(
      paste0(sub("^(\\d{4}).*$", "\\1-01-01", period_raw)),
      format = "%Y-%m-%d"
    ))
  }

  parsed
}

format_period_label <- function(date_obj) {
  format(as.Date(date_obj), "%Y-%m")
}

get_forecast_target <- function(series_tbl) {
  latest_date <- max(series_tbl$period_date)
  latest_label <- format_period_label(latest_date)
  target_date <- seq(latest_date, by = "month", length.out = 2)[2]
  target_label <- format_period_label(target_date)

  list(
    latest_observation = latest_label,
    forecast_target_period = target_label,
    latest_date = latest_date,
    target_date = target_date
  )
}

load_project_series <- function(dataflow_id = PROJECT_META$dataflow_id, lang = "en") {
  raw_data <- fetch_tuik_raw(dataflow_id = dataflow_id, lang = lang)
  series_tbl <- prepare_forecast_series(raw_data)
  target_info <- get_forecast_target(series_tbl)

  list(
    meta = PROJECT_META,
    raw_data = raw_data,
    series = series_tbl,
    target = target_info,
    access_date = Sys.Date(),
    access_method = attr(raw_data, "access_method")
  )
}
