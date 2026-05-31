# One-time package setup for the forecasting project.

required_cran <- c("remotes", "knitr", "dplyr", "kableExtra", "rmarkdown", "readxl", "crul")

for (pkg in required_cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

if (!requireNamespace("tuikr", quietly = TRUE)) {
  remotes::install_github("emraher/tuikr")
}

if (requireNamespace("renv", quietly = TRUE)) {
  renv::init(bare = TRUE)
  renv::snapshot()
  message("renv environment initialized and renv.lock created.")
} else {
  message("Install renv first if you need renv.lock: install.packages('renv')")
}

message("Setup complete. Open forecasting_project.Rmd and knit.")
