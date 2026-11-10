# =============================================================================
# R/01_prepare_data.R
# Builds the analysis data from the PIAAC Cycle 2 public-use files in data/raw/.
#
#   Rscript R/01_prepare_data.R            (make data)
#   Rscript R/01_prepare_data.R --force    rebuild files that are already up to date
#
# Writes
#   data/derived/<CC>.rds            log-wage sample, the 12 countries with continuous earnings
#   data/derived/untrimmed/<CC>.rds  the same respondents before the wage-trimming rules
#   data/derived/countries.csv       manifest with sample sizes and data availability
#   data/derived_decile/<CC>.rds     earnings-decile sample, 19 countries
#   data/derived_decile/countries.csv
# Sample rules and variable coding: R/piaac_prepare.R.
# =============================================================================
suppressPackageStartupMessages(library(haven))
here::i_am("R/01_prepare_data.R")
root <- here::here()
source(file.path(root, "R", "piaac_prepare.R"))
force <- "--force" %in% commandArgs(trailingOnly = TRUE)

code_time <- file.mtime(file.path(root, "R", "piaac_prepare.R"))
up_to_date <- function(out, src) !force && file.exists(out) && file.mtime(out) > max(file.mtime(src), code_time)
avail <- function(d, v) all(v %in% names(d)) && all(sapply(v, function(x) any(!is.na(d[[x]]))))

# ---- log-wage sample ---------------------------------------------------------------------
out_dir <- file.path(root, "data", "derived")
dir.create(file.path(out_dir, "untrimmed"), showWarnings = FALSE, recursive = TRUE)
rows <- list()
for (cc in main_countries) {
  f <- raw_file(cc); out <- file.path(out_dir, paste0(cc, ".rds"))
  if (up_to_date(out, f)) {
    d <- readRDS(out)
    cat(sprintf("%s  up to date (n = %d)\n", cc, nrow(d)))
  } else {
    t0 <- Sys.time()
    d <- prepare_revised(prepare_country(f, cc))
    saveRDS(d, out)
    saveRDS(prepare_revised(prepare_country(f, cc, rules = list(trim = c(0, 1), median_ratio = Inf))),
            file.path(out_dir, "untrimmed", paste0(cc, ".rds")))
    cat(sprintf("%s  prepared: n = %d, %d replicate weights (%s), %d dropped by wage rules  [%.1fs]\n",
                cc, nrow(d), attr(d, "n_reps"), attr(d, "ve_method"),
                attr(d, "n_dropped_by_wage_rules"), as.numeric(Sys.time() - t0, units = "secs")))
  }
  rows[[cc]] <- data.frame(
    country = cc, name = unname(country_names[cc]), n = nrow(d),
    n_eligible = attr(d, "n_eligible_before_wage_rules"),
    n_dropped_wage_rules = attr(d, "n_dropped_by_wage_rules"),
    ve_method = attr(d, "ve_method"), n_reps = attr(d, "n_reps"),
    has_occupation2 = avail(d, "influence_d"), has_long_scales = avail(d, paste0(trait_names, "_B")),
    has_tenure = avail(d, "tenure_years"), has_numeracy_use = avail(d, "numeracy_use"),
    stringsAsFactors = FALSE)
}
man <- do.call(rbind, rows); man <- man[order(man$country), ]
utils::write.csv(man, file.path(out_dir, "countries.csv"), row.names = FALSE)
cat("\nLog-wage sample:\n"); print(man, row.names = FALSE)

# ---- earnings-decile sample ----------------------------------------------------------------
out_d <- file.path(root, "data", "derived_decile")
dir.create(out_d, showWarnings = FALSE)
rows <- list()
for (cc in decile_countries) {
  f <- raw_file(cc); out <- file.path(out_d, paste0(cc, ".rds"))
  if (up_to_date(out, f)) {
    d <- readRDS(out)
    cat(sprintf("%s  up to date (n = %d)\n", cc, nrow(d)))
  } else {
    t0 <- Sys.time()
    d <- prepare_revised(prepare_country(f, cc, outcome = "decile"))
    saveRDS(d, out)
    cat(sprintf("%s  prepared (decile outcome): n = %d, %d replicate weights (%s)  [%.1fs]\n",
                cc, nrow(d), attr(d, "n_reps"), attr(d, "ve_method"), as.numeric(Sys.time() - t0, units = "secs")))
  }
  rows[[cc]] <- data.frame(
    country = cc, name = unname(country_names[cc]), n = nrow(d),
    ve_method = attr(d, "ve_method"), n_reps = attr(d, "n_reps"),
    continuous = cc %in% main_countries,
    has_occupation2 = any(!is.na(d$occupation2)), has_tenure = any(is.finite(d$tenure_years)),
    has_long_scales = sum(is.finite(d$EXTR_B)) > 100, stringsAsFactors = FALSE)
}
man_d <- do.call(rbind, rows)
utils::write.csv(man_d, file.path(out_d, "countries.csv"), row.names = FALSE)
cat("\nEarnings-decile sample:\n"); print(man_d, row.names = FALSE)
