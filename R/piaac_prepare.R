# =============================================================================
# R/piaac_prepare.R
# Data preparation: one PIAAC Cycle 2 public-use file -> analysis data frame.
#
# prepare_country()   sample rules and variable coding for one country
# prepare_revised()   adds job-cell demand measures and background levels
# load_country_data() reads the prepared files written by R/01_prepare_data.R
#
# Nothing here estimates anything. All decisions about who is in the sample
# and how variables are coded live in this file.
# =============================================================================

# The 12 countries that release continuous hourly earnings (log-wage sample) and
# the 19 countries of the earnings-decile sample (the 12 plus 7 that release
# earnings only as deciles). Not usable: Austria and Germany (no occupation
# codes), Japan and the United States (no Big Five scores), Israel (earnings unit
# errors, see the study).
main_countries   <- c("CHL", "HRV", "CZE", "EST", "FRA", "IRL", "ITA", "LVA", "LTU", "NZL", "POL", "ESP")
decile_countries <- c("BEL", "CAN", "CHL", "CZE", "ESP", "EST", "FRA", "GBR", "HRV", "HUN", "IRL", "ITA",
                      "KOR", "LTU", "LVA", "NZL", "POL", "PRT", "SGP")

trait_names <- c("EXTR", "CONS", "OPEM", "EMOS", "AGRE")
task_names  <- c("influence", "organisation", "learning", "pressure", "cooperation")
matched_task <- setNames(task_names, trait_names)

task_items <- c("H2_Q05a", "H2_Q05b", "H2_Q04a", "H2_Q04b",
                "H2_Q09a", "H2_Q09b", "H2_Q09c", "H2_Q12", "H2_Q01")
numeracy_items <- paste0("F2_Q03", letters[1:5])
pv_names <- c(paste0("PVLIT", 1:10), paste0("PVNUM", 1:10), paste0("PVAPS", 1:10))

facet_names <- list(
  EXTR = c("EXTR_ASSE", "EXTR_SOCI", "EXTR_ENER"),
  CONS = c("CONS_ORGA", "CONS_PROD", "CONS_RESP"),
  OPEM = c("OPEM_AEST", "OPEM_CURI", "OPEM_IMAG"),
  EMOS = c("EMOS_ANXI", "EMOS_DEPR", "EMOS_VOLA"),
  AGRE = c("AGRE_COMP", "AGRE_RESP", "AGRE_TRUS"))

required_vars <- c("SEQID", "CNTRYID", trait_names, task_items, pv_names,
  "AGEG10LFS", "GENDER_R", "EDCAT6_TC1", "C2_D05", "D2_Q04",
  "EARNHRBONUSC2", "EARNHRC2", "EARNFLAGC2",
  "ISCOSKIL4", "D2_Q03", "D2_Q07a",
  paste0("SPFWT", 0:80), "VEMETHOD", "VENREPS", "VEFAYFAC")

optional_vars <- c("AGE_R", "ISCO1C", "ISCO2C", "A2_Q03a", "J2_Q04c", "J2_Q05c",
  "D2_Q09a", "D2_Q11_C", "D2_Q13", "D2_Q05a1", "D2_Q05a2",
  "H2_Q02a", "H2_Q02b", "H2_Q02c", "H2_Q02d", "H2_Q02e",
  "H2_Q03a", "H2_Q10", "H2_Q08b", "H2_Q08c", "H2_Q06b",
  "H2_Q13a", "H2_Q13b", "H2_Q13c", numeracy_items, "NUMWORKC2",
  paste0(trait_names, "_6"), unlist(facet_names), "EARNHRBONUSDCLC2", "EARNHRDCLC2")

country_names <- c(
  AUT = "Austria", BEL = "Belgium (Flanders)", CAN = "Canada", CHE = "Switzerland",
  CHL = "Chile", CZE = "Czechia", DEU = "Germany", DNK = "Denmark", ESP = "Spain",
  EST = "Estonia", FIN = "Finland", FRA = "France", GBR = "England (UK)",
  HRV = "Croatia", HUN = "Hungary", IRL = "Ireland", ISR = "Israel", ITA = "Italy",
  JPN = "Japan", KOR = "Korea", LTU = "Lithuania", LVA = "Latvia", NLD = "Netherlands",
  NOR = "Norway", NZL = "New Zealand", POL = "Poland", PRT = "Portugal",
  SGP = "Singapore", SVK = "Slovak Republic", SWE = "Sweden", USA = "United States")

# ---- wage-quality rules -------------------------------------------------------
# Decided on 20 September 2026 after inspecting every country's distribution:
#   * keep positive hourly earnings including bonuses (EARNHRBONUSC2);
#   * drop the weighted bottom and top 1% within each country (OECD convention);
#   * drop values below 1/20 or above 20 times the weighted country median.
#     This guards against unit errors (monthly pay entered as hourly), which
#     affect about 4% of Israeli employees and a handful of cases elsewhere.
# The untrimmed sample, earnings without bonuses and directly reported earnings
# are kept as columns so that papers can report them as checks.
wage_rules <- list(trim = c(0.01, 0.99), median_ratio = 20)

weighted_quantile <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(p, function(pp) x[which(cw >= pp)[1]], numeric(1))
}

wage_keep <- function(wage, w, rules = wage_rules) {
  ok <- is.finite(wage) & wage > 0 & is.finite(w) & w > 0
  keep <- ok
  if (any(ok)) {
    q <- weighted_quantile(wage[ok], w[ok], c(rules$trim, 0.5))
    keep[ok] <- wage[ok] >= q[1] & wage[ok] <= q[2] &
      wage[ok] >= q[3] / rules$median_ratio & wage[ok] <= q[3] * rules$median_ratio
  }
  keep
}

# ---- helpers ---------------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(unclass(x)))

wstd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  m <- sum(w[ok] * x[ok]) / sum(w[ok])
  s <- sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
  (x - m) / s
}

scale01 <- function(x, k = 5) ifelse(x %in% 1:k, (x - 1) / (k - 1), NA_real_)
binary  <- function(x) ifelse(x %in% 1:2, as.numeric(x == 1), NA_real_)

file_country <- function(path) toupper(substr(basename(path), 4, 6))

# ---- main function -------------------------------------------------------------------
prepare_country <- function(path, country = file_country(path), rules = wage_rules,
                            survey_year = 2023, outcome = c("continuous", "decile")) {
  # outcome = "decile": the outcome is the within-country decile of hourly
  # earnings including bonuses (EARNHRBONUSDCLC2, 1-10), released in more
  # countries than continuous earnings; no wage-trimming rules apply.
  outcome <- match.arg(outcome)
  d <- as.data.frame(haven::read_sav(path,
    col_select = tidyselect::any_of(c(required_vars, optional_vars))))
  absent <- setdiff(required_vars, names(d))
  if (length(absent)) stop("Missing required variables: ", paste(absent, collapse = ", "))
  for (v in setdiff(optional_vars, names(d))) d[[v]] <- NA_real_

  ve_method <- tolower(as.character(d$VEMETHOD[1]))
  n_reps    <- as.integer(num(d$VENREPS[1]))
  fay       <- num(d$VEFAYFAC[1])
  character_vars <- c("SEQID", "VEMETHOD", "ISCO1C", "ISCO2C")
  for (v in setdiff(c(required_vars, optional_vars), character_vars)) d[[v]] <- num(d[[v]])

  # --- eligibility (before wage rules) ---
  elig <- d$AGEG10LFS %in% 2:4 & d$GENDER_R %in% 1:2 & d$EDCAT6_TC1 %in% 1:6 &
    d$C2_D05 %in% 1 & d$D2_Q04 %in% 1 &
    (if (outcome == "decile") d$EARNHRBONUSDCLC2 %in% 1:10 else
       is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0) &
    d$ISCOSKIL4 %in% 1:4 & d$D2_Q03 %in% 1:3 & d$D2_Q07a %in% 1:6 &
    is.finite(d$SPFWT0) & d$SPFWT0 > 0
  elig <- elig & Reduce("&", lapply(d[c(trait_names, pv_names)], is.finite)) &
    Reduce("&", lapply(d[task_items], function(x) x %in% 1:5))
  elig[is.na(elig)] <- FALSE
  if (!any(elig)) stop(country, ": no eligible respondents")

  # --- wage rules, applied within the eligible sample ---
  keep_w <- elig
  if (outcome == "continuous")
    keep_w[elig] <- wage_keep(d$EARNHRBONUSC2[elig], d$SPFWT0[elig], rules)
  n_before <- sum(elig); n_after <- sum(keep_w)
  d <- d[keep_w, , drop = FALSE]
  if (anyDuplicated(d$SEQID)) stop(country, ": duplicate respondent identifiers")

  # --- replicate weights: only those the country actually uses ---
  unused <- paste0("SPFWT", seq_len(80))[-seq_len(n_reps)]
  d <- d[, setdiff(names(d), unused), drop = FALSE]

  # --- outcomes ---
  d$country  <- country
  d$log_wage <- if (outcome == "decile") d$EARNHRBONUSDCLC2 else log(d$EARNHRBONUSC2)
  d$earnings_decile <- d$EARNHRBONUSDCLC2
  d$log_wage_nobonus <- ifelse(is.finite(d$EARNHRC2) & d$EARNHRC2 > 0, log(d$EARNHRC2), NA_real_)
  d$wage_reported_directly <- d$EARNFLAGC2 %in% 1

  # --- task scales (0 = never, 1 = always) ---
  d$influence    <- (d$H2_Q05a + d$H2_Q05b - 2) / 8
  d$organisation <- (d$H2_Q04a + d$H2_Q04b - 2) / 8
  d$learning     <- (d$H2_Q09a + d$H2_Q09b + d$H2_Q09c - 3) / 12
  d$pressure     <- (d$H2_Q12 - 1) / 4
  d$cooperation  <- (d$H2_Q01 - 1) / 4

  # --- covariates with readable labels ---
  d$age_band  <- factor(d$AGEG10LFS, levels = 2:4, labels = c("25-34", "35-44", "45-54"))
  d$gender    <- factor(d$GENDER_R, levels = 1:2, labels = c("Men", "Women"))
  d$education <- factor(d$EDCAT6_TC1, levels = 1:6,
    labels = c("Lower secondary or less", "Upper secondary", "Post-secondary non-tertiary",
               "Tertiary professional", "Bachelor", "Master or research"))
  d$occupation <- factor(d$ISCOSKIL4, levels = 1:4,
    labels = c("Skilled", "Semi-skilled white-collar", "Semi-skilled blue-collar", "Elementary"))
  d$sector <- factor(d$D2_Q03, levels = 1:3, labels = c("Private", "Public", "Non-profit"))
  d$establishment_size <- factor(d$D2_Q07a, levels = 1:6,
    labels = c("1-10", "11-49", "50-249", "250-499", "500-999", "1000+"))

  # --- optional information (NA where a country does not release it) ---
  d$exact_age <- ifelse(d$AGE_R %in% 25:54, d$AGE_R, NA_real_)
  d$occupation1 <- factor(ifelse(grepl("^[0-9]$", d$ISCO1C), d$ISCO1C, NA_character_))
  valid_isco2 <- c("01", "02", "03", "11", "12", "13", "14", "21", "22", "23", "24", "25", "26",
    "31", "32", "33", "34", "35", "41", "42", "43", "44", "51", "52", "53", "54",
    "61", "62", "63", "71", "72", "73", "74", "75", "81", "82", "83", "91", "92", "93", "94", "95", "96")
  d$occupation2 <- factor(ifelse(d$ISCO2C %in% valid_isco2, d$ISCO2C, NA_character_))
  d$team        <- binary(d$H2_Q02a)
  d$supervisor  <- binary(d$D2_Q09a)
  d$satisfied   <- ifelse(d$D2_Q13 %in% 1:5, as.numeric(d$D2_Q13 <= 2), NA_real_)
  d$method_discretion <- scale01(d$H2_Q08b)
  d$pace_discretion   <- scale01(d$H2_Q08c)
  d$complex_problem   <- scale01(d$H2_Q06b)
  d$sharing <- scale01(d$H2_Q03a)
  d$helping <- scale01(d$H2_Q10)
  d$weekly_hours <- ifelse(d$D2_Q11_C %in% 1:60, d$D2_Q11_C, NA_real_)
  d$born_in_country  <- factor(ifelse(d$A2_Q03a %in% 1:2, d$A2_Q03a, NA), levels = 1:2, labels = c("Yes", "No"))
  d$mother_education <- factor(ifelse(d$J2_Q04c %in% 1:3, d$J2_Q04c, NA), levels = 1:3, labels = c("Low", "Medium", "High"))
  d$father_education <- factor(ifelse(d$J2_Q05c %in% 1:3, d$J2_Q05c, NA), levels = 1:3, labels = c("Low", "Medium", "High"))

  # Years with the current employer: survey year minus the year the job started.
  # Start year is released in every wage country except Estonia.
  start_year <- ifelse(d$D2_Q05a2 %in% 1950:survey_year, d$D2_Q05a2, NA_real_)
  d$tenure_years <- pmax(0, survey_year - start_year)

  # Numeracy use at work: mean of five frequency items, 0 (never) to 1 (every day).
  nu <- sapply(numeracy_items, function(v) scale01(d[[v]]))
  d$numeracy_use <- ifelse(rowSums(is.na(nu)) == 0, rowMeans(nu), NA_real_)

  # Second half of the six-item scales (where the BFI-2-S was administered).
  # Only the published z-scores exist, not the items. With S6 = (S3 + SB)/2 in
  # raw units, SB = 2*S6 - S3. Under parallel items the ratio sd(S6)/sd(S3)
  # equals corr(S3, S6), so SB is proportional to 2*rho*Z6 - Z3. This is an
  # approximation and is labelled as such wherever it is used.
  for (t in trait_names) {
    z6 <- d[[paste0(t, "_6")]]
    if (sum(is.finite(z6)) > 100) {
      rho <- stats::cor(d[[t]], z6, use = "complete.obs")
      d[[paste0(t, "_B")]] <- wstd(2 * rho * z6 - d[[t]], d$SPFWT0)
    } else d[[paste0(t, "_B")]] <- NA_real_
  }

  attr(d, "country")   <- country
  attr(d, "ve_method") <- if (ve_method %in% c("fay", "jk1", "jk2")) ve_method else "fay"
  attr(d, "n_reps")    <- n_reps
  attr(d, "fay")       <- if (is.finite(fay)) fay else 0.3
  attr(d, "n_eligible_before_wage_rules") <- n_before
  attr(d, "n_dropped_by_wage_rules")      <- n_before - n_after
  attr(d, "wage_rules") <- rules
  d
}

add_background_levels <- function(d, vars = c("born_in_country", "mother_education", "father_education")) {
  # Keeps the common sample intact: missing background becomes its own level.
  for (v in vars) {
    x <- as.character(d[[v]]); x[is.na(x)] <- "Not reported"
    d[[v]] <- factor(x)
  }
  d
}

loo_cell_mean <- function(x, cell, w, min_n = 15) {
  ok <- !is.na(x) & !is.na(cell) & !is.na(w)
  out <- rep(NA_real_, length(x))
  sw  <- stats::ave(ifelse(ok, w, 0), cell, FUN = sum)
  swx <- stats::ave(ifelse(ok, w * x, 0), cell, FUN = sum)
  n   <- stats::ave(as.numeric(ok), cell, FUN = sum)
  out[ok] <- ((swx - w * x) / (sw - w))[ok]
  out[n < min_n] <- NA_real_
  out
}

add_cell_demand <- function(d, cell = "occupation2", w = "SPFWT0", min_n = 15, suffix = "_d") {
  # Leave-one-out weighted cell means of each task index, z-standardised.
  # `cell` may name several columns, finest first: where the finest cell has
  # fewer than min_n respondents, the next broader cell is used instead.
  for (t in task_names) {
    m <- rep(NA_real_, nrow(d))
    for (cl in cell) {
      fill <- is.na(m)
      m[fill] <- loo_cell_mean(d[[t]], d[[cl]], d[[w]], min_n)[fill]
    }
    d[[paste0(t, suffix)]] <- wstd(m, d[[w]])
    d[[paste0(t, "_z")]]   <- wstd(d[[t]], d[[w]])
  }
  d
}

prepare_revised <- function(d, min_n = 15) {
  at <- attributes(d)
  d <- add_background_levels(d)
  d$job_cell   <- interaction(d$occupation, d$sector, d$establishment_size, drop = TRUE)
  d$occ_sector <- interaction(d$occupation, d$sector, drop = TRUE)
  d <- add_cell_demand(d, cell = c("job_cell", "occ_sector", "occupation"), min_n = min_n, suffix = "_c")
  if ("occupation2" %in% names(d) && any(!is.na(d$occupation2)))
    d <- add_cell_demand(d, cell = "occupation2", min_n = min_n, suffix = "_d")
  for (a in setdiff(names(at), c("names", "row.names", "class"))) attr(d, a) <- at[[a]]
  d
}

# ---- loading prepared data (written by R/01_prepare_data.R) -------------------------------------------------------------------
derived_dir <- function() file.path(here::here(), "data", "derived")

load_country_data <- function(countries = NULL, variant = c("main", "untrimmed"), dir = derived_dir()) {
  # variant "untrimmed": the same eligibility rules but no wage trimming.
  variant <- match.arg(variant)
  manifest_file <- file.path(dir, "countries.csv")
  if (!file.exists(manifest_file))
    stop("No prepared data in ", dir, ". Run `make data` (Rscript R/01_prepare_data.R) first.")
  man <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
  if (is.null(countries)) countries <- man$country
  sub <- if (variant == "main") dir else file.path(dir, variant)
  out <- lapply(countries, function(cc) readRDS(file.path(sub, paste0(cc, ".rds"))))
  setNames(out, countries)
}

country_manifest <- function(dir = derived_dir())
  utils::read.csv(file.path(dir, "countries.csv"), stringsAsFactors = FALSE)

# ---- locating the public-use files -------------------------------------------------------------
raw_dir <- function() file.path(here::here(), "data", "raw")

korea_merged <- function(main, dir = raw_dir()) {
  # Korea releases the Big Five scores in a separate file (prg_2korms_big5.sav);
  # they are merged into the main file by SEQID and the result is written once to
  # data/raw/merged/PRGKORP2.sav.
  big5 <- list.files(dir, pattern = "^prg_2korms_big5\\.sav$", full.names = TRUE, recursive = TRUE)
  if (!length(big5)) stop("Korea: prg_2korms_big5.sav (Big Five scores) not found in ", dir)
  out <- file.path(dir, "merged", "PRGKORP2.sav")
  dir.create(dirname(out), showWarnings = FALSE)
  if (file.exists(out) && file.mtime(out) > max(file.mtime(main), file.mtime(big5[1]))) return(out)
  b <- as.data.frame(haven::read_sav(big5[1]))
  add <- setdiff(names(b), c("SEQID", "CNTRYID"))
  d <- as.data.frame(haven::read_sav(main,
    col_select = tidyselect::any_of(setdiff(c(required_vars, optional_vars), add))))
  d[add] <- b[match(d$SEQID, b$SEQID), add]
  haven::write_sav(d, out)
  out
}

raw_file <- function(cc, dir = raw_dir()) {
  # PRG<CC>P2.sav anywhere under data/raw/ (zip archives must be extracted first).
  f <- list.files(dir, pattern = sprintf("^PRG%sP2\\.sav$", cc), full.names = TRUE, recursive = TRUE)
  f <- f[!grepl("/merged/", f, fixed = TRUE)]
  if (!length(f)) stop("Public-use file PRG", cc, "P2.sav not found in ", dir)
  if (cc == "KOR") korea_merged(f[1], dir) else f[1]
}
