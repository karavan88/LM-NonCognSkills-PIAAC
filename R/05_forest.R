# =============================================================================
# R/05_forest.R
# Does the wage return to a non-cognitive skill differ with education and literacy?
# Generalized random forests (Athey, Tibshirani and Wager 2019; grf::causal_forest with a
# continuous treatment), country by country.
#
#   PIAAC_CORES=4 Rscript R/05_forest.R                        log-wage sample, 12 countries
#   PIAAC_VARIANT=decile PIAAC_CORES=4 Rscript R/05_forest.R   earnings-decile sample, 19 countries
#
# Environment: PIAAC_CORES (countries in parallel, default 1), FOREST_TREES (default 2000).
#
# For each country and each of the 5 skills, one forest:
#   outcome     log hourly wage (or the earnings decile)
#   treatment   the skill, in within-country SD units (continuous)
#   covariates  the other 4 skills, literacy (mean of the 10 plausible values), education,
#               age group, gender, nativity, parental education
#   weights     final survey weight (SPFWT0)
# The forest estimates the return to the skill conditional on the covariates. Reported:
#   SKILL|ape             average partial effect (the return net of the covariates)
#   SKILL|graduates       average partial effect among tertiary graduates
#   SKILL|non_graduates   ... among workers without a tertiary degree
#   SKILL|diff_edu        graduates minus non-graduates
#   SKILL|lit_top         ... in the top third of literacy (weighted thirds within country)
#   SKILL|lit_bottom      ... in the bottom third
#   SKILL|diff_lit        top minus bottom third
#   SKILL|blp_literacy    best linear projection of the conditional return on literacy (SD)
#   SKILL|blp_tertiary    ... and on tertiary education
#   SKILL|calibration     coefficient "differential.forest.prediction" of grf::test_calibration;
#                         a positive, significant value indicates heterogeneity
# Standard errors are those of grf (doubly robust scores); they use the survey weight but not
# the replicate weights, and literacy enters as the mean of its plausible values. Differences
# between two groups take the two group estimates as independent.
# Forests are grown with a fixed seed on 1 thread, so a rerun reproduces the files.
# Output: results/art_forest.rds or results_decile/art_forest.rds, a named list (countries) of
# data frames with columns term, estimate, se; forest_summary.csv holds the cross-country
# means (each country weighted equally, se = sqrt(sum se_c^2) / K) for the sets of countries
# the documents report.
# =============================================================================
here::i_am("R/05_forest.R")
root <- here::here()
source(file.path(root, "R", "piaac_prepare.R"))
source(file.path(root, "R", "estimators.R"))
source(file.path(root, "R", "country_summaries.R"))
stopifnot(requireNamespace("grf", quietly = TRUE))

variant <- Sys.getenv("PIAAC_VARIANT", "main")
cores <- max(1L, as.integer(Sys.getenv("PIAAC_CORES", "1")))
n_trees <- as.integer(Sys.getenv("FOREST_TREES", "2000"))
results_dir <- file.path(root, if (variant == "decile") "results_decile" else "results")
dir.create(results_dir, showWarnings = FALSE)
if (variant == "decile") {
  ddir <- file.path(root, "data", "derived_decile")
  paper_countries <- utils::read.csv(file.path(ddir, "countries.csv"))$country
  country_list <- load_country_data(paper_countries, dir = ddir)
} else {
  paper_countries <- main_countries
  country_list <- load_country_data(paper_countries)
}
names(country_list) <- paper_countries

covariates <- "literacy + education + age_band + gender + born_in_country + mother_education + father_education"

forest_country <- function(d) {
  d$literacy <- (rowMeans(d[, paste0("PVLIT", 1:10)]) - 250) / 50
  d <- complete_for(d, paste(base_rhs, "+", covariates), c("log_wage", "SPFWT0"))
  wt <- d$SPFWT0 / mean(d$SPFWT0)
  lit_z <- wstd(d$literacy, d$SPFWT0)
  tertiary <- as.integer(d$education) >= 4
  o <- order(d$literacy); cw <- cumsum(wt[o]) / sum(wt)
  third <- integer(nrow(d)); third[o] <- 1L + (cw > 1/3) + (cw > 2/3)
  do.call(rbind, lapply(trait_names, function(tr) {
    X <- stats::model.matrix(stats::as.formula(paste("~", paste(setdiff(trait_names, tr), collapse = " + "), "+", covariates)), d)[, -1, drop = FALSE]
    X <- X[, apply(X, 2, stats::sd) > 0, drop = FALSE]
    cf <- grf::causal_forest(X, d$log_wage, d[[tr]], sample.weights = wt, num.trees = n_trees,
                             num.threads = 1, seed = 20261118)
    ape <- function(s = NULL) { a <- grf::average_treatment_effect(cf, subset = s); c(a[["estimate"]], a[["std.err"]]) }
    gap <- function(a, b) c(a[1] - b[1], sqrt(a[2]^2 + b[2]^2))
    g <- ape(tertiary); n <- ape(!tertiary); hi <- ape(third == 3L); lo <- ape(third == 1L)
    blp <- grf::best_linear_projection(cf, cbind(lit_z = lit_z, tertiary = as.numeric(tertiary)))
    tc <- grf::test_calibration(cf)
    out <- rbind(ape = ape(), graduates = g, non_graduates = n, diff_edu = gap(g, n),
                 lit_top = hi, lit_bottom = lo, diff_lit = gap(hi, lo),
                 blp_literacy = blp["lit_z", 1:2], blp_tertiary = blp["tertiary", 1:2],
                 calibration = tc["differential.forest.prediction", 1:2])
    data.frame(term = paste0(tr, "|", rownames(out)), estimate = out[, 1], se = out[, 2],
               row.names = NULL, stringsAsFactors = FALSE)
  }))
}

t0 <- Sys.time()
res <- parallel::mclapply(paper_countries, function(cc) forest_country(country_list[[cc]]),
                          mc.cores = min(cores, length(paper_countries)), mc.preschedule = FALSE)
bad <- vapply(res, inherits, logical(1), "try-error")
if (any(bad)) stop("Forest failed in: ", paste(paper_countries[bad], collapse = ", "), "\n", res[[which(bad)[1]]])
names(res) <- paper_countries
saveRDS(res, file.path(results_dir, "art_forest.rds"))

# ---- cross-country means for the sets of countries the documents report --------------------------
sets <- list(all = paper_countries)
if (variant == "decile") {
  sets$same12 <- intersect(paper_countries, main_countries)   # the countries of the log-wage sample
  sets$new7 <- setdiff(paper_countries, main_countries)       # countries that release deciles only
}
long <- country_long(res)
summary_rows <- do.call(rbind, lapply(names(sets), function(s) {
  x <- long[long$country %in% sets[[s]], ]
  do.call(rbind, lapply(split(x, x$term), function(y) {
    k <- nrow(y); m <- mean(y$estimate); se <- sqrt(sum(y$se^2)) / k
    data.frame(set = s, term = y$term[1], skill = sub("\\|.*$", "", y$term[1]), quantity = sub("^.*\\|", "", y$term[1]),
               estimate = m, se = se, lo = m - z975 * se, hi = m + z975 * se, p = 2 * stats::pnorm(-abs(m / se)),
               countries = k, n_sig10 = sum(y$p < 0.10), n_positive = sum(y$estimate > 0))
  }))
}))
utils::write.csv(summary_rows, file.path(results_dir, "forest_summary.csv"), row.names = FALSE)
message(sprintf("Forests written to %s (%d countries, %.0f s)", results_dir, length(res),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
