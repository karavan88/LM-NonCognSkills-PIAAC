# =============================================================================
# R/03_estimate.R
# Weighted least squares estimates with design-based inference (replicate
# weights x plausible values): everything the manuscript reports apart from the
# mediation estimates, which R/04_mediation.R computes with mediation::mediate().
#
#   PIAAC_CORES=4 Rscript R/03_estimate.R                        log-wage sample, 12 countries
#   PIAAC_VARIANT=decile PIAAC_CORES=4 Rscript R/03_estimate.R   earnings-decile sample, 19 countries
#
# Environment: PIAAC_CORES (countries estimated in parallel, default 1),
#              PIAAC_REFRESH=1 (re-estimate even if a result file exists).
# Output: results/*.rds (log wages) or results_decile/art_*.rds (deciles). Each file is
# a named list (countries) of data frames with columns term, estimate, se.
# =============================================================================
here::i_am("R/03_estimate.R")
root <- here::here()
source(file.path(root, "R", "piaac_prepare.R"))
source(file.path(root, "R", "estimators.R"))
source(file.path(root, "R", "country_summaries.R"))

options(piaac.cores = max(1L, as.integer(Sys.getenv("PIAAC_CORES", "1"))))
refresh <- Sys.getenv("PIAAC_REFRESH", "0") == "1"
variant <- Sys.getenv("PIAAC_VARIANT", "main")
results_dir <- file.path(root, if (variant == "decile") "results_decile" else "results")
dir.create(results_dir, showWarnings = FALSE)

if (variant == "decile") {
  ddir <- file.path(root, "data", "derived_decile")
  paper_countries <- utils::read.csv(file.path(ddir, "countries.csv"))$country
  country_list <- lapply(load_country_data(paper_countries, dir = ddir), add_numeracy_use_cell)
  file.copy(file.path(ddir, "countries.csv"), file.path(results_dir, "countries.csv"), overwrite = TRUE)
} else {
  paper_countries <- main_countries
  country_list <- lapply(load_country_data(paper_countries), add_numeracy_use_cell)
  untrimmed_list <- load_country_data(paper_countries, variant = "untrimmed")
}
names(country_list) <- paper_countries
has <- function(v) sapply(country_list, function(d) all(v %in% names(d)) && all(sapply(v, function(x) any(!is.na(d[[x]])))))
A_countries <- names(which(has("influence_d")))                   # 2-digit occupations released
tenure_countries <- names(which(has("tenure_years")))
B_countries <- names(which(has(paste0(trait_names, "_B"))))        # long personality scales (split halves)

cached <- function(name, expr, prefix = "art_") {
  f <- file.path(results_dir, paste0(prefix, name, ".rds"))
  if (file.exists(f) && !refresh) return(invisible(readRDS(f)))
  t0 <- Sys.time(); val <- expr; saveRDS(val, f)
  message(sprintf("%-22s %6.1f s", name, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(val)
}
rc <- function(fun, countries = paper_countries, data = country_list)
  run_countries(data[countries], fun, n_pv = 10, reps_on_pv = 1)

# ---- complementarity with cognitive skills, education and job demands (Figures 4-6, Table 3) ----
cached("skills_compl", rc(skills_complementarity_estimator()))
cached("split_samples", rc(split_sample_estimator()))                  # returns by education and literacy third
cached("pc_variants",  rc(with_cell_demand(pc_variants_estimator())))
cached("mp_A",         rc(with_cell_demand(all_pairs_estimator("_d", "occupation2")), countries = A_countries))
cached("mp_B",         rc(with_cell_demand(all_pairs_estimator("_z", "occupation"))))
cached("mp_C",         rc(with_cell_demand(all_pairs_estimator("_c", "occupation"))))
cached("mp_nojob_B",   rc(with_cell_demand(all_pairs_nojob_estimator("_z"))))
cached("mp_nojob_A",   rc(with_cell_demand(all_pairs_nojob_estimator("_d")), countries = A_countries))

# ---- sorting into tasks (Figure 7, Table 4) -------------------------------------------------
cached("alloc_own",    rc(allocation_estimator()))
cached("alloc_cell",   rc(with_cell_demand(allocation_estimator(suffix = "_c"))))

if (variant == "main") {
  cached("alloc_job",    rc(allocation_estimator(job_controls = TRUE)))
  cached("alloc_tenure", rc(allocation_estimator(tenure = TRUE), countries = tenure_countries))

  # ---- returns to education and literacy on the same scale ------------------------------------
  cached("yardstick", rc(yardstick_estimator()))

  # ---- return net of education and cognitive skills: main estimate and checks (Appendix Figure A1)
  rhs_hc <- rhs_upto(2)                       # baseline + education + literacy quadratic
  hc_alt <- function(terms) mean_gradient_estimator(paste(c(base_rhs, terms), collapse = " + "))
  cached("m1",            rc(mean_gradient_estimator(rhs_hc)), prefix = "")
  cached("rob_numeracy",  rc(hc_alt(c("education", "numeracy", "I(numeracy^2)"))), prefix = "")
  cached("rob_both",      rc(hc_alt(c("education", "literacy", "I(literacy^2)", "numeracy", "I(numeracy^2)"))), prefix = "")
  cached("rob_untrimmed", rc(mean_gradient_estimator(rhs_hc), data = untrimmed_list), prefix = "")
  cached("rob_nobonus",   rc(with_outcome(mean_gradient_estimator(rhs_hc), "log_wage_nobonus")), prefix = "")
  # split-half instrumental variables (Gillen, Snowberg and Yariv 2019), countries with the long scales
  cached("rob_oriv",      rc(oriv_estimator(controls_upto(2)), countries = B_countries), prefix = "")
  # ---- robustness values of the direct effects (Cinelli and Hazlett 2020; Appendix Figure A2) ----
  cached("rv_M1",         rc(robustness_value_estimator(rhs_hc)), prefix = "")
  cached("rv_full",       rc(robustness_value_estimator(rhs_upto(6))), prefix = "")   # full direct-effect model
}
# ---- the indirect effect through industry: by industry, by sector, without education ----------
# (Appendix Figures A3 and A4, Appendix Tables A8 and A9)
cached("industry_split", rc(industry_split_estimator()))
grp <- function(...) effects_by_group_estimator(list(...))
everyone <- function(d) rep(TRUE, nrow(d))
occ2_countries <- names(which(has("occupation2")))
b_occ2 <- blocks4; b_occ2$job_sorting <- c("occupation2", "sector", "establishment_size")
cached("effects_all",          rc(grp(all = everyone)))
cached("effects_no_education", rc(grp(all = function(d) d$industry != "P")))                  # ISIC section P
cached("effects_sector",       rc(grp(public = function(d) d$sector == "Public", private = function(d) d$sector == "Private")))
cached("effects_occ",          rc(grp(all = everyone), countries = occ2_countries))
cached("effects_occ2",         rc(effects_by_group_estimator(list(all = everyone), b_occ2), countries = occ2_countries))
cached("effects_occ2_noteach", rc(effects_by_group_estimator(list(all = function(d) is.na(d$occupation2) | d$occupation2 != "23"), b_occ2),
                                  countries = occ2_countries))                               # ISCO-08 23: teaching professionals
utils::write.csv(data.frame(country = paper_countries, t(sapply(country_list, function(d) c(
  public = sum(d$sector == "Public"), private = sum(d$sector == "Private"), education = sum(d$industry == "P"),
  teachers = if ("occupation2" %in% names(d)) sum(d$occupation2 == "23", na.rm = TRUE) else 0)))),
  file.path(results_dir, "group_sizes.csv"), row.names = FALSE)

# ---- returns to education with and without the non-cognitive skills -----------------------------
# Country estimates in art_education_return.rds; cross-country means (each country weighted equally,
# se = sqrt(sum se_c^2) / K) in education_return.csv, where share = mean difference / mean premium
# of the version the difference starts from.
edu_ret <- cached("education_return", rc(education_return_estimator()))
edu_long <- country_long(edu_ret); edu_long <- edu_long[is.finite(edu_long$estimate) & is.finite(edu_long$se), ]
edu_mean <- do.call(rbind, lapply(split(edu_long, edu_long$term), function(x) {
  k <- nrow(x); m <- mean(x$estimate); s <- sqrt(sum(x$se^2)) / k
  data.frame(term = x$term[1], measure = sub("\\|.*$", "", x$term[1]), version = sub("^.*\\|", "", x$term[1]),
             estimate = m, se = s, lo = m - z975 * s, hi = m + z975 * s, p = 2 * stats::pnorm(-abs(m / s)),
             countries = k, n_sig = sum(x$p < 0.05)) }))
ref <- sub("^.*-", "", edu_mean$version); is_diff <- grepl("-", edu_mean$version)
edu_mean$share <- ifelse(is_diff, edu_mean$estimate / edu_mean$estimate[match(paste0(edu_mean$measure, "|", ref), edu_mean$term)], NA_real_)
utils::write.csv(edu_mean, file.path(results_dir, "education_return.csv"), row.names = FALSE)

# ---- outputs the documents read as they are -----------------------------------------------------
# Tests of homogeneity across countries (Cochran 1954; Appendix Table A10) and the descriptive
# statistics of Table 1. Rendering never computes: it reads these files.
split_res <- readRDS(file.path(results_dir, "art_industry_split.rds"))
edu_terms <- c("premium|P", "OPEM|P|contrib", "AGRE|P|contrib", "OPEM|P|sorting", "AGRE|P|sorting")
utils::write.csv(do.call(rbind, lapply(edu_terms, function(t) data.frame(term = t, homogeneity_test(split_res, t)))),
                 file.path(results_dir, "homogeneity_education.csv"), row.names = FALSE)
hc <- industry_homogeneity(split_res)
utils::write.csv(data.frame(quantity = c("premium", paste0("sorting|", trait_names)), rejected = c(hc$premium, hc$sorting),
                            tested = length(hc$industries)), file.path(results_dir, "homogeneity_industries.csv"), row.names = FALSE)
wm <- function(x, w) stats::weighted.mean(x, w, na.rm = TRUE)
utils::write.csv(do.call(rbind, lapply(paper_countries, function(cc) {
  d <- country_list[[cc]]; wt <- d$SPFWT0
  data.frame(country = cc, n = nrow(d), women = 100 * wm(d$gender == "Women", wt), tertiary = 100 * wm(as.integer(d$education) >= 4, wt),
             literacy = wm(rowMeans(d[, paste0("PVLIT", 1:10)]), wt), influence = wm(d$influence, wt), organisation = wm(d$organisation, wt),
             learning = wm(d$learning, wt), pressure = wm(d$pressure, wt), cooperation = wm(d$cooperation, wt)) })),
  file.path(results_dir, "descriptives.csv"), row.names = FALSE)

message("Estimates written to ", results_dir)
