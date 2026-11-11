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
  # ---- the industry indirect effect split by industry (Appendix figure) --------------------------
  cached("industry_split", run_countries(country_list, industry_split_estimator(), n_pv = 10, reps_on_pv = 1))
  # ---- robustness values of the direct effects (Cinelli and Hazlett 2020; Appendix Figure A2) ----
  cached("rv_M1",         rc(robustness_value_estimator(rhs_hc)), prefix = "")
  cached("rv_full",       rc(robustness_value_estimator(rhs_upto(6))), prefix = "")   # full direct-effect model
}
message("Estimates written to ", results_dir)
