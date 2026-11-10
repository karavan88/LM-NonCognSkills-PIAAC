# =============================================================================
# R/04_mediation.R
# Causal mediation estimates with mediation::mediate() (Imai, Keele and Yamamoto
# 2010; Imai and Yamamoto 2013; Tingley et al. 2014): the total effect of each
# non-cognitive skill on log hourly wages, its indirect effects through 4 mediator
# blocks (education, cognitive skills, occupation and employer, tasks) and its
# direct effect, country by country.
#
#   Rscript R/04_mediation.R                              log-wage sample, 12 countries
#   PIAAC_VARIANT=decile Rscript R/04_mediation.R         earnings-decile sample, 19 countries
#   MED_BLOCKS=sup|occ2|broadA Rscript R/04_mediation.R   alternative occupation and employer blocks
#                                                         (Appendix Table A7)
#
# Environment: MED_SIMS (quasi-Bayesian draws, default 1000), MED_PV ("mean" of the
# 10 plausible values, default, or "1" for PV1), MED_RHO_SIMS (medsens draws, 200).
#
# For each country: the full wage equation (skills + baseline + 4 blocks) gives, for
# every block k, the index H_k = X_k beta_k (its fitted contribution). Mediator
# equations: H_k ~ skills + baseline. Outcome equation: log wage ~ skills + baseline
# + H_1 + ... + H_4. mediate(treat = skill, mediator = H_k) returns the average
# causal mediation effect through block k; a second model with the single index
# H_all = sum of the H_k returns the total effect and the average direct effect.
# Models are weighted least squares with the final survey weight (SPFWT0). Intervals
# are mediate()'s quasi-Bayesian intervals (the survey weight enters as a precision
# weight), not the replicate-weight variance used elsewhere in the study.
# Output: results_mediation/art_channels[_<blocks>].rds, a named list (countries) of
# data frames with columns term ("mean|SKILL|total", "...|direct", "...|<block>"),
# estimate, se; the _full.rds files add the interval bounds and p-values.
# Sensitivity to unobserved confounding of the mediators (log-wage sample, main blocks):
# medsens() on the cumulative indices gives rho*, the error correlation at which the
# indirect effect is zero (art_rho.rds), and the indirect effect under each assumed
# rho (med_sens_grid.rds). run_info.rds records the settings.
# =============================================================================

here::i_am("R/04_mediation.R")
root <- here::here()
source(file.path(root, "R", "piaac_prepare.R"))
source(file.path(root, "R", "estimators.R"))
stopifnot(requireNamespace("mediation", quietly = TRUE))

variant  <- Sys.getenv("PIAAC_VARIANT", "main")
sims     <- as.integer(Sys.getenv("MED_SIMS", "1000"))
rho_sims <- as.integer(Sys.getenv("MED_RHO_SIMS", "200"))
pv_mode  <- Sys.getenv("MED_PV", "mean")
# MED_BLOCKS: "main" (the 4 blocks), "sup" (supervisory responsibility added to the occupation
# and employer block), "occ2" (2-digit occupations instead of the 4 groups, countries that release
# them), "broadA" (the 4 blocks in those same countries). Appendix Table A7.
blocks_mode <- Sys.getenv("MED_BLOCKS", "main")
out_dir  <- file.path(root, if (variant == "decile") "results_mediation_decile" else "results_mediation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_name <- if (blocks_mode == "main") "art_channels" else paste0("art_channels_", blocks_mode)
if (blocks_mode == "sup")  blocks4$job_sorting <- c(blocks4$job_sorting, "supervisor")
if (blocks_mode == "occ2") blocks4$job_sorting <- c("occupation2", "sector", "establishment_size")

if (variant == "decile") {
  ddir <- file.path(root, "data", "derived_decile")
  paper_countries <- utils::read.csv(file.path(ddir, "countries.csv"))$country
  country_list <- load_country_data(paper_countries, dir = ddir)
} else {
  paper_countries <- main_countries
  country_list <- load_country_data(paper_countries)
}
names(country_list) <- paper_countries
if (blocks_mode %in% c("occ2", "broadA"))   # countries that release 2-digit occupations
  paper_countries <- paper_countries[sapply(country_list[paper_countries], function(d)
    "occupation2" %in% names(d) && any(!is.na(d$occupation2)))]

set_cognitive <- function(d) {
  # literacy and numeracy in 50-point units centred at 250, from PV1 or the mean of the 10 PVs
  if (pv_mode == "1") return(set_pv(d, 1))
  d$literacy <- (rowMeans(d[, paste0("PVLIT", 1:10)]) - 250) / 50
  d$numeracy <- (rowMeans(d[, paste0("PVNUM", 1:10)]) - 250) / 50
  d
}

full_rhs <- paste(c(base_rhs, unlist(blocks4)), collapse = " + ")
H_names  <- paste0("H_", names(blocks4))

block_indices <- function(d) {
  # H_k = X_k beta_k from the full weighted wage equation, one column per block.
  m <- wls(paste("log_wage ~", full_rhs), d)
  X <- stats::model.matrix(m); beta <- stats::coef(m); beta[is.na(beta)] <- 0
  tl <- attr(stats::terms(m), "term.labels"); asg <- attr(X, "assign")
  for (k in names(blocks4)) {
    cols <- which(asg > 0 & tl[pmax(asg, 1)] %in% blocks4[[k]])
    d[[paste0("H_", k)]] <- drop(X[, cols, drop = FALSE] %*% beta[cols])
  }
  d$H_all <- rowSums(d[, H_names])
  d
}

med_row <- function(term, est, draws, ci, p)
  data.frame(term = term, estimate = est, se = stats::sd(draws), lo = ci[1], hi = ci[2], p = p,
             stringsAsFactors = FALSE)

one_country <- function(d, cc) {
  d <- set_cognitive(d)
  d <- complete_for(d, full_rhs, c("log_wage", "SPFWT0")); d$.w <- d$SPFWT0
  d <- block_indices(d)
  m_med <- lapply(stats::setNames(H_names, H_names), function(h) wls(paste(h, "~", base_rhs), d))
  m_out <- wls(paste("log_wage ~", base_rhs, "+", paste(H_names, collapse = " + ")), d)
  m_med_all <- wls(paste("H_all ~", base_rhs), d)
  m_out_all <- wls(paste("log_wage ~", base_rhs, "+ H_all"), d)
  rows <- list(); add_check <- c()
  for (tr in trait_names) {
    set.seed(20261109)
    ma <- mediation::mediate(m_med_all, m_out_all, treat = tr, mediator = "H_all",
                             control.value = -0.5, treat.value = 0.5, sims = sims)
    rows[[length(rows) + 1]] <- med_row(paste("mean", tr, "total", sep = "|"), ma$tau.coef, ma$tau.sims, ma$tau.ci, ma$tau.p)
    rows[[length(rows) + 1]] <- med_row(paste("mean", tr, "direct", sep = "|"), ma$z0, ma$z0.sims, ma$z0.ci, ma$z0.p)
    acme_sum <- 0
    for (k in names(blocks4)) {
      set.seed(20261109)
      mk <- mediation::mediate(m_med[[paste0("H_", k)]], m_out, treat = tr, mediator = paste0("H_", k),
                               control.value = -0.5, treat.value = 0.5, sims = sims)
      rows[[length(rows) + 1]] <- med_row(paste("mean", tr, k, sep = "|"), mk$d0, mk$d0.sims, mk$d0.ci, mk$d0.p)
      acme_sum <- acme_sum + mk$d0
    }
    add_check[tr] <- acme_sum - ma$d0     # the 4 block ACMEs add up to the ACME through H_all
  }
  out <- do.call(rbind, rows)
  # rho*: medsens() on the cumulative indices, one skill (rho* does not depend on the skill); main blocks only.
  # The full sensitivity grid is kept: the ACME under each assumed error correlation rho, with its interval.
  sens <- if (blocks_mode != "main") NULL else lapply(seq_along(blocks4), function(k) {
    d$Hc <- rowSums(d[, H_names[seq_len(k)], drop = FALSE])
    mm <- wls(paste("Hc ~", base_rhs), d); mo <- wls(paste("log_wage ~", base_rhs, "+ Hc"), d)
    set.seed(20261109)
    md <- mediation::mediate(mm, mo, treat = "CONS", mediator = "Hc", control.value = -0.5, treat.value = 0.5, sims = rho_sims)
    s <- mediation::medsens(md, rho.by = 0.01, effect.type = "indirect", sims = rho_sims)
    list(rho_star = s$err.cr.d[1],
         grid = data.frame(blocks = k, rho = s$rho, acme = s$d0, lo = s$lower.d0, hi = s$upper.d0))
  })
  rho <- if (is.null(sens)) rep(NA_real_, length(blocks4)) else sapply(sens, `[[`, "rho_star")
  message(sprintf("%s  n = %5d  max |sum ACME_k - ACME_all| = %.2e  rho* = %s", cc, nrow(d),
                  max(abs(add_check)), paste(sprintf("%.2f", rho), collapse = " ")))
  list(channels = out, additivity = add_check,
       rho = data.frame(term = paste0("rho_star|blocks_1to", seq_along(blocks4)), estimate = rho, se = NA_real_),
       sens_grid = if (is.null(sens)) NULL else do.call(rbind, lapply(sens, `[[`, "grid")))
}

t0 <- Sys.time()
res <- lapply(paper_countries, function(cc) one_country(country_list[[cc]], cc))
names(res) <- paper_countries
message(sprintf("done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

channels_full <- lapply(res, `[[`, "channels")
channels <- lapply(channels_full, function(x) x[, c("term", "estimate", "se")])   # term, estimate, se: the shape of every result file
saveRDS(channels, file.path(out_dir, paste0(out_name, ".rds")))
saveRDS(channels_full, file.path(out_dir, paste0(out_name, "_full.rds")))
if (blocks_mode == "main") {
  saveRDS(lapply(res, `[[`, "rho"), file.path(out_dir, "art_rho.rds"))          # rho* by country (Figure 3a)
  saveRDS(lapply(res, `[[`, "sens_grid"), file.path(out_dir, "med_sens_grid.rds"))  # ACME under each rho (Figure 3b)
  saveRDS(list(variant = variant, sims = sims, rho_sims = rho_sims, pv = pv_mode, date = Sys.time(),
               mediation_version = as.character(utils::packageVersion("mediation"))),
          file.path(out_dir, "run_info.rds"))
}
