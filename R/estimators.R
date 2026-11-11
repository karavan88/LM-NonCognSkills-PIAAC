# =============================================================================
# R/estimators.R
# Estimators of the study. Source R/piaac_prepare.R first.
#
# Every estimator has the signature fun(d, w) and returns a NAMED NUMERIC VECTOR,
# so that with_uncertainty() can wrap it: the point estimate averages the 10
# plausible values of literacy and numeracy, the standard error combines the
# replicate-weight sampling variance (Fay's BRR, k = 0.3; JK2 in France) with the
# between-plausible-value variance by Rubin's rule. The mediation estimates
# themselves are computed by mediation::mediate() in R/04_mediation.R.
#
# Columns of a prepared country data set (see prepare_country()):
#   log_wage; EXTR CONS OPEM EMOS AGRE; age_band gender education
#   occupation sector establishment_size
#   influence organisation learning pressure cooperation (+ _z, _c, _d versions)
#   SPFWT0 and the replicate weights SPFWT1..SPFWTnn the country uses
#   PVLIT1..10, PVNUM1..10, PVAPS1..10
#   born_in_country mother_education father_education (with "Not reported")
# Optional, by country: occupation2, tenure_years, numeracy_use, EXTR_B ... AGRE_B
#   (second three-item halves of the long personality scales), supervisor.
# =============================================================================

# ---- specification: baseline controls and the 6 mediator blocks --------------
# The baseline holds characteristics fixed before labour-market entry; the 5
# non-cognitive skills are always entered jointly.
base_rhs <- paste(
  "EXTR + CONS + OPEM + EMOS + AGRE + age_band + gender +",
  "born_in_country + mother_education + father_education"
)
blocks4 <- list(
  education   = "education",                          # six ISCED-based categories
  cognitive   = c("literacy", "I(literacy^2)"),       # assessed literacy, 50-point units
  experience  = c("exper", "I(exper^2)", "exper_miss", "tenure", "I(tenure^2)", "tenure_miss"),
  job_sorting = c("occupation", "sector", "establishment_size"),   # occupation and workplace
  industry    = "industry",                                        # ISIC Rev. 4 section
  tasks       = task_names)

rhs_upto <- function(k, base = base_rhs, b = blocks4) {
  # Right-hand side with the first k blocks: k = 0 baseline only; 2 adds education
  # and cognitive skills; 4 adds all mediator blocks.
  paste(c(base, unlist(b[seq_len(k)])), collapse = " + ")
}

controls_upto <- function(k) {
  # rhs_upto(k) without the 5 skill terms (for the instrumental-variable check).
  sub("^EXTR \\+ CONS \\+ OPEM \\+ EMOS \\+ AGRE \\+ ", "", rhs_upto(k))
}

# ---- weighted least squares, complete cases and plausible values ---------------
set_pv <- function(d, i) {
  d$literacy <- (d[[paste0("PVLIT", i)]] - 250) / 50
  d$numeracy <- (d[[paste0("PVNUM", i)]] - 250) / 50
  if (paste0("PVAPS", i) %in% names(d))
    d$aps <- (d[[paste0("PVAPS", i)]] - 250) / 50
  d
}

complete_for <- function(d, rhs, extra = NULL) {
  v <- unique(c(all.vars(as.formula(paste("~", rhs))), extra))
  v <- intersect(v, names(d))
  droplevels(d[stats::complete.cases(d[, v, drop = FALSE]), , drop = FALSE])
}

usable_formula <- function(formula_chr, d) {
  # Factor terms that have a single level in this data set (for example "born
  # in country" among Polish men, or a subgroup defined by the factor itself)
  # are dropped from the formula.
  f <- stats::as.formula(formula_chr)
  vars <- intersect(all.vars(f), names(d))
  single <- vars[vapply(vars, function(v) is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(single)) {
    tl <- attr(stats::terms(f), "term.labels")
    drop <- vapply(tl, function(t) any(strsplit(t, ":", fixed = TRUE)[[1]] %in% single), logical(1))
    f <- stats::reformulate(tl[!drop], response = deparse(f[[2]]))
  }
  f
}

wls <- function(formula_chr, d) {
  # d must contain the weight column .w
  stats::lm(usable_formula(formula_chr, d), data = d, weights = .w)
}

# ---- uncertainty: replicate weights x plausible values -----------------------
replicate_names <- function(d) grep("^SPFWT[1-9][0-9]*$", names(d), value = TRUE)

with_uncertainty <- function(d, fun, n_pv = 10,
                             method = attr(d, "ve_method"), fay = attr(d, "fay"),
                             reps_on_pv = 1) {
  # fun(d, w) -> named numeric vector.
  # Sampling variance from replicate weights (on the PVs listed in reps_on_pv,
  # the OECD shortcut uses the first PV); imputation variance across all PVs.
  # The replication method (Fay's BRR or a jackknife) and Fay's factor are read
  # from the prepared data; France uses JK2, everyone else Fay with k = 0.3.
  # Subsetting a data frame with `[` drops attributes, so fall back to the
  # design columns that prepare_country() keeps (VEMETHOD, VEFAYFAC).
  if (is.null(method)) method <- if ("VEMETHOD" %in% names(d)) tolower(as.character(d$VEMETHOD[1])) else "fay"
  if (is.null(fay) || !is.finite(fay)) fay <- if ("VEFAYFAC" %in% names(d) && is.finite(d$VEFAYFAC[1])) d$VEFAYFAC[1] else 0.3
  method <- match.arg(method, c("fay", "jk2", "jk1"))
  reps <- replicate_names(d)
  R <- length(reps)
  scale <- switch(method, fay = 1 / (R * (1 - fay)^2), jk2 = 1, jk1 = (R - 1) / R)

  pv_list <- lapply(seq_len(n_pv), function(i) fun(set_pv(d, i), "SPFWT0"))
  by_pv <- matrix(unlist(pv_list), ncol = n_pv,
                  dimnames = list(names(pv_list[[1]]), NULL))
  est <- rowMeans(by_pv)
  B <- if (n_pv > 1) apply(by_pv, 1, stats::var) else rep(0, nrow(by_pv))

  U_list <- lapply(reps_on_pv, function(i) {
    di <- set_pv(d, i)
    rep_est <- matrix(unlist(lapply(reps, function(r) fun(di, r))),
                      nrow = nrow(by_pv))
    scale * rowSums((rep_est - by_pv[, i])^2)
  })
  U <- rowMeans(matrix(unlist(U_list), nrow = nrow(by_pv)))
  V <- U + (1 + 1 / n_pv) * B
  data.frame(term = names(est), estimate = est, se = sqrt(V),
             row.names = NULL, stringsAsFactors = FALSE)
}

# ---- complementarity: skill x literacy, skill x tertiary, skill x job demand ------
complementarity_rhs <- function(suffix = "_z", cell_fe = "occupation",
                                all_pairs = FALSE) {
  tk <- paste0(task_names, suffix)
  inter <- if (all_pairs) {
    paste0("(", paste(trait_names, collapse = " + "), ") * (",
           paste(tk, collapse = " + "), ")")
  } else paste(paste0(trait_names, " * ", tk), collapse = " + ")
  paste(c(inter, "age_band + gender + born_in_country + mother_education",
          "father_education + education + literacy + I(literacy^2)",
          "exper + I(exper^2) + exper_miss + tenure + I(tenure^2) + tenure_miss", cell_fe, "industry + sector + establishment_size"), collapse = " + ")
}

pick_interaction <- function(cf, trait, task) {
  nm <- intersect(c(paste0(trait, ":", task), paste0(task, ":", trait)), names(cf))
  cf[[nm]]
}

all_pairs_estimator <- function(suffix = "_z", cell_fe = "occupation") {
  # All 25 trait x demand interactions plus the match-premium contrast.
  rhs <- complementarity_rhs(suffix, cell_fe, all_pairs = TRUE)
  function(d, w) {
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d))
    out <- c()
    for (tr in trait_names) for (tk in task_names)
      out[paste(tr, tk, sep = ":")] <- pick_interaction(cf, tr, paste0(tk, suffix))
    is_match <- names(out) %in% paste(trait_names, matched_task[trait_names], sep = ":")
    c(out, matched_mean = mean(out[is_match]), mismatched_mean = mean(out[!is_match]),
      match_premium = mean(out[is_match]) - mean(out[!is_match]))
  }
}

# ---- complementarity between non-cognitive skills, cognitive skills and education ----
# Interactions of each trait with assessed literacy (within-country SD units)
# and with tertiary education, on top of the full education-and-literacy
# controls. Positive coefficients indicate complements.
skills_complementarity_estimator <- function() {
  inter <- paste(c(paste0(trait_names, ":lit_z"), paste0(trait_names, ":tertiary")), collapse = " + ")
  rhs <- paste(base_rhs, "+ education + literacy + I(literacy^2) +", inter)
  function(d, w) {
    d$lit_z <- wstd(d$literacy, d[[w]])
    d$tertiary <- as.numeric(as.integer(d$education) >= 4)
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d))
    c(stats::setNames(sapply(trait_names, function(tr) pick_interaction(cf, tr, "lit_z")),
                      paste0(trait_names, "|x_literacy")),
      stats::setNames(sapply(trait_names, function(tr) pick_interaction(cf, tr, "tertiary")),
                      paste0(trait_names, "|x_tertiary")))
  }
}

# ---- yardstick: returns to education and literacy on the same scale -----------------
yardstick_estimator <- function() {
  rhs <- paste(base_rhs, "+ education + lit_z")
  function(d, w) {
    d$lit_z <- wstd(d$literacy, d[[w]])
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d))
    g <- function(nm) if (nm %in% names(cf) && is.finite(cf[[nm]])) cf[[nm]] else 0
    c(literacy_sd = cf[["lit_z"]],
      bachelor_vs_upper = g("educationBachelor") - g("educationUpper secondary"),
      stats::setNames(cf[trait_names], paste0("trait|", trait_names)))
  }
}

# ---- positive control: numeracy x numeracy use under several control sets ------------
pc_variants_estimator <- function() {
  job <- "+ exper + I(exper^2) + exper_miss + tenure + I(tenure^2) + tenure_miss + occupation + industry + sector + establishment_size"
  specs <- list(
    own_full   = list(use = "numeracy_use",   rhs = paste("+ education", job)),
    own_nojob  = list(use = "numeracy_use",   rhs = "+ education"),
    own_none   = list(use = "numeracy_use",   rhs = ""),
    cell_full  = list(use = "numeracy_use_c", rhs = paste("+ education", job)),
    cell_nojob = list(use = "numeracy_use_c", rhs = "+ education"))
  function(d, w) {
    sapply(specs, function(s) {
      rhs <- paste("numeracy * .use +", base_rhs, s$rhs)
      dd <- d; dd$.use <- dd[[s$use]]
      dd <- complete_for(dd, rhs, c("log_wage", w)); dd$.w <- dd[[w]]
      dd$.use <- wstd(dd$.use, dd$.w)
      cf <- stats::coef(wls(paste("log_wage ~", rhs), dd))
      pick_interaction(cf, "numeracy", ".use")
    })
  }
}

# ---- match premium without job controls --------------------------------------------
all_pairs_nojob_estimator <- function(suffix = "_z") {
  tk <- paste0(task_names, suffix)
  rhs <- paste0("(", paste(trait_names, collapse = " + "), ") * (", paste(tk, collapse = " + "), ")",
                " + age_band + gender + born_in_country + mother_education + father_education",
                " + education + literacy + I(literacy^2)")
  function(d, w) {
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d))
    out <- c()
    for (tr in trait_names) for (k in task_names)
      out[paste(tr, k, sep = ":")] <- pick_interaction(cf, tr, paste0(k, suffix))
    is_match <- names(out) %in% paste(trait_names, matched_task[trait_names], sep = ":")
    c(out, matched_mean = mean(out[is_match]), mismatched_mean = mean(out[!is_match]),
      match_premium = mean(out[is_match]) - mean(out[!is_match]))
  }
}

# ---- co-worker numeracy use (leave-one-out cell mean, full-sample weight) ------------
add_numeracy_use_cell <- function(d, w = "SPFWT0", min_n = 15) {
  m <- rep(NA_real_, nrow(d))
  for (cl in c("job_cell", "occ_sector", "occupation")) {
    fill <- is.na(m)
    m[fill] <- loo_cell_mean(d$numeracy_use, d[[cl]], d[[w]], min_n)[fill]
  }
  d$numeracy_use_c <- m
  d
}

# ---- co-worker demand re-estimated inside every replicate --------------------------------
# The leave-one-out cell means (and their within-country standardization) are
# recomputed with the weight of the current replicate, so that the replicate
# variance includes the sampling error of the demand measures themselves.
with_cell_demand <- function(fun) {
  function(d, w) {
    d <- add_cell_demand(d, cell = c("job_cell", "occ_sector", "occupation"), w = w, suffix = "_c")
    if ("occupation2" %in% names(d) && any(!is.na(d$occupation2)))
      d <- add_cell_demand(d, cell = "occupation2", w = w, suffix = "_d")
    d <- add_numeracy_use_cell(d, w = w)
    fun(d, w)
  }
}

# ---- sorting into tasks ---------------------------------------------------------------
# Matched task regressed on the skills; the tenure interaction distinguishes selection
# into tasks from jobs shaping reported personality.
allocation_estimator <- function(job_controls = FALSE, tenure = FALSE, suffix = "") {
  # suffix = ""   : the worker's own task report (0-1 index) is the outcome
  # suffix = "_c" : demand of the job cell (what co-workers report, z-score)
  function(d, w) {
    out <- c()
    for (tr in trait_names) {
      tk <- paste0(matched_task[[tr]], suffix)
      rhs <- paste(base_rhs, "+ education + literacy + I(literacy^2)")
      if (job_controls) rhs <- paste(rhs, "+ occupation + industry + sector + establishment_size")
      if (tenure) rhs <- paste(rhs, "+", paste0(tr, ":tenure_years"), "+ tenure_years")
      dd <- complete_for(d, rhs, c(tk, w)); dd$.w <- dd[[w]]
      cf <- stats::coef(wls(paste(tk, "~", rhs), dd))
      out[paste(tr, "alloc", sep = "|")] <- cf[[tr]]
      if (tenure) out[paste(tr, "x_tenure", sep = "|")] <-
        pick_interaction(cf, tr, "tenure_years")
    }
    out
  }
}

# ---- robustness: return net of education and cognitive skills ------------------------
mean_gradient_estimator <- function(rhs, focal = trait_names) {
  # Weighted mean regression of log wages; returns "mean|TRAIT" coefficients.
  function(d, w) {
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d))
    setNames(cf[focal], paste("mean", focal, sep = "|"))
  }
}

wrss <- function(fit) sum(stats::weights(fit) * stats::resid(fit)^2)

robustness_value_estimator <- function(rhs, focal = trait_names,
                                       bench = c("mother_education", "father_education")) {
  # Cinelli and Hazlett (2020). For each trait: partial R2 of the trait in the
  # wage equation, f2 = R2 / (1 - R2), RV = (sqrt(f2^2 + 4 f2) - f2) / 2.
  # Benchmarks: partial R2 of parental education (jointly) in the wage
  # equation and in each trait's equation on the remaining covariates.
  # Everything is a ratio of weighted residual sums of squares, so it runs
  # under the replicate weights like any other estimator.
  function(d, w) {
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    tl <- attr(stats::terms(stats::as.formula(paste("~", rhs))), "term.labels")
    full <- wls(paste("log_wage ~", rhs), d); rss_full <- wrss(full)
    out <- c()
    for (tr in focal) {
      r_no <- wrss(wls(paste("log_wage ~", paste(setdiff(tl, tr), collapse = " + ")), d))
      r2 <- (r_no - rss_full) / r_no; f2 <- r2 / (1 - r2)
      out[paste0("rv|", tr)] <- 0.5 * (sqrt(f2^2 + 4 * f2) - f2)
      out[paste0("r2_wage|", tr)] <- r2
      t_full <- wrss(wls(paste(tr, "~", paste(setdiff(tl, tr), collapse = " + ")), d))
      t_no   <- wrss(wls(paste(tr, "~", paste(setdiff(tl, c(tr, bench)), collapse = " + ")), d))
      out[paste0("bench_trait|", tr)] <- (t_no - t_full) / t_no
    }
    r_nob <- wrss(wls(paste("log_wage ~", paste(setdiff(tl, bench), collapse = " + ")), d))
    out["bench_wage"] <- (r_nob - rss_full) / r_nob
    out
  }
}

with_outcome <- function(fun, outcome) {
  # Wraps an estimator so that `outcome` (e.g. "log_wage_nobonus") replaces log_wage.
  function(d, w) { d$log_wage <- d[[outcome]]; fun(d, w) }
}

oriv_estimator <- function(controls_rhs, a = trait_names,
                           b = paste0(trait_names, "_B"), demand_suffix = NULL) {
  # Obviously-related IV (Gillen, Snowberg and Yariv 2019): each three-item half
  # instruments the other; stacked, with replicate weights providing inference.
  # With demand_suffix (e.g. "_d"), the matched trait x demand interactions are
  # instrumented by (other half) x demand; demand itself is taken as error-free.
  function(d, w) {
    dem <- if (is.null(demand_suffix)) NULL else paste0(matched_task[trait_names], demand_suffix)
    d <- complete_for(d, controls_rhs, c("log_wage", a, b, dem, w))
    mk <- function(x, z, id) {
      s <- d; s[paste0(trait_names, "_x")] <- d[x]; s[paste0(trait_names, "_z")] <- d[z]
      s$.rep <- id; s
    }
    s <- rbind(mk(a, b, 0), mk(b, a, 1)); s$.w <- s[[w]]
    x_terms <- paste0(trait_names, "_x"); z_terms <- paste0(trait_names, "_z")
    if (!is.null(dem)) {
      x_terms <- c(x_terms, dem, paste0(trait_names, "_x:", dem))
      z_terms <- c(z_terms, dem, paste0(trait_names, "_z:", dem))
    }
    f <- stats::as.formula(paste(
      "log_wage ~", paste(x_terms, collapse = " + "), "+ .rep +", controls_rhs,
      "|", paste(z_terms, collapse = " + "), "+ .rep +", controls_rhs))
    cf <- stats::coef(AER::ivreg(f, data = s, weights = .w))
    out <- setNames(cf[paste0(trait_names, "_x")], trait_names)
    if (!is.null(dem))
      out <- c(out, setNames(cf[paste0(trait_names, "_x:", dem)],
                             paste0(trait_names, ":", dem)))
    out
  }
}

# ---- pooling across countries --------------------------------------------------------
pool_countries <- function(est, se, labels = names(est)) {
  # DerSimonian-Laird random effects; the pooled estimate is the primary result.
  k <- length(est); v <- se^2; wi <- 1 / v
  fixed <- sum(wi * est) / sum(wi)
  Q <- sum(wi * (est - fixed)^2)
  tau2 <- max(0, (Q - (k - 1)) / (sum(wi) - sum(wi^2) / sum(wi)))
  ws <- 1 / (v + tau2)
  mu <- sum(ws * est) / sum(ws); se_mu <- sqrt(1 / sum(ws))
  list(pooled = mu, se = se_mu, z = mu / se_mu,
       ci = mu + c(-1, 1) * stats::qnorm(0.975) * se_mu,
       prediction = mu + c(-1, 1) * stats::qt(0.975, max(k - 2, 1)) * sqrt(tau2 + se_mu^2),
       tau2 = tau2, I2 = max(0, (Q - (k - 1)) / Q), Q = Q, k = k)
}

run_countries <- function(country_list, fun, cores = getOption("piaac.cores", 1L), ...) {
  # Applies with_uncertainty() to every prepared country data set, one country
  # per core when cores > 1 (forking; fine on macOS and Linux from Rscript).
  worker <- function(cc) with_uncertainty(country_list[[cc]], fun, ...)
  cores <- min(as.integer(cores), length(country_list))
  out <- if (cores > 1L) parallel::mclapply(names(country_list), worker, mc.cores = cores,
                                             mc.preschedule = FALSE)
         else lapply(names(country_list), worker)
  bad <- vapply(out, inherits, logical(1), "try-error")
  if (any(bad)) stop("Estimation failed in: ", paste(names(country_list)[bad], collapse = ", "),
                     "\n", out[[which(bad)[1]]])
  setNames(out, names(country_list))
}

pool_terms <- function(results) {
  # results: named list (countries) of with_uncertainty() data frames.
  terms <- Reduce(intersect, lapply(results, function(r) r$term))
  do.call(rbind, lapply(terms, function(tm) {
    est <- sapply(results, function(r) r$estimate[r$term == tm])
    se  <- sapply(results, function(r) r$se[r$term == tm])
    p <- pool_countries(est, se)
    data.frame(term = tm, pooled = p$pooled, se = p$se,
               p = 2 * stats::pnorm(-abs(p$z)), tau2 = p$tau2, I2 = p$I2,
               pred_low = p$prediction[1], pred_high = p$prediction[2], k = p$k)
  }))
}

# ---- industry indirect effect split by industry -------------------------------------------------
# The ACME through the industry block is a sum over industries of a_jm * b_m: a_jm is the effect of
# skill j on working in industry m (linear probability, given the baseline), b_m the industry's wage
# premium in the full outcome equation. Premia are measured relative to the employment-weighted
# average job, so each term reads as "share shifted into industry m times its premium"; because the
# a_jm sum to zero over industries, the terms still add up to the industry ACME.
industry_split_estimator <- function(b = blocks4) {
  rhs <- paste(c(base_rhs, unlist(b)), collapse = " + ")
  function(d, w) {
    d <- complete_for(d, rhs, c("log_wage", w)); d$.w <- d[[w]]
    cf <- stats::coef(wls(paste("log_wage ~", rhs), d)); lev <- levels(d$industry)
    prem <- stats::setNames(c(0, cf[paste0("industry", lev[-1])]), lev); prem[is.na(prem)] <- 0
    share <- tapply(d$.w, d$industry, sum)[lev] / sum(d$.w)
    prem <- prem - sum(share * prem)
    out <- c()
    for (l in lev) {
      d$.y <- as.numeric(d$industry == l)
      a <- stats::coef(wls(paste(".y ~", base_rhs), d))[trait_names]
      out[paste0(trait_names, "|", l, "|contrib")] <- a * prem[[l]]
      out[paste0(trait_names, "|", l, "|sorting")] <- a
      out[paste0("premium|", l)] <- prem[[l]]
      out[paste0("share|", l)] <- share[[l]]
    }
    out
  }
}
