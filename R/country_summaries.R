# =============================================================================
# R/country_summaries.R
# Country-level reporting. Every result is a named list (one element per
# country) of data frames with columns term, estimate, se. For each term the
# manuscript reports each country's estimate with a 95% interval and the
# cross-country MEAN (each country weighted equally), with a 95% interval from
# se = sqrt(sum se_c^2) / K (countries are independent samples). The interval
# treats the countries as fixed: it describes sampling uncertainty about the
# average of THESE countries. Means keep identities: the indirect and direct
# effects add up to the total effect. A Median row is also computed (its
# interval by simulation) but the manuscript does not use it (summary_rows).
# =============================================================================

z975 <- stats::qnorm(0.975)

country_long <- function(results, terms = NULL) {
  out <- do.call(rbind, lapply(names(results), function(cc) {
    r <- results[[cc]]
    if (!is.null(terms)) r <- r[r$term %in% terms, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    data.frame(country = cc, term = r$term, estimate = r$estimate, se = r$se,
               stringsAsFactors = FALSE)
  }))
  out$lo <- out$estimate - z975 * out$se
  out$hi <- out$estimate + z975 * out$se
  out$p  <- 2 * stats::pnorm(-abs(out$estimate / out$se))
  out
}

median_ci <- function(est, se, n_sim = 20000, seed = 20261003) {
  # The interval of the median is simulated only when the document reports a Median row (summary_rows);
  # otherwise only the median itself is returned, which keeps rendering fast.
  if (exists("summary_rows") && !"Median" %in% get("summary_rows")) return(c(estimate = stats::median(est), se = NA_real_, lo = NA_real_, hi = NA_real_))
  # Estimates reported without standard errors (rho* from medsens) have no interval.
  if (!all(is.finite(se))) return(c(estimate = stats::median(est), se = NA_real_, lo = NA_real_, hi = NA_real_))
  set.seed(seed)
  k <- length(est)
  draws <- matrix(stats::rnorm(n_sim * k, rep(est, each = n_sim), rep(se, each = n_sim)),
                  nrow = n_sim)
  med <- apply(draws, 1, stats::median)
  c(estimate = stats::median(est), se = stats::sd(med),
    lo = unname(stats::quantile(med, 0.025)), hi = unname(stats::quantile(med, 0.975)))
}

cross_country <- function(long) {
  # long: output of country_long(). Returns Mean and Median rows per term.
  do.call(rbind, lapply(split(long, long$term), function(x) {
    k <- nrow(x); m <- mean(x$estimate); s <- sqrt(sum(x$se^2)) / k
    md <- median_ci(x$estimate, x$se)
    wi <- 1 / x$se^2; Q <- sum(wi * (x$estimate - sum(wi * x$estimate) / sum(wi))^2)
    i2 <- if (is.finite(Q) && Q > 0) max(0, (Q - (k - 1)) / Q) else 0
    clear_pos <- sum(x$lo > 0); clear_neg <- sum(x$hi < 0)
    rbind(
      data.frame(country = "Mean", term = x$term[1], estimate = m, se = s,
                 lo = m - z975 * s, hi = m + z975 * s, p = 2 * stats::pnorm(-abs(m / s)),
                 k = k, I2 = i2, n_pos = clear_pos, n_neg = clear_neg,
                 n_same_sign = sum(sign(x$estimate) == sign(m))),
      data.frame(country = "Median", term = x$term[1], estimate = md[["estimate"]],
                 se = md[["se"]], lo = md[["lo"]], hi = md[["hi"]], p = NA_real_,
                 k = k, I2 = i2, n_pos = clear_pos, n_neg = clear_neg,
                 n_same_sign = sum(sign(x$estimate) == sign(md[["estimate"]]))))
  }))
}

country_and_summary <- function(results, terms = NULL) {
  long <- country_long(results, terms)
  long$k <- NA; long$I2 <- NA; long$n_pos <- NA; long$n_neg <- NA; long$n_same_sign <- NA
  rbind(long, cross_country(long))
}

homogeneity_test <- function(results, term) {
  # Cochran's (1954) Q: are the country estimates of one term equal? Chi-square with K - 1 df.
  x <- country_long(results, term); w <- 1 / x$se^2
  Q <- sum(w * (x$estimate - sum(w * x$estimate) / sum(w))^2)
  list(Q = Q, df = nrow(x) - 1, p = stats::pchisq(Q, nrow(x) - 1, lower.tail = FALSE))
}

industry_homogeneity <- function(split, min_share = 0.02, level = 0.05) {
  # Descriptive count over industries: in how many is equality across countries rejected (Cochran's Q)
  # for the industry wage differential and for the effect of each skill on working in the industry?
  # split: industry_split_estimator() results. Industries with a mean employment share of at least
  # min_share and an estimate in every country.
  K <- length(split); has <- function(term) sum(sapply(split, function(x) any(x$term == term))) == K
  secs <- setdiff(unique(unlist(lapply(split, function(x) sub("^premium[|]", "", grep("^premium[|]", x$term, value = TRUE))))), "Not reported")
  secs <- secs[sapply(secs, function(s) has(paste0("premium|", s)))]
  share <- sapply(secs, function(s) mean(sapply(split, function(x) x$estimate[x$term == paste0("share|", s)])))
  secs <- secs[share >= min_share]
  rejected <- function(terms) sum(sapply(terms, function(t) homogeneity_test(split, t)$p < level))
  list(industries = secs, premium = rejected(paste0("premium|", secs)),
       sorting = sapply(trait_names, function(t) rejected(paste0(t, "|", secs, "|sorting"))))
}
