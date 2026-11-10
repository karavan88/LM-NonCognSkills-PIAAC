# =============================================================================
# R/figures.R
# Figures of the manuscript. Source after country_summaries.R. Every function
# takes results and returns a ggplot (or patchwork) object; save_figure() writes
# PDF (vector) and PNG.
#
# Design rules: one ink colour for country estimates; blue diamond = mean of
# countries; the indirect effects in a five-colour order (cool colours = skill
# mediators, warm = job mediators, violet = direct effect); thin marks; recessive
# grid; the countries in the same alphabetical order in every panel.
# =============================================================================
library(ggplot2)
library(patchwork)

pal <- list(
  ink = "#1f1e1c", ink2 = "#52514e", muted = "#8a8984", grid = "#e7e6e2",
  zero = "#9c9b96", mean = "#2a78d6", median = "#eb6834",
  parts = c("Education" = "#2a78d6", "Cognitive skills" = "#1baf7a",
            "Occupation and employer" = "#eda100", "Tasks" = "#e87ba4",
            "Direct effect" = "#4a3aa7"),
  gender = c(Men = "#4a3aa7", Women = "#1baf7a"),
  div_lo = "#d03b3b", div_mid = "#f0efec", div_hi = "#2a78d6")

trait_label <- c(EXTR = "Extraversion", CONS = "Conscientiousness",
                 OPEM = "Open-mindedness", EMOS = "Emotional stability",
                 AGRE = "Agreeableness")
task_label <- c(influence = "Influencing", organisation = "Organizing",
                learning = "Learning", pressure = "Time pressure",
                cooperation = "Cooperation")
matched_task_label <- c(EXTR = "Influencing", CONS = "Organizing", OPEM = "Learning",
                        EMOS = "Time pressure", AGRE = "Cooperation")
part_label <- c(education = "Education", cognitive = "Cognitive skills",
                job_sorting = "Occupation and employer", tasks = "Tasks", direct = "Direct effect")
country_label <- c(CHL = "Chile", HRV = "Croatia", CZE = "Czechia", EST = "Estonia",
                   FRA = "France", IRL = "Ireland", ITA = "Italy", LVA = "Latvia",
                   LTU = "Lithuania", NZL = "New Zealand", POL = "Poland", ESP = "Spain",
                   BEL = "Belgium (Fl.)", CAN = "Canada", GBR = "England (UK)", HUN = "Hungary",
                   KOR = "Korea", PRT = "Portugal", SGP = "Singapore")
summary_rows <- "Mean"   # summary rows drawn in the forest plots ("Median" is available but unused)
# Legend pieces for the summary rows in use.
sum_cols <- function() c(Mean = pal$mean, Median = pal$median)[summary_rows]
sum_labs <- function(suffix = " (95% CI)") paste0(summary_rows, suffix)
n_sum <- function() length(summary_rows)
lp <- "%"   # unit label: 100 x coefficient, approximate percentage differences

theme_paper <- function(base = 9) {
  theme_minimal(base_size = base, base_family = "sans") %+replace% theme(
    text = element_text(colour = pal$ink),
    axis.text = element_text(colour = pal$ink2, size = base - 1),
    axis.title = element_text(colour = pal$ink2, size = base - 0.5),
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = pal$grid, linewidth = 0.3),
    strip.text = element_text(face = "bold", size = base - 1.4, hjust = 0.5, colour = pal$ink,
                              margin = margin(t = 3, b = 3)),
    strip.clip = "off",
    legend.position = "bottom", legend.title = element_blank(),
    legend.text = element_text(size = base - 1, colour = pal$ink2),
    legend.key.size = unit(9, "pt"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = base, hjust = 0, margin = margin(b = 4)),
    panel.border = element_rect(colour = "#bdbcb6", fill = NA, linewidth = 0.4),
    panel.background = element_rect(fill = "#fbfbfa", colour = NA),
    strip.background = element_rect(fill = "#ecebe7", colour = "#bdbcb6", linewidth = 0.4),
    panel.spacing.x = unit(10, "pt"), panel.spacing.y = unit(8, "pt"),
    plot.margin = margin(6, 8, 4, 4))
}

# Axis labels with a true minus sign (U+2212) and no trailing zeros.
minus_lab <- function(x) sub("^-", "\u2212", format(x, trim = TRUE, drop0trailing = TRUE))
# Cell labels: one decimal, true minus, and no "-0.0".
num1 <- function(x) { s <- formatC(round(x, 1), format = "f", digits = 1)
  s[s == "-0.0"] <- "0.0"; sub("^-", "\u2212", s) }

save_figure <- function(p, name, dir, width = 6.5, height = 4) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".pdf")), p, width = width, height = height,
         device = grDevices::cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), p, width = width, height = height,
         dpi = 300, device = ragg::agg_png, bg = "white")
  invisible(file.path(dir, paste0(name, ".png")))
}

split3 <- function(term, i) sapply(strsplit(term, "|", fixed = TRUE), `[`, i)

# Rows: the countries present, alphabetically (top to bottom), a gap, then Mean, Median.
row_levels <- function(countries) {
  present <- unname(country_label[intersect(names(country_label), countries)])
  rev(c(sort(present), " ", summary_rows))
}
as_rows <- function(country) {
  lab <- ifelse(country %in% names(country_label), country_label[country], country)
  factor(lab, levels = row_levels(unique(country)))
}
# Significance class of a country estimate: p < .05, p < .10, or neither.
sig_class <- function(est, se) {
  z <- abs(est / se)
  ifelse(z >= stats::qnorm(0.975), "p05", ifelse(z >= stats::qnorm(0.95), "p10", "ns"))
}

prep_forest <- function(tab, scale = 100) {
  tab <- tab[!(tab$country %in% c("Mean", "Median")) | tab$country %in% summary_rows, ]   # unused summary rows out
  tab$row <- as_rows(tab$country)
  tab$kind <- ifelse(tab$country %in% summary_rows, tab$country, "Country")
  tab$clear <- ifelse(tab$kind == "Country", sig_class(tab$estimate, tab$se), NA)
  tab[c("estimate", "lo", "hi")] <- scale * tab[c("estimate", "lo", "hi")]
  tab
}

forest_layers <- function(xlab, point_size = 1.5) {
  list(
    geom_vline(xintercept = 0, colour = pal$zero, linewidth = 0.35),
    geom_linerange(aes(xmin = lo, xmax = hi, colour = kind, linewidth = kind)),
    scale_linewidth_manual(values = c(Country = 0.45, Mean = 0.9, Median = 0.9), guide = "none"),
    geom_point(data = function(d) d[d$kind == "Country", ],
               aes(fill = clear), shape = 21, size = point_size, colour = pal$ink, stroke = 0.4),
    geom_point(data = function(d) d[d$kind != "Country", ],
               aes(fill = kind), shape = 23, size = point_size + 0.8, stroke = 0),
    scale_colour_manual(values = c(Country = pal$ink2, sum_cols()), guide = "none"),
    scale_fill_manual(values = c(p05 = pal$ink, p10 = "#a8a7a2", ns = "white", sum_cols()),
                      breaks = c("p05", "p10", "ns", summary_rows),
                      labels = c("Country, p < .05", "Country, p < .10", "Country, n.s.", sum_labs()),
                      drop = FALSE),
    scale_y_discrete(drop = FALSE),
    scale_x_continuous(labels = minus_lab),
    labs(x = xlab, y = NULL),
    guides(fill = guide_legend(nrow = 1, override.aes = list(
      shape = c(21, 21, 21, rep(23, n_sum())), size = c(2.2, 2.2, 2.2, rep(2.6, n_sum())),
      stroke = c(0.4, 0.4, 0.4, rep(0, n_sum())), colour = c(pal$ink, pal$ink, pal$ink, rep(NA, n_sum()))))))
}

# ---- Returns by country --------------------------------------------------------------
fig_returns_by_country <- function(seq_res, spec = "M0",
                                   xlab = "Return per SD of the skill (%)", title = NULL) {
  tab <- country_and_summary(seq_res, paste0(names(trait_label), "|", spec))
  tab$trait <- factor(trait_label[sub("\\|.*", "", tab$term)], levels = trait_label)
  tab <- prep_forest(tab)
  p <- ggplot(tab, aes(estimate, row)) + forest_layers(xlab) +
    facet_wrap(~trait, nrow = 1) + theme_paper()
  if (!is.null(title)) p <- p + labs(title = title)
  p
}

# ---- Generic: one country forest per trait, for any term pattern ----------------------
# terms: named vector trait -> term. scale multiplies estimates (100 = %).
fig_forest_traits <- function(res, terms, xlab, scale = 100, free_x = FALSE, title = NULL) {
  tab <- country_and_summary(res, unname(terms))
  tab$trait <- factor(trait_label[names(terms)[match(tab$term, terms)]], levels = trait_label)
  tab <- prep_forest(tab, scale)
  p <- ggplot(tab, aes(estimate, row)) + forest_layers(xlab) +
    facet_wrap(~trait, nrow = 1, scales = if (free_x) "free_x" else "fixed") + theme_paper()
  if (free_x) p <- p + scale_x_continuous(labels = minus_lab, breaks = scales::breaks_pretty(n = 3))
  if (!is.null(title)) p <- p + labs(title = title)
  p
}
trait_terms <- function(fmt) setNames(sprintf(fmt, names(trait_label)), names(trait_label))
stack_panels <- function(..., heights = NULL) {
  patchwork::wrap_plots(list(...), ncol = 1, heights = heights) +
    patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")
}

# ---- Mediation estimates by country ------------------------------------------------------------
fig_channels_by_country <- function(ch, unit = "%", keep = NULL, mean_label = "Mean", title = NULL, xlab = NULL) {
  terms <- as.vector(outer(paste0("mean|", names(trait_label), "|"), c(names(part_label), "total"), paste0))
  tab <- country_and_summary(ch, terms)
  tab <- tab[tab$country != "Median", ]          # medians do not add up; see the table
  if (!is.null(keep)) tab <- tab[tab$country %in% c(keep, "Mean"), ]
  tab$trait <- factor(trait_label[split3(tab$term, 2)], levels = trait_label)
  tab$part <- split3(tab$term, 3)
  present <- sort(unname(country_label[intersect(names(country_label), tab$country)]))
  tab$row <- factor(ifelse(tab$country == "Mean", mean_label, country_label[tab$country]),
                    levels = rev(c(present, " ", mean_label)))
  bars <- tab[tab$part != "total", ]
  bars$Part <- factor(part_label[bars$part], levels = part_label)
  tot <- tab[tab$part == "total", ]
  ggplot(bars, aes(100 * estimate, row)) +
    geom_vline(xintercept = 0, colour = pal$zero, linewidth = 0.35) +
    geom_col(aes(fill = Part), width = 0.72, colour = "white", linewidth = 0.25) +
    geom_point(data = tot, shape = 21, size = 1.7, fill = "white", colour = pal$ink, stroke = 0.6) +
    scale_fill_manual(values = pal$parts) +
    scale_y_discrete(drop = FALSE) +
    scale_x_continuous(labels = minus_lab, breaks = scales::breaks_pretty(n = 4)) +
    facet_wrap(~trait, nrow = 1, scales = "free_x") +
    labs(x = if (is.null(xlab)) paste0("Indirect and direct effects per SD of the skill (", unit, "); circle = total effect") else xlab,
         y = NULL, title = title) +
    theme_paper() + guides(fill = guide_legend(nrow = 1))
}

# ---- Robustness values ------------------------------------------------------------------
fig_robustness_values <- function(rv) {
  bench <- country_and_summary(rv, c("bench_wage", paste0("bench_trait|", names(trait_label))))
  bm <- bench[bench$country == "Mean", ]
  b_max <- 100 * max(bm$estimate[bm$term == "bench_wage"], bm$estimate[grepl("bench_trait", bm$term)])
  terms <- setNames(paste0("rv|", names(trait_label)), names(trait_label))
  tab <- country_and_summary(rv, unname(terms))
  tab$trait <- factor(trait_label[names(terms)[match(tab$term, terms)]], levels = trait_label)
  tab <- prep_forest(tab); tab$clear[tab$kind == "Country"] <- "ns"   # a robustness value is not a test
  ggplot(tab, aes(estimate, row)) +
    annotate("rect", xmin = 0, xmax = b_max, ymin = -Inf, ymax = Inf, fill = "#e4e3de") +
    forest_layers("Robustness value (% of residual variance in both log wages and the skill)") +
    scale_fill_manual(values = c(ns = "white", sum_cols()), breaks = c("ns", summary_rows),
                      labels = c("Country (95% CI)", sum_labs())) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(
      shape = c(21, rep(23, n_sum())), size = c(2.2, rep(2.6, n_sum())),
      stroke = c(0.4, rep(0, n_sum())), colour = c(pal$ink, rep(NA, n_sum()))))) +
    scale_x_continuous(limits = c(0, NA), oob = scales::oob_squish, expand = expansion(mult = c(0, 0.05)),
                       labels = minus_lab, breaks = scales::breaks_pretty(n = 3)) +
    facet_wrap(~trait, nrow = 1) + theme_paper()
}

# ---- Complementarity with cognitive skills and education ------------------------------------
fig_skills_complementarity <- function(sc, scale = 100, unit = "%") {
  stack_panels(
    fig_forest_traits(sc, trait_terms("%s|x_literacy"), NULL, scale, free_x = TRUE,
                      title = "a. Non-cognitive skill \u00d7 literacy (per SD of literacy)"),
    fig_forest_traits(sc, trait_terms("%s|x_tertiary"),
                      paste0("Change in the return per SD of the skill (", unit, ")"), scale, free_x = TRUE,
                      title = "b. Non-cognitive skill \u00d7 tertiary education"))
}

# ---- Complementarity with job demands ---------------------------------------------------------
fig_interaction_heatmap <- function(res, title, lim = NULL, unit = "%") {
  terms <- as.vector(outer(names(trait_label), names(task_label), paste, sep = ":"))
  tab <- country_and_summary(res, terms)
  tab <- tab[tab$country == "Mean", ]
  tab$trait <- factor(trait_label[sub(":.*", "", tab$term)], levels = rev(trait_label))
  tab$task <- factor(task_label[sub(".*:", "", tab$term)], levels = task_label)
  tab$matched <- match(sub(":.*", "", tab$term), names(trait_label)) ==
                 match(sub(".*:", "", tab$term), names(task_label))
  if (is.null(lim)) lim <- max(abs(100 * tab$estimate)) * 1.05
  ggplot(tab, aes(task, trait)) +
    geom_tile(aes(fill = 100 * estimate), colour = "white", linewidth = 1) +
    geom_tile(data = tab[tab$matched, ], fill = NA, colour = pal$ink, linewidth = 0.6,
              width = 0.94, height = 0.94) +
    geom_text(aes(label = paste0(num1(100 * estimate), ifelse(lo > 0 | hi < 0, "*", ""))),
              size = 2.6, colour = pal$ink) +
    scale_fill_gradient2(low = pal$div_lo, mid = pal$div_mid, high = pal$div_hi,
                         midpoint = 0, limits = c(-lim, lim), labels = minus_lab,
                         name = paste0("Change in the return\nper SD of demand\n(", unit, ")")) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = title) +
    theme_paper() + theme(panel.grid = element_blank(), legend.position = "right",
                          legend.title = element_text(size = 7, colour = pal$ink2),
                          legend.key.height = unit(16, "pt"),
                          axis.text.x.top = element_text(margin = margin(b = 2)))
}

fig_premium_by_country <- function(mp, pc, unit = "%", title = NULL, ncountries = NULL) {
  lab <- c(A = "Match premium:\nco-worker demand,\n2-digit occupation",
           C = "Match premium:\nco-worker demand,\nbroad job cell",
           B = "Match premium:\nown report",
           PCc = "Numeracy × use:\nco-worker demand",
           PCo = "Numeracy × use:\nown report")
  tabs <- lapply(names(mp), function(s) {
    t <- country_and_summary(mp[[s]], "match_premium"); t$spec <- lab[[s]]; t })
  t1 <- country_and_summary(pc, "cell_full"); t1$spec <- lab[["PCc"]]
  t2 <- country_and_summary(pc, "own_full");  t2$spec <- lab[["PCo"]]
  tab <- prep_forest(do.call(rbind, c(tabs, list(t1, t2))))
  tab$spec <- factor(tab$spec, levels = lab)
  ggplot(tab, aes(estimate, row)) +
    forest_layers(paste0("Interaction (", unit, " per SD of skill per SD of demand)")) +
    facet_wrap(~spec, nrow = 1) + labs(title = title) + theme_paper() +
    theme(strip.text = element_text(size = 7, face = "bold", lineheight = 0.9))
}

# ---- Allocation -------------------------------------------------------------------------------
fig_allocation <- function(alloc_own, alloc_cell) {
  m <- setNames(paste0(names(trait_label), "|alloc"), names(trait_label))
  stack_panels(
    fig_forest_traits(alloc_own, m, NULL, 100, free_x = TRUE,
                      title = "a. Own report of the matched task (0\u20131 index \u00d7 100)"),
    fig_forest_traits(alloc_cell, m, "Difference in the matched task per SD of the skill", 100, free_x = TRUE,
                      title = "b. Co-worker demand for the matched task (SD \u00d7 100)")) &
    theme(strip.text = element_text(size = 7.4, face = "bold"))
}

# ---- Robustness of the returns net of education and cognitive skills -------------------------
fig_robustness_checks <- function(checks, unit = "%") {
  rows <- do.call(rbind, lapply(names(checks), function(lab) {
    t <- country_and_summary(checks[[lab]], paste0("mean|", names(trait_label)))
    t <- t[t$country == "Mean", ]; t$check <- lab; t }))
  rows$trait <- factor(trait_label[sub(".*\\|", "", rows$term)], levels = trait_label)
  rows$check <- factor(rows$check, levels = rev(names(checks)))
  rows$main <- rows$check == names(checks)[1]
  ggplot(rows, aes(100 * estimate, check)) +
    geom_vline(xintercept = 0, colour = pal$zero, linewidth = 0.35) +
    geom_vline(data = rows[rows$main, ], aes(xintercept = 100 * estimate),
               colour = pal$mean, linewidth = 0.35, linetype = "22") +
    geom_linerange(aes(xmin = 100 * lo, xmax = 100 * hi), colour = pal$ink2, linewidth = 0.45) +
    geom_point(aes(fill = main), shape = 21, size = 1.8, colour = pal$ink, stroke = 0.4) +
    scale_fill_manual(values = c(`TRUE` = pal$mean, `FALSE` = "white"), guide = "none") +
    facet_wrap(~trait, nrow = 1, scales = "free_x") +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3), labels = minus_lab) +
    labs(x = paste0("Return per SD net of education and cognitive skills (mean of countries, ", unit, ")"),
         y = NULL) + theme_paper() + theme(strip.text = element_text(size = 7.3, face = "bold"))
}

# ---- Sensitivity of the indirect effects (Imai, Keele and Yamamoto 2010), from medsens() ----------
# rho: rho* of the cumulative indirect effect through blocks 1..k, by country (panel a).
# sens: named list (countries) of medsens() grids with columns blocks, rho, acme; panel b shows
# the ACME under each assumed error correlation rho as a share of the ACME at rho = 0.
channel_label <- c(blocks_1to1 = "Education", blocks_1to2 = "+ Cognitive skills",
                   blocks_1to3 = "+ Occupation\nand employer", blocks_1to4 = "+ Tasks")
fig_channel_sensitivity <- function(rho, rho_grid = seq(0, 0.4, by = 0.01), sens) {
  terms <- paste0("rho_star|", names(channel_label))
  tab <- country_and_summary(rho, terms)
  tab$channel <- factor(channel_label[sub("rho_star\\|", "", tab$term)], levels = channel_label)
  tab <- prep_forest(tab, scale = 1); tab$clear[tab$kind == "Country"] <- "ns"   # rho* is not a test
  pa <- ggplot(tab, aes(estimate, row)) +
    geom_vline(xintercept = c(0.1, 0.2, 0.3), colour = "#bdbcb6", linewidth = 0.3, linetype = "22") +
    forest_layers("rho*: error correlation at which the indirect effect through these blocks is zero") +
    scale_fill_manual(values = c(ns = "white", sum_cols()), breaks = c("ns", summary_rows),
                      labels = c("Country", sum_labs(" of countries"))) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(
      shape = c(21, rep(23, n_sum())), size = c(2.2, rep(2.6, n_sum())),
      stroke = c(0.4, rep(0, n_sum())), colour = c(pal$ink, rep(NA, n_sum()))))) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = minus_lab,
                       expand = expansion(mult = c(0, 0.02))) +
    facet_wrap(~channel, nrow = 1) + theme_paper() +
    labs(title = "a. How strong a hidden factor would erase the indirect effect")
  grid <- do.call(rbind, lapply(seq_along(channel_label), function(k) {
    sh <- sapply(sens, function(g) { g <- g[g$blocks == k, ]
      stats::approx(g$rho, g$acme / g$acme[which.min(abs(g$rho))], xout = rho_grid)$y })
    data.frame(channel = factor(channel_label[[k]], levels = channel_label), rho = rho_grid,
               mean = rowMeans(sh), lo = apply(sh, 1, min), hi = apply(sh, 1, max))
  }))
  cols <- setNames(unname(pal$parts[1:4]), levels(tab$channel))
  pb <- ggplot(grid, aes(rho, 100 * mean, colour = channel, fill = channel)) +
    geom_hline(yintercept = 100, colour = pal$zero, linewidth = 0.35) +
    geom_vline(xintercept = c(0.1, 0.2, 0.3), colour = "#bdbcb6", linewidth = 0.3, linetype = "22") +
    geom_ribbon(aes(ymin = 100 * lo, ymax = 100 * hi), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = cols, labels = c("Education", "+ Cognitive skills", "+ Occupation/employer", "+ Tasks")) +
    scale_fill_manual(values = cols, labels = c("Education", "+ Cognitive skills", "+ Occupation/employer", "+ Tasks")) +
    scale_x_continuous(breaks = seq(0, 0.4, 0.1), labels = minus_lab, expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = "Correlation induced by hidden factors (rho)", y = "Share of the indirect effect that remains (%)",
         title = "b. How much of the indirect effect remains under a hidden factor") +
    theme_paper() + theme(panel.grid.major.y = element_line(colour = pal$grid, linewidth = 0.3),
                          plot.title.position = "panel", legend.position = c(0.98, 0.96),
                          legend.justification = c(1, 1), legend.direction = "horizontal",
                          legend.background = element_rect(fill = "#fbfbfa", colour = NA)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE), fill = "none")
  patchwork::wrap_plots(list(pa, pb), ncol = 1, heights = c(1.15, 1)) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}
