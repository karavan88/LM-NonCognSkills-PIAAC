# =============================================================================
# R/latex_tables.R
# Tables of the manuscript. In PDF output, ltable() returns booktabs
# LaTeX: a full-width tabular* that Quarto places in a table float with its
# caption (notes are set inside the float, so they move with the table), or a
# longtable for tables longer than a page (notes in the last foot). In other
# formats it returns a pipe table, and table_notes() prints the notes.
#
# Cells are plain text with light markdown: *italic*, `code`, \* for a
# literal asterisk, and @tbl-/@fig- cross-references.
# =============================================================================

# Light markdown -> LaTeX for cells and notes.
md2tex <- function(x) {
  d <- dim(x)
  x <- md2tex_(x); dim(x) <- d; x
}
md2tex_ <- function(x) {
  x <- gsub("\\\\\\*", "\001", x)                                   # protect \*
  x <- gsub("`([^`]+)`", "\\\\texttt{\\1}", x)                       # `code`
  x <- gsub("\\*([^*]+)\\*", "\\\\textit{\\1}", x)                   # *italic*
  x <- gsub("@tbl-([A-Za-z0-9-]+)", "Table~\\\\ref{tbl-\\1}", x)
  x <- gsub("@fig-([A-Za-z0-9-]+)", "Figure~\\\\ref{fig-\\1}", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("\001", "*", x, fixed = TRUE)
  # underscores only inside \texttt{}
  m <- gregexpr("\\\\texttt\\{[^}]*\\}", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(s) gsub("_", "\\\\_", s))
  x
}

# Significance stars as a superscript (LaTeX) or plain asterisks (other formats).
# p < 0.10 is marked with a dagger (marginal evidence).
# options(piaac.stars = "ilr") switches to the economics convention (* .10, ** .05, *** .01).
sig <- function(p, latex = knitr::is_latex_output(), scheme = getOption("piaac.stars", "default")) {
  s <- if (scheme == "ilr")
    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))) else
    ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.10, "+", ""))))
  if (latex) ifelse(s == "", "", ifelse(s == "+", "$^{\\dagger}$", paste0("$^{", s, "}$")))
  else ifelse(s == "+", "\u2020", gsub("\\*", "\\\\*", s))
}

# A multi-line header cell: lines separated by "\n".
hcell <- function(x, align = "c") {
  ifelse(grepl("\n", x),
         paste0("\\begin{tabular}[b]{@{}", align, "@{}}", gsub("\n", "\\\\\\\\{}", x), "\\end{tabular}"),
         x)
}

#' @param body    data.frame/matrix of character cells (first column = stub)
#' @param header  character vector of column headers ("\n" for line breaks)
#' @param align   one letter per column, e.g. "lcccc"
#' @param groups  list of spanner rows; each a list of list(label, from, to)
#' @param panels  named integer vector: panel title -> first body row of the panel
#' @param sub     optional matrix (same shape as body, stub ignored): second line under each row
#' @param space_before body rows preceded by a small vertical space
#' @param notes   notes text (light markdown)
#' @param long    use longtable (for tables longer than a page)
#' @param colspec optional explicit LaTeX column specification (overrides align)
ltable <- function(body, header, align, groups = NULL, panels = NULL, sub = NULL,
                   space_before = integer(0), notes = NULL, long = FALSE, colspec = NULL,
                   tabcolsep = NULL, latex = knitr::is_latex_output()) {
  body <- as.matrix(body); storage.mode(body) <- "character"; body[is.na(body)] <- ""
  if (!is.null(sub)) { sub <- as.matrix(sub); storage.mode(sub) <- "character"; sub[is.na(sub)] <- "" }
  k <- ncol(body)
  if (!latex && knitr::pandoc_to("docx"))
    return(docx_table(body, header, align, groups, panels, sub, space_before))
  if (!latex) return(pipe_fallback(body, header, panels, sub, groups))

  body <- md2tex(body); if (!is.null(sub)) sub <- md2tex(sub)
  header <- md2tex(header)
  spec <- if (!is.null(colspec)) colspec else
    paste0("@{\\extracolsep{\\fill}}", align, "@{}")
  row <- function(cells) paste0(paste(cells, collapse = " & "), " \\\\")

  head <- "\\toprule"
  for (g in groups) {
    cells <- character(0); rules <- character(0); col <- 1
    for (s in g) {
      if (s$from > col) cells <- c(cells, rep("", s$from - col))
      w <- s$to - s$from + 1
      cells <- c(cells, sprintf("\\multicolumn{%d}{c}{%s}", w, hcell(md2tex(s$label))))
      rules <- c(rules, sprintf("\\cmidrule(lr){%d-%d}", s$from, s$to))
      col <- s$to + 1
    }
    if (col <= k) cells <- c(cells, rep("", k - col + 1))
    head <- c(head, row(cells), paste(rules, collapse = " "))
  }
  head <- c(head, row(mapply(function(h, a) hcell(h, if (a == "l") "l" else "c"),
                             header, strsplit(gsub("[^lcr]", "", align), "")[[1]][seq_len(k)])),
            "\\midrule")

  lines <- character(0)
  for (i in seq_len(nrow(body))) {
    if (!is.null(panels) && i %in% panels) {
      title <- md2tex(names(panels)[panels == i])
      if (i > 1) lines <- c(lines, "\\addlinespace[4pt]")
      lines <- c(lines, sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\", k, title),
                 "\\addlinespace[1pt]")
    } else if (i %in% space_before) lines <- c(lines, "\\addlinespace[3pt]")
    lines <- c(lines, row(body[i, ]))
    if (!is.null(sub) && any(nzchar(sub[i, -1]))) lines <- c(lines, row(c("", sub[i, -1])))
  }

  note_tex <- if (!is.null(notes)) md2tex(notes) else NULL
  sep <- if (!is.null(tabcolsep)) sprintf("\\setlength{\\tabcolsep}{%gpt}", tabcolsep) else NULL
  out <- if (!long) {
    c("\\footnotesize\\setstretch{1.0}", sep,
      sprintf("\\begin{tabular*}{\\linewidth}{%s}", spec), head, lines, "\\bottomrule",
      "\\end{tabular*}",
      if (!is.null(note_tex))
        c("\\par\\vspace{4pt}",
          paste0("\\begin{minipage}{\\linewidth}\\footnotesize\\setstretch{1.0}", note_tex,
                 "\\end{minipage}")))
  } else {
    # Notes follow the longtable (it does not float); \LTpost is set small in the preamble.
    c(sprintf("\\begin{longtable}{%s}", spec), head, "\\endhead",
      sprintf("\\midrule \\multicolumn{%d}{r}{\\textit{Continued on next page}} \\\\", k), "\\endfoot",
      "\\bottomrule", "\\endlastfoot", lines, "\\end{longtable}",
      if (!is.null(note_tex))
        paste0("\\begin{nolinenumbers}{\\footnotesize\\setstretch{1.0}\\noindent ", note_tex,
               "\\par}\\end{nolinenumbers}"))
  }
  knitr::asis_output(paste(out, collapse = "\n"))
}

# Pipe table for HTML and other formats: second lines are appended to the cells,
# panel titles become italic rows, spanners are prefixed to the column headers.
pipe_fallback <- function(body, header, panels, sub, groups) {
  if (!is.null(sub)) body[, -1] <- trimws(paste(body[, -1], sub[, -1]))
  header <- gsub("\n", " ", header)
  for (g in groups) for (s in g)
    header[s$from:s$to] <- paste0(gsub("\n", " ", s$label), ": ", header[s$from:s$to])
  out <- body
  if (!is.null(panels)) {
    out <- NULL
    for (i in seq_len(nrow(body))) {
      if (i %in% panels) out <- rbind(out, c(paste0("*", names(panels)[panels == i], "*"),
                                             rep("", ncol(body) - 1)))
      out <- rbind(out, body[i, ])
    }
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE); names(out) <- header
  knitr::kable(out, format = "pipe", row.names = FALSE,
               align = c("l", rep("r", ncol(out) - 1)))
}

# Notes under a table in non-LaTeX formats (in LaTeX they are inside the float).
table_notes <- function(notes, latex = knitr::is_latex_output())
  if (latex) "" else if (knitr::pandoc_to("docx")) paste0("\n::: {custom-style=\"Table Note\"}\n", notes, "\n:::\n") else notes

# Word: a booktabs-style flextable with spanners, italic panel rows, second lines
# as separate rows, and superscript significance marks. Notes follow as text.
docx_font <- "Times New Roman"
docx_table <- function(body, header, align, groups, panels, sub, space_before) {
  plain <- function(x) {
    x <- gsub("\\\\\\*", "*", x)                       # \* -> *
    x <- gsub("`([^`]+)`", "\\1", x)
    x <- gsub("&emsp;", "\u2003\u2003", x, fixed = TRUE)
    x <- gsub("(^|[^*])\\*([^*]+)\\*($|[^*])", "\\1\\2\\3", x)   # *italic* -> plain
    x
  }
  k <- ncol(body)
  rows <- list(); kind <- character(0); gap <- logical(0)
  for (i in seq_len(nrow(body))) {
    if (!is.null(panels) && i %in% panels) {
      rows[[length(rows) + 1]] <- c(names(panels)[panels == i], rep("", k - 1))
      kind <- c(kind, "panel"); gap <- c(gap, i > 1)
    }
    rows[[length(rows) + 1]] <- body[i, ]
    kind <- c(kind, "body"); gap <- c(gap, i %in% space_before && !(i %in% panels))
    if (!is.null(sub) && any(nzchar(sub[i, -1]))) {
      rows[[length(rows) + 1]] <- c("", sub[i, -1]); kind <- c(kind, "sub"); gap <- c(gap, FALSE)
    }
  }
  df <- as.data.frame(do.call(rbind, lapply(rows, plain)), stringsAsFactors = FALSE)
  names(df) <- paste0("V", seq_len(k))
  ft <- flextable::flextable(df)
  ft <- flextable::delete_part(ft, "header")
  ft <- flextable::add_header_row(ft, values = plain(header), colwidths = rep(1, k), top = TRUE)
  for (g in rev(groups)) {
    vals <- character(0); wid <- integer(0); col <- 1
    for (sp in g) {
      if (sp$from > col) { vals <- c(vals, rep("", sp$from - col)); wid <- c(wid, rep(1L, sp$from - col)) }
      vals <- c(vals, plain(gsub("\n", " ", sp$label))); wid <- c(wid, sp$to - sp$from + 1L); col <- sp$to + 1
    }
    if (col <= k) { vals <- c(vals, rep("", k - col + 1)); wid <- c(wid, rep(1L, k - col + 1)) }
    ft <- flextable::add_header_row(ft, values = vals, colwidths = wid, top = TRUE)
  }
  # superscript significance marks
  for (j in 2:k) for (i in seq_len(nrow(df))) {
    v <- df[i, j]; m <- regmatches(v, regexec("^(.*?)([*\u2020]+)$", v))[[1]]
    if (length(m) == 3)
      ft <- flextable::compose(ft, i = i, j = j, part = "body",
              value = flextable::as_paragraph(m[2], flextable::as_sup(m[3])))
  }
  al <- strsplit(gsub("[^lcr]", "", align), "")[[1]][seq_len(k)]
  al <- c(l = "left", c = "center", r = "right")[al]
  for (j in seq_len(k)) ft <- flextable::align(ft, j = j, align = al[j], part = "all")
  thin <- officer::fp_border(color = "black", width = 0.5)
  thick <- officer::fp_border(color = "black", width = 1.25)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = thick, part = "header")
  ft <- flextable::hline_bottom(ft, border = thin, part = "header")
  ft <- flextable::hline_bottom(ft, border = thick, part = "body")
  if (length(groups)) for (sp in groups[[1]])
    ft <- flextable::hline(ft, i = 1, j = sp$from:sp$to, border = thin, part = "header")
  pr <- which(kind == "panel")
  for (i in pr) ft <- flextable::merge_at(ft, i = i, j = 1:k, part = "body")
  if (length(pr)) ft <- flextable::italic(ft, i = pr, part = "body")
  if (length(pr)) ft <- flextable::align(ft, i = pr, align = "left", part = "body")
  ft <- flextable::font(ft, fontname = docx_font, part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::padding(ft, padding.top = 0.5, padding.bottom = 0.5, padding.left = 2,
                           padding.right = 2, part = "all")
  if (any(gap)) ft <- flextable::padding(ft, i = which(gap), padding.top = 6, part = "body")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  # Fixed column widths on a 6.5-inch text block, proportional to the longest
  # entry in each column (header lines included); panel titles span the table.
  hd <- strsplit(plain(header), "\n")
  need <- sapply(seq_len(k), function(j) {
    cells <- df[[j]][kind != "panel"]
    max(nchar(cells), nchar(hd[[j]]), 4) })
  wd <- 0.064 * need + 0.12
  wd <- wd * min(1, 6.5 / sum(wd)); wd <- wd * 6.5 / sum(wd)
  ft <- flextable::width(ft, j = seq_len(k), width = wd)
  ft <- flextable::hrule(ft, rule = "auto", part = "all")
  ft <- flextable::set_table_properties(ft, layout = "fixed", opts_word = list(split = TRUE))
  ft <- flextable::paginate(ft, init = FALSE, hdr_ftr = TRUE)   # header stays with the first rows
  ft
}
