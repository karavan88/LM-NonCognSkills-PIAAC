# =============================================================================
# R/02_sample_flow.R
# Sample construction counts for Table 1, computed from the public-use files in
# data/raw/ with the eligibility rules of R/piaac_prepare.R applied one at a time.
# Writes results/sample_flow.csv.
#   Rscript R/02_sample_flow.R            (make flow)
# =============================================================================
here::i_am("R/02_sample_flow.R")
root <- here::here()
source(file.path(root, "R", "piaac_prepare.R"))
cols <- c("AGEG10LFS", "GENDER_R", "EDCAT6_TC1", "C2_D05", "D2_Q04", "EARNHRBONUSC2", "ISCOSKIL4",
          "D2_Q03", "D2_Q07a", "SPFWT0", trait_names, task_items, pv_names)
man <- country_manifest()
flow <- do.call(rbind, lapply(main_countries, function(cc) {
  d <- as.data.frame(haven::read_sav(raw_file(cc), col_select = tidyselect::any_of(cols)))
  d[] <- lapply(d, function(x) suppressWarnings(as.numeric(unclass(x))))
  m <- d$AGEG10LFS %in% 2:4;                                   n_age <- sum(m)
  m <- m & d$C2_D05 %in% 1 & d$D2_Q04 %in% 1;                  n_emp <- sum(m)
  m <- m & is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0 & is.finite(d$SPFWT0) & d$SPFWT0 > 0
  n_wage <- sum(m)
  m <- m & d$GENDER_R %in% 1:2 & d$EDCAT6_TC1 %in% 1:6 &
    Reduce(`&`, lapply(d[c(trait_names, pv_names)], is.finite)); n_skill <- sum(m)
  m <- m & Reduce(`&`, lapply(d[task_items], function(x) x %in% 1:5)); n_task <- sum(m)
  m <- m & d$ISCOSKIL4 %in% 1:4 & d$D2_Q03 %in% 1:3 & d$D2_Q07a %in% 1:6; n_job <- sum(m)
  data.frame(country = cc, all = nrow(d), age = n_age, employee = n_emp, wage = n_wage,
             skills = n_skill, tasks = n_task, job = n_job, final = man$n[man$country == cc])
}))
dir.create(file.path(root, "results"), showWarnings = FALSE)
utils::write.csv(flow, file.path(root, "results", "sample_flow.csv"), row.names = FALSE)
print(flow)
