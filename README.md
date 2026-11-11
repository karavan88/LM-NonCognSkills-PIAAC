# Returns to non-cognitive skills: do they arise through education, job sorting, or work tasks?

Code that reproduces every number, table and figure of the manuscript from the
PIAAC Cycle 2 public-use files. The study estimates a causal mediation model of
the wage return to each of the 5 non-cognitive skills (Big Five), with 6 blocks of
mediators (education; cognitive skills; experience and tenure; occupation and sector;
industry; job tasks), in 12
countries with continuous hourly earnings and 7 further countries that release
earnings as deciles. The mediation estimates are outputs of `mediation::mediate()`
(Imai, Keele and Yamamoto 2010; Tingley et al. 2014), and their sensitivity
analysis of `medsens()`; the other estimates are
weighted least squares with design-based inference (80 replicate weights, 10
plausible values).

Author: Garen Avanesian. Manuscript under review (2026). The data are not
redistributed here; see *Data*.

## Requirements

- R 4.4 (the study used 4.4.1) with the packages `here`, `haven`, `tidyselect`,
  `mediation` (4.5.1), `AER`, `ggplot2`, `patchwork`, `ragg`, `knitr`, `rmarkdown`.
  `make check` lists what is installed.
- Quarto 1.5 or later; a TeX distribution with XeLaTeX and the TeX Gyre fonts for
  the PDF appendix (the main document renders to Word).
- About 4 GB of disk for the public-use files and 2 hours of computing for the
  mediation runs (see *Run time*).

## Data

Download the PIAAC Cycle 2 public-use files (SPSS format, `PRG<CCC>P2.sav`) from
the OECD, <https://www.oecd.org/en/about/programmes/piaac/piaac-data.html>, for
the 19 countries of the study and extract them into `data/raw/` (subfolders are
fine):

| Sample | Countries |
|---|---|
| Log hourly wages (12) | CHL, CZE, ESP, EST, FRA, HRV, IRL, ITA, LTU, LVA, NZL, POL |
| Earnings decile (the 12 plus 7) | BEL (Flanders), CAN, GBR (England), HUN, KOR, PRT, SGP |

Korea releases the Big Five scores in a separate file, `prg_2korms_big5.sav`,
which comes with its public-use file; place it in `data/raw/` as well. The
preparation script merges it by respondent identifier. Nothing in `data/` is
tracked by git.

## Run

```
make data        # analysis data: data/derived/ (12 countries), data/derived_decile/ (19)
make flow        # sample-construction counts for Table 1
make estimates   # weighted least squares estimates, both samples (CORES=4 by default)
make mediation   # mediate() runs: both samples and 3 alternative blocks
make render      # output/main.docx (main document) and output/appendix.pdf
```

`make all` runs the 5 steps in order. The scripts can also be called directly;
each file header documents its environment variables (`PIAAC_CORES`,
`PIAAC_VARIANT=decile`, `MED_BLOCKS`, `MED_SIMS`, `MED_PV`). The results of
`make estimates` and `make mediation` are included in the repository
(`results*/`), so `make render` works without re-estimating; delete a result
file or set `PIAAC_REFRESH=1` to recompute it.

### Run time

On a laptop with 8 cores: data preparation 1 minute; `make estimates` 15 minutes with 3 cores (9 for the log-wage sample, 6 for the deciles);
the mediation runs are single-threaded and take about 1 hour (log wages), 1.5 hours
(deciles) and 40 to 60 minutes for each of the 3 block variants when run one or two
at a time; running all 5 at once on 16 GB of memory is slower. Rendering takes about 2 minutes.

## What produces what

| Step | Script | Output | Used for |
|---|---|---|---|
| Data | `R/01_prepare_data.R` | `data/derived*/` | all estimates; Table 1 descriptives |
| Flow | `R/02_sample_flow.R` | `results/sample_flow.csv` | Table 1 |
| WLS | `R/03_estimate.R` | `results/art_skills_compl.rds`, `art_pc_variants.rds`, `art_mp_*.rds` | Figures 4 to 6, Table 3 |
| | | `results/art_alloc_*.rds` | Figure 7, Table 4 |
| | | `results/art_industry_split.rds` | Appendix Figure A3 (industry effect by industry) |
| | | `results/m1.rds`, `rob_*.rds`, `rv_M1.rds` | Appendix Figures A1 and A2 |
| | | `results_decile/art_*.rds` | decile panels of Figures 5 and 7, Tables 3 and 4 |
| Mediation | `R/04_mediation.R` | `results_mediation/art_channels.rds` | Figures 1 and 2, Table 2, Appendix Tables A2 and A3 |
| | | `results_mediation/art_rho.rds`, `med_sens_grid.rds` | Figure 3 (sensitivity, ρ*) |
| | | `results_mediation_decile/art_channels.rds` | decile panels, Appendix Tables A4 and A5 |
| | | `results_mediation/art_channels_{sup,occ2,broadA}.rds` | Appendix Table A7 |
| Render | `manuscript/main.qmd`, `appendix.qmd` | `output/` | the documents |

Every result file is a named list (one element per country) of data frames with
columns `term`, `estimate`, `se`. The `_full.rds` files of the mediation runs add
the interval bounds and p-values; `run_info.rds` records the settings.

## How the estimates are computed

- **Sample and variables** (`R/piaac_prepare.R`): employees aged 25 to 54 with
  positive hourly earnings including bonuses; the weighted 1% tails and values
  more than 20 times from the median are dropped within country. The 5
  non-cognitive skills are standardized within country; literacy and numeracy
  enter in 50-point units; tasks are 0 to 1 indices of the PIAAC task items, with
  co-worker demand as leave-one-out cell means.
- **Mediation** (`R/04_mediation.R`): the baseline holds the 5 skills, age group,
  gender, nativity and parental education. The outcome equation adds 6 blocks:
  education; cognitive skills (literacy quadratic); experience and tenure (years of
  paid work and years with the current employer, each with its square); occupation
  and sector (occupation group, private/public/non-profit sector, establishment
  size); industry (21 ISIC Rev. 4 sections); and 5 task indices. Missing experience,
  tenure and industry carry indicator variables, so the sample does not change. For
  each country, the full weighted wage equation gives each block's index
  H_k = X_k β_k. `mediate()` is called with each skill as the treatment (−0.5 to
  +0.5 SD) and each H_k as the mediator, the outcome equation containing all 6
  indices; a second call with the single index of all blocks returns the total and
  direct effects. Literacy and numeracy are the mean of the 10 plausible values; the
  survey weight enters as a precision weight; intervals are quasi-Bayesian (1,000
  draws, seed 20261109).
- **Sensitivity** (`R/04_mediation.R`): `medsens()` takes the index of each cumulative
  set of blocks as a single mediator and recomputes the ACME under an assumed
  correlation ρ between the errors of the mediator and outcome equations (Imai,
  Keele and Yamamoto 2010). It gives ρ*, the correlation at which the ACME is zero,
  and the ACME on a grid of ρ, from which the share remaining at ρ = 0.1, 0.2 and
  0.3 is read. The robustness values of the direct effects (Cinelli and Hazlett 2020)
  are computed in `R/03_estimate.R`.
- **Other estimates** (`R/03_estimate.R`): every other quantity is weighted least
  squares; the point estimate averages the 10 plausible values and the standard
  error combines the replicate-weight variance (Fay's balanced repeated
  replication, k = 0.3; JK2 in France) with the between-plausible-value variance
  by Rubin's rule. Co-worker demand is recomputed inside each replicate.
- **Units**: coefficients are multiplied by 100 and read as approximate
  percentage differences in hourly wages (%); decile coefficients are in
  percentile points.

## Repository layout

```
R/                  pipeline scripts (01 to 04) and the functions they share
manuscript/         main.qmd (anonymous main document), appendix.qmd, references, Word template
results/            WLS results, log-wage sample (committed)
results_decile/     WLS results, earnings-decile sample (committed)
results_mediation/  mediate() results, log-wage sample and block variants (committed)
results_mediation_decile/
data/               raw/ (public-use files, not tracked), derived/, derived_decile/
output/             rendered documents and figures (not tracked)
```

## License

MIT (see `LICENSE`). PIAAC data are subject to the OECD terms of use.
