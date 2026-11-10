# Reproduces the study from the PIAAC Cycle 2 public-use files (see README.md).
#
#   make data        prepare the analysis data (data/derived/, data/derived_decile/)
#   make flow        sample-construction counts for Table 1 (results/sample_flow.csv)
#   make estimates   weighted least squares estimates, both samples (results/, results_decile/)
#   make mediation   mediation::mediate() estimates, both samples and 3 block variants
#   make render      main document (docx) and appendix (pdf) into output/
#   make all         everything above, in order
#   make check       confirm that R, Quarto and the R packages are available

CORES ?= 4
UTF8 := LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

.PHONY: all data flow estimates mediation render check
all: data flow estimates mediation render

data:
	Rscript R/01_prepare_data.R

flow:
	Rscript R/02_sample_flow.R

estimates:
	PIAAC_CORES=$(CORES) Rscript R/03_estimate.R
	PIAAC_VARIANT=decile PIAAC_CORES=$(CORES) Rscript R/03_estimate.R

mediation:
	Rscript R/04_mediation.R
	PIAAC_VARIANT=decile Rscript R/04_mediation.R
	MED_BLOCKS=sup Rscript R/04_mediation.R
	MED_BLOCKS=occ2 Rscript R/04_mediation.R
	MED_BLOCKS=broadA Rscript R/04_mediation.R

render:
	cd manuscript && $(UTF8) quarto render main.qmd --to docx
	cd manuscript && $(UTF8) quarto render appendix.qmd --to pdf

check:
	Rscript -e 'for (p in c("here", "haven", "tidyselect", "mediation", "AER", "ggplot2", "patchwork", "ragg", "knitr", "rmarkdown")) cat(sprintf("%-12s %s\n", p, tryCatch(as.character(packageVersion(p)), error = function(e) "MISSING")))'
	quarto --version
