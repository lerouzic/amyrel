# amyrel

## Reproducing the results

1. Getting the tabular text datafiles
 * Locate into the 'data' directory
 * Run compet-clean.R, devtime-clean.R, lifespan-clean.R, mortality-clean.R
2. Obtaining figures and tables
 * Locate into the 'scripts' directory
 * Run compet.R, devtime.R, lifespan.R, mortality.R, weight.R
 * The results will be in the figures/ directory

## Directory organization

* **/data**: All the original datafiles (Microsoft Excel sheets) and scripts to turn them into tabular text files for the analysis.
  | Excel sheet | R script | Tabular text file(s) |
  |-------------|----------|----------------------|
  | compet.xlsx | compet-clean.R | compet.txt |
  | devtime.xlsx | devtime-clean.R | devtime.txt |
  | lifespan.xlsx | lifespan-clean.R | lifespan.txt |
  | mortality.xlsx | mortality-clean.R | mortality.txt, mortality-weight.txt |

* **/scripts**: Analysis scripts and helper functions.
  Two files (colors.R and model-evol.R) contain constants and functions to be used by other files.
  | R script | Corresponding data set | Figures produced | Tables produced |
  |----------|------------------------|------------------|-----------------|
  | compet.R | data/compet.txt | figures/compet-chi2.pdf, | figures/compet-model.txt |
  |          |                 | figures/compet-param.txt, | |
  |          |                 | figures/compet-timeseries-C.pdf, | |
  |          |                 | figures/compet-timeseries-G.pdf, | |
  |          |                 | figures/compet-timeseries-S.pdf  | |
  | devtime.R | data/devtime.txt | figures/devtime-bxpall.pdf, | figures/devtime-model.txt |
  |           |                  | figures/devtime-bxpline.pdf, | |
  |           |                  | figures/devtime-bxprep.pdf   | |
  | lifespan.R | data/lifespan.txt | figures/lifespan-dist.pdf | figures/lifespan-model.txt, |
  |          |                   | figures/lifespan-tscox.pdf | figures/lifespan-modelcox.txt |
  |          |                  | figures/lifespan-ts.pdf | |
  | mortality.R | data/mortality.txt | figures/mortality-hatched.pdf, | figures/mortality-hatched.txt, |
  |          |                  | figures/mortality-surv.pdf     | figures/mortality-surv.txt |
  | weight.R | data/mortality-weight.txt | figures/weight-bxp.pdf | figures/weight-aov.txt |
  
* **/figures**: empty directory, the location where figures and tables will be located

## List of necessary software/packages

* R version 4.1.2
 * Package 'here' v 1.0.1
 * Package 'gtools' v 3.9.5
 * Package 'lme4' v 1.1-35.5
 * Package 'multcomp' v 1.4-25
 * Package 'survival' v 3.7.0
 * Package 'bbmle' v 1.0.25.1
 * Package 'readxl' v 1.4.3
 
