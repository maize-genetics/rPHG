# rPHG <img src="man/figures/logo.png" align="right" width="120" />

[![Life Cycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![R-CMD-check](https://github.com/maize-genetics/rPHG/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/maize-genetics/rPHG/actions/workflows/check-standard.yaml) [![codecov](https://codecov.io/gh/maize-genetics/rPHG/branch/master/graph/badge.svg?token=4D0JSKT0UC)](https://codecov.io/gh/maize-genetics/rPHG) [![DOI](https://img.shields.io/badge/Bioinformatics-10.1093%2Fbioinformatics%2Fbtac410-brightgreen)](https://doi.org/10.1093/bioinformatics/btac410)

## Objective
The main goal of developing this package is to construct an R-based front-end to connect to the Practical Haplotype Graph - a general, graph-based, computational framework for genotype inference.

## Citation
To cite `rPHG`, please use the following citation:

> Bradbury et al. (2022). The Practical Haplotype Graph, a platform for storing and using pangenomes for imputation. Bioinformatics, 38(15), 3698–3702, https://doi.org/10.1093/bioinformatics/btac410

## Installation
If you do not have experience working with and setting up `rJava` with your R installation, *it is recommended that you read the long-form documentation*. This walkthrough can be found [here](https://rphg.maizegenetics.net/articles/rphg_walkthrough.html). If you are already fairly comfortable working with Java JDK and `rJava`, you can follow the following commands.

Package source code can be installed directly from this BitBucket repository using the `devtools` package:

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_bitbucket(repo = "bucklerlab/rPHG")
```
