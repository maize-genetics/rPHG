> [!note]
> With the release of [PHGv2](https://github.com/maize-genetics/phg_v2), this project is now **superseded**. Please use [rPHG2](https://github.com/maize-genetics/rPHG2/) instead.

# rPHG <img src="man/figures/logo.png" align="right" width="120"/>

[![Life Cycle Status](https://img.shields.io/badge/lifecycle-superseded-red.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![R-CMD-check](https://github.com/maize-genetics/rPHG/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/maize-genetics/rPHG/actions/workflows/check-standard.yaml) [![codecov](https://codecov.io/gh/maize-genetics/rPHG/branch/master/graph/badge.svg?token=4D0JSKT0UC)](https://codecov.io/gh/maize-genetics/rPHG) [![DOI](https://img.shields.io/badge/Bioinformatics-10.1093%2Fbioinformatics%2Fbtac410-brightgreen)](https://doi.org/10.1093/bioinformatics/btac410)

## Objective

`rPHG` is a system to interact with and retrieve information from a Practical Haplotype Graph (PHG) - a general, graph-based, computational framework for genotype inference. This is accomplished by leveraging the [Breeding](https://brapi.org/) and [PHG](https://bitbucket.org/bucklerlab/practicalhaplotypegraph) APIs.

## Citation

To cite `rPHG`, please use the following citation:

> Bradbury et al. (2022). The Practical Haplotype Graph, a platform for storing and using pangenomes for imputation. Bioinformatics, 38(15), 3698--3702, <https://doi.org/10.1093/bioinformatics/btac410>

## Installation

If you do not have experience working with and setting up `rJava` with your R installation, *it is recommended that you read the long-form documentation*. This walkthrough can be found [here](https://rphg.maizegenetics.net/articles/rphg_walkthrough.html). If you are already fairly comfortable working with Java JDK and `rJava`, you can follow the following commands.

Package source code can be installed directly from this BitBucket repository using the `devtools` package:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github(repo = "maize-genetics/rPHG")
```


