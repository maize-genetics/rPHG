## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    fig.path='figure/graphics-',
    cache.path='cache/graphics-',
    fig.align='center',
    external=TRUE,
    echo=TRUE,
    warning=FALSE
)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  if (!require("devtools")) install.packages("devtools")
#  devtools::install_bitbucket(
#      repo = "bucklerlab/rphg",
#      ref = "master",
#      build_vignettes = TRUE
#  )

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(rPHG)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  options(java.parameters = c("-Xmx<memory>", "-Xms<memory>"))

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  rPHG::startLogger(fullPath = NULL, fileName = NULL)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # Example path location (not run)
#  configPath <- "/home/bm646/Temporary/phg_tests/configSQLite.txt"
#  
#  phgMethods <- rPHG::showPHGMethods(configFile = configPath)
#  phgMethods

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  phgObj <- graphBuilder(
#      configFile = config_path,
#      methods = "GATK_PIPELINE"
#  )

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  phgObj

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # Make a `GRanges` object
#  rr <- SummarizedExperiment::rowRanges(phgObj)
#  rr

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  GenomicRanges::ranges(rr)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  SummarizedExperiment::ranges(phgObj)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  SummarizedExperiment::assays(phgObj)$hapID

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  S4Vectors::metadata(phgObj)$jObj

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  rPHG::numHaploPerRange(phgObject = phgObj)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(magrittr)
#  
#  # (1) Non-pipe example
#  haploPlot <- plotNumHaplo(numHaploPerRange(phgObject = phgObj))
#  
#  # (2) Pipe example. Need to load `magrittr` package first!
#  haploPlot <- phgObj %>%
#      rPHG::numHaploPerRange() %>%
#      rPHG::plotNumHaplo()
#  
#  # Return visualization
#  haploPlot

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(magrittr)
#  
#  phgObj %>%
#      rPHG::plotMutualInfo(
#        refRanges = assays(phgObj)$hapID %>%
#          rownames()
#      )
