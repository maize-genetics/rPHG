## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "figure/graphics-",
  cache.path = "cache/graphics-",
  fig.align = "center",
  external = TRUE,
  echo = TRUE,
  warning = FALSE
)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  if (!require("devtools")) install.packages("devtools")
#  devtools::install_github(repo = "maize-genetics/rPHG")

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
#    configFile = config_path,
#    methods = "GATK_PIPELINE"
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
#  qTaxa <- taxaByNode(phgObj, start = 1, end = 35000, seqnames = "1")
#  head(qTaxa)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(dplyr)
#  qTaxa |>
#      filter(hap_id == "112") |>
#      pull(taxa_id) |>
#      unlist()

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # Get taxa set in reference ranges 1 and 5
#  taxaByNode(phgObj, rrSet = c(1, 5))

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  phgObj |>
#      plotGraph(
#          seqnames = 1,
#          start = 1000,
#          end = 100000,
#          samples = c("Z001E0001", "Z001E0002", "Z001E0004", "Z001E0096")
#      )

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  phgObj |>
#      plotGraph(
#          seqId = 1,
#          start = 1000,
#          end   = 100000,
#          samples = c("Z001E0001", "Z001E0002", "Z001E0004", "Z001E0096"),
#          sampleHighlight = "Z001E0001"
#      )

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  phgObj |>
#      plotGraph(
#          seqId = 1,
#          start = 1000,
#          end   = 100000,
#          samples = c("Z001E0001", "Z001E0002", "Z001E0004", "Z001E0096"),
#          sampleHighlight = "Z001E0001",
#          colMajor = "#4287f5",
#          colMinor = "#818ea3"
#      )

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
#    rPHG::numHaploPerRange() %>%
#    rPHG::plotNumHaplo()
#  
#  # Return visualization
#  haploPlot

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(magrittr)
#  
#  phgObj %>%
#    rPHG::plotMutualInfo(
#      refRanges = assays(phgObj)$hapID %>%
#        rownames()
#    )

