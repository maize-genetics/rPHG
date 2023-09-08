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
#  # install.packages("pak")
#  pak::pak("maize-genetics/rPHG")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(rPHG)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  options(java.parameters = c("-Xmx<memory>", "-Xms<memory>"))

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  startLogger(fullPath = NULL, fileName = NULL)

