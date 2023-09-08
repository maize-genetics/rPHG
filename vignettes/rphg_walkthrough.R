## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "figure/graphics-",
  cache.path = "cache/graphics-",
  fig.align = "center",
  external = TRUE,
  echo = TRUE,
  warning = FALSE
)

library(rPHG)
logFile    <- tempfile(fileext = ".txt")
configFile <- tempfile()
rPHG:::createConfigFile(configFile)

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> PHGLocalCon()

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
"demo.hub.maizegenetics.net" |> PHGServerCon()

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> 
    PHGLocalCon() |> 
    showPHGMethods()

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> 
    PHGLocalCon() |> 
    PHGMethod("PATH_METHOD")

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> 
    PHGLocalCon() |> 
    PHGMethod("PATH_METHOD") |> 
    readSamples()

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> 
    PHGLocalCon() |> 
    PHGMethod("PATH_METHOD") |> 
    readRefRanges()

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
configFile |> 
    PHGLocalCon() |> 
    PHGMethod("PATH_METHOD") |> 
    readHaplotypeIds()

## ---- echo=TRUE, eval=TRUE, message=FALSE-------------------------------------
configFile |> 
    PHGLocalCon() |> 
    PHGMethod("PATH_METHOD") |> 
    readPHGDataSet()

