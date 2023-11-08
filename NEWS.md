## rPHG 0.2.1
* Added new function, `plotDot()`
  + Creates dot plots from `.anchorspro` files
* Added new function, `gvcfMetrics()`
  + Creates summary metrics for a directory of gVCF files


## rPHG 0.2.0
* Unified workflow for both local and server instances
* Added new class, `PHGServerCon`
  + New object for connecting to remote public PHG servers using BrAPI 
    endpoints
* Added new class, `PHGLocalCon`
  + New object for connecting to local SQLite or PostgreSQL database
    instances
* Added new class, `HaplotypeGraph`
  + Wrapper for PHG API Java graph object
* Prior objects now use method dispatch for singular set of methods to return
  relevant PHG data:
  + `PHGMethod()`
  + `showPHGMethods()`
  + `readSamples()`
  + `readRefRanges()`
  + `readHaplotypeIds()`
  + `readPHGDataSet()`
* Updated summary methods:
  + `numHaploPerRefRange()`
  + `calcMutualInfo()`
  + `plotGraph()`
  + `plotMutualInfo()`


## rPHG 0.1.18
* Modified `availablePHGMethods()`:
  + Now returns only method IDs for graphs with more than 100 samples in the
    data
  + This prevents publicly available testing methods to be displayed for
    new PHG users.


## rPHG 0.1.17
* Added new `"DEMO"` method parameter for `PHGMethod()` constructor:
  + specified as `PHGMethod("DEMO")`
  + Will download a small section (25 samples x 1000 reference ranges)
    of the Goodman/Buckler path data
  + Useful for testing and demonstrations


## rPHG 0.1.16
* Added new function, `plotGraph()`
  + Returns a `visNetwork` object for a given section of PHG data filtered
    by reference range
* Updated installation information on README


## rPHG 0.1.15
* Added new function, `taxaByNode()`
  + Returns a `tibble` object of taxa IDs that are found
    in a group of haplotype IDs for a given set of reference
    range IDs
* Update to latest jar files including PHG build


## rPHG 0.1.14
* Update logging function (`startLogger()`)
* Add new tests to expand code coverage
* Deprecate BrAPI calls that use external `khttp` client in PHG jar


## rPHG 0.1.13
* Update to latest jar files including PHG build


## rPHG 0.1.12
* Added BrAPI endpoint methods for obtaining PHG data from BrAPI webservices:
  + `BrapiCon`
  + `BrapiConPHG`
  + `readRefRanges()`
  + `readSamples()`
  + `readTable()`
  + `readPHGDatasetFromBrapi()`
  + `filterRefRanges()`
  + `filterSamples()`
  + `serverInfo()`
  + `availablePHGMethods()`
* Added Kotlin methods for improved data parsing speeds.
* Added new parameters in `graphBuilder()`
  + `buildType` - do you want the graph built from paths or haplotypes?


## rPHG 0.1.11
* Added NEWS file for tracking version updates.
* Added error checks for catching C stack usage errors for configuration file
  checks in functions that require configuration file path parameters.
* Fixed edge cases in `plotNumHaplo()` and `plotMutualInfo()`.
