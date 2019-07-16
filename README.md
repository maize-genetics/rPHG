## rPHG

### Objective
The main goal of developing this package is to construct an R-based front-end to connect to the Practical Haplotype Graph - a general, graph-based, computational framework for genotype inference.

### Installation
If you do not have experience working with and setting up `rJava` with your R installation, *it is recommended that you read the long-form documentation*. This walkthrough can be found [here](https://bitbucket.org/tasseladmin/tassel-5-source/wiki/rtassel_walkthrough.md). If you are already fairly comfortable working with Java JDK and `rJava`, you can follow the following commands.

Package source code can be installed directly from this BitBucket repository using the `remotes` package:

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_bitbucket(repo = "bucklerlab/rPHG")
```
