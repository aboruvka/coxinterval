# R package for Cox models with interval-censored data

To install run from the source parent directory

```
R CMD build coxinterval
R CMD check coxinterval_<current version number>.tar.gz
```

and from the R terminal

```
library(coxinterval, lib.loc = "~/papers/code/coxinterval.Rcheck")
```
