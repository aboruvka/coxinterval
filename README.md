# R package for Cox models with interval-censored data

Download source to some directory `<path to source>/coxinterval`
To install run from the command line

```
R CMD build coxinterval
R CMD check coxinterval_0.1-1.tar.gz
```

and from the R terminal

```
library(coxinterval, lib.loc = "<path to source>/coxinterval.Rcheck")
```

