# Cox-type models for interval-censored data

To install this package, download the source into a folder `coxinterval` and run from the command line
```
R CMD build coxinterval
R CMD check coxinterval_0.1-1.tar.gz
R CMD INSTALL coxinterval_0.1-1.tar.gz
```
Instead of `R CMD INSTALL`, the package can be loaded and unloaded in an R session with
```
library(coxinterval, lib.loc = "coxinterval.Rcheck")
detach(package:coxinterval, unload = TRUE)
```
respectively.

This R package requires the callable C library from [IBM ILOG CPLEX Optimization Studio](https://www14.software.ibm.com/webapp/iwm/web/reg/signup.do?source=scholars), version 12.5 or higher and, with Windows, the 32-bit environment. Available `Makevars` files should correctly find default installations of CPLEX on Linux, Mac OS X and Windows systems. A custom installation directory for CPLEX should be specified in DOS/POSIX format with the `CPLEXDIR` environment variable. CPLEX ships with multiple library formats. By default `coxinterval` uses the first one returned by `ls`. To override this, provide the DOS/POSIX path to the desired library using the `CPLEXLIBDIR` environment variable.

Installation of `coxinterval` with 32-bit CPLEX on a 64-bit Windows system can be achieved by replacing the last two `R` commands with
```
R --arch=i386 CMD check --no-multiarch coxinterval_0.1-1.tar.gz
R --arch=i386 CMD INSTALL --no-multiarch coxinterval_0.1-1.tar.gz
```
Note however that the package is available only for 32-bit R.
