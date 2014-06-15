## coxinterval

*An R package for Cox-type models with interval-censored data*

### Requirements

Required software for coxinterval is summarized as follows. System-specific notes for items 1 and 2 are provided below.

1. Development tools for R
2. C API for CPLEX available with [IBM ILOG CPLEX Optimization Studio 12.5+](http://www-01.ibm.com/software/commerce/optimization/cplex-optimizer/). Members of [IBM's Academic Initiative](http://www-304.ibm.com/ibm/university/academic/pub/page/academic_initiative) can obtain CPLEX at [no charge](https://www.ibm.com/developerworks/community/blogs/jfp/entry/cplex_studio_in_ibm_academic_initiative?lang=en).
3. Contributed R package [timereg](http://cran.r-project.org/web/packages/timereg/index.html)

coxinterval also depends on the Matrix, parallel and survival packages from the standard R library.

##### Unix-alike

1. Install the r-base-dev package. For further details, see the [R download](http://cran.r-project.org/bin/linux/) documentation for your Linux distribution.

##### Mac OS X

1. Install software listed in the [tools](http://cran.r-project.org/bin/macosx/tools) section of the Mac OS X R download documentation. With R 3+, this amounts to the GNU Fortran compiler gfortran.

##### Windows

1. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
2. Install 32-bit CPLEX. Note that this means coxinterval must be used with 32-bit R. On a 64-bit Windows system use `R --arch=i386 CMD check --no-multiarch` and `R --arch=i386 CMD INSTALL --no-multiarch` in the following build and install instructions.

### Building and installing

[Download the source](https://github.com/aboruvka/coxinterval/releases), unpack to a folder called "coxinterval" and run from the command line:
```
R CMD build coxinterval
R CMD check coxinterval_<version>.tar.gz
```
where `coxinterval_<version>.tar.gz` is the tarball generated by `R CMD build`. The check should give only one note on the package size. CPLEX libraries are static, which inflates the size of the shared object file `libs/coxinterval.so`.

To install the library `coxinterval.Rcheck` generated by `R CMD check` run:
```
R CMD INSTALL coxinterval_<version>.tar.gz
```
Alternatively coxinterval can be loaded and unloaded in an R session with
```
library(coxinterval, lib.loc = "coxinterval.Rcheck")
detach(package:coxinterval, unload = TRUE)
```
respectively.

##### Overriding build defaults for custom CPLEX installations

The package's Makevars files finds default installations of CPLEX on Unix-alike, Mac OS X and Windows systems. A custom installation directory can be specified in DOS/POSIX format with the `CPLEXHOME` or `CPLEXDIR` environment variables. One of these must be defined for a system with multiple CPLEX installations. CPLEX provides various library formats. By default Makevars(.win) links to the first one returned by `ls`. To override this, provide the DOS/POSIX path to the desired library in the `CPLEXLIBDIR` environment variable.
