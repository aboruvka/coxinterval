## coxinterval

*An R package for Cox-type models with interval-censored data*

### Requirements

Required software for coxinterval is summarized as follows. System-specific notes for CPLEX are provided below.

* C API for CPLEX available with [IBM ILOG CPLEX Optimization Studio 12.5+](http://www-01.ibm.com/software/commerce/optimization/cplex-optimizer/). Members of [IBM's Academic Initiative](http://www-304.ibm.com/ibm/university/academic/pub/page/academic_initiative) can obtain CPLEX at [no charge](https://www.ibm.com/developerworks/community/blogs/jfp/entry/cplex_studio_in_ibm_academic_initiative?lang=en).
* Contributed R package [timereg](http://cran.r-project.org/web/packages/timereg/index.html)

coxinterval also depends on the Matrix, parallel and survival packages from the standard R library.

##### Windows

Install 32-bit CPLEX. Note that this means coxinterval must be used with 32-bit R. On a 64-bit Windows system use `R --arch=i386 CMD INSTALL --no-multiarch` in the following install instructions.

### Installing

[Download the package tarball](https://github.com/aboruvka/coxinterval/releases) and run from the command line:
```
R CMD INSTALL coxinterval_<version>.tar.gz
```
Alternatively in an R session run:
```
install.packages("coxinterval_<version>.tar.gz", repos = NULL, type = "source")
```

##### Overriding make defaults for custom CPLEX installations

The package's Makevars files finds default installations of CPLEX on Unix-alike, Mac OS X and Windows systems. A custom installation directory can be specified in DOS/POSIX format with the `CPLEXHOME` or `CPLEXDIR` environment variables. One of these must be defined for a system with multiple CPLEX installations. CPLEX provides various library formats. By default Makevars(.win) links to the first one returned by `ls`. To override this, provide the DOS/POSIX path to the desired library in the `CPLEXLIBDIR` environment variable.
