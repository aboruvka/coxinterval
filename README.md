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

1. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). From a command prompt run:

```
set PATH
```

and ensure that the `PATH` environment variable includes the folders for both R and Rtools binaries. If not, update `PATH`. For example:

```
set PATH=C:/Rtools/bin;C:/Rtools/gcc-4.6.3/bin;C:/Program Files/R/R-3.1.0/bin;%PATH%
```

2. Install 32-bit CPLEX. Note that this means coxinterval must be used with 32-bit R. 

### Installing

Download the package tarball [`coxinterval_<version>.tar.gz`](https://github.com/aboruvka/coxinterval/releases).

##### Unix-alike and Mac OS X

From your system command line run:

```
R CMD INSTALL coxinterval_<version>.tar.gz
```

Alternatively from an R session run:

```
install.packages("<path to tarball>/coxinterval_<version>.tar.gz", repos = NULL, type = "source")
```

##### Windows

From a command prompt run:

```
R --arch=i386 CMD INSTALL --no-multiarch coxinterval_<version>.tar.gz
```

##### Overriding build defaults for custom CPLEX installations

The package's Makevars files finds default installations of CPLEX, which have the following general form.

- Unix-alike: `/opt/*/ILOG/CPLEX*`
- Mac OS X: `/Users/*/Applications/IBM/ILOG/CPLEX*`
- Windows 64-bit: `C:/PROGRA~2/IBM/ILOG/CPLEX*`
- Windows 32-bit: `C:/PROGRA~1/IBM/ILOG/CPLEX*`

A custom installation directory can be specified in POSIX format with the `CPLEXHOME` or `CPLEXDIR` environment variables. One of these must be defined for a system with multiple CPLEX installations. For example, on a 64-bit Windows system with CPLEX installed in the directory

```
C:\Program Files (x86)\CPLEX_Studio126
```

we can define `CPLEXDIR` from a command prompt with:

```
set CPLEXDIR=C:/PROGRA~2/CPLEX_Studio126
```

CPLEX provides various library formats. By default Makevars(.win) links to the first one returned by `ls`. To override this, provide the POSIX-style path to the desired library in the `CPLEXLIBDIR` environment variable.
