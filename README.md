## coxinterval

*An R package for Cox-type models with interval-censored data*

### Requirements

Required software for coxinterval is summarized as follows. System-specific notes for items 1 and 2 are provided below.

1. Development tools for R
2. C API for CPLEX available with [IBM ILOG CPLEX Optimization Studio 12.5+](http://www-01.ibm.com/software/commerce/optimization/cplex-optimizer/). Members of [IBM's Academic Initiative](http://www-304.ibm.com/ibm/university/academic/pub/page/academic_initiative) can obtain CPLEX at [no charge](https://www.ibm.com/developerworks/community/blogs/jfp/entry/cplex_studio_in_ibm_academic_initiative?lang=en). Without CPLEX, coxinterval offers reduced functionality.
3. Contributed R package [timereg](http://cran.r-project.org/web/packages/timereg/index.html)

coxinterval also depends on the Matrix, parallel and survival packages from the standard R library.

##### Unix-alike

1. Install the r-base-dev package. For further details, see the [R download](http://cran.r-project.org/bin/linux/) documentation for your Linux distribution.

##### Mac OS X

1. Install software listed in the [tools](http://cran.r-project.org/bin/macosx/tools) section of the Mac OS X R download documentation. With R 3+, this amounts to the GNU Fortran compiler gfortran.

##### Windows

1. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). From a command prompt run `set PATH` and ensure that the value for the `PATH` environment variable includes the folders for both R and Rtools binaries. If not, update `PATH`. For example:

   ```shell
    set PATH=C:/Rtools/bin;C:/Rtools/gcc-4.6.3/bin;C:/Program Files/R/R-3.1.0/bin;%PATH%
   ```
2. Install 32-bit CPLEX. Note that this means that the package's CPLEX-dependent functions are limited to 32-bit R.

### Installing

Download the package tarball [`coxinterval_<version>.tar.gz`](https://github.com/aboruvka/coxinterval/releases).

From your system command line run:

```shell
R CMD INSTALL coxinterval_<version>.tar.gz
```

Alternatively from an R session run:

```R
install.packages("coxinterval_<version>.tar.gz", repos = NULL, type = "source")
```

##### Overriding build defaults for custom CPLEX installations

The package's [Makevars](http://cran.r-project.org/doc/manuals/r-release/R-exts.html#Using-Makevars) files uses non-standard [GNU make extensions](http://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages) to locate default installations of CPLEX, which have the following general form.

- Unix-alike: `/opt/*/ILOG/CPLEX*`
- Mac OS X: `/Users/*/Applications/IBM/ILOG/CPLEX*`
- Windows 64-bit: `C:/PROGRA~2/IBM/ILOG/CPLEX*`
- Windows 32-bit: `C:/PROGRA~1/IBM/ILOG/CPLEX*`

For systems with custom or multiple installations of CPLEX, an installation directory must specified in POSIX-type format with the `CPLEXDIR` environment variable. For example, on a 64-bit Windows system with CPLEX installed in the directory

```
C:\Program Files (x86)\CPLEX_Studio126
```

we can precede the above installation instructions by running

```shell
set CPLEXDIR=C:/PROGRA~2/CPLEX_Studio126
```

from the command prompt.

GNU make extensions are further used to set names for include, libraries and compiler flags following CPLEX's directory structure and Makefile. In particular `PKG_CPPFLAGS` is appended with `CPX_FLAGS`, which includes the directory `-I$(CPLEXDIR)/cplex/include` containing the `ilcplex/cplex.h` header file. `PKG_LIBS` is appended with `CPX_LIBS`, which has the general form

```
-L$(CPLEXDIR)/cplex/lib/<machine>/<libformat> -lcplex -lm
```

In the absence of GNU make or to override these settings, define the environment variables `CPX_FLAGS` and `CPX_LIBS` before installing.

##### Installing without both CPLEX and GNU make

For systems without both GNU make and CPLEX, set the environment variable `NO_CPLEX` before installing:

```shell
export NO_CPLEX=TRUE # Unix-alike
setenv NO_CPLEX=TRUE # OS X
set NO_CPLEX=TRUE    # Windows
```
