# coxinterval

*An R package for Cox-type models with interval-censored data*

## Requirements

Required software for coxinterval is summarized as follows. System-specific notes for items 1 and 2 are provided below.

1. Development tools for R
2. C API for CPLEX available with [IBM ILOG CPLEX Optimization Studio 12.5+](http://www-01.ibm.com/software/commerce/optimization/cplex-optimizer/). Members of [IBM's Academic Initiative](http://www-304.ibm.com/ibm/university/academic/pub/page/academic_initiative) can obtain CPLEX at [no charge](https://www.ibm.com/developerworks/community/blogs/jfp/entry/cplex_studio_in_ibm_academic_initiative?lang=en). Without CPLEX, coxinterval offers reduced functionality.
3. Contributed R package [timereg](http://cran.r-project.org/web/packages/timereg/index.html)

coxinterval also depends on the Matrix, parallel and survival packages from the standard R library.

### Linux

1. Install the r-base-dev package. For further details, see the [R download](http://cran.r-project.org/bin/linux/) documentation for your Linux distribution.

### Mac

1. Install software listed in the [tools](http://cran.r-project.org/bin/macosx/tools) section of the Mac OS X R download documentation. With R 3+, this amounts to the GNU Fortran compiler gfortran.

### Windows

1. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). From a command prompt run `set PATH` and ensure that the value for the `PATH` environment variable includes the folders for both R and Rtools binaries. If not, update `PATH`. For example:

   ```shell
    set PATH=C:/Rtools/bin;C:/Rtools/gcc-4.6.3/bin;C:/Program Files/R/R-3.1.0/bin;%PATH%
   ```
2. Install 32-bit CPLEX. Note that this means that the package's CPLEX-dependent functions are limited to 32-bit R.

## Installing

To install from CRAN, run in an R session:

```R
> install.packages("coxinterval", type = "source")
```

To install a development release from GitHub, [download the package tarball](https://github.com/aboruvka/coxinterval/releases) to your R session working directory and run:

```R
> install.packages("coxinterval_<version>.tar.gz", repos = NULL, type = "source")
```

#### Overriding installer defaults

In the interest of easy installation over complete portability, the package's [Makevars](http://cran.r-project.org/doc/manuals/r-release/R-exts.html#Using-Makevars) files uses non-standard [GNU make extensions](http://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages) to locate CPLEX's default directory, which depends on the system.

- Linux: `/opt/*/ILOG/CPLEX*`
- Mac: `/Users/*/Applications/IBM/ILOG/CPLEX*`
- Windows 64-bit: `C:/PROGRA~2/IBM/ILOG/CPLEX*`
- Windows 32-bit: `C:/PROGRA~1/IBM/ILOG/CPLEX*`

With custom or multiple installations of CPLEX, a directory must specified in POSIX-like format with the `CPLEXDIR` environment variable. For example, on a 64-bit Windows system with both 32- and 64-bit versions of CPLEX 12.6, the `install.packages` command should be preceded with:

```R
> Sys.setenv(CPLEXDIR = "C:/PROGRA~2/IBM/ILOG/CPLEX_Studio126")
```

On systems without GNU make, the compiler variables must be set directly. Continuing the above example:

```R
> Sys.setenv(CPLEXINVARS =
    paste('-I"', Sys.getenv("CPLEXDIR"), '/cplex/include"', sep = ""))
> Sys.setenv(CPLEXLNVARS =
    paste('-L"', Sys.getenv("CPLEXDIR"),
          '/cplex/lib/x86_windows_vs2010/stat_mda" -lcplex1260 -lm', sep = ""))
```

Note the use of quotes for correct reference to directory names. These are unnecessary on (mostly) POSIX-compliant systems like Linux and Mac.

In general the include and linking directories have the form

```
<CPLEXDIR>/cplex/include
<CPLEXDIR>/cplex/lib/<machine or compiler>/<library format>
```

respectively, where include directory points to the header file `<CPLEXDIR>/cplex/include/ilcplex/cplex.h`. The choice of the library format has no consequence for CPLEX's C API. Adequate linking option settings are `-lcplex -lm` under Linux and Mac. With Windows it is necessary to specify the CPLEX version number in the library name: `-lcplex<version> -lm`, where `<version>` can be obtained from the library file name `<CPLEXDIR>/cplex/lib/<compiler>/<library format>/cplex<version>.lib`.

For systems without both GNU make and CPLEX, precede the `install.packages` command by setting the `NOCPLEX` variable to a non-empty value:

```R
> Sys.setenv(NOCPLEX = "TRUE")
```
