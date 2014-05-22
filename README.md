cublas
======

This Haskell library provides FFI bindings for both the
[CUBLAS](https://developer.nvidia.com/cuBLAS) and
[CUSPARSE](https://developer.nvidia.com/cuSPARSE)
CUDA C libraries. Template Haskell and
[language-c](http://hackage.haskell.org/package/language-c) are used to 
automatically parse the C headers for the libraries and create the 
proper FFI declarations.

The main interfaces to use are `Foreign.CUDA.Cublas` for CUBLAS and
`Foreign.CUDA.Cusparse` for CUSPARSE. There is some primitive marhsalling
done between C types and Haskell types. For more direct FFI imports, use
the `Foreign.CUDA.Cublas.FFI` and `Foreign.CUDA.Cusparse.FFI` modules.

The `Cublas` typeclass represents elements for which CUBLAS operations can
be performed. Its instances are `CFloat`, `CDouble`, `Complex CFloat`, and
`Complex CDouble`. Similarly, there is a `Cusparse` typeclass which has
the same instances.

### Documentation

[See the Haddock documentation](http://bmsherman.github.io/haddock/cublas/index.html).

Installation
------------

First, CUDA and Autoconf should be installed. This has been tested with
CUDA version 6.0. Additionally, you may need
to add some CUDA directories to your `PATH` and `LD_LIBRARY_PATH`
environment variables.

Then, in the base directory, prepare a configure script by running
```shell
autoconf configure.ac > configure
```

Then (also in the base directory),
```shell
cabal configure
cabal install
```

