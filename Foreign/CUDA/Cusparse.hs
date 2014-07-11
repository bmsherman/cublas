{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
This module provides the most convenient FFI bindings to the CUSPARSE
library. The names of functions and the role of arguments closely follows
the CUSPARSE C API, which is documented here:

<http://docs.nvidia.com/cuda/cusparse/>
-}

module Foreign.CUDA.Cusparse (
  -- * Types
  module Foreign.CUDA.Cusparse.Types,
  -- * Initialization and destruction
  create, destroy,
  -- * BLAS functions
  module Foreign.CUDA.Cusparse,
  -- * Error handling
  module Foreign.CUDA.Cusparse.Error )
where

import Foreign.CUDA.Cublas.TH
import Foreign.CUDA.Cusparse.FFI

import Foreign.CUDA.Cusparse.Types
import Foreign.CUDA.Cusparse.Error

$(doIO $ makeClassDecs "cusparse" cusparseFile)
