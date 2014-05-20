{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Foreign.CUDA.Cublas (
  -- * Types
  module Foreign.CUDA.Cublas.Types,
  -- * Initialization and destruction
  create, destroy,
  -- * BLAS functions
  module Foreign.CUDA.Cublas,
  -- * Error handling
  module Foreign.CUDA.Cublas.Error )
where

import Foreign.CUDA.Cublas.TH
import Foreign.CUDA.Cublas.FFI

import Foreign.CUDA.Cublas.Types
import Foreign.CUDA.Cublas.Error

$(doIO $ makeClassDecs "cublas" cublasFile)
