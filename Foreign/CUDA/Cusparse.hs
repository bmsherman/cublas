{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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
