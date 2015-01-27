{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Cublas.FFI where

import Foreign.CUDA.Cublas.Types
import Foreign.CUDA.Cublas.TH
import Foreign.C.Types

$(doIO $ makeFFIDecs  "cublas" cublasFile)
$(doIO $ makeAllFuncs "cublas" cublasFile)
