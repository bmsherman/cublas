{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.CuFFT.FFI where

import Foreign.CUDA.Cublas.TH
import Foreign.CUDA.CuFFT.Types
import Foreign.C.Types

$(doIO $ makeFFIDecs  "cufft" cufftFile)
$(doIO $ makeAllFuncs "cufft" cufftFile)
