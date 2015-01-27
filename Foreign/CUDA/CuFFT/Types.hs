{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.CUDA.CuFFT.Types where

import Foreign.CUDA.Cublas.THBase

$(doIO $ makeTypes "cufft" cufftFile)
