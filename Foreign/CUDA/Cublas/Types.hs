{-# LANGUAGE TemplateHaskell #-}

module Foreign.CUDA.Cublas.Types where

import Foreign.CUDA.Cublas.THBase

$(doIO $ makeTypes "cublas" cublasFile)
