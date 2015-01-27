{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.CUDA.Cusparse.Types where

import Foreign.CUDA.Cublas.THBase

$(doIO $ makeTypes "cusparse" cusparseFile)
