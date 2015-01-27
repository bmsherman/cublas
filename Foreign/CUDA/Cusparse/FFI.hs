{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Cusparse.FFI where

import Foreign.CUDA.Cublas.TH
import Foreign.CUDA.Cusparse.Types
import Foreign.C.Types

$(doIO $ makeFFIDecs  "cusparse" cusparseFile)
$(doIO $ makeAllFuncs "cusparse" cusparseFile)
