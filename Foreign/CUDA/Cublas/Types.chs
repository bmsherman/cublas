{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Cublas.Types (

  -- * Types
  Handle(..), Status(..),
  Operation(..),
  SideMode(..),FillMode(..), DiagType(..), PointerMode(..), AtomicsMode(..),

) where

import Foreign (Ptr)

#include <cublas_v2.h>
{# context lib="cublas" #}


-- | Types

newtype Handle = Handle { useHandle :: {# type cublasHandle_t #}}

{# enum cublasStatus_t as Status
  { underscoreToCase }
  with prefix="CUBLAS_STATUS" deriving (Eq, Show) #}

{# enum cublasOperation_t as Operation
  { underscoreToCase }
  with prefix="CUBLAS_OP" deriving (Eq, Show) #}

{# enum cublasSideMode_t as SideMode
  { underscoreToCase }
  with prefix="CUBLAS" deriving (Eq, Show) #}

{# enum cublasFillMode_t as FillMode
  { underscoreToCase }
  with prefix="CUBLAS_FILL_MODE" deriving (Eq, Show) #}

{# enum cublasDiagType_t as DiagType
  { underscoreToCase }
  with prefix="CUBLAS_DIAG" deriving (Eq, Show) #}

{# enum cublasPointerMode_t as PointerMode
  { underscoreToCase }
  with prefix="CUBLAS_POINTER_MODE" deriving (Eq, Show) #}

{# enum cublasAtomicsMode_t as AtomicsMode
  { underscoreToCase }
  with prefix="CUBLAS_ATOMICS" deriving (Eq, Show) #}
