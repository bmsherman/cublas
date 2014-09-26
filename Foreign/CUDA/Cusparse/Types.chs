{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Cusparse.Types (

  -- * Types
  Handle(..), HybMat(..), MatDescr(..),
  Csrsv2Info(..), Csric02Info (..), Csrilu02Info (..),
  Bsrsv2Info(..), Bsric02Info (..), Bsrilu02Info (..),
  Bsrsm2Info(..),
  SolvePolicy (..),  SolveAnalysisInfo(..),
  Operation(..), Status(..),
  Action(..), Direction(..), FillMode(..), PointerMode(..), DiagType(..),
  IndexBase(..), MatrixType(..), HybPartition(..),

) where

import Foreign (Ptr)

#include <cusparse_v2.h>
{# context lib="cusparse" #}


-- | Types
--
newtype Handle = Handle { useHandle :: {# type cusparseHandle_t #}}

newtype HybMat = HybMat { useHybMat :: {# type cusparseHybMat_t #}}

newtype MatDescr = MatDescr { useMatDescr :: {# type cusparseMatDescr_t #}}

newtype Csrsv2Info = Csrsv2Info { useCsrsv2Info :: {# type csrsv2Info_t #}}
newtype Csric02Info = Csric02Info { useCsric02Info :: {# type csric02Info_t #}}
newtype Csrilu02Info = Csrilu02Info { useCsrilu02Info :: {# type csrilu02Info_t #}}
newtype Bsrsv2Info = Bsrsv2Info { useBsrsv2Info :: {# type bsrsv2Info_t #}}
newtype Bsric02Info = Bsric02Info { useBsric02Info :: {# type bsric02Info_t #}}
newtype Bsrilu02Info = Bsrilu02Info { useBsrilu02Info :: {# type bsrilu02Info_t #}}
newtype Bsrsm2Info = Bsrsm2Info { useBsrsm2Info :: {# type bsrsm2Info_t #}}

newtype SolveAnalysisInfo = SolveAnalysisInfo { useSolveAnalysisInfo :: {# type cusparseSolveAnalysisInfo_t #}}

{# enum cusparseSolvePolicy_t as SolvePolicy
  { underscoreToCase }
  with prefix="CUSPARSE_SOLVE_POLICY" deriving (Eq, Show) #}


{# enum cusparseStatus_t as Status
  { underscoreToCase }
  with prefix="CUSPARSE_STATUS" deriving (Eq, Show) #}

{# enum cusparseAction_t as Action
  { underscoreToCase }
  with prefix="CUSPARSE_ACTION" deriving (Eq, Show) #}

{# enum cusparseDirection_t as Direction
  { underscoreToCase }
  with prefix="CUSPARSE_DIRECTION" deriving (Eq, Show) #}

{# enum cusparseOperation_t as Operation
  { underscoreToCase }
  with prefix="CUSPARSE_OPERATION" deriving (Eq, Show) #}

{# enum cusparseIndexBase_t as IndexBase
  { underscoreToCase }
  with prefix="CUSPARSE_INDEX_BASE" deriving (Eq, Show) #}

{# enum cusparseHybPartition_t as HybPartition
  { underscoreToCase }
  with prefix="CUSPARSE_HYB_PARTITION" deriving (Eq, Show) #}


{# enum cusparseMatrixType_t as MatrixType
  { underscoreToCase }
  with prefix="CUSPARSE_MATRIX_TYPE" deriving (Eq, Show) #}

{# enum cusparseFillMode_t as FillMode
  { underscoreToCase }
  with prefix="CUSPARSE_FILL_MODE" deriving (Eq, Show) #}

{# enum cusparseDiagType_t as DiagType
  { underscoreToCase }
  with prefix="CUSPARSE_DIAG_TYPE" deriving (Eq, Show) #}

{# enum cusparsePointerMode_t as PointerMode
  { underscoreToCase }
  with prefix="CUSPARSE_POINTER_MODE" deriving (Eq, Show) #}
