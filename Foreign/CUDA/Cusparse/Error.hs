{-# LANGUAGE DeriveDataTypeable       #-}

module Foreign.CUDA.Cusparse.Error where
import Foreign.CUDA.Cusparse.Types (Status (..))

-- System
import Data.Typeable
import Control.Exception


-- Describe each error code
--
describe :: Status -> String
describe Success         = "success"
describe NotInitialized  = "library not initialised"
describe AllocFailed     = "resource allocation failed"
describe InvalidValue    = "unsupported value or parameter passed to a function"
describe ArchMismatch    = "unsupported on current architecture"
describe MappingError    = "access to GPU memory failed"
describe ExecutionFailed = "execution failed"
describe InternalError   = "internal error"
describe MatrixTypeNotSupported = "matrix type not supported by this function"


-- Exceptions ------------------------------------------------------------------
--
data CUSPARSEException
  = ExitCode  Status
  | UserError String
  deriving Typeable

instance Exception CUSPARSEException

instance Show CUSPARSEException where
  showsPrec _ (ExitCode  s) = showString ("CUSPARSE Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUSPARSE Exception: " ++ s)


-- | Raise a CUSPARSEException in the IO Monad
--
cusparseError :: String -> IO a
cusparseError s = throwIO (UserError s)


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
resultIfOk :: (Status, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)

