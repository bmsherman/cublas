{-# LANGUAGE DeriveDataTypeable       #-}

module Foreign.CUDA.CuFFT.Error where
import Foreign.CUDA.CuFFT.Types (Result (..))

-- System
import Data.Typeable
import Control.Exception

describe :: Result -> String
describe = show

-- Exceptions ------------------------------------------------------------------
--
data CUFFTException
  = ExitCode  Result
  | UserError String
  deriving Typeable

instance Exception CUFFTException

instance Show CUFFTException where
  showsPrec _ (ExitCode  s) = showString ("CUFFT Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUFFT Exception: " ++ s)


-- | Raise a CUFFTException in the IO Monad
--
cufftError :: String -> IO a
cufftError s = throwIO (UserError s)


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
resultIfOk :: (Result, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)

