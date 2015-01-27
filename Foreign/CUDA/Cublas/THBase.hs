{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.CUDA.Cublas.THBase where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (join, guard, forM, (<=<))

import Data.Char (toUpper, toLower)
import Data.List (stripPrefix, isSuffixOf, isPrefixOf)
import Data.Maybe (mapMaybe, fromMaybe)

import qualified Foreign as F

import Language.C
import Language.C.System.GCC
import Language.Haskell.TH as TH

import System.FilePath.Posix ((</>))

import Debug.Trace

cublasFile, cusparseFile, cufftFile :: FilePath
cublasFile = CUDA_INCLUDE_DIR </> "cublas_v2.h"
cusparseFile = CUDA_INCLUDE_DIR </> "cusparse_v2.h"
cufftFile = CUDA_INCLUDE_DIR </> "cufft.h"

maybeExternalDec :: CExternalDeclaration a -> Maybe (CDeclaration a)
maybeExternalDec (CDeclExt d) = Just d
maybeExternalDec _ = Nothing

getExternalDecls :: FilePath -> IO [CDecl]
getExternalDecls fp = do
  Right (CTranslUnit xs _) <- parseCFile (newGCC "/usr/bin/gcc") Nothing [] fp
  return $ mapMaybe maybeExternalDec xs

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix string = 
  reverse <$> stripPrefix (reverse suffix) (reverse string)

typeDefName :: Ident -> Maybe String
typeDefName ident = do
  guard (not ("cuda" `isPrefixOf` tName))
  let n = removeSuff "_t" tName
  Just $ (remove "cufft" . remove "cublas" . remove "cusparse") n
  where
  remove prefix name = fromMaybe name (stripPrefix prefix name)
  removeSuff suffix name = fromMaybe name (stripSuffix suffix name)
  tName = identToString ident

maybeTypeDef :: CDeclaration a -> Maybe String
maybeTypeDef (CDecl [CStorageSpec (CTypedef _)
  , CTypeSpec (CSUType (CStruct CStructTag (Just structName) _ _ _) _)]
  [(Just (CDeclr (Just typedefName) [CPtrDeclr [] _] Nothing [] _)
    ,Nothing,Nothing)] _) = typeDefName typedefName
-- the following case is for CuFFT Handle
maybeTypeDef d@(CDecl [CStorageSpec (CTypedef _)
  , CTypeSpec (CIntType _)]
  [(Just (CDeclr (Just typedefName) [] Nothing [] _)
    ,Nothing,Nothing)] _) = traceShow (fmap (const ()) d) True `seq` typeDefName typedefName
maybeTypeDef _ = Nothing

maybeEnum ::  CDeclaration a -> Maybe EnumT
maybeEnum (CDecl [CStorageSpec (CTypedef _)
  , CTypeSpec (CEnumType (CEnum enumName (Just constrs) _ _) _)]
  [(Just (CDeclr (Just typedefName) _ Nothing [] _)
    ,Nothing,Nothing)] _) = do
      constructors <- mapM f constrs
      return $ EnumT (identToString typedefName) constructors where
  f :: (Ident, Maybe (CExpression a)) -> Maybe (String, Int)
  f (constrName, Just (CConst (CIntConst i _))) = 
    Just (identToString constrName, fromIntegral (getCInteger i))
  f _  = Nothing
maybeEnum _ = Nothing

modifyEnumNames :: String -> EnumT -> Maybe EnumT
modifyEnumNames prefix (EnumT tyName constrs) = do
  tyName' <- stripPrefix prefix (remove "_t" tyName)
  constrs' <- forM constrs $ \(n, v) -> do
    n' <- stripPrefix (upperFirst prefix) (underscoreToCase n)
    return (sideAdjustment n', v)
  let constrs'' = let (ns, vs) = unzip constrs' in zip
        (map sideAdjustment (stripCommonPrefix tyName' ns))
        vs
  return $ EnumT tyName' constrs''
  where
  remove suffix name = fromMaybe name (stripSuffix suffix name)
  safeSplit (x : xs) = Just (x, xs)
  safeSplit _ = Nothing
  stripCommonPrefix :: Eq a => [a] -> [[a]] -> [[a]]
  stripCommonPrefix (x : xs) yss = fromMaybe yss $ do
    (heads, tails) <- unzip <$> mapM safeSplit yss
    guard (all (x ==) heads)
    return $ stripCommonPrefix xs tails
  stripCommonPrefix _ yss = yss
  sideAdjustment n | n `elem` ["Left", "Right"] = "Side" ++ n
                   | otherwise = n
  upperFirst (x : xs) = toUpper x : xs

-- | Copied from c2hs code
underscoreToCase :: String -> String
underscoreToCase lexeme =
  let
    ps = filter (not . null) . parts $ lexeme
  in
  concat . map adjustCase $ ps
  where
    parts s = let (l, s') = break (== '_') s
              in
              l : case s' of
                    []      -> []
                    (_:s'') -> parts s''

    adjustCase (c:cs) = toUpper c : map toLower cs

typeDefTyName :: String -> TH.Name
typeDefTyName (x : xs) = mkName (toUpper x : xs)

typeDefUseName :: String -> TH.Name
typeDefUseName (x : xs) = mkName ("use" ++ toUpper x : xs)

typeDefNewtype :: String -> Q Dec
typeDefNewtype name = do
  ptrType <- [t| F.Ptr () |]
  let recCon = (RecC tyName [(useName, NotStrict, ptrType)])
  return $ NewtypeD [] tyName [] recCon []
  where
  tyName = typeDefTyName name
  useName = typeDefUseName name

data EnumT = EnumT String [(String, Int)]

makeEnum :: EnumT -> Q [Dec]
makeEnum (EnumT tyName constrs) = return [ defineEnum, enumInstance ] where
  tyN = mkName tyName
  defineEnum = DataD [] tyN [] constructors derived
  constructors = [ NormalC (mkName name) [] | (name, val) <- constrs ]
  derived = map mkName ["Eq", "Show"]
  instanceSig = AppT (ConT (mkName "Enum")) (ConT tyN)
  enumInstance = InstanceD [] instanceSig 
    [mk "toEnum" snd, mk "fromEnum" fst]
  patterns = [ ( (ConP n [], LitE i) 
               , (LitP i, ConE n) )
             | (name, v) <- constrs, let i = IntegerL (fromIntegral v)
             , let n = mkName name ]
  mk fname op = FunD (mkName fname)
    [ Clause [pat] (NormalB val) [] | (pat, val) <- map op patterns ]

makeTypes :: String -> FilePath -> IO (Q [Dec])
makeTypes str fp = do
  decls <- getExternalDecls fp
  return $ (++)
    <$> (mapM typeDefNewtype $ mapMaybe maybeTypeDef decls)
    <*> (concat <$> makeEnums decls)
  where
  makeEnums = mapM makeEnum . mapMaybe (modifyEnumNames str <=< maybeEnum)

doIO :: IO (Q [a]) -> Q [a]
doIO = join . runIO
