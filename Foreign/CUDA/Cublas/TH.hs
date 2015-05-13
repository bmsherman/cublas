
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Cublas.TH (
  module Foreign.CUDA.Cublas.TH,
  module Foreign.CUDA.Cublas.THBase ) where

import Foreign.CUDA.Cublas.THBase

import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>), void)

import GHC.Exts (groupWith)

import Language.Haskell.TH as TH
import Language.C
import Language.C.System.GCC

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nubBy, stripPrefix)
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

import Debug.Trace
import qualified Foreign.C.Types as C
import qualified Foreign as F
import Foreign.Storable.Complex ()
import Data.Complex (Complex(..))



import Foreign.CUDA as FC
import qualified Foreign.CUDA.Runtime.Stream as FC

import qualified Foreign.CUDA.Cublas.Types as BL
import qualified Foreign.CUDA.Cusparse.Types as SP
import qualified Foreign.CUDA.Cublas.Error as BL
import qualified Foreign.CUDA.Cusparse.Error as SP
import qualified Foreign.CUDA.CuFFT.Error as FFT

try :: [(Bool, a)] -> a
try ((p,y):conds) = if p then y else try conds
try [] = error "TH.try: No match!"

data TypeInfo = TI 
  { ctype    :: Q Type
  , hsinput  :: Either Convert Create
  , hsoutput :: Maybe (Either Convert Destroy) }

data TypeDat = TD 
  { ct    :: Q Type
  , hst   :: Q Type 
  , c2hs  :: (Q Exp, ExpType)
  , hs2c  :: (Q Exp, ExpType) }

data ExpType = Pure | Monadic

data TypeC = VoidC | IntC | FloatC | DoubleC | EnumC String 
           | ComplexC TypeC | ArbStructC String | PtrC TypeC | PhonyC TH.Name
           | ArrC TypeC
           | SizeTC
           deriving (Eq, Show)

prim :: Q Type -> Q Type -> Q Exp -> Q Exp -> TypeDat
prim ct hst c2hs hs2c = TD ct hst (c2hs, Pure) (hs2c, Pure)

simple :: Q Type -> TypeDat
simple t = prim t t [| id |] [| id |]

bothc :: (a -> b) -> Complex a -> Complex b
bothc f (a :+ b) = f a :+ f b 

typeDat :: TypeC -> TypeDat
typeDat (PhonyC n)  = simple (varT n)
typeDat VoidC       = simple [t| () |] 
typeDat SizeTC      = simple [t| C.CSize |] 
typeDat (PtrC t)    = prim [t| F.Ptr $(ctype) |] [t| FC.DevicePtr $(ctype) |] [| FC.DevicePtr |]       [| FC.useDevicePtr |] where
  ctype = ct (typeDat t)
typeDat (ArrC t)    = typeDat (PtrC t)
typeDat IntC        = prim [t| C.CInt |]    [t| Int |]    [| fromIntegral |]          [| fromIntegral |]
typeDat FloatC      = simple [t| C.CFloat |]
typeDat DoubleC     = simple [t| C.CDouble |]
typeDat (EnumC str) = case ty of
  Just t -> prim [t| C.CInt |] t
    [| toEnum . fromIntegral |] [| fromIntegral . fromEnum |]
  Nothing -> error ("typeDat.EnumC : Missing type: " ++ str)
  where
  ty = do
    let str' = fromMaybe str (stripSuffix "_t" str)
    n <- stripPrefix "cublas"   str'
     <|> stripPrefix "cusparse" str'
     <|> stripPrefix "cufft" str'
    return $ conT (mkName n)
typeDat (ArbStructC str) = case str of
  "cudaStream_t" -> prim [t| F.Ptr () |] [t| FC.Stream |] [| FC.Stream |] [| FC.useStream |]
  _ -> let tyName = typeDefTyName str; useName = typeDefUseName str in
       prim [t| F.Ptr () |] (conT tyName) (conE tyName) (varE useName)
typeDat (ComplexC t) = prim
  [t| Complex $(ctype) |]
  [t| Complex $(hstype) |]
  [| bothc $(fromC) |]
  [| bothc $(toC) |]
  where
  TD ctype hstype (fromC, Pure) (toC, Pure) = typeDat t


convertT x y = Left (Convert x y)
createT = Right . Create
destroyT = Right . Destroy

data Convert = Convert (Q Type) (Q Exp)
newtype Create = Create (Q Exp)
newtype Destroy = Destroy (Q Exp)

pointerify :: Q Type -> Q Type
pointerify x = [t| F.Ptr $(x) |]

useT :: TypeC -> TypeInfo
useT = useT' . typeDat where
  useT' (TD ct hst c2hs (hs2c,purity)) = TI
    ct
    (convertT hst exp)
    Nothing
    where
    exp = case purity of Pure -> [| return . $(hs2c) |]; Monadic -> hs2c

inT :: TypeC -> TypeInfo
inT (PtrC t) = inT' (typeDat t) where
  inT' (TD ct hst c2hs (hs2c,purity)) = TI
    (pointerify ct)
    (convertT hst exp)
    (Just (destroyT [| F.free |]))
    where
    exp = case purity of Pure -> [| F.new . $(hs2c) |] ; Monadic -> undefined
inT (ArrC t) = inT' (typeDat t) where
  inT' (TD ct hst c2hs (hs2c,purity)) = TI
    (pointerify ct)
    (convertT [t| [ $(hst) ] |] exp)
    (Just (destroyT [| FC.free . FC.DevicePtr |]))
    where
    exp = case purity of Pure -> [| fmap FC.useDevicePtr . FC.newListArray . map $(hs2c) |] ; Monadic -> undefined

outT :: TypeC -> TypeInfo
outT (PtrC t) = outT' (typeDat t) where
  outT' (TD ct hst (c2hs,purity) hs2c) = TI
    ct
    (createT [| F.malloc |])
    (Just (convertT hst [| \p -> do { x <- F.peek p ; F.free p; $(exp) x } |]))
    where
    exp = case purity of Pure -> [| return . $(c2hs) |] ; Monadic -> c2hs

inOutT :: TypeC -> TypeInfo
inOutT (PtrC t) = inOutT' (typeDat t) where
  inOutT' (TD ct hst (c2hs,purity1) (hs2c,purity2)) = TI
    (pointerify ct)
    (convertT hst exp1)
    (Just (convertT hst [| \p -> do { x <- F.peek p ; F.free p; $(exp2) x } |]))
    where
    exp1 = case purity1 of Pure -> [| F.new . $(hs2c) |] ; Monadic -> hs2c
    exp2 = case purity2 of Pure -> [| return . $(c2hs) |] ; Monadic -> c2hs

convert :: [String] -> CTypeSpecifier a -> TypeC
convert _ (CVoidType _) = VoidC
--CCharType a	 
--CShortType a	 
convert _ (CIntType _) = IntC
--CLongType a	 
convert _ (CFloatType _) = FloatC
convert _ (CDoubleType _) = DoubleC
--CSignedType a	 
--CUnsigType a	 
--CBoolType a	 
--CComplexType a
convert arbstructs (CTypeDef ident _) = try 
  [ (s `elem` ["cudaStream_t"], ArbStructC s)
  , (fromMaybe False ((`elem` arbstructs) <$> typedef), ArbStructC (case typedef of Just n -> n))
  , (s `elem` ["cuComplex", "cufftComplex"], ComplexC FloatC)
  , (s `elem` ["cuDoubleComplex", "cufftDoubleComplex"], ComplexC DoubleC)
  , (s=="size_t", SizeTC)
  , (True, EnumC s) ]
  where
  typedef = typeDefName ident
  s = identToString ident
convert _ _ = VoidC

convert' :: [String] -> [CDeclarationSpecifier a] -> TypeC
convert' arbstructs specs = case specs of
  (CTypeSpec x:_) -> convert arbstructs x
  (_:xs) -> convert' arbstructs xs
  [] -> error "convert': invalid CDeclarationSpecifier list"

typeOf :: [String] -> (TypeC -> Q Type) -> CDeclaration a -> Q Type
typeOf arbstructs proj (CDecl basetype [(Just (CDeclr (Just ident) ptrs _ _ _), _, _)] _) = 
  foldr f (proj $ convert' arbstructs basetype) ptrs
  where
  f (CPtrDeclr _ _) b = [t| F.Ptr $(b) |]
  f _ _ = error "haven't implemented other things"

pointerification :: CDeclaration a -> (TypeC -> TypeC)
pointerification (CDecl _ [(Just (CDeclr _ ptrs _ _ _), _, _)] _) = foldr (.) id $ map f ptrs where
  f (CPtrDeclr _ _) = PtrC
  f (CArrDeclr _ _ _) = ArrC
  f _ = id --possible there are other things that should be here?

baseType :: [String] -> CDeclaration a -> TypeC
baseType arbstructs (CDecl basetype _ _) = convert' arbstructs basetype

cType :: [String] -> CDeclaration a -> TypeC
cType arbstructs d = (pointerification d) (baseType arbstructs d)
  

typeInfo :: String -> CVar -> TypeInfo
typeInfo fn (n, typec) = ($ typec) $ try
  [ {- CUBLAS -}
    (case typec of ArrC _ -> True; otherwise -> False
      , inT)
  , (n `elem` ["alpha", "beta", "a", "b", "c", "d1", "d2", "x1", "y1", "s"]
      , inT)
  , ( ("create" `isPrefixOf` fn && not (fn == "createIdentityPermutation")) 
       || n == "result"
      , outT)
    {- CuSPARSE -}
  , ("DevHostPtr" `isSuffixOf` n
      , outT)
    {- End -}
  , (True
      , useT)
  ]

declName :: CDeclaration a -> Maybe String
declName (CDecl _ [(Just (CDeclr (Just ident) _ _ _ _), _, _)] _) = Just (identToString ident)
declName _ = Nothing

outMarshall :: TypeC -> (Q Exp, Q Type -> Q Type)
outMarshall (EnumC "cublasStatus_t") = ([| BL.resultIfOk |], id)
outMarshall (EnumC "cusparseStatus_t") = ([| SP.resultIfOk |], id)
outMarshall (EnumC "cufftResult") = ([| FFT.resultIfOk |], id)
outMarshall VoidC = ([| return . snd |], id)
outMarshall x = ([| return . fst |], const (hst $ typeDat x))



createf' :: (String, CFunction) -> Q [Dec]
createf' (foreignname, cf@(CFunction fname rettype args)) = do
  ins <- mapM (safeName "_in") args
  toCs <- mapM (safeName "_out") args
  (outstatements, (outtypes, outs)) <- second (unzip . catMaybes) . unzip <$> collect (zip3 args argsTI toCs)
  let instatements = map inMarsh (zip3 argsTI ins toCs)
  ret <- newName "res"
  let runstatement = bindS (varP ret) (foldl f z toCs)
  let returnstatement = [| $(checkStatusExp) ( $(outputConv) $(varE ret), $(tupE (map varE outs)) ) |]
  expr <- doE $ concat [instatements, runstatement:outstatements, [noBindS returnstatement]]
  let usedins = map snd . filter (isused . fst) $ zip argsTI ins
  let fdec = FunD fcall [Clause (map VarP usedins) (NormalB expr) []]
  tdec <- sigD fcall $ funTypeMod checkStatusType argsTI
  return [tdec, fdec]
  where
  safeName :: String -> CVar -> Q TH.Name
  safeName end (s:str, _) = newName (toLower s : str ++ end)

  argsTI = functionTypeInfo cf
  (outputConv, _) = c2hs (typeDat rettype)
  (checkStatusExp, checkStatusType) = outMarshall rettype
  isused (TI _ (Left _) _) = True
  isused _ = False
  fcall = mkName fname
  z = varE (mkName foreignname)
  f x e = appE x (varE e)
  inMarsh (ti,e,e') = case hsinput ti of
    Left (Convert t a) -> bindS (varP e') (appE a (varE e))
    Right (Create a)   -> bindS (varP e') a
  collect ( (arg, TI _ _ (Just cleanup), e) : xs) = do
    e' <- safeName "_out" arg
    let outinfo = case cleanup of
          Left (Convert t a) -> (bindS (varP e') (appE a (varE e)), Just (t, e'))
          Right (Destroy a) ->  (noBindS (appE a (varE e)), Nothing)
    ys <- collect xs
    return (outinfo:ys)
  collect (_:xs) = collect xs
  collect [] = return []
  collecti (TI _ (Left (Convert t _)) _) = 
    [t]
  collecti _ = []




funname :: CDeclaration a -> String
funname (CDecl _ [(Just (CDeclr (Just ident ) _ _ _ _), _, _)] _) = identToString ident
funname _ = "Weird!"

desired :: String -> CFunction -> Bool
desired "cufft" (CFunction name _ _) = "cufft" `isPrefixOf` name
desired prefix (CFunction name _ _) = 
    any (`isPrefixOf` name) (map (prefix ++) ("Get" : map (:[]) "SDCZX"))
    && not ("color" `isSuffixOf` name)

infol :: Show a =>  CDerivedDeclarator a -> Maybe [[String]]
infol (CFunDeclr (Right (ys,_)) _ _) = Just $ map f ys where
  f (CDecl specs _ _) = map show specs
infol _ = Nothing

funArgs :: CDeclarator a -> Maybe [CDeclaration a]
funArgs (CDeclr _ [(CFunDeclr (Right (ys,_)) _ _)] _ _ _) = Just ys
funArgs _ = Nothing

funDecl :: CDeclaration a -> Maybe (CDeclarator a)
funDecl (CDecl _ [(Just declarator, _, _)] _) = Just declarator
funDecl _ = Nothing

maybeFunction :: Show a => [String] -> CDeclaration a -> Maybe (CFunction)
maybeFunction arbstructs d@(CDecl returnType _ _) = do
  args <- funArgs =<< funDecl d
  retName <- declName d
  argNames <- mapM declName args
  let argTypes = map (cType arbstructs) args
  return $ CFunction retName (convert' arbstructs returnType) (zip argNames argTypes)


type CVar = (String, TypeC)
data CFunction = CFunction
  { cfName  :: String
  , cfRetTy :: TypeC
  , cfArgs  :: [CVar] }
  deriving Show



createf :: FilePath -> CFunction -> Q Dec
createf fp (CFunction name ret args) = 
  forImpD cCall safe{-unsafe-} (fp ++ ' ':name) (mkName name) cFunType
  where
  cFunType = foldr f z (map (ct . typeDat . snd) args)
  z = [t| IO $(ct . typeDat $ ret) |]
  f x y = [t| $(x) -> $(y) |]


sharedDecs :: String -> [CFunction] -> [(String, [(String, CFunction)])]
sharedDecs prefix xs = xs'' where
  g x@(CFunction s ret args) = do
    newname <- dropc <$> goodName prefix s
    return (s, (CFunction newname ret args))
  xs' = mapMaybe g xs
  dropc name = if last name == 'c' then init name else name --for dot, ger, ...
  xs'' = map ( (cfName . snd . head) &&& id) .
    filter sdFilter . groupWith (tail . cfName . snd) $ xs'
  sdFilter xs = length xs == 4 && not (
    any (`isInfixOf` (fst (head xs))) ["rot_v2", "rotg_v2", "hybsv_analysis", "numericBoost"] )

mkClass :: String -> [CFunction] -> Q Dec
mkClass (p:prefix) xs = classD (return []) className [PlainTV typeName] [] decs where
  className = mkName (toUpper p:prefix)
  typeName = mkName "a"
  decs = map (f . phonifyF) xs
  mkPhony :: TypeC -> TypeC
  mkPhony (PtrC t) = PtrC (mkPhony t)
  mkPhony (ArrC t) = ArrC (mkPhony t)
  mkPhony x = let t' = PhonyC typeName in
    case x of DoubleC -> t'; FloatC -> t'; ComplexC _ -> t'; y -> y
  phonifyF :: CFunction -> CFunction
  phonifyF (CFunction name ret args) =
    CFunction name (mkPhony ret) (map (second mkPhony) args)
  f cfunc@(CFunction name _ _) = 
    sigD (mkName (tail name)) (funType $ functionTypeInfo cfunc)

mkClassInstances :: String -> [(String, [(String,CFunction)])] -> [Q Dec]
mkClassInstances (p:prefix) xs = map (\c -> makeInstance c $ map (f c) xs) "sdcz" where
  makeInstance c decs = instanceD (return []) classSig (decs) where
    classSig = appT (return . ConT $ mkName (toUpper p:prefix)) (ct . typeDat $ typeMap c)
  f c (_, funcs) = (!! 1) <$> createf' (foreignn, CFunction name ret args) where
    [(foreignn,CFunction (_:name) ret args)] = filter (\(_,CFunction (s:_) _ _)-> s==c) funcs

typeMap :: Char -> TypeC
typeMap 'c' = ComplexC FloatC
typeMap 'z' = ComplexC DoubleC
typeMap 'd' = DoubleC
typeMap 's' = FloatC
typeMap _ = error "typeMap: Invalid character"

makeClassDecs :: String -> FilePath -> IO (Q [Dec])
makeClassDecs str fp = do
  sds <- sharedDecs str <$> getDesiredFunctions str fp
  return $ sequence (mkClass str (map (snd . head . snd) sds) : mkClassInstances str sds)

makeFFIDecs :: String -> FilePath -> IO (Q [Dec])
makeFFIDecs str fp = sequence . map (createf fp) <$> getDesiredFunctions str fp

getDesiredFunctions :: String -> FilePath -> IO [CFunction]
getDesiredFunctions str fp = do
  decls <- getExternalDecls fp
  let arbstructs = mapMaybe maybeTypeDef decls
  putStrLn "ArbStructs:"
  print arbstructs
  return . nubBy ((==) `on` cfName) . filter (desired str) $ 
    mapMaybe (maybeFunction arbstructs) decls

makeAllFuncs :: String -> FilePath -> IO (Q [Dec])
makeAllFuncs str fp = fmap concat . sequence . mapMaybe (fmap createf' . alter)
  <$> getDesiredFunctions str fp where
  alter (CFunction fname rettype args) = do
    newname <- goodName str fname
    return (fname, CFunction newname rettype args)

goodName :: String -> String -> Maybe String
goodName prefix = f where
  v2suff = "_v2"
  l = length prefix
  f str = if pre == prefix then Just (toLower x : xs) else Nothing
    where
    (pre, name) = splitAt l str
    (name', v2) = splitAt (length name - length v2suff) name
    (x : xs) = if v2 == v2suff then name' else name

inTypes :: [TypeInfo] -> [Q Type]
inTypes = mapMaybe f where
  f (TI _ (Left (Convert t _)) _) = Just t
  f _ = Nothing

outTypes :: [TypeInfo] -> [Q Type]
outTypes = mapMaybe f where
  f (TI _ _ (Just (Left (Convert t _)))) = Just t
  f _ = Nothing

functionTypeInfo :: CFunction -> [TypeInfo]
functionTypeInfo (CFunction fname ret args) = map (typeInfo fname) args

funTypeMod :: (Q Type -> Q Type) -> [TypeInfo] -> Q Type
funTypeMod f args = foldr arrow z ins where
  arrow x y = [t| $(x) -> $(y) |]
  z = [t| IO $( f $ foldl appT (tupleT (length outs)) outs) |]
  [ins, outs] = map ($ args) [inTypes, outTypes]

funType :: [TypeInfo] -> Q Type
funType = funTypeMod id
