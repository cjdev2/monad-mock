{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
This module provides Template Haskell functions for automatically generating
types representing typeclass methods for use with "Control.Monad.Mock". The
resulting datatypes can be used with 'Control.Monad.Mock.runMock' or
'Control.Monad.Mock.runMockT' to mock out functionality in unit tests.

The primary interface to this module is the 'makeAction' function, which
generates an action GADT given a list of mtl-style typeclass constraints. For
example, consider a typeclass that encodes side-effectful monadic operations:

@
class 'Monad' m => MonadFileSystem m where
  readFile :: 'FilePath' -> m 'String'
  writeFile :: 'FilePath' -> 'String' -> m ()
@

The typeclass has an obvious, straightforward instance for 'IO'. However, one
of the main value of using a typeclass is that a alternate, pure instance may
be provided for unit tests, which is what 'MockT' provides. Therefore, one
might use 'makeAction' to automatically generate the necessary datatype and
instances:

@
'makeAction' \"FileSystemAction\" ['ts'| MonadFileSystem |]
@

This generates three things:

  1. A @FileSystemAction@ GADT with constructors that correspond to the
     methods of @MonadFileSystem@.

  2. An 'Action' instance for @FileSystemAction@.

  3. A @MonadFileSystem@ instance for @'MockT' FileSystemAction m@.

The generated code effectively looks like this:

@
data FileSystemAction r where
  ReadFile :: 'FilePath' -> FileSystemAction 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystemAction ()
deriving instance 'Eq' (FileSystemAction r)
deriving instance 'Show' (FileSystemAction r)

instance 'Action' FileSystemAction where
  'eqAction' (ReadFile a) (ReadFile b)
    = if a '==' b then 'Just' 'Refl' else 'Nothing'
  'eqAction' (WriteFile a b) (WriteFile c d)
    = if a '==' c && b '==' d then 'Just' 'Refl' else 'Nothing'
  'eqAction' _ _ = 'Nothing'

instance 'Monad' m => MonadFileSystem ('MockT' FileSystemAction m) where
  readFile a = 'mockAction' "readFile" (ReadFile a)
  writeFile a b = 'mockAction' "writeFile" (WriteFile a b)
@

This can then be used in tandem with 'Control.Monad.Mock.runMock' to unit-test
a function that interacts with the file system in a completely pure way:

@
copyFile :: MonadFileSystem m => 'FilePath' -> 'FilePath' -> m ()
copyFile a b = do
  x <- readFile a
  writeFile b x

spec = describe "copyFile" '$'
  it "reads a file and writes its contents to another file" '$'
    'Control.Exception.evaluate' '$' copyFile "foo.txt" "bar.txt"
      'Data.Function.&' 'Control.Monad.Mock.runMock' [ ReadFile "foo.txt" ':->' "contents"
                , WriteFile "bar.txt" "contents" ':->' () ]
@
-}
module Control.Monad.Mock.TH (makeAction, deriveAction, ts) where

import Control.Monad (replicateM, when, zipWithM)
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.List (foldl', nub, partition)
import Data.Type.Equality ((:~:)(..))
import GHC.Exts (Constraint)
import Language.Haskell.TH

import Control.Monad.Mock (Action(..), MockT, mockAction)
import Control.Monad.Mock.TH.Internal.TypesQuasi (ts)

-- | Given a list of monadic typeclass constraints of kind @* -> 'Constraint'@,
-- generate a type with an 'Action' instance with constructors that have the
-- same types as the methods.
--
-- @
-- class 'Monad' m => MonadFileSystem m where
--   readFile :: 'FilePath' -> m 'String'
--   writeFile :: 'FilePath' -> 'String' -> m ()
--
-- 'makeAction' "FileSystemAction" ['ts'| MonadFileSystem |]
-- @
makeAction :: String -> Cxt -> Q [Dec]
makeAction actionNameStr classTs = do
    traverse_ assertDerivableConstraint classTs

    actionParamName <- newName "r"
    let actionName = mkName actionNameStr
        actionTypeCon = ConT actionName

    classInfos <- traverse reify (map unappliedTypeName classTs)
    methods <- traverse classMethods classInfos
    actionCons <- concat <$> zipWithM (methodsToConstructors actionTypeCon) classTs methods

    let actionDec = DataD [] actionName [PlainTV actionParamName] Nothing actionCons []
        mkStandaloneDec derivT = standaloneDeriveD' [] (derivT `AppT` (actionTypeCon `AppT` VarT actionParamName))
        standaloneDecs = [mkStandaloneDec (ConT ''Eq), mkStandaloneDec (ConT ''Show)]
    actionInstanceDec <- deriveAction' actionTypeCon actionCons
    classInstanceDecs <- zipWithM (mkInstance actionTypeCon) classTs methods

    return $ [actionDec] ++ standaloneDecs ++ [actionInstanceDec] ++ classInstanceDecs
  where
    -- | Ensures that a provided constraint is something monad-mock can actually
    -- derive an instance for. Specifically, it must be a constraint of kind
    -- @* -> 'Constraint'@, and anything else is invalid.
    assertDerivableConstraint :: Type -> Q ()
    assertDerivableConstraint classType = do
      info <- reify $ unappliedTypeName classType
      (ClassD _ _ classVars _ _) <- case info of
        ClassI dec _ -> return dec
        _ -> fail $ "makeAction: expected a constraint, given ‘" ++ show (ppr classType) ++ "’"

      let classArgs = typeArgs classType
      let mkClassKind vars = foldr (\a b -> AppT (AppT ArrowT a) b) (ConT ''Constraint) (reverse varKinds)
            where varKinds = map (\(KindedTV _ k) -> k) vars
          constraintStr = show (ppr (ConT ''Constraint))

      when (length classArgs > length classVars) $
        fail $ "makeAction: too many arguments for class\n"
            ++ "      in: " ++ show (ppr classType) ++ "\n"
            ++ "      for class of kind: " ++ show (ppr (mkClassKind classVars))

      when (length classArgs == length classVars) $
        fail $ "makeAction: cannot derive instance for fully saturated constraint\n"
            ++ "      in: " ++ show (ppr classType) ++ "\n"
            ++ "      expected: * -> " ++ constraintStr ++ "\n"
            ++ "      given: " ++ constraintStr

      when (length classArgs < length classVars - 1) $
        fail $ "makeAction: cannot derive instance for multi-parameter typeclass\n"
            ++ "      in: " ++ show (ppr classType) ++ "\n"
            ++ "      expected: * -> " ++ constraintStr ++ "\n"
            ++ "      given: " ++ show (ppr (mkClassKind $ drop (length classArgs) classVars))

    -- | Converts a class’s methods to constructors for an action type. There
    -- are two operations involved in this conversion:
    --
    --   1. Capitalize the first character of the method name to make it a valid
    --      data constructor name.
    --
    --   2. Replace the type variable bound by the typeclass constraint. To
    --      explain this step, consider the following typeclass:
    --
    --      > class Monad m => MonadFoo m where
    --      >   foo :: String -> m Foo
    --
    --      The signature for @foo@ is really as follows:
    --
    --      > forall m. MonadFoo m => String -> m Foo
    --
    --      However, when converted to a GADT, we want it to look like this:
    --
    --      > data SomeAction f where
    --      >   Foo :: String -> SomeAction Foo
    --
    --      Specifically, we want to remove the @m@ quantified type variable,
    --      and we want to replace it with the @SomeAction@ type constructor
    --      itself.
    --
    --      To accomplish this, 'methodToConstructors' accepts two 'Type's,
    --      where the first is the action type constructor, and the second is
    --      the constraint which must be removed.
    methodsToConstructors :: Type -> Type -> [Dec] -> Q [Con]
    methodsToConstructors actionT classT = traverse (methodToConstructor actionT classT)

    -- | Converts a single class method into a constructor for an action type.
    methodToConstructor :: Type -> Type -> Dec -> Q Con
    methodToConstructor actionT classT (SigD name typ) = do
      let constructorName = methodNameToConstructorName name
      newT <- replaceClassConstraint classT actionT typ
      let (tyVars, ctx, argTs, resultT) = splitFnType newT
          noStrictness = Bang NoSourceUnpackedness NoSourceStrictness
          gadtCon = GadtC [constructorName] (map (noStrictness,) argTs) resultT
      return $ ForallC tyVars ctx gadtCon
    methodToConstructor _ _ _ = fail "methodToConstructor: internal error; report a bug with the monad-mock package"

    -- | Converts an ordinary term-level identifier, which starts with a
    -- lower-case letter, to a data constructor, which starts with an upper-
    -- case letter.
    methodNameToConstructorName :: Name -> Name
    methodNameToConstructorName name = mkName (toUpper c : cs)
      where (c:cs) = nameBase name

    mkInstance :: Type -> Type -> [Dec] -> Q Dec
    mkInstance actionT classT methodSigs = do
      mVar <- newName "m"
      methodImpls <- traverse mkInstanceMethod methodSigs
      let instanceHead = classT `AppT` (ConT ''MockT `AppT` actionT `AppT` VarT mVar)
      return $ InstanceD Nothing [ConT ''Monad `AppT` VarT mVar] instanceHead methodImpls

    mkInstanceMethod :: Dec -> Q Dec
    mkInstanceMethod (SigD name typ) = do
      let constructorName = methodNameToConstructorName name
          arity = fnTypeArity typ

      argNames <- replicateM arity (newName "x")
      let pats = map VarP argNames
          conCall = foldl' AppE (ConE constructorName) (map VarE argNames)
          mockCall = VarE 'mockAction `AppE` LitE (StringL $ nameBase name) `AppE` conCall

      return $ FunD name [Clause pats (NormalB mockCall) []]
    mkInstanceMethod _ = fail "mkInstanceMethod: internal error; report a bug with the monad-mock package"

-- | Implements the class constraint replacement functionality as described in
-- the documentation for 'methodsToConstructors'. Given a type that represents
-- the typeclass whose constraint must be removed and a type used to replace the
-- constrained type variable, it replaces the uses of that type variable
-- everywhere in the quantified type and removes the constraint.
replaceClassConstraint :: Type -> Type -> Type -> Q Type
replaceClassConstraint classType replacementType (ForallT vars preds typ) =
  let -- split the provided class into the typeclass and its arguments:
      --
      --             MonadFoo Int Bool
      --             ^^^^^^^^ ^^^^^^^^
      --                 |       |
      --  unappliedClassType   classTypeArgs
      unappliedClassType = unappliedType classType
      classTypeArgs = typeArgs classType

      -- find the constraint that belongs to the typeclass by searching for the
      -- constaint with the same base type
      ([replacedPred], newPreds) = partition ((unappliedClassType ==) . unappliedType) preds

      -- Get the type vars that we need to replace, and match them with their
      -- replacements. Since we have already validated that classType is the
      -- same as replacedPred but missing one argument (via
      -- assertDerivableConstraint), we can easily align the types we need to
      -- replace with their instantiations.
      replacedVars = typeVarNames replacedPred
      replacementTypes = classTypeArgs ++ [replacementType]

      -- get the remaining vars in the forall quantification after stripping out
      -- the ones we’re replacing
      newVars = filter ((`notElem` replacedVars) . tyVarBndrName) vars

      -- actually perform the replacement substitution for each type var and its replacement
      replacedT = foldl' (flip $ uncurry substituteTypeVar) typ (zip replacedVars replacementTypes)
  in return $ ForallT newVars newPreds replacedT
replaceClassConstraint _ _ _ = fail "replaceClassConstraint: internal error; report a bug with the monad-mock package"

-- | Given the name of a type of kind @* -> *@, generate an 'Action' instance.
--
-- @
-- data FileSystemAction r where
--   ReadFile :: 'FilePath' -> FileSystemAction 'String'
--   WriteFile :: 'FilePath' -> 'String' -> FileSystemAction ()
-- deriving instance 'Eq' (FileSystemAction r)
-- deriving instance 'Show' (FileSystemAction r)
--
-- 'deriveAction' ''FileSystemAction
-- @
deriveAction :: Name -> Q [Dec]
deriveAction name = do
    info <- reify name
    (tyCon, dataCons) <- extractActionInfo info
    instanceDecl <- deriveAction' tyCon dataCons
    return [instanceDecl]
  where
    -- | Given an 'Info', asserts that it represents a type constructor and extracts
    -- its type and constructors.
    extractActionInfo :: Info -> Q (Type, [Con])
    extractActionInfo (TyConI (DataD _ actionName _ _ cons _))
      = return (ConT actionName, cons)
    extractActionInfo _
      = fail "deriveAction: expected type constructor"

-- | The implementation of 'deriveAction', given the type constructor for an
-- action and a list of constructors. This is useful for 'makeAction', since it
-- emits the type definition as part of its result, so there is no 'Name' bound
-- for 'deriveAction' to 'reify'.
deriveAction' :: Type -> [Con] -> Q Dec
deriveAction' tyCon dataCons = do
    eqActionDec <- deriveEqAction dataCons
    let instanceHead = ConT ''Action `AppT` tyCon
    return $ InstanceD Nothing [] instanceHead [eqActionDec]
  where
    -- | Given a list of constructors for a particular type, generates a definition
    -- of 'eqAction'.
    deriveEqAction :: [Con] -> Q Dec
    deriveEqAction cons = do
      clauses <- traverse deriveEqActionCase cons
      let fallthroughClause = Clause [WildP, WildP] (NormalB (ConE 'Nothing)) []
          clauses' = if length clauses > 1 then clauses ++ [fallthroughClause] else clauses
      return $ FunD 'eqAction clauses'

    -- | Given a single constructor for a particular type, generates one of the
    -- cases of 'eqAction'. Used by 'deriveEqAction'.
    deriveEqActionCase :: Con -> Q Clause
    deriveEqActionCase con = do
      binderNames <- replicateM (conNumArgs con) ((,) <$> newName "x" <*> newName "y")

      let name = conName con
          fstPat = ConP name (map (VarP . fst) binderNames)
          sndPat = ConP name (map (VarP . snd) binderNames)

          mkPairwiseComparison x y = VarE '(==) `AppE` VarE x `AppE` VarE y
          pairwiseComparisons = map (uncurry mkPairwiseComparison) binderNames

          bothComparisons x y = VarE '(&&) `AppE` x `AppE` y
          allComparisons = foldr bothComparisons (ConE 'True) pairwiseComparisons

          conditional = CondE allComparisons (ConE 'Just `AppE` ConE 'Refl) (ConE 'Nothing)

      return $ Clause [fstPat, sndPat] (NormalB conditional) []

-- | Extracts the 'Name' of a 'Con'.
conName :: Con -> Name
conName (NormalC name _) = name
conName (RecC name _) = name
conName (InfixC _ name _) = name
conName (ForallC _ _ con) = conName con
conName (GadtC [name] _ _) = name
conName (GadtC names _ _) = error $ "conName: internal error; non-singleton GADT constructor names: " ++ show names
conName (RecGadtC [name] _ _) = name
conName (RecGadtC names _ _) = error $ "conName: internal error; non-singleton GADT record constructor names: " ++ show names

-- | Extracts the number of arguments a 'Con' accepts.
conNumArgs :: Con -> Int
conNumArgs (NormalC _ bts) = length bts
conNumArgs (RecC _ vbts) = length vbts
conNumArgs (InfixC _ _ _) = 2
conNumArgs (ForallC _ _ con) = conNumArgs con
conNumArgs (GadtC _ bts _) = length bts
conNumArgs (RecGadtC _ vbts _) = length vbts

-- | Given a potentially applied type, like @T a b@, returns the base, unapplied
-- type name, like @T@.
unappliedType :: Type -> Type
unappliedType t@ConT{} = t
unappliedType (AppT t _) = unappliedType t
unappliedType other = error $ "unappliedType: internal error; expected plain applied type, given " ++ show other

-- | Like 'unappliedType', but extracts the 'Name' instead of 'Type'.
unappliedTypeName :: Type -> Name
unappliedTypeName t = let (ConT name) = unappliedType t in name

-- | The counterpart to 'unappliedType', this gets the arguments a type is
-- applied to.
typeArgs :: Type -> [Type]
typeArgs (AppT t a) = typeArgs t ++ [a]
typeArgs _          = []

-- | Given a function type, splits it into its components: quantified type
-- variables, constraint context, argument types, and result type. For
-- example, applying 'splitFnType' to
-- @forall a b c. (Foo a, Foo b, Bar c) => a -> b -> c@ produces
-- @([a, b, c], (Foo a, Foo b, Bar c), [a, b], c)@.
splitFnType :: Type -> ([TyVarBndr], Cxt, [Type], Type)
splitFnType (a `AppT` b `AppT` c) | a == ArrowT =
  let (tyVars, ctx, args, result) = splitFnType c
  in (tyVars, ctx, b:args, result)
splitFnType (ForallT tyVars ctx a) =
  let (tyVars', ctx', args, result) = splitFnType a
  in (tyVars ++ tyVars', ctx ++ ctx', args, result)
splitFnType a = ([], [], [], a)

fnTypeArity :: Type -> Int
fnTypeArity t = let (_, _, args, _) = splitFnType t in length args

-- | Substitutes a type variable with a type within a particular type. This is
-- used by 'replaceClassConstraint' to swap out the constrained and quantified
-- type variable with the type variable bound within the record declaration.
substituteTypeVar :: Name -> Type -> Type -> Type
substituteTypeVar initial replacement = doReplace
  where doReplace (ForallT a b t) = ForallT a b (doReplace t)
        doReplace (AppT a b) = AppT (doReplace a) (doReplace b)
        doReplace (SigT t k) = SigT (doReplace t) k
        doReplace t@(VarT n)
          | n == initial = replacement
          | otherwise    = t
        doReplace other = other

-- |  Given a type, returns a list of all of the unique type variables contained
-- within it.
typeVarNames :: Type -> [Name]
typeVarNames (VarT n) = [n]
typeVarNames (AppT a b) = nub (typeVarNames a ++ typeVarNames b)
typeVarNames _ = []

-- | Given any arbitrary 'TyVarBndr', gets its 'Name'.
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name

-- | Given some 'Info' about a class, get its methods as 'SigD' declarations.
classMethods :: Info -> Q [Dec]
classMethods (ClassI (ClassD _ _ _ _ methods) _) = return $ removeDefaultSigs methods
  where removeDefaultSigs = filter $ \case
          DefaultSigD{} -> False
          _             -> True
classMethods other = fail $ "classMethods: internal error; expected a class type, given " ++ show other

{------------------------------------------------------------------------------|
| The following definitions abstract over differences in base and              |
| template-haskell between GHC versions. This allows the same code to work     |
| without writing CPP everywhere and ending up with a small mess.              |
|------------------------------------------------------------------------------}

standaloneDeriveD' :: Cxt -> Type -> Dec
#if MIN_VERSION_template_haskell(2,12,0)
standaloneDeriveD' = StandaloneDerivD Nothing
#else
standaloneDeriveD' = StandaloneDerivD
#endif
