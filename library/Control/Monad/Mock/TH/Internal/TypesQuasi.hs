{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Mock.TH.Internal.TypesQuasi (ts) where

import Control.Monad ((<=<))
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Meta.Syntax.Translate (toType)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax hiding (Loc)
import Language.Haskell.TH.Quote

-- | A quasi-quoter like the built-in @[t| ... |]@ quasi-quoter, but produces
-- a /list/ of types instead of a single type. Each type should be separated by
-- a comma.
--
-- >>> [ts| Bool, (), String |]
-- [ConT GHC.Types.Bool,ConT GHC.Tuple.(),ConT GHC.Base.String]
-- >>> [ts| Maybe Int, Monad m |]
-- [AppT (ConT GHC.Base.Maybe) (ConT GHC.Types.Int),AppT (ConT GHC.Base.Monad) (VarT m)]
ts :: QuasiQuoter
ts = QuasiQuoter
  { quoteExp = \str -> case parseTypesSplitOnCommas str of
      ParseOk tys -> lift =<< mapM resolveTypeNames tys
      ParseFailed _ msg -> fail msg
  , quotePat = error "ts can only be used in an expression context"
  , quoteType = error "ts can only be used in an expression context"
  , quoteDec = error "ts can only be used in an expression context"
  }

parseTypesSplitOnCommas :: String -> ParseResult [Type]
parseTypesSplitOnCommas = fmap (map toType) . mapM parseType <=< lexSplitOnCommas

lexSplitOnCommas :: String -> ParseResult [String]
lexSplitOnCommas str = splitOnSrcSpans str <$> lexSplittingCommas str

splitOnSrcSpans :: String -> [SrcSpan] -> [String]
splitOnSrcSpans str [] = [str]
splitOnSrcSpans str spans@(x:xs) = case x of
  SrcSpan { srcSpanStartLine = line, srcSpanStartColumn = col }
    | line > 1 ->
      let (l, _:ls) = break (== '\n') str
          (r:rs) = splitOnSrcSpans ls (map advanceLine spans)
      in (l ++ "\n" ++ r) : rs
    | col > 1 ->
      let (currentLs, nextLs) = span ((== line) . srcSpanStartLine) spans
          (c:cs) = str
          (r:rs) = splitOnSrcSpans cs (map advanceColumn currentLs ++ nextLs)
      in (c : r) : rs
    | otherwise ->
      let (currentLs, nextLs) = span ((== line) . srcSpanStartLine) xs
          (_:cs) = str
      in "" : splitOnSrcSpans cs (map advanceColumn currentLs ++ nextLs)


advanceLine :: SrcSpan -> SrcSpan
advanceLine s@SrcSpan { srcSpanStartLine = line } = s { srcSpanStartLine = line - 1 }

advanceColumn :: SrcSpan -> SrcSpan
advanceColumn s@SrcSpan { srcSpanStartColumn = col } = s { srcSpanStartColumn = col - 1 }

lexSplittingCommas :: String -> ParseResult [SrcSpan]
lexSplittingCommas = fmap splittingCommas . lexTokenStream

splittingCommas :: [Loc Token] -> [SrcSpan]
splittingCommas = map loc . go
  where go [] = []
        go (x@Loc{ unLoc = Comma }:xs) = x : go xs
        go (Loc{ unLoc = LeftParen }:xs) = go $ skipUntil RightParen xs
        go (Loc{ unLoc = LeftSquare }:xs) = go $ skipUntil RightSquare xs
        go (Loc{ unLoc = LeftCurly }:xs) = go $ skipUntil RightCurly xs
        go (_:xs) = go xs

        skipUntil _ [] = []
        skipUntil d (Loc{ unLoc = LeftParen }:xs) = skipUntil d $ skipUntil RightParen xs
        skipUntil d (Loc{ unLoc = LeftSquare }:xs) = skipUntil d $ skipUntil RightSquare xs
        skipUntil d (Loc{ unLoc = LeftCurly }:xs) = skipUntil d $ skipUntil RightCurly xs
        skipUntil d (Loc{ unLoc = t }:xs)
          | t == d    = xs
          | otherwise = skipUntil d xs

resolveTypeNames :: Type -> Q Type
resolveTypeNames (AppT a b) = AppT <$> resolveTypeNames a <*> resolveTypeNames b
resolveTypeNames (ConT nm) = ConT <$> resolveTypeName nm
resolveTypeNames (ForallT tyVars ctx t) = ForallT tyVars <$> mapM resolveTypeNames ctx <*> resolveTypeNames t
resolveTypeNames (InfixT a n b) = InfixT <$> resolveTypeNames a <*> resolveTypeName n <*> resolveTypeNames b
resolveTypeNames (ParensT t) = ParensT <$> resolveTypeNames t
resolveTypeNames (SigT t k) = SigT <$> resolveTypeNames t <*> resolveTypeNames k
resolveTypeNames (UInfixT a n b) = UInfixT <$> resolveTypeNames a <*> resolveTypeName n <*> resolveTypeNames b
resolveTypeNames t@ArrowT{} = return t
resolveTypeNames t@ConstraintT = return t
resolveTypeNames t@EqualityT = return t
resolveTypeNames t@ListT = return t
resolveTypeNames t@LitT{} = return t
resolveTypeNames t@PromotedConsT = return t
resolveTypeNames t@PromotedNilT = return t
resolveTypeNames t@PromotedT{} = return t
resolveTypeNames t@PromotedTupleT{} = return t
resolveTypeNames t@StarT = return t
resolveTypeNames t@TupleT{} = return t
resolveTypeNames t@UnboxedTupleT{} = return t
resolveTypeNames t@VarT{} = return t
resolveTypeNames t@WildCardT = return t
#if MIN_VERSION_template_haskell(2,12,0)
resolveTypeNames t@UnboxedSumT{} = return t
#endif

resolveTypeName :: Name -> Q Name
resolveTypeName (Name (OccName str) NameS) = lookupTypeName str >>= \case
  Just nm -> return nm
  Nothing -> fail $ "unbound type name ‘" ++ str ++ "’"
resolveTypeName nm = return nm
