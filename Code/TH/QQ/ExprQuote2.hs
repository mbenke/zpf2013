-- Based on http://www.haskell.org/haskellwiki/Quasiquotation
module ExprQuote2 where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Expr2

expr  :: QuasiQuoter
expr  =  QuasiQuoter 
  { quoteExp = quoteExprExp
  , quotePat = quoteExprPat
  , quoteDec = undefined
  , quoteType = undefined
  }

quoteExprExp s = do
  pos <- getPosition
  exp <- parseExp pos s
  dataToExpQ (const Nothing  `extQ` antiExprExp) exp

-- dataToExpQ :: Data a => (forall b. Data b => b -> Maybe (Q Exp)) -> a -> Q Exp

quoteExprPat s = do
  pos <- getPosition
  exp <- parseExp pos s
  dataToPatQ (const Nothing `extQ` antiExprPat) exp

antiExprPat :: Exp -> Maybe (TH.Q TH.Pat)
antiExprPat (EMetaVar v) = Just $ TH.varP (TH.mkName v)
antiExprPat _ = Nothing

antiExprExp :: Exp -> Maybe (TH.Q TH.Exp)
antiExprExp (EMetaVar v) = Just $ TH.varE (TH.mkName v)
antiExprExp _ = Nothing

getPosition = fmap transPos TH.location where
  transPos loc = (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
                 
