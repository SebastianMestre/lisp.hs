module Main where

import qualified Data.Map as Map
import Data.Char
import Data.Functor
import Data.Maybe
import Control.Applicative
import Control.Monad

newtype Parser a = Parser {
  run :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser g) = Parser h
    where h s = (g s) >>= (\(s, a) -> Just (s, f a))

instance Applicative Parser where
  pure a = Parser g
    where g s = Just (s, a)
  (Parser f) <*> (Parser g) = Parser h
    where
      h s = (f s) >>= (\(s2, fa) ->
            (g s2) >>= (\(s3, a) ->
            Just (s3, fa a)))

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser f) <|> (Parser g) = Parser h
    where
      h s = let r1 = f s
          in case r1 of
            Nothing -> g s
            _ -> r1

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f []                 = Nothing
    f (s:ss) | s == c    = Just (ss, s)
             | otherwise = Nothing

parseString s = traverse parseChar s

validate :: Parser a -> (a -> Bool) -> Parser a
validate (Parser f) p = Parser g
  where g s = f s >>= (\(rs, x) ->
              if p x
                then Just (rs, x)
                else Nothing)

parseMany :: (Char -> Bool) -> Parser String
parseMany p = Parser g
  where
    g []                 = Just ([], [])
    g (x:xs) | p x       = let Just (rs, bb) = g xs
                           in Just (rs, x:bb)
             | otherwise = Just(x:xs, [])

parseSome p = validate (parseMany p) (not . null)

parseInt :: Parser Int
parseInt = read <$> parseSome isDigit

parseId :: Parser String
parseId = parseSome isAlpha

parseWs :: Parser String
parseWs = parseSome isSpace

data Cst
  = IntCst Int
  | IdCst String
  | ListCst [Cst]
  deriving Show

parseAtom = (fmap IntCst parseInt) <|> (fmap IdCst parseId)


parseList =
  parseChar '(' *>
  liftA2 (:) parseExpr (many (parseWs *> parseExpr)) <*
  parseChar ')'

parseExpr = parseAtom <|> (fmap ListCst parseList)

data Expr
  = IntExpr Int
  | IdExpr String
  | ConsExpr Expr Expr
  | NilExpr
  deriving Show

desugar :: Cst -> Expr
desugar (IntCst n) = (IntExpr n)
desugar (IdCst s) = (IdExpr s)
desugar (ListCst l) = desugarList l
  where desugarList [] = NilExpr
        desugarList (x:xs) = ConsExpr (desugar x) (desugarList xs)

type Env = Map.Map String Expr

eval :: Env -> Expr -> Expr
eval env n@(IntExpr _)  = n
eval env (IdExpr id)  = env Map.! id
eval env (ConsExpr (IdExpr "quote") (ConsExpr e NilExpr))  = e
eval env (ConsExpr (IdExpr "quote") e)  = undefined
eval env (ConsExpr (IdExpr "eval") (ConsExpr e NilExpr))  = eval env $ eval env e
eval env (ConsExpr (IdExpr "eval")  e)  = undefined
eval env (ConsExpr (IdExpr "cons") (ConsExpr e1 (ConsExpr e2 NilExpr))) = ConsExpr (eval env e1) (eval env e2)
eval env (ConsExpr (IdExpr "let") (ConsExpr (ConsExpr (IdExpr id) (ConsExpr e1 NilExpr)) (ConsExpr e2 NilExpr))) =
  let bound = eval env e1
      newenv = Map.insert id bound env
  in
      eval newenv e2
eval env e  = undefined

interpret :: String -> Maybe Expr
interpret str = do
  (_, cst) <- run parseExpr str
  return $ eval Map.empty $ desugar cst

prettyPrint :: Expr -> String
prettyPrint (IdExpr id) = id
prettyPrint (IntExpr x) = show x
prettyPrint NilExpr = "<nil>"
prettyPrint l@(ConsExpr h t) = "(" ++ pInner l ++ ")"
  where
  pInner :: Expr -> String
  pInner (ConsExpr h t) = prettyPrint h ++ pTail t
    where
    pTail NilExpr = ""
    pTail t@(ConsExpr _ _) = " " ++ pInner t
    pTail t = " . " ++ prettyPrint t

go x = interpret x >>= (return . prettyPrint)

main :: IO ()
main = return ()
