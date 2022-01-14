import Text.ParserCombinators.Parsec

data Term = Var String | Application Term Term | Lambda String Term

instance Show Term where
  show (Var x) = x
  show (Lambda x lt) = "(λ " ++ x ++ " . " ++ show lt ++ ")"
  show (Application lt1 lt2) = "(" ++ show lt1 ++ " " ++ show lt2 ++ ")"


freeVars :: Term -> [String]
freeVars = aux []
  where
    aux env (Var x) | elem x env = [] 
                    | otherwise  = [x]
    aux env (Lambda x e) = aux (x:env) e
    aux env (Application e1 e2) = aux env e1 ++ aux env e2


normalForm :: Term -> Bool
normalForm (Var _)                      = True
normalForm (Application (Lambda _ _) _) = False
normalForm (Application e1 e2)          = normalForm e1 && normalForm e2
normalForm (Lambda _ e)                 = normalForm e


listOVars :: [String]
listOVars = [s:(show d)| s <- ['a'..'z'], d <- [1,2]]


substitute :: Term -> String -> Term -> Term
substitute a@(Var y) x f  | x == y = f
                        | otherwise = a
substitute a@(Lambda y e) x f 
                        | x == y = a
                        | otherwise = (Lambda y (substitute e x f))
substitute (Application e1 e2) x f = Application (substitute e1 x f) (substitute e2 x f)


alphaRename :: Term -> String -> Term
alphaRename = ar []
  where
  ar b m@(Var y) x      | elem y b = Var x
                        | otherwise  = m
  ar b q@(Lambda y e) x | x == y  = q
                        | null b    = Lambda x $ ar (y:b) e x
                        | otherwise = Lambda y $ ar b e x
  ar b (Application e1 e2) x = (Application (ar b e1 x) (ar b e2 x))


boundVars :: Term -> [String]
boundVars (Var v) = []
boundVars (Application e1 e2) = boundVars e1 ++ boundVars e2
boundVars (Lambda x e) = [x] ++ boundVars e


betaReduce :: Term -> Term -> Term
betaReduce (Lambda x e) v  = case v of
  Var s | s == x    -> e 
        | elem s bs -> let s1 = Var $ head listOVars in substitute e x s1
        | otherwise -> st v
  x@(Lambda s e1)   -> st $ x
  Application e1 e2 -> betaReduce (br e1) e2
  where
    bs = boundVars e
    br = betaReduce (Lambda x e)
    st = substitute e x


normalizeN :: Term -> Term
normalizeN (Var x) = Var x 
normalizeN (Lambda x e) = Lambda x (normalizeN e)
normalizeN (Application (Lambda x e1) (Var y))
  | not (normalForm e1) = betaReduce (Lambda x (normalizeN e1)) (Var y)
  | otherwise = substitute e1 x (Var y)
normalizeN (Application (Lambda x e1) e2)
  | not (normalForm e2) = Application (Lambda x e1) (normalizeN e2)
  | otherwise = substitute e1 x e2
normalizeN (Application e1 e2) 
  | not (normalForm e2) = Application e1 (normalizeN e2) 
  | otherwise = Application (normalizeN e1) e2


normalize :: Term -> Term
normalize t | normalForm t = t
             | otherwise    = let e = normalizeN t in normalize e


data TermS = VarS String | ApplicationS TermS [TermS] | LambdaS [String] TermS

instance Show TermS where
  show (VarS x) = x
  show (LambdaS xe e) = "(λ " ++ (charSSS xe) ++ " . " ++ show e ++ ")"
  show (ApplicationS e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"


deSugar :: TermS -> Term
deSugar (VarS x) = Var x
deSugar (ApplicationS e1 es) = case es of
          []  -> deSugar e1
          [x] -> Application (deSugar e1) (deSugar x)
          (x:xs)  -> deSugar $ ApplicationS (ApplicationS e1 [x]) xs
deSugar (LambdaS es e) = case es of
          []  -> error "Stuck!"
          [x] -> Lambda x (deSugar e)
          (x:xs) -> Lambda x (deSugar $ LambdaS xs e)


removeB :: String -> String
removeB a@([x])
      | x == ' ' = []
      | otherwise= a
removeB (x:xs)
      | x == ' ' = removeB xs
      | otherwise= x:(removeB xs)


charS :: Char -> String
charS c = (c:[])


charSS :: [Char] -> [String]
charSS [x]    = (charS x):[]
charSS (x:xs) = (charS x):(charSS xs)


charSSS :: [String] -> String
charSSS [x]   = x
charSSS (x:xs)= x ++ " " ++ (charSSS xs)


bindAlpha :: Parser Char
bindAlpha = oneOf ['t','m','n','f','x']


parseVars :: Parser TermS
parseVars = VarS <$> charS <$> bindAlpha


parseLambdaS :: [Char] -> TermS -> TermS
parseLambdaS xs body = LambdaS (charSS xs) body


parseLambdaBase :: Parser TermS
parseLambdaBase = do
  char 'λ'
  bindings <- many1 bindAlpha
  char '.'
  parseLambdaS bindings <$> parseTermS


parseApplicationS :: Parser TermS
parseApplicationS = ApplicationS <$> parseTermBase <*> many1 parseTermBase


converApplicationS :: [TermS] -> TermS
converApplicationS [x] = x
converApplicationS (x:xs) = ApplicationS x xs


p :: Parser TermS 
p = char '(' *> parseTermS <* char ')'


parseTermBase :: Parser TermS
parseTermBase = try parseVars <|> try p


parseTermS :: Parser TermS
parseTermS = try parseLambdaBase <|> try parseApplicationS <|> parseTermBase


parseTermSS :: Parser TermS
parseTermSS = converApplicationS <$> many1 parseTermS


lsp :: Parser TermS
lsp = converApplicationS <$> (try (many1 parseTermBase) <|> many1 parseTermSS)


parseRules :: String -> TermS
parseRules s = case parse lsp ['C','H','E','N'] s of
  (Left e) -> error (show e)
  (Right x) -> x


testCaseS :: TermS
testCaseS = (parseRules . removeB) lambdaCase


testCase :: Term
testCase = (normalize . deSugar) testCaseS


zero :: Term
zero = Lambda "f" $ Lambda "x" $ Var "x"


one :: Term
one = Lambda "f" $ Lambda "x" $ Application (Var "f") (Var "x")


suc :: Term
suc = Lambda "n" $ Lambda "f" $ Lambda "x" $ Application (Var "f") (Application (Application (Var "n") (Var "f")) (Var "x"))


lambdaCase = "(λ t . (λ m n . n (λ n f x . n f (f x)) m) ((λ n . (λ m n f . m (n f)) n n) ((λ n . (λ m n f . m (n f)) n n) t)) ((λ n . (λ m n f . m (n f)) n n) t)) ((λ m n . n (λ n f x . n f (f x)) m) ((λ m n f . m (n f)) (λ f x . x) ((λ m n . n (λ n f x . n f (f x)) m) ((λ n f x . n f (f x)) (λ f x . x)) ((λ n f x . n f (f x)) (λ f x . x)))) ((λ n f x . n f (f x)) ((λ n f x . n f (f x)) (λ f x . x))))"


main = do
  print(testCase)