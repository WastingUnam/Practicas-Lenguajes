module Interp where

import Grammars
import Data.List (nub, (\\))

-- RETO 3: sustitucion nominal que evita captura

freeVars :: ASA -> [String]
freeVars expr = case expr of
  Num _          -> []
  Boolean _      -> []
  Id x           -> [x]
  Add es         -> nub $ concatMap freeVars es
  Sub es         -> nub $ concatMap freeVars es
  Mul es         -> nub $ concatMap freeVars es
  Div es         -> nub $ concatMap freeVars es
  And es         -> nub $ concatMap freeVars es
  Or es          -> nub $ concatMap freeVars es
  Lt es          -> nub $ concatMap freeVars es
  Gt es          -> nub $ concatMap freeVars es
  Le es          -> nub $ concatMap freeVars es
  Ge es          -> nub $ concatMap freeVars es
  Expt e1 e2     -> nub $ freeVars e1 ++ freeVars e2
  EqP e1 e2      -> nub $ freeVars e1 ++ freeVars e2
  Not e          -> freeVars e
  Add1 e         -> freeVars e
  Sub1 e         -> freeVars e
  ZeroP e        -> freeVars e
  Let binds body ->
    let varsInVals = concatMap (freeVars . snd) binds
        boundVars  = map fst binds
        bodyFVs    = freeVars body \\ boundVars
    in nub (varsInVals ++ bodyFVs)
  LetStar [] body -> freeVars body
  LetStar ((x, e) : rest) body ->
    nub (freeVars e ++ (freeVars (LetStar rest body) \\ [x]))

names :: ASA -> [String]
names expr = case expr of
  Num _          -> []
  Boolean _      -> []
  Id x           -> [x]
  Add es         -> nub $ concatMap names es
  Sub es         -> nub $ concatMap names es
  Mul es         -> nub $ concatMap names es
  Div es         -> nub $ concatMap names es
  And es         -> nub $ concatMap names es
  Or es          -> nub $ concatMap names es
  Lt es          -> nub $ concatMap names es
  Gt es          -> nub $ concatMap names es
  Le es          -> nub $ concatMap names es
  Ge es          -> nub $ concatMap names es
  Expt e1 e2     -> nub $ names e1 ++ names e2
  EqP e1 e2      -> nub $ names e1 ++ names e2
  Not e          -> names e
  Add1 e         -> names e
  Sub1 e         -> names e
  ZeroP e        -> names e
  Let binds body ->
    nub $ map fst binds ++ concatMap (names . snd) binds ++ names body
  LetStar binds body ->
    nub $ map fst binds ++ concatMap (names . snd) binds ++ names body

freshName :: [String] -> String
freshName forbidden = head [ c | c <- candidates, c `notElem` forbidden ]
  where
    candidates = [ [x] | x <- ['z', 'y' .. 'a'] ] ++ [ "x" ++ show i | i <- [(1 :: Int)..] ]

sust :: ASA -> String -> ASA -> ASA
sust expr x s = sustMany expr [(x, s)]

sustMany :: ASA -> [Binding] -> ASA
sustMany expr [] = expr
sustMany expr env = case expr of
  Num n          -> Num n
  Boolean b      -> Boolean b
  Id v           -> case lookup v env of
                      Just val -> val
                      Nothing  -> Id v
  Add es         -> Add (map rec es)
  Sub es         -> Sub (map rec es)
  Mul es         -> Mul (map rec es)
  Div es         -> Div (map rec es)
  And es         -> And (map rec es)
  Or es          -> Or (map rec es)
  Lt es          -> Lt (map rec es)
  Gt es          -> Gt (map rec es)
  Le es          -> Le (map rec es)
  Ge es          -> Ge (map rec es)
  Expt e1 e2     -> Expt (rec e1) (rec e2)
  EqP e1 e2      -> EqP (rec e1) (rec e2)
  Not e          -> Not (rec e)
  Add1 e         -> Add1 (rec e)
  Sub1 e         -> Sub1 (rec e)
  ZeroP e        -> ZeroP (rec e)

  Let binds body ->
    let binds'      = [ (v, rec e) | (v, e) <- binds ]
        boundVars   = map fst binds
        activeEnv   = filter (\(v, _) -> v `notElem` boundVars) env
        freeInSubst = concatMap (freeVars . snd) activeEnv
        reserved    = nub (names body ++ map fst env ++ freeInSubst ++ boundVars)
        (newBinds, newBody) = renameLet binds' body reserved freeInSubst
    in Let newBinds (sustMany newBody activeEnv)

  LetStar [] body -> LetStar [] (rec body)
  LetStar ((x, e) : rest) body ->
    let e' = rec e
        freeInSubst = concatMap (freeVars . snd) env
        reserved = nub (names (LetStar rest body) ++ map fst env ++ freeInSubst ++ [x])
    in if x `elem` freeInSubst
       then
         let z = freshName reserved
             restBody' = sust (LetStar rest body) x (Id z)
         in case restBody' of
              LetStar rest' body' ->
                let activeEnv = filter (\(v, _) -> v /= z) env
                in case sustMany (LetStar rest' body') activeEnv of
                     LetStar finalRest finalBody -> LetStar ((z, e') : finalRest) finalBody
                     other                       -> other
              _ -> error "Error inesperado en renombrado de LetStar"
       else
         let activeEnv = filter (\(v, _) -> v /= x) env
         in case sustMany (LetStar rest body) activeEnv of
              LetStar rest' body' -> LetStar ((x, e') : rest') body'
              other               -> other

  where
    rec e = sustMany e env

    renameLet [] b _ _ = ([], b)
    renameLet ((v, val) : bs) b res fvs
      | v `elem` fvs =
          let fresh = freshName res
              b'    = sust b v (Id fresh)
              res'  = fresh : res
              (bs', b'') = renameLet bs b' res' fvs
          in ((fresh, val) : bs', b'')
      | otherwise =
          let (bs', b') = renameLet bs b res fvs
          in ((v, val) : bs', b')

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.

bigStep :: ASA -> Maybe ASA
bigStep expr = case expr of
  Num n
    | n >= 0    -> Just (Num n)
    | otherwise -> Nothing
  Boolean b     -> Just (Boolean b)
  Id _          -> Nothing

  Add es -> do
    nums <- mapM evalNum es
    Just (Num (sum nums))

  Sub es -> do
    nums <- mapM evalNum es
    case nums of
      (x:xs) -> Just (Num (max 0 (foldl (-) x xs)))
      []     -> Nothing

  Mul es -> do
    nums <- mapM evalNum es
    Just (Num (product nums))

  Div es -> do
    nums <- mapM evalNum es
    case nums of
      (x:xs) | all (/= 0) xs -> Just (Num (foldl div x xs))
      _                      -> Nothing

  And es -> do
    bools <- mapM evalBool es
    Just (Boolean (and bools))

  Or es -> do
    bools <- mapM evalBool es
    Just (Boolean (or bools))

  Lt es -> do
    nums <- mapM evalNum es
    Just (Boolean (isOrdered (<) nums))

  Gt es -> do
    nums <- mapM evalNum es
    Just (Boolean (isOrdered (>) nums))

  Le es -> do
    nums <- mapM evalNum es
    Just (Boolean (isOrdered (<=) nums))

  Ge es -> do
    nums <- mapM evalNum es
    Just (Boolean (isOrdered (>=) nums))

  Expt e1 e2 -> do
    n1 <- evalNum e1
    n2 <- evalNum e2
    Just (Num (n1 ^ n2))

  EqP e1 e2 -> do
    v1 <- bigStep e1
    v2 <- bigStep e2
    case (v1, v2) of
      (Num n1, Num n2)         -> Just (Boolean (n1 == n2))
      (Boolean b1, Boolean b2) -> Just (Boolean (b1 == b2))
      _                        -> Nothing

  Not e -> do
    v <- bigStep e
    case v of
      Boolean b -> Just (Boolean (not b))
      Num _     -> Just (Boolean False)
      _         -> Nothing

  Add1 e -> do
    n <- evalNum e
    Just (Num (n + 1))

  Sub1 e -> do
    n <- evalNum e
    Just (Num (max 0 (n - 1)))

  ZeroP e -> do
    n <- evalNum e
    Just (Boolean (n == 0))

  Let binds body ->
    let vars = map fst binds
    in if length vars /= length (nub vars)
       then Nothing
       else do
         vals <- mapM (bigStep . snd) binds
         bigStep (sustMany body (zip vars vals))

  LetStar [] body -> bigStep body
  LetStar ((x, e) : rest) body -> do
    v <- bigStep e
    bigStep (sust (LetStar rest body) x v)

  where
    evalNum e = do
      res <- bigStep e
      case res of
        Num n -> Just n
        _     -> Nothing

    evalBool e = do
      res <- bigStep e
      case res of
        Boolean b -> Just b
        _         -> Nothing

    isOrdered rel xs = all (uncurry rel) (zip xs (tail xs))