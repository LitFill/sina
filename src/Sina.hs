{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Sina where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.RWS
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Text.Parsec hiding (count)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Data.Map qualified as Map
import Data.Set qualified as Set


type Name = String


data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Name Expr
    | EApp Expr Expr
    | ELet Name Expr Expr
    | EInt Integer
    | EBool Bool
    | EBin Oper Expr Expr
    deriving (Eq, Show)


data Oper = Add | Sub deriving (Eq, Show)


newtype TVar = TV Name deriving (Eq, Ord, Show)
data Type
    = TCon Name [Type]
    | TVar TVar
    deriving (Eq, Show)


pattern TInt :: Type
pattern TInt = TCon "Int" []


pattern TBool :: Type
pattern TBool = TCon "Bool" []


infixr 2 :->
pattern (:->) :: Type -> Type -> Type
pattern a :-> b = TCon "->" [a, b]


--------------
--- Parser ---
--------------

ws :: Parser ()
ws = spaces


reserved :: String -> Parser ()
reserved s = try $ do
    _ <- string s
    notFollowedBy alphaNum
    ws


identifier :: Parser String
identifier = try $ do
    name <- many1 alphaNum
    if name `elem` ["if", "then", "else", "let", "in", "True", "False"]
        then fail $ "reserved word " ++ show name
        else name <$ ws

operTable :: OperatorTable String () Identity Expr
operTable =
    [
        [ Infix (spaces *> char '+' <* spaces >> return (EBin Add)) AssocLeft
        , Infix (spaces *> char '-' <* spaces >> return (EBin Sub)) AssocLeft
        ]
    ]


parseExpr :: Parser Expr
parseExpr = buildExpressionParser operTable parseTerm


parseTerm :: Parser Expr
parseTerm =
    try parseIf
        <|> try parseAbs
        <|> try parseLet
        <|> parseAppExpr


parseAppExpr :: Parser Expr
parseAppExpr = do
    f <- parseLit
    args <- many parseLit
    return $ foldl EApp f args


parseIf :: Parser Expr
parseIf = do
    reserved "if"
    a <- parseExpr
    reserved "then"
    b <- parseExpr
    reserved "else"
    EIf a b <$> parseExpr


parseAbs :: Parser Expr
parseAbs = do
    char '\\' *> ws
    var <- many1 alphaNum
    ws
    string "->" *> ws
    EAbs var <$> parseExpr


parseLet :: Parser Expr
parseLet = do
    reserved "let"
    var <- many1 alphaNum
    ws *> char '=' *> ws
    expr <- parseExpr
    reserved "in"
    ELet var expr <$> parseExpr


parseLit :: Parser Expr
parseLit =
    (EInt . read <$> many1 digit <* ws)
        <|> (EBool True <$ reserved "True")
        <|> (EBool False <$ reserved "False")
        <|> (char '(' *> ws *> parseExpr <* char ')' <* ws)
        <|> (EVar <$> identifier)


--------------
--- Runner ---
--------------

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f = \case
    Left l -> Left $ f l
    Right r -> Right r


data Scheme = Forall (Set.Set TVar) Type


data Constraint = Constraint Type Type


type Context = Map.Map Name Scheme


type Count = Int


type Constraints = [Constraint]


type Infer a = RWST Context Constraints Count (Except String) a


constrain :: Type -> Type -> Infer ()
constrain t = tell . (: []) . Constraint t


fresh :: Infer Type
fresh = do
    count <- get
    put (count + 1)
    return . TVar . TV $ show count


type Subst = Map.Map TVar Type


class Substitutable a where
    apply :: Subst -> a -> a
    tvs :: a -> Set.Set TVar


instance Substitutable Type where
    tvs = \case
        TVar tv -> Set.singleton tv
        TCon _ ts -> foldr (Set.union . tvs) Set.empty ts
    apply s = \case
        t@(TVar tv) -> Map.findWithDefault t tv s
        TCon c ts -> TCon c $ map (apply s) ts


instance Substitutable Scheme where
    tvs (Forall vs t) = tvs t `Set.difference` vs
    apply s (Forall vs t) = Forall vs $ apply (foldr Map.delete s vs) t


instance Substitutable Constraint where
    tvs (Constraint t1 t2) = tvs t1 `Set.union` tvs t2
    apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)


instance (Substitutable a) => Substitutable [a] where
    tvs = foldr (Set.union . tvs) Set.empty
    apply s = map (apply s)


------------------
--- Pretty Print ---
------------------

ppType :: Type -> String
ppType = \case
    TVar (TV n) -> "?" <> n
    TCon "->" [a, b] -> ppArrow a b
    TCon n [] -> n
    TCon n ts -> n <> " " <> unwords (map ppParenType ts)


ppParenType :: Type -> String
ppParenType t@(TCon "->" _) = "(" <> ppType t <> ")"
ppParenType t = ppType t


ppArrow :: Type -> Type -> String
ppArrow a b = ppParenType a <> " -> " <> ppType b


ppScheme :: Scheme -> String
ppScheme (Forall vs t)
    | Set.null vs = ppType t
    | otherwise =
        "forall " <> unwords (map (\(TV n) -> n) (Set.toList vs)) <> ". " <> ppType t


ppConstraint :: Constraint -> String
ppConstraint (Constraint a b) = ppType a <> " ~ " <> ppType b


ppSubst :: Subst -> String
ppSubst s
    | Map.null s = "{}"
    | otherwise =
        "{ "
            <> intercalate
                ", "
                [show (TV n) <> " |-> " <> ppType t | (TV n, t) <- Map.toList s]
            <> " }"


ppExpr :: Expr -> String
ppExpr = \case
    EInt n -> show n
    EBool True -> "True"
    EBool False -> "False"
    EVar v -> v
    EAbs p b -> "\\" <> p <> " -> " <> ppExpr b
    EApp f a -> ppApp f <> " " <> ppAtom a
    EBin op a b -> ppAtom a <> " " <> ppOper op <> " " <> ppAtom b
    EIf c a b -> "if " <> ppExpr c <> " then " <> ppExpr a <> " else " <> ppExpr b
    ELet v e b -> "let " <> v <> " = " <> ppExpr e <> " in " <> ppExpr b


ppAtom :: Expr -> String
ppAtom e@(EVar _) = ppExpr e
ppAtom e@(EInt _) = ppExpr e
ppAtom e@(EBool _) = ppExpr e
ppAtom e@(EAbs _ _) = "(" <> ppExpr e <> ")"
ppAtom e@(EApp _ _) = "(" <> ppExpr e <> ")"
ppAtom e@EBin {} = "(" <> ppExpr e <> ")"
ppAtom e@EIf {} = "(" <> ppExpr e <> ")"
ppAtom e@ELet {} = "(" <> ppExpr e <> ")"


ppApp :: Expr -> String
ppApp e@(EApp _ _) = ppExpr e
ppApp e = ppAtom e


ppOper :: Oper -> String
ppOper Add = "+"
ppOper Sub = "-"


compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) (b `u` a)
  where
    u = Map.union


generalize :: Context -> Type -> Scheme
generalize ctx t = Forall (tvs t `Set.difference` tvs (Map.elems ctx)) t


instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
    let vars = Set.toList vs
    ftvs <- traverse (const fresh) vars
    let subst = Map.fromList (zip vars ftvs)
    return $ apply subst t


infer :: Expr -> Infer Type
infer = \case
    EInt _ -> pure TInt
    EBool _ -> pure TBool
    EVar v -> do
        ctx <- ask
        Map.lookup v ctx & \case
            Just t -> instantiate t
            Nothing -> throwError $ "Undefined variable " ++ v
    EIf c a b -> do
        ct <- infer c
        at <- infer a
        bt <- infer b
        constrain ct TBool
        constrain at bt
        return at
    EAbs p e -> do
        pt <- fresh
        let ps = Forall Set.empty pt
        et <- local (Map.insert p ps) (infer e)
        return $ pt :-> et
    EApp f a -> do
        ft <- infer f
        at <- infer a
        rt <- fresh
        constrain ft (at :-> rt)
        return rt
    EBin o a b -> do
        at <- infer a
        bt <- infer b
        t <- fresh
        case o of
            Add -> constrain (TInt :-> (TInt :-> TInt)) (at :-> (bt :-> t))
            Sub -> constrain (TInt :-> (TInt :-> TInt)) (at :-> (bt :-> t))
        return t
    ELet v e b -> do
        et <- infer e
        ctx <- ask
        let es = generalize ctx et
        local (Map.insert v es) (infer b)


type Solve a = Except String a


unify :: Type -> Type -> Solve Subst
unify = \cases
    a b | a == b -> return Map.empty
    (TVar v) t -> bind v t
    t (TVar v) -> bind v t
    (TCon n1 ts1) (TCon n2 ts2)
        | n1 /= n2 ->
            throwError $
                concat ["Type mismatch ", show (TCon n1 ts1), " and ", show (TCon n2 ts2)]
        | otherwise -> unifyMany ts1 ts2
  where
    unifyMany = \cases
        [] [] -> return Map.empty
        [] _ -> throwError "Mismatched type constructor arity"
        _ [] -> throwError "Mismatched type constructor arity"
        (t1 : ts1) (t2 : ts2) -> do
            s1 <- unify t1 t2
            s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
            return $ s2 `compose` s1


bind :: TVar -> Type -> Solve Subst
bind v t
    | v `Set.member` tvs t =
        throwError $ concat ["Infinite type ", show v, " ~ ", show t]
    | otherwise = return $ Map.singleton v t


solve :: Subst -> [Constraint] -> Solve Subst
solve = \cases
    s [] -> return s
    s (Constraint t1 t2 : cs) -> do
        s1 <- unify t1 t2
        solve (s1 `compose` s) (apply s1 cs)


runSolve :: [Constraint] -> Either String Subst
runSolve = runExcept . solve Map.empty


runInfer :: Expr -> Either String (Type, Count, Constraints)
runInfer e = runExcept $ runRWST (infer e) Map.empty 0
