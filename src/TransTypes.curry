-- ---------------------------------------------------------------------------
--- Generation of data and instance declarations for the Curry->Haskell
--- compiler.
--- The input is a FlatCurry program and the output is an AbstractHaskell
--- program with instance declarations that can be easily pretty printed
---
--- @author Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version January 2011
-- ---------------------------------------------------------------------------
module TransTypes (transTypes) where

import qualified FlatCurry.Types as FC
import FlatCurry.Goodies
import AbstractHaskell.Types
import AbstractHaskell.Goodies
import Data.List
import Data.Map
import Names
  ( mkChoiceName, mkChoicesName, mkFailName, mkGuardName, mkFoConsName
  , mkHoConsName, renameModule, unGenRename, unRenameModule, renameQName
  , unRenameQName, curryPrelude, funcPrefix, genRename)
import Analysis

-- ---------------------------------------------------------------------------
-- Generate code for user-defined types
-- ---------------------------------------------------------------------------

--- Translate a list of FlatCurry type declarations into the
--- corresponding type and instance declarations for Haskell.
transTypes :: ConsHOResult -> [FC.TypeDecl] -> [TypeDecl]
transTypes hoResult = concatMap (genTypeDeclarations hoResult)


genTypeDeclarations :: ConsHOResult -> FC.TypeDecl -> [TypeDecl]
genTypeDeclarations hoResult tdecl = case tdecl of
  (FC.TypeSyn qf vis tnums texp) ->
    [TypeSyn qf (fcy2absVis vis) (map fcy2absTVar tnums) (fcy2absTExp [] texp)]
  t@(FC.Type qf vis tnums cs)
      -- type names are always exported to avoid ghc type errors.
      -- TODO: Describe why/which errors may occur.
    | Prelude.null cs -> Type qf Public targs [] : []
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      -- !!! HACK: Since the C_Bool type and some of its type class     !!!
      -- !!! instances need to be defined in the runtime system,        !!!
      -- !!! we do NOT generate them here.                              !!!
      -- !!! Only a type declaration without constructors is generated. !!!
      -- !!! It is required to generate an export declaration for type  !!!
      -- !!! C_Bool in the Curry_Prelude.                               !!!
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    | isBoolType qf   -> Type qf Public []    [] : []
    | otherwise       -> Type qf Public targs ds : instanceDecls
    where
      isBoolType = (==) ("Curry_Prelude", "C_Bool")
      isDictType = (==) "OP_uscore_Dict_hash_" . take 20 . snd
      ds = concatMap (fcy2absCDecl targs hoResult) cs ++
            [ Cons (mkChoiceName  qf) 3 vis' [coverType, idType, ctype, ctype]
            , Cons (mkChoicesName qf) 2 vis' [coverType, idType, clisttype]
            , Cons (mkFailName    qf) 2 vis' [coverType, failInfoType]
            , Cons (mkGuardName   qf) 2 vis' [coverType, constraintType, ctype]
            ]
      -- For dictionaries that represent type classes we only generate empty
      -- instances.
      instanceDecls = map ($t) [ showInstance       (isDictType qf) hoResult
                               , readInstance       (isDictType qf)
                               , nondetInstance     (isDictType qf)
                               , generableInstance  (isDictType qf) hoResult
                               , normalformInstance (isDictType qf) hoResult
                               , unifiableInstance  (isDictType qf) hoResult
                               , curryInstance      (isDictType qf)
                               ]
      vis'      = fcy2absVis vis
      targs     = map fcy2absTVar tnums
      ctype     = TCons qf (map TVar targs)
      clisttype = listType ctype
  _ -> error "TransTypes.genTypeDeclarations"

fcy2absVis :: FC.Visibility -> Visibility
fcy2absVis FC.Public  = Public
fcy2absVis FC.Private = Private

fcy2absTVar :: FC.TVarIndex -> TVarIName
fcy2absTVar i = (i, 't' : show i)

fcy2absTVarKind :: (FC.TVarIndex, FC.Kind) -> (TVarIName, Kind)
fcy2absTVarKind (i, k) = (fcy2absTVar i, fcy2absKind k)

fcy2absKind :: FC.Kind -> Kind
fcy2absKind FC.KStar          = KindStar
fcy2absKind (FC.KArrow k1 k2) = KindArrow (fcy2absKind k1) (fcy2absKind k2)

fcy2absCDecl :: [TVarIName] -> ConsHOResult -> FC.ConsDecl -> [ConsDecl]
fcy2absCDecl targs hoResult (FC.Cons qf ar vis texps)
  | isHigherOrder = [foCons, hoCons]
  | otherwise     = [foCons]
  where
    isHigherOrder = Data.Map.lookup qf hoResult == Just ConsHO
    foCons = Cons (mkFoConsName qf) ar vis' (map (fcy2absTExp   targs) texps)
    hoCons = Cons (mkHoConsName qf) ar vis' (map (fcy2absHOTExp targs) texps)
    vis' = fcy2absVis vis

fcy2absTExp :: [TVarIName] -> FC.TypeExpr -> TypeExpr
fcy2absTExp vs = genContext vs' . fcy2absTExp'
  where
    vs' = map (\v -> (v, KindStar)) vs -- the kind does not matter, trust me.
    fcy2absTExp' (FC.TVar i)          =
      TVar (fcy2absTVar i)
    fcy2absTExp' (FC.TCons qf texps)  =
      TCons qf (map fcy2absTExp' texps)
    fcy2absTExp' (FC.FuncType t1 t2)  =
      FuncType (fcy2absTExp' t1) $
        FuncType coverType $
          FuncType consStoreType (fcy2absTExp' t2)
    fcy2absTExp' (FC.ForallType is t) =
      ForallType (map fcy2absTVarKind is) [] $ fcy2absTExp' t

fcy2absHOTExp :: [TVarIName] -> FC.TypeExpr -> TypeExpr
fcy2absHOTExp vs = genContext vs' . fcy2absHOTExp'
  where
    vs' = map (\v -> (v, KindStar)) vs -- the kind does not matter, trust me.
    fcy2absHOTExp' (FC.TVar         i)  =
      TVar (fcy2absTVar i)
    fcy2absHOTExp' (FC.TCons   qf tys)  =
      TCons qf (map fcy2absHOTExp' tys)
    fcy2absHOTExp' (FC.FuncType t1 t2)  =
      funcType (fcy2absHOTExp' t1) (fcy2absHOTExp' t2)
        where funcType ta tb = TCons (basics "Func") [ta, tb]
    fcy2absHOTExp' (FC.ForallType is t) =
      ForallType (map fcy2absTVarKind is) [] $ fcy2absHOTExp' t

-- Note: all types are fully quantified and in weak prenex form.
-- To generate Curry contexts for each type with kind *,
-- we pass down the kind of bound type variables and pass up types
-- with kind * that contain at least one type variable.
-- Those TypeExprs are parts of the whole TypeExpr.
-- When traversing back up, we add Curry contexts to ForallTypes
-- as far up the TypeExpr as possible
-- while still having all required type variables in scope.
-- In contrast to genContext from TransFunctions,
-- we get a list of already bound variables here.
-- Those variables are already bound by the left side of a datatype definition
genContext :: [(TVarIName, Kind)] -> TypeExpr -> TypeExpr
genContext bvs = snd . toTypeSig' bvs
  where
    toTypeSig' vs (FuncType ty1 ty2) =
      let (cty1, ty1') = toTypeSig' vs ty1
          (cty2, ty2') = toTypeSig' vs ty2
      in (cty1++cty2, FuncType ty1' ty2')
    toTypeSig' vs (ForallType tvs cs ty) =
      let vs' = vs ++ tvs
          (cty, ty') = toTypeSig' vs' ty
          (here, before) = partition (isSaturatedWith tvs) $ nub cty
      in (before, ForallType tvs (nub $ cs ++ map mkContext here) ty')
    toTypeSig' vs t@(TVar tv) = case Prelude.lookup tv vs of
      Just KindStar -> ([t], t)
      _             -> ([] , t)
    toTypeSig' vs t@(TCons qname tys) =
      let (ctys, tys') = unzip $ map (toTypeSig' vs) tys
      in if qname == (curryPrelude, "C_Apply") && isTypeVar (head tys)
           then (t : concat ctys, TCons qname tys')
           else (    concat ctys, TCons qname tys')

    -- if any TypeVar in ty is quanitifed by vs, we have to add a Context for ty
    isSaturatedWith vs ty = any (elemFst vs) $ nub $ typeVars ty []

    typeVars (TVar tv) vs = tv:vs
    typeVars (TCons _ tys) vs = foldr typeVars vs tys
    typeVars (ForallType _ _ ty) vs = typeVars ty vs
    typeVars (FuncType ty1 ty2) vs = typeVars ty1 (typeVars ty2 vs)

    elemFst []         _ = False
    elemFst ((x,_):xs) e = x == e || elemFst xs e

    arityKind KindStar         = 0
    arityKind (KindArrow _ k2) = arityKind k2 + 1

    isTypeVar ty = case ty of
      TVar _ -> True
      _      -> False

    mkContext ty = Context (curryPrelude, "Curry") [ty]

-- ---------------------------------------------------------------------------
-- Generate instance of Show class:
-- ---------------------------------------------------------------------------
showInstance :: Bool -> ConsHOResult -> FC.TypeDecl -> TypeDecl
showInstance isDict hoResult tdecl = case tdecl of
  (FC.Type qf _ tnums cdecls)
    | not isDict -> mkInstance (basics "Show") [] ctype targs $
       if isListType qf then [showRule4List] else
         [ ( pre "showsPrec"
         , simpleRule [PVar d, mkChoicePattern qf "i" ]
              (applyF (basics "showsChoice")  [Var d, Var cd, Var i, Var x, Var y])
           )
         , ( pre "showsPrec"
            , simpleRule [PVar d, mkChoicesPattern qf]
              (applyF (basics "showsChoices") [Var d, Var cd, Var i, Var xs])
           )
         , ( pre "showsPrec"
            , simpleRule [PVar d, mkGuardPattern qf]
              (applyF (basics "showsGuard")   [Var d, Var cd, Var c, Var e])
           )
         , ( pre "showsPrec"
            , simpleRule [PVar us, mkFailPattern qf]
              (applyF (pre "showChar")        [charc '!'])
           )
         ] ++ concatMap (showConsRule hoResult) cdecls
    | otherwise -> mkEmptyInstance (basics "Show") ctype
   where [cd, d,i,x,y,xs,c,e,us] = newVars ["cd", "d","i","x","y","xs","c","e","_"]
         targs = map fcy2absTVar tnums
         ctype = TCons qf (map TVar targs)
  _ -> error "TransTypes.showInstance"

  -- Generate specific show for lists (only for finite lists!)
showRule4List :: (QName, Rule)
showRule4List =
    (pre "showsPrec",
    Rule [] (SimpleRhs (constF (pre "showsPrec4CurryList"))) [])

-- Generate Show instance rule for a data constructor:
showConsRule :: ConsHOResult -> FC.ConsDecl -> [(QName, Rule)]
showConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = Data.Map.lookup qn hoResult == Just ConsHO

    rule name = ( pre "showsPrec"
                , case take 8 (snd name) of
                    "OP_Cons"  -> showListCons name
                    "OP_Tuple" -> showTupleCons name
                    _          -> showBody name
                )

     -- specific definition to show a list constructor:
    showListCons name = simpleRule [PVar d, consPattern name "x" carity]
      $ applyF (pre "showParen")
          [ applyF (pre ">") [Var d, intc 5]
          , foldr1 (\f1 f2 -> InfixApply f1 (pre ".") f2)
              [ applyF (pre "showsPrec") [intc 6, Var x1]
              , applyF (pre "showChar")  [charc ':']
              , applyF (pre "showsPrec") [intc 5, Var x2]
              ]
          ]
          where [d,x1,x2] = newVars ["d","x1","x2"]

     -- specific definition to show a tuple constructor
    showTupleCons name = simpleRule [PVar (0,"_"), consPattern name "x" carity]
      $ InfixApply
          (applyF (pre "showString") [string2ac "("])
          (pre ".")
          (foldr (\f1 f2 -> InfixApply f1 (pre ".") f2)
                  (applyF (pre "showChar") [Lit (Charc ')')])
                  (intersperse (applyF (pre ":") [Lit (Charc ',')])
                      (map (\i->applyF (pre "shows") [mkVar "x" i])
                        [1..carity])))

    showBody name = simpleRule [PVar (0,"_"), consPattern name "x" carity] $
      if carity == 0
      then applyF (pre "showString") [string2ac (unGenRename (snd qn))]
      else InfixApply
            (applyF (pre "showString") [string2ac ('(':unGenRename (snd qn))])
            (pre ".")
            (foldr (\x xs -> InfixApply
                            (applyF (pre "showChar") [Lit (Charc ' ')])
                            (pre ".")
                            (InfixApply x (pre ".") xs)
                  )
                  (applyF (pre "showChar") [Lit (Charc ')')])
                  (map (\i -> applyF (pre "shows") [mkVar "x" i])
                      [1..carity]))

-- ---------------------------------------------------------------------------
-- Generate instance of Read class:
--
-- TODO: No instance for higher-order constructors
-- ---------------------------------------------------------------------------
readInstance :: Bool -> FC.TypeDecl -> TypeDecl
readInstance isDict tdecl = case tdecl of
  (FC.Type qf _ tnums cdecls)
    | not isDict -> mkInstance (pre "Read") [] ctype targs [rule]
    | otherwise -> mkEmptyInstance (basics "Read") ctype
   where
        targs = map fcy2absTVar tnums
        ctype = TCons qf (map TVar targs)
        rule | isListType  qf = readListRule qf
             | isTupleType qf = readTupleRule (head cdecls)
             | otherwise      = readRule cdecls
  _ -> error "TransTypes.readInstance"

-- Generate special Read instance rule for lists
-- according to the following scheme:
-- @
-- instance Read t0 => Read (OP_List t0) where
--  readsPrec d s = map readList (readsPrec d s)
--   where
--     readList :: ([a], String) -> (OP_List a, String)
--     readList (xs,s) = (foldr OP_Cons OP_List xs, s)
-- @
readListRule :: QName -> (QName, Rule)
readListRule (mn, _) =
  ( pre "readsPrec"
  , Rule [PVar d, PVar s]
      (SimpleRhs $ applyF (pre "map") [ constF (mn,"readList")
                                      , applyF (pre "readsPrec") [Var d, Var s]
                                      ])
      [LocalFunc (tfunc (mn, "readList") 1 Private
        (tupleType [listType (TVar (0, "a")), stringType] ~>
         tupleType [TCons (mn, "OP_List") [TVar (0, "a")], stringType])
        [simpleRule [tuplePat [PVar xs, PVar s2]]
           (tupleExpr [applyF (pre "foldr") [ constF (mn,"OP_Cons")
                                            , constF (mn,"OP_List")
                                            , Var xs
                                            ] , Var s2])])
      ]
  ) where [d,s,xs,s2] = newVars ["d","s","xs","s2"]

-- Generate special Read instance rule for tuple constructors
-- according to the following scheme:
-- @
-- instance (Read t1, ..., tn) => Read (OP_TupleN t1 ... tn) where
--   readsPrec d s = map readTuple (readsPrec d s)
--    where
--     readTuple :: ((t1, ..., tn), String) -> (OP_TupleN, String)
--     readTuple ((x1, ..., xn), s) = (OP_TupleN x1 ... xn, s)
-- @
readTupleRule :: FC.ConsDecl -> (QName, Rule)
readTupleRule (FC.Cons qn@(mn,_) carity _ _) =
  ( pre "readsPrec"
  , Rule [PVar d, PVar s]
      (SimpleRhs $ applyF (pre "map") [ constF (mn,"readTuple")
                                      , applyF (pre "readsPrec") [Var d, Var s]
                                      ])
      [LocalFunc (tfunc (mn,"readTuple") 1 Private
        (tupleType [tupleType tvars, stringType] ~>
         tupleType [TCons (mn, "OP_Tuple" ++ show carity) tvars, stringType])
        [simpleRule [tuplePat [ tuplePat $ map (mkPVar "x") [1 .. carity]
                              , PVar s2
                              ]]
          (tupleExpr [ applyF qn (map (mkVar "x") [1 .. carity])
                     , Var s2])])]
  ) where [d,s,s2] = newVars ["d","s","s2"]
          tvars    = map mkTVar [0 .. carity - 1]


-- Generate Read instance rule for data constructors:

--instance (Read t0,Read t1) => Read (C_Either t0 t1) where
  --  readsPrec d r =
  --    (++) (readParen(d>10)
  --                   (\s -> [(C_Left x1,r1)
  --                        | (_,r0) <- readQualified "Prelude" "Left" s,
  --                          (x1,r1) <- readsPrec 11 r0])
  --                   r)
  --         (readParen(d>10)
  --                   (\s -> [(C_Right(x1),r1)
  --                        | (_,r0) <- readQualified "Prelude" "Right" s,
  --                          (x1,r1) <- readsPrec 11 r0])
  --                   r)
readRule :: [FC.ConsDecl] -> (QName, Rule)
readRule cdecls =
  ( pre "readsPrec"
  ,  simpleRule [PVar maybeD, PVar (1,"s")]
       (foldr1 (\e1 e2 -> InfixApply e1 (pre "++") e2) $ map readParen cdecls)
  )
  where
    maybeD = if isDNeeded then (0,"d") else (0,"_")
    isDNeeded = or [ arity > 0 | (FC.Cons _ arity _ _) <- cdecls ]

readParen :: FC.ConsDecl -> Expr
readParen (FC.Cons qn@(mn,_) carity _ _) = applyF (pre "readParen")
  [ if carity == 0 then constF (pre "False") -- no parentheses required
                   else applyF (pre ">") [Var (0, "d"), intc 10]
  , Lambda [PVar (2,"r")]
      (ListComp
        (tupleExpr [ applyF qn (map (mkVar "x") [1 .. carity])
                   , mkVar "r" carity
                   ]
        )
        (SPat (tuplePat [PVar (0, "_"), PVar (1, "r0")])
              (applyF (basics "readQualified")
                      [ string2ac (unRenameModule mn)
                      , string2ac (unGenRename (snd qn))
                      , Var (2,"r")
                      ] )
              : map genReadsPrec [1 .. carity]
        )
      )
  , Var (1, "s")
  ]
    where
    genReadsPrec i =
      SPat (tuplePat [mkPVar "x" i, mkPVar "r" i])
           (applyF (pre "readsPrec") [Lit (Intc 11), mkVar "r" (i - 1) ])


-- ---------------------------------------------------------------------------
-- Generate instance of NonDet class:
-- ---------------------------------------------------------------------------
nondetInstance :: Bool -> FC.TypeDecl -> TypeDecl
nondetInstance isDict tdecl = case tdecl of
  (FC.Type qf _ tnums _)
    | not isDict -> mkInstance (basics "NonDet") [] ctype []
     $ specialConsRules qf ++ tryRules qf ++ matchRules qf
    | otherwise -> mkEmptyInstance (basics "NonDet") ctype
   where
     targs = map fcy2absTVar tnums
     ctype = TCons qf (map TVar targs)
  _ -> error "TransTypes.nondetInstance"

specialConsRules :: QName -> [(QName, Rule)]
specialConsRules qf = map nameRule
  [ ("choiceCons" , mkChoiceName  qf)
  , ("choicesCons", mkChoicesName qf)
  , ("failCons"   , mkFailName    qf)
  , ("guardCons"  , mkGuardName   qf)
  ]
  where nameRule (name, fun) = (basics name, simpleRule [] (constF fun))

tryRules :: QName -> [(QName, Rule)]
tryRules qf = map nameRule
  [ simpleRule [mkChoicePattern  qf "i"] $ applyF (basics "tryChoice")  [cd, i, x, y]
  , simpleRule [mkChoicesPattern qf    ] $ applyF (basics "tryChoices") [cd, i, xs]
  , simpleRule [mkFailPattern    qf    ] $ applyF (basics "Fail")       [cd, info]
  , simpleRule [mkGuardPattern   qf    ] $ applyF (basics "Guard")      [cd, c, e]
  , simpleRule [PVar (2,"x")           ] $ applyF (basics "Val")        [x]
  ]
  where [i,x,y,xs,c,e,cd,info] = map Var $ newVars ["i","x","y","xs","c","e", "cd","info"]
        nameRule rule  = (basics "try", rule)

{-
match f _ _ _ _ _ (Choice i x y) = f i x y
match _ f _ _ _ _ (Choices i@(NarrowedID _ _) xs) = f i xs
match _ f _ _ _ _ (Choices i@(CovNarrowedID _ _ _) xs) = f i xs
match _ _ f _ _ _ (Choices i@(FreeID _ _) xs) = f i xs
match _ _ f _ _ _ (Choices i@(CovFreeID _ _ _) xs) = f i xs
match _ _ _ _ _ _ (Choices (ChoiceID _ _) _) = error " ..."
match _ _ _ f _ _ Fail = f
match _ _ _ _ f _ (Guard c e) = f c e
match _ _ _ _ _ f x = f x
-}
matchRules :: QName -> [(QName, Rule)]
matchRules qf = map nameRule
  [ simpleRule (matchAt 0 ++ [mkChoicePattern  qf "i"])
    $ applyV f [cd, i, x, y]
  , simpleRule (matchAt 1 ++ [mkNarrowedChoicesPattern qf "i"])
    $ applyV f [cd, i, xs]
  , simpleRule (matchAt 2 ++ [mkFreeChoicesPattern qf "i"])
    $ applyV f [cd, i, xs]
  , simpleRule (PVar (0,"_") : underscores ++ [mkVarChoicesPattern qf])
    $ applyF (pre "error")
      [ InfixApply
        (string2ac (showQName (unRenameQName qf) ++ ".match: Choices with ChoiceID "))
        (pre "++")
        (applyF (pre "show") [i])
      ]
  , simpleRule (matchAt 3 ++ [mkFailPattern qf])
    $ applyV f [cd,info]
  , simpleRule (matchAt 4 ++ [mkGuardPattern qf])
    $ applyV f [cd, c, e]
  , simpleRule (matchAt 5 ++ [PVar (7,"x")])
    $ applyV f [x]
  ]
  where underscores = map PVar $ newVars ["_","_","_","_","_"]
        [i,x,y,xs,e,c,cd,info] = map Var $ newVars ["i","x","y","xs","e","c","cd","info"]
        f = (1, "f")
        nameRule rule  = (basics "match", rule)
        matchAt n = take n underscores ++ PVar f : drop n underscores

-- ---------------------------------------------------------------------------
-- Generate instance of Generable class
-- ---------------------------------------------------------------------------
-- TODO generators for constructor arguments can pe the same idsupplies
--      for different constructors; change bind accordingly

generableInstance :: Bool -> ConsHOResult -> FC.TypeDecl -> TypeDecl
generableInstance isDict hoResult tdecl = case tdecl of
  (FC.Type qf _ tnums cdecls)
    | not isDict -> mkInstance (basics "Generable") [] ctype targs
        [(basics "generate", simpleRule [PVar s,PVar c]  genBody)]
    | otherwise -> mkEmptyInstance (basics "Generable") ctype
   where
      targs = map fcy2absTVar tnums
      ctype = TCons qf (map TVar targs)
      [s,c] = newVars ["s","c"]
      idSupply = Var s

      genBody =
       applyF (mkChoicesName qf)
       [ Var c
       , applyF (basics "freeID") [arities, idSupply]
       , list2ac $ map genCons cdecls
       ]


      genCons (FC.Cons qn arity _ _)
        | Data.Map.lookup qn hoResult
          == Just ConsHO = applyF (mkHoConsName qn) (consArgs2gen arity)
        | otherwise      = applyF qn (consArgs2gen arity)

      arities = list2ac $ map (intc . consArity) cdecls

      consArgs2gen n = map (applyF (basics "generate") . (:[Var c]))
                            $ mkSuppList n idSupply
  _ -> error "TransTypes.generableInstance"

-- ---------------------------------------------------------------------------
-- Generate instance of NormalForm class:
-- ---------------------------------------------------------------------------
normalformInstance :: Bool -> ConsHOResult -> FC.TypeDecl -> TypeDecl
normalformInstance isDict hoResult tdecl = case tdecl of
  (FC.Type qf _ tnums cdecls)
    | not isDict -> mkInstance (basics "NormalForm") [] ctype targs $ concat
         -- $!!
       [ concatMap (normalformConsRule hoResult (basics "$!!")) cdecls
       , normalFormExtConsRules qf (basics "$!!")
           (basics "nfChoice") (basics "nfChoices")
         -- $##
       , concatMap (normalformConsRule hoResult (basics "$##")) cdecls
       , normalFormExtConsRules qf (basics "$##")
           (basics "gnfChoice") (basics "gnfChoices")
       -- showCons
       , concatMap (showConsConsRule hoResult) cdecls
       , [showConsCatchRule qf]
       -- searchNF
       , concatMap (searchNFConsRule hoResult) cdecls
       , [searchNFCatchRule qf]
       ]
    | otherwise -> mkEmptyInstance (basics "NormalForm") ctype
   where targs = map fcy2absTVar tnums
         ctype = TCons qf (map TVar targs)
         --[cd, cont,i,x,y,xs] = newVars ["cd", "cont","i","x","y","xs"]
  _ -> error "TransTypes.normalformInstance"

-- Generate NormalForm instance rule for a data constructor
normalformConsRule :: ConsHOResult -> QName -> FC.ConsDecl -> [(QName, Rule)]
normalformConsRule hoResult funcName (FC.Cons qn _ _ texps)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = Data.Map.lookup qn hoResult == Just ConsHO
    carity = length texps
    rule name = (funcName, simpleRule
      ([PVar (1,"cont"), PComb name (map (\i -> PVar (i,'x':show i)) [1..carity])] ++ cdCsPVar)
          (nfBody name))

    nfBody name =
      foldr (\i exp -> applyF funcName
                        ([Lambda ([PVar (i,'y':show i)] ++ cdCsPVar) exp,Var (i,'x':show i)] ++ cdCsVar))
            (applyV (1,"cont")
                    ([applyF name (map (\i -> Var (i,'y':show i)) [1..carity])] ++ cdCsVar))
            [1..carity]
    cdCsPVar = [PVar (4,"d"), PVar (3,"cs")]
    cdCsVar  = [Var (4,"d") ,  Var (3,"cs")]

normalFormExtConsRules :: QName -> QName -> QName -> QName -> [(QName, Rule)]
normalFormExtConsRules qf funcName choiceFunc choicesFunc =
  [(funcName, simpleRule [PVar cont, mkChoicePattern qf "i", PVar d, PVar cs]
        (applyF choiceFunc
                [Var cont , Var cd, Var i, Var x, Var y, Var cd, Var cs]))
  , (funcName, simpleRule [PVar cont, mkChoicesPattern qf , PVar d, PVar cs]
        (applyF choicesFunc
                [Var cont, Var cd, Var i, Var xs, Var d, Var cs]))
  , (funcName, simpleRule [PVar cont, mkGuardPattern qf, PVar d, PVar cs]
        (applyF (basics "guardCons")
                [Var cd, Var c,
                 applyF funcName [Var cont, Var e
                                 , Var d, applyF (basics "addCs") [Var c, Var cs]]]))
  , (funcName, simpleRule [PVar us, mkFailPattern qf, PVar us, PVar us]
                (applyF (basics "failCons") [Var cd, Var info]))
  ]

 where [d, info, c, cs, cd, cont,i,x,y,e,xs ,us]
          = newVars ["d", "info", "c", "cs", "cd", "cont","i","x","y","e","xs", "_"]

-- Generate searchNF instance rule for a data constructor
showConsConsRule :: ConsHOResult -> FC.ConsDecl -> [(QName, Rule)]
showConsConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons  = Data.Map.lookup qn hoResult == Just ConsHO
    rule name = ( basics "showCons"
                , simpleRule [consPattern name "_" carity]
                  (string2ac $ intercalate " " $
                    showQName (unRenameQName name) : replicate carity "_")
                )

showConsCatchRule :: QName -> (QName, Rule)
showConsCatchRule qf
  = ( basics "showCons"
    , simpleRule [PVar x]
      ( applyF (pre "error")
        [ InfixApply
          (string2ac (showQName (unRenameQName qf) ++ ".showCons: no constructor: "))
          (pre "++")
          (applyF (pre "show") [Var x])
        ])
    )
  where [x] = newVars ["x"]

-- Generate searchNF instance rule for a data constructor
searchNFConsRule :: ConsHOResult -> FC.ConsDecl -> [(QName, Rule)]
searchNFConsRule hoResult (FC.Cons qn carity _ _)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons  = Data.Map.lookup qn hoResult == Just ConsHO
    rule name = ( basics "searchNF"
                , simpleRule [PVar mbSearch, PVar cont, consPattern name "x" carity]
                  (nfBody name)
                )

    nfBody name = foldr (\i exp -> applyV search
                      [Lambda [mkPVar "y" i] exp, mkVar "x" i])
          (applyV cont
                  [applyF name (map (mkVar "y") [1 .. carity])])
          [1..carity]
    [search,cont,us] = newVars ["search","cont","_"]
    mbSearch = if carity == 0 then us else search

searchNFCatchRule :: QName -> (QName, Rule)
searchNFCatchRule qf
  = ( basics "searchNF"
    , simpleRule [PVar us1, PVar us2, PVar x]
      ( applyF (pre "error")
        [ InfixApply
          (string2ac (showQName (unRenameQName qf) ++ ".searchNF: no constructor: "))
          (pre "++")
          (applyF (pre "show") [Var x])
        ])
    )
  where [us1,us2,x] = newVars ["_","_","x"]

-- ---------------------------------------------------------------------------
-- Generate instance of Unifiable class:
-- ---------------------------------------------------------------------------
unifiableInstance :: Bool -> ConsHOResult -> FC.TypeDecl -> TypeDecl
unifiableInstance isDict hoResult tdecl = case tdecl of
  (FC.Type qf _ tnums cdecls)
    | not isDict -> mkInstance (basics "Unifiable") [] ctype targs $ concat
         -- unification
       [ concatMap (unifiableConsRule hoResult (basics "=.=") (basics "=:=")) cdecls
       , [newFail (basics "=.=")]
         -- lazy unification (functional patterns)
       , concatMap (unifiableConsRule hoResult (basics "=.<=") (basics "=:<=")) cdecls
       , [newFail (basics "=.<=")]
         -- bind
       , concatMap (bindConsRule hoResult (basics "bind")
                      (\cov ident arg -> applyF (basics "bind") [cov, ident, arg])
                      (applyF (pre "concat"))) (zip [0 ..] cdecls)
       , [ bindChoiceRule   qf (basics "bind")
         , bindFreeRule     qf (basics "bind")
         , bindNarrowedRule qf (basics "bind")
         , bindChoicesRule  qf (basics "bind")
         , bindFailRule     qf (basics "bind")
         , bindGuardRule    qf False
         ]
         -- lazy bind (function patterns)
       , concatMap (bindConsRule hoResult (basics "lazyBind")
                      (\cov ident arg -> InfixApply ident (basics ":=:") (applyF (basics "LazyBind") [applyF (basics "lazyBind") [cov, ident, arg]])) head) (zip [0 ..] cdecls)
       , [ bindChoiceRule   qf (basics "lazyBind")
         , bindFreeRule     qf (basics "lazyBind")
         , bindNarrowedRule qf (basics "lazyBind")
         , bindChoicesRule  qf (basics "lazyBind")
         , bindFailRule     qf (basics "lazyBind")
         , bindGuardRule    qf True
         ]
       ]
    | otherwise -> mkEmptyInstance (basics "Unifiable") ctype
   where targs = map fcy2absTVar tnums
         ctype = TCons qf (map TVar targs)
         newFail qn = (qn, simpleRule [PVar (1,"a"), PVar (2,"b"), PVar (3, "cd"), PVar (4, "_")]
                           (applyF (basics "Fail_C_Bool") [Var (3, "cd"), applyF (basics "unificationFail") [applyF (basics "showCons") [Var (1,"a")], applyF (basics "showCons") [Var (2,"b")]]])
                       )
  _ -> error "TransTypes.unifiableInstance"

-- Generate Unifiable instance rule for a data constructor
unifiableConsRule :: ConsHOResult -> QName -> QName -> FC.ConsDecl -> [(QName, Rule)]
unifiableConsRule hoResult consFunc genFunc (FC.Cons qn _ _ texps)
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]

  where
    isHoCons = Data.Map.lookup qn hoResult == Just ConsHO
    rule name = ( consFunc, simpleRule [consPattern name "x" carity
                                       , consPattern name "y" carity
                                       , PVar nestingDepth
                                       , PVar cs]
              (unifBody genFunc) )
    unifBody funcName
      | carity == 0 = constF (basics "C_True")
      | otherwise   = foldr1 (\x xs -> applyF (basics "&")
                             [x, xs, Var nestingDepth, Var cs])
                        (map (\i -> applyF funcName
                               [Var (i,'x':show i), Var (i,'y':show i)
                               , Var nestingDepth, Var cs])
                        [1 .. carity])
    carity = length texps
    cs = (carity + 2, "cs")
    nestingDepth = (carity + 1, "d")

-- Generate bindRules for a data constructor:
--  bindConsRules :: [FC.ConsDecl] -> (Expr -> Expr) -> (Expr -> Expr) -> [Rule]
bindConsRule :: ConsHOResult -> QName -> (Expr -> Expr -> Expr -> Expr)
             -> ([Expr] -> Expr) -> (Int, FC.ConsDecl) -> [(QName, Rule)]
bindConsRule hoResult funcName bindArgs combine (num, (FC.Cons qn _ _ texps))
  | isHoCons  = map rule [qn, mkHoConsName qn]
  | otherwise = [rule qn]
  where
    isHoCons = Data.Map.lookup qn hoResult == Just ConsHO
    rule name = (funcName,
      simpleRule [PVar (1,"cd"), PVar (2, "i"), PComb name $ map (\i -> PVar (i, 'x':show i)) [3 .. (length texps) + 2] ]
        ( InfixApply
            (InfixApply (Var (2, "i")) (basics ":=:")
              (applyF (basics "ChooseN") [intc num, intc $ length texps]))
            (pre ":")
            (combine [list2ac (zipWith3 bindArgs
                                        (repeat (Var (1, "cd")))
                                        (mkIdList (length texps) (Var (2, "i")))
                    (map (\i -> Var (i, 'x':show i)) [3 ..(length texps) + 2]))]
            )))

-- bind i (Choice_TYPENAME j l r) = [ConstraintChoice j (bind i l) (bind i r)]
-- lazyBind i (Choice_TYPENAME j l r) = [ConstraintChoice j (lazyBind i l) (lazyBind i r)]
bindChoiceRule :: QName -> QName -> (QName, Rule)
bindChoiceRule qf funcName = (funcName,
  simpleRule [PVar d, PVar i, mkChoicePattern qf "j"]
    ( list2ac [ applyF (basics "ConstraintChoice")
                [Var cd, Var j
                , applyF funcName [Var d, Var i, Var x]
                , applyF funcName [Var d, Var i, Var y]
                ]
              ]
     )) where [d,i,j,x,y,cd] = newVars ["d","i","j","x","y","cd"]

-- bind i (Choices_TYPENAME j@(FreeID _ _) xs) = [i :=: BindTo j]
-- lazyBind i (Choices_TYPENAME j@(FreeID _ _) xs) = [i :=: BindTo j]
bindFreeRule ::QName -> QName -> (QName, Rule)
bindFreeRule qf funcName@(_,bname) = (funcName,
  simpleRule
    [ PVar d, PVar i, mkFreeChoicesPattern qf "j"]
    (applyF (pre (bname ++ "OrNarrow")) [Var d, Var i, Var cd, Var j, Var xs]))
      where [d,i,j,cd, xs] = newVars  ["d","i","j", "cd", "xs"]

-- bind i (Choices_TYPENAME j@(NarrowedID _ _) xs) = [ConstraintChoices j (map (bind i) xs)]
-- lazyBind i (Choices_TYPENAME j@(NarrowedID _ _) xs) = [ConstraintChoices j (map (lazyBind i) xs)]
bindNarrowedRule :: QName -> QName -> (QName, Rule)
bindNarrowedRule qf funcName = (funcName,
  simpleRule
    [ PVar d, PVar i, mkNarrowedChoicesPattern qf "j"]
    ( list2ac [ applyF (basics "ConstraintChoices")
                [ Var cd, Var j
                , applyF (pre "map") [applyF funcName [Var d, Var i], Var xs]
                ]
              ]
    ))
      where [d,i,j,xs, cd] = newVars ["d", "i","j","xs", "cd"]

-- bind _ c@(Choices_TYPENAME (ChoiceID _) _) = error ("Choices with ChoiceID: " ++ show c)
-- lazyBind _ c@(Choices_TYPENAME (ChoiceID _) _) = error ("Choices with ChoiceID: " ++ show c)
bindChoicesRule :: QName -> QName -> (QName, Rule)
bindChoicesRule qf funcName = (funcName,
  simpleRule
    [ PVar us1, PVar us2, mkVarChoicesPattern qf]
    ( applyF (pre "error")
      [ InfixApply
        (string2ac (showQName (unRenameQName qf) ++ '.' : snd funcName
                                  ++ ": Choices with ChoiceID: "))
        (pre "++")
        (applyF (pre "show") [Var i])
      ]
    ))
  where [us1,us2,i] = newVars ["_","_","i"]

-- bind _ Fail_TYPENAME = [Unsolvable]
-- lazyBind _ Fail_TYPENAME = [Unsolvable]
bindFailRule :: QName -> QName -> (QName, Rule)
bindFailRule qf funcName = (funcName,
  simpleRule [PVar (1, "_"),PVar (2,"_"), mkFailPattern qf]
              (list2ac [applyF (basics "Unsolvable") [Var info]]))
 where [info] = newVars ["info"]

-- bind i (Guard_TYPENAME cs e) = cs ++ bind i e
-- lazyBind i (Guard_TYPENAME cs e) = cs ++ [i :=: LazyBind (lazyBind i e)]
bindGuardRule :: QName -> Bool -> (QName, Rule)
bindGuardRule qf lazy = (funcName,
  simpleRule [PVar d, PVar i, mkGuardPattern qf]
    (InfixApply (applyF (basics "getConstrList") [Var c]) (pre "++") bindings))
  where
    [d,i,c,e] = newVars ["d", "i","c","e"]
    funcName = basics $ if lazy then "lazyBind" else "bind"
    bindings = if lazy
      then list2ac [InfixApply (Var i) (basics ":=:")
                     (applyF (basics "LazyBind")
                        [applyF funcName [Var d, Var i, Var e]])
                   ]
      else applyF funcName [Var d, Var i, Var e]

-- ---------------------------------------------------------------------------
-- Generate instance of Curry class
-- ---------------------------------------------------------------------------

curryInstance :: Bool -> FC.TypeDecl -> TypeDecl
curryInstance isDict tdecl = case tdecl of
  (FC.Type qf _ tnums _)
    | not isDict -> mkInstance (basics "Curry") [] ctype targs []
    | otherwise -> mkEmptyInstance (basics "Curry") ctype
   where
      targs = map fcy2absTVar tnums
      ctype = TCons qf (map TVar targs)
  _ -> error "TransTypes.curryInstance"

-- ---------------------------------------------------------------------------
-- Auxiliary functions
-- ---------------------------------------------------------------------------

mkChoicePattern :: QName -> String -> Pattern
mkChoicePattern qn idStr = PComb (mkChoiceName  qn) [PVar cd, PVar idVar , PVar x, PVar y]
  where [cd, idVar ,x,y] = newVars ["cd", idStr ,"x","y"]

mkChoicesPattern :: QName -> Pattern
mkChoicesPattern qn = PComb (mkChoicesName qn) [PVar cd, PVar i, PVar xs]
  where [cd, i,xs] = newVars ["cd", "i","xs"]

mkNarrowedChoicesPattern :: QName -> String -> Pattern
mkNarrowedChoicesPattern qn asName = PComb (mkChoicesName qn)
 [PVar cd, PAs i (PComb (basics "NarrowedID") [PVar u1, PVar u2]), PVar xs]
 where [i,cd, u1,u2,xs] = newVars [asName,"cd","_","_","xs"]

mkFreeChoicesPattern :: QName -> String -> Pattern
mkFreeChoicesPattern qn asName = PComb (mkChoicesName qn)
 [PVar cd, PAs i (PComb (basics "FreeID") [PVar u1, PVar u2]), PVar xs]
 where [i,cd,u1,u2,xs] = newVars [asName,"cd", "_","_","xs"]

mkVarChoicesPattern :: QName -> Pattern
mkVarChoicesPattern qn = PComb (mkChoicesName qn) [PVar cd, PVar i, PVar xs]
 where [cd,i,xs] = newVars ["cd","i","_"]

mkFailPattern :: QName -> Pattern
mkFailPattern qn = PComb (mkFailName qn) [PVar cd, PVar info]
  where [cd,info] = newVars ["cd","info"]

mkGuardPattern :: QName -> Pattern
mkGuardPattern qn = PComb (mkGuardName qn) [PVar cd, PVar c, PVar e]
  where [cd, c,e] = newVars ["cd", "c","e"]

mkPVar :: String -> Int -> Pattern
mkPVar n i = PVar $ mkVarName n i

mkVar :: String -> Int -> Expr
mkVar n i = Var $ mkVarName n i

mkTVar :: Int -> TypeExpr
mkTVar i = TVar (i, 't' : show i)

mkVarName :: String -> Int -> (Int, String)
mkVarName n i
  | n == "_"  = (i, n)
  | otherwise = (i, n ++ show i)

newVars :: [String] -> [(Int, String)]
newVars = zip [1..]

mkInstance :: QName -> [QName] -> TypeExpr -> [TVarIName] -> [(QName, Rule)]
           -> TypeDecl
mkInstance qn addContexts ctype targs = Instance qn ctype $
  concatMap (\name -> map (\tv -> Context name [TVar tv]) targs) (qn:addContexts)

mkEmptyInstance :: QName -> TypeExpr -> TypeDecl
mkEmptyInstance qn ctype = Instance qn ctype [] []

consPattern :: QName -> String -> Int -> Pattern
consPattern qn varName carity
  = PComb qn $ map (PVar . mkVarName varName) [1 .. carity]

catchAllCase :: QName -> Expr -> [(QName, Rule)]
catchAllCase qn retVal
  = [(qn, simpleRule [PVar (1,"_"), PVar (2,"_"), PVar (3, "d"), PVar (4,"_")] retVal)]

simpleRule :: [Pattern] -> Expr -> Rule
simpleRule ps e = Rule ps (SimpleRhs e) []

intc :: Int -> Expr
intc i = Lit $ Intc i

charc :: Char -> Expr
charc c = Lit $ Charc c

mkIdList :: Int -> Expr -> [Expr]
mkIdList num initid
  | num == 0    = []
  | num == 1    = [left initid]
  | otherwise   = mkIdList' num initid
  where
    mkIdList' n i
      | n == 1    = [i]
      | otherwise = mkIdList' (n - half) (left i) ++ mkIdList' half (right i)
      where
        half = n `div` 2

mkSuppList :: Int -> Expr -> [Expr]
mkSuppList num supp
  | num == 0    = []
  | num == 1    = [leftsupp supp]
  | otherwise   = mkSuppList' num supp
  where
    mkSuppList' n s
      | n == 1    = [s]
      | otherwise = mkSuppList' (n - half) (leftsupp s) ++ mkSuppList' half (rightsupp s)
      where
        half = n `div` 2

isListType :: QName -> Bool
isListType qn = qn == renameQName ("Prelude", "[]")

isTupleType :: QName -> Bool
isTupleType (m,t) = m == renameModule "Prelude" && take 8 t == "OP_Tuple"

showQName :: QName -> String
showQName (m,t) = m ++ '.' : t

-- ---------------------------------------------------------------------------
-- Frequently used symbols
-- ---------------------------------------------------------------------------

left :: Expr -> Expr
left  i = applyF (basics "leftID" ) [i]

right :: Expr -> Expr
right i = applyF (basics "rightID") [i]

leftsupp :: Expr -> Expr
leftsupp  s = applyF (basics "leftSupply" ) [s]

rightsupp :: Expr -> Expr
rightsupp s = applyF (basics "rightSupply") [s]

idType :: TypeExpr
idType = baseType (basics "ID")

coverType :: TypeExpr
coverType = baseType (basics "Cover")

consStoreType :: TypeExpr
consStoreType = baseType (basics "ConstStore")

failInfoType :: TypeExpr
failInfoType = baseType (basics "FailInfo")

constraintType :: TypeExpr
constraintType = baseType (basics "Constraints")

basics :: String -> QName
basics n = ("Basics", n)

curryPre :: String -> QName
curryPre n = (curryPrelude, n)

narrow :: QName
narrow = basics "narrow"

narrows :: QName
narrows = basics "narrows"

cover :: QName
cover = basics "cover"

incCover :: QName
incCover = basics "incCover"

curryAnd :: QName
curryAnd = curryPre $ funcPrefix True D FuncFO ++ genRename "&&"

curryOr :: QName
curryOr = curryPre $ funcPrefix True D FuncFO ++ genRename "||"

curryLt :: QName
curryLt = curryPre $ funcPrefix True D FuncFO ++ genRename "<"
