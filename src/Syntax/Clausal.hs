module Syntax.Clausal
  ( Literal (..),
    fromFormula,
    vars,
    fnConsts,
    objConsts,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst (..))
import Syntax.Formula (Formula (..))
import Syntax.Term (Term (..))
import qualified Syntax.Term as Term
import Syntax.Variable (Var (..))

-- | A relation or negated relation that is applied to term arguments.
data Literal
  = -- | Positive literal.
    LPos RltnConst [Term]
  | -- | Negative literal.
    LNeg RltnConst [Term]
  deriving (Eq, Show)

-- | Transform the propositional formula into its clausal form. The clausal form
-- is equisatisfiable with the original formula.
--
-- The returned value represents a conjunction of a list of "clauses" where each
-- "clause" represents a disjunction of a list of literals.
fromFormula :: Formula -> [[Literal]]
fromFormula =
  removeOperators
    . distributeDisjunctions
    . removeUniversals
    . removeExistentials
    . negsIn
    . removeImpIffs

-- | Return the set of variables in the literal.
vars :: Literal -> Set Var
vars lit = case lit of
  LPos _ trms -> Set.unions (Term.vars <$> trms)
  LNeg _ trms -> Set.unions (Term.vars <$> trms)

-- | Return the set of object constants in the literal.
objConsts :: Literal -> Set ObjConst
objConsts lit = case lit of
  LPos _ trms -> Set.unions (Term.objConsts <$> trms)
  LNeg _ trms -> Set.unions (Term.objConsts <$> trms)

-- | Return the set of function constants in the literal.
fnConsts :: Literal -> Set FnConst
fnConsts lit = case lit of
  LPos _ trms -> Set.unions (Term.fnConsts <$> trms)
  LNeg _ trms -> Set.unions (Term.fnConsts <$> trms)

-- -----------------------------------------------------------------------------
-- REMOVE IMPLICATIONS AND EQUIVALENCES

-- | A propositional formula without implication or equivalence connectives.
data ImpIffsRemoved
  = IFalse
  | ITrue
  | IAtom RltnConst [Term]
  | INot ImpIffsRemoved
  | IAnd ImpIffsRemoved ImpIffsRemoved
  | IOr ImpIffsRemoved ImpIffsRemoved
  | IForAll Var ImpIffsRemoved
  | IExists Var ImpIffsRemoved
  deriving (Eq, Ord, Show)

-- | Remove implication and equivalence connectives.
removeImpIffs :: Formula -> ImpIffsRemoved
removeImpIffs fo = case fo of
  FFalse -> IFalse
  FTrue -> ITrue
  FAtom rltnConst args -> IAtom rltnConst args
  FNot p -> INot (removeImpIffs p)
  FAnd p q -> IAnd (removeImpIffs p) (removeImpIffs q)
  FOr p q -> IOr (removeImpIffs p) (removeImpIffs q)
  FImp p q -> IOr (INot $ removeImpIffs p) (removeImpIffs q)
  FIff p q ->
    IAnd
      (IOr (INot $ removeImpIffs p) (removeImpIffs q))
      (IOr (removeImpIffs p) (INot $ removeImpIffs q))
  FForAll x p -> IForAll x (removeImpIffs p)
  FExists x p -> IExists x (removeImpIffs p)

-- -----------------------------------------------------------------------------
-- PUSH NEGATIONS IN AND CANCEL THEM OUT

-- | A propsitional formula where negations have been pushed down and canceled
-- out such that the only negations in the formula are the ones on negative
-- literals.
data NegsIn
  = NFalse
  | NTrue
  | NPosLit RltnConst [Term]
  | NNegLit RltnConst [Term]
  | NAnd NegsIn NegsIn
  | NOr NegsIn NegsIn
  | NForAll Var NegsIn
  | NExists Var NegsIn
  deriving (Eq, Ord, Show)

-- | Push negations down and cancel them out so that the only negations in the
-- formula are the ones on negative literals.
negsIn :: ImpIffsRemoved -> NegsIn
negsIn fo = case fo of
  IFalse -> NFalse
  ITrue -> NTrue
  IAtom rltnConst args -> NPosLit rltnConst args
  INot IFalse -> NTrue
  INot ITrue -> NFalse
  INot (IAtom rltnConst args) -> NNegLit rltnConst args
  INot (INot p) -> negsIn p
  INot (IAnd p q) -> NOr (negsIn $ INot p) (negsIn $ INot q)
  INot (IOr p q) -> NAnd (negsIn $ INot p) (negsIn $ INot q)
  INot (IForAll x p) -> NExists x (negsIn $ INot p)
  INot (IExists x p) -> NForAll x (negsIn $ INot p)
  IAnd p q -> NAnd (negsIn p) (negsIn q)
  IOr p q -> NOr (negsIn p) (negsIn q)
  IForAll x p -> NForAll x (negsIn p)
  IExists x p -> NExists x (negsIn p)

-- -----------------------------------------------------------------------------
-- REMOVE EXISTENTIAL QUANTIFICATIONS (SKOLEMIZE)

-- | A propsitional formula where existential quantifiers have been removed and
-- existentially quantified variables have been replaces with Skolem constants
-- and Skolem functions.
data ExistentialsRemoved
  = EFalse
  | ETrue
  | EPosLit RltnConst [Term]
  | ENegLit RltnConst [Term]
  | EAnd ExistentialsRemoved ExistentialsRemoved
  | EOr ExistentialsRemoved ExistentialsRemoved
  | EForAll Var ExistentialsRemoved
  deriving (Eq, Ord, Show)

removeExistentials :: NegsIn -> ExistentialsRemoved
removeExistentials fo =
  let (_, fo') = removeExs Set.empty fo
   in fo'

removeExs :: Set String -> NegsIn -> (Set String, ExistentialsRemoved)
removeExs avoid fo = case fo of
  NFalse -> (avoid, EFalse)
  NTrue -> (avoid, ETrue)
  NPosLit rltnConst args -> (avoid, EPosLit rltnConst args)
  NNegLit rltnConst args -> (avoid, ENegLit rltnConst args)
  NAnd p q ->
    let (avoid', p') = removeExs avoid p
        (avoid'', q') = removeExs avoid' q
     in (avoid'', EAnd p' q')
  NOr p q ->
    let (avoid', p') = removeExs avoid p
        (avoid'', q') = removeExs avoid' q
     in (avoid'', EOr p' q')
  NForAll x p ->
    let (avoid', p') = removeExs avoid p
     in (avoid', EForAll x p')
  NExists x p ->
    if Set.null free
      then
        let name = variant ("c_" ++ varName x) avoid
            skolemObj = TObj (ObjConst name)
            avoid' = name `Set.insert` avoid
            fo' = subst x skolemObj p
         in removeExs avoid' fo'
      else
        let name = variant ("f_" ++ varName x) avoid
            skolemFn = TFn (FnConst name (Set.size free)) (TVar <$> Set.toAscList free)
            avoid' = name `Set.insert` avoid
            fo' = subst x skolemFn p
         in removeExs avoid' fo'
    where
      free = freeVars fo

-- | Return the set free domain variables in the formula.
freeVars :: NegsIn -> Set Var
freeVars fo = case fo of
  NFalse -> Set.empty
  NTrue -> Set.empty
  NPosLit _ args -> Set.unions (Term.vars <$> args)
  NNegLit _ args -> Set.unions (Term.vars <$> args)
  NAnd p q -> freeVars p `Set.union` freeVars q
  NOr p q -> freeVars p `Set.union` freeVars q
  NForAll x p -> Set.delete x (freeVars p)
  NExists x p -> Set.delete x (freeVars p)

-- | Substitute the given term for the specified free variable in the formula.
-- Bound varibles in the formula are renamed as needed to avoid clashing with
-- variables in the substituted terms.
subst :: Var -> Term -> NegsIn -> NegsIn
subst targetVar sub fo = case fo of
  NFalse -> NFalse
  NTrue -> NTrue
  NPosLit rltnConst args -> NPosLit rltnConst (termSubst targetVar sub <$> args)
  NNegLit rltnConst args -> NNegLit rltnConst (termSubst targetVar sub <$> args)
  NAnd p q -> NAnd (subst targetVar sub p) (subst targetVar sub q)
  NOr p q -> NOr (subst targetVar sub p) (subst targetVar sub q)
  NForAll x p -> substQ targetVar sub NForAll x p
  NExists x p -> substQ targetVar sub NExists x p

-- | Substitutes the given term for the specified free variable in the
-- quantified formula. The variable binding is renamed as needed to avoid
-- clashing with variables in the subsituted terms.
substQ ::
  -- | The variable to replace.
  Var ->
  -- | The term to substitute.
  Term ->
  -- | Function for recreating the quantified formula into which terms are
  -- substituted.
  (Var -> NegsIn -> NegsIn) ->
  -- | The variable binding of the quantified formula into which terms are
  -- substituted.
  Var ->
  -- | The scope of the quantified formula into which terms are substituted.
  NegsIn ->
  NegsIn
substQ targetVar subTrm mkQuant x p =
  -- If the target variable is not a free variable in the quntifiers scope, so
  -- the substitution does nothing.
  if targetVar `Set.member` free
    then
      let -- Determine if substituting the new term in for the target variable
          -- would introduce a term that contains the bound variable.
          isClash = (x `Set.member` Term.vars subTrm)
          -- Come up with a new name for the bound variable that doesn't clash
          -- with any free variables in the quantifer scope or any variables in
          -- the new term.
          x' =
            if isClash
              then
                let avoidVars = free `Set.union` Term.vars subTrm
                 in Var $ variant (varName x) (Set.map varName avoidVars)
              else x
          -- Replace any instances of the bound variable x to x' before
          -- substituting in the new term for the target variable.
          p' = subst x (TVar x') p
          p'' = subst targetVar subTrm p
       in mkQuant x' p''
    else mkQuant x p
  where
    free = x `Set.delete` freeVars p

-- TODO: Use Substitution.applyToTerm instead.

-- | Substitute the given term for the specified free variable in the term.
termSubst :: Var -> Term -> Term -> Term
termSubst x sub trm = case trm of
  TVar (Var name) ->
    if name == varName x
      then sub
      else trm
  TObj _ -> trm
  TFn fnConst args -> TFn fnConst (termSubst x sub <$> args)

-- | Returns a variant of the given string that is distinct from the list of
-- strings to avoid. Returns the string unchanged if it is not in the list of
-- strings to avoid.
variant :: String -> Set String -> String
variant s avoid =
  if s `elem` avoid
    then variant (s ++ "'") avoid
    else s

-- -----------------------------------------------------------------------------
-- REMOVE UNIVERSAL QUANTIFICATIONS

-- | A propsitional formula where universal quantifiers have been removed.
data UniversalsRemoved
  = UFalse
  | UTrue
  | UPosLit RltnConst [Term]
  | UNegLit RltnConst [Term]
  | UAnd UniversalsRemoved UniversalsRemoved
  | UOr UniversalsRemoved UniversalsRemoved
  deriving (Eq, Ord, Show)

removeUniversals :: ExistentialsRemoved -> UniversalsRemoved
removeUniversals fo = case fo of
  EFalse -> UFalse
  ETrue -> UTrue
  EPosLit rltnConst args -> UPosLit rltnConst args
  ENegLit rltnConst args -> UNegLit rltnConst args
  EAnd p q -> UAnd (removeUniversals p) (removeUniversals q)
  EOr p q -> UOr (removeUniversals p) (removeUniversals q)
  EForAll x p -> removeUniversals p

-- -----------------------------------------------------------------------------
-- DISTRIBUTE DISJUNCTIONS TO PUT FORMULA IN CNF

-- A formula in conjunction normal form where the top of the formula is a
-- multi-element conjunction and each conjunct is a multi-element disjunction.
data CnfConjunction
  = CAnd CnfConjunction CnfConjunction
  | CDisj CnfDisjunction

-- A disjunction in a conjunction normal form formula.
data CnfDisjunction
  = CFalse
  | CTrue
  | CPosLit RltnConst [Term]
  | CNegLit RltnConst [Term]
  | COr CnfDisjunction CnfDisjunction
  deriving (Eq, Ord, Show)

distributeDisjunctions :: UniversalsRemoved -> CnfConjunction
distributeDisjunctions fo = case fo of
  UFalse -> CDisj CFalse
  UTrue -> CDisj CTrue
  UPosLit rltnConst args -> CDisj (CPosLit rltnConst args)
  UNegLit rltnConst args -> CDisj (CNegLit rltnConst args)
  UAnd p q -> CAnd (distributeDisjunctions p) (distributeDisjunctions q)
  UOr p (UAnd q r) ->
    CAnd
      (distributeDisjunctions (UOr p q))
      (distributeDisjunctions (UOr p r))
  UOr (UAnd p q) r ->
    CAnd
      (distributeDisjunctions (UOr p r))
      (distributeDisjunctions (UOr q r))
  UOr p q ->
    let p' = distributeDisjunctions p
        q' = distributeDisjunctions q
     in case (p', q') of
          (p, CAnd ql qr) -> distributeLeftDisj p ql qr
          (CAnd pl pr, CDisj qd) -> distributeRightDisj' pl pr qd
          (CDisj pd, CDisj qd) -> CDisj (COr pd qd)

-- | Distributes disjunction with the left formula p over the conjuncts in q.
--
-- p or (q1 and ... and qn) -> (p or q1) and ... (p or qn)
distributeLeftDisj :: CnfConjunction -> CnfConjunction -> CnfConjunction -> CnfConjunction
distributeLeftDisj p qLeft qRight =
  let left =
        case qLeft of
          CDisj qLeftD ->
            case p of
              CAnd pLeft pRight -> distributeRightDisj' pLeft pRight qLeftD
              CDisj pd -> CDisj (COr pd qLeftD)
          CAnd rLeft rRight -> distributeLeftDisj p rLeft rRight
      right =
        case qRight of
          CDisj qRightD ->
            case p of
              CAnd pLeft pRight -> distributeRightDisj' pLeft pRight qRightD
              CDisj pd -> CDisj (COr pd qRightD)
          CAnd rLeft rRight -> distributeLeftDisj p rLeft rRight
   in CAnd left right

-- | Distributes disjunction with the right formula q over the conjuncts in p.
--
-- (p1 and ... and pn) or q -> (p1 or q) and ... (pn or q)
distributeRightDisj' :: CnfConjunction -> CnfConjunction -> CnfDisjunction -> CnfConjunction
distributeRightDisj' pLeft pRight q =
  let left =
        case pLeft of
          CDisj pLeftD -> CDisj (COr pLeftD q)
          CAnd pLeftLeft pLeftRight -> distributeRightDisj' pLeftLeft pLeftRight q
      right =
        case pRight of
          CDisj pRightD -> CDisj (COr pRightD q)
          CAnd pRightLight pRightRight -> distributeRightDisj' pRightLight pRightRight q
   in CAnd left right

-- -----------------------------------------------------------------------------
-- REMOVE OPERATORS TO CREATE CLAUSES

removeOperators :: CnfConjunction -> [[Literal]]
removeOperators conj = case conj of
  CAnd p q -> removeOperators p ++ removeOperators q
  CDisj disj ->
    case removeOrs disj of
      Just clause -> [clause]
      Nothing -> []

removeOrs :: CnfDisjunction -> Maybe [Literal]
removeOrs disj = case disj of
  CFalse -> Just [] -- Unsatisfiable clause.
  CTrue -> Nothing -- Clause would be a tautology so we omit it.
  CPosLit rltnConst args -> Just [LPos rltnConst args]
  CNegLit rltnConst args -> Just [LNeg rltnConst args]
  COr p q -> do
    pDisjs <- removeOrs p
    qDisjs <- removeOrs q
    return $ pDisjs ++ qDisjs
