------------------------------------------------------------------------------
--- This library provides a function to compute the rigid/flex status
--- of a FlatCurry expression (right-hand side of a function definition).
---
--- @author Michael Hanus
--- @version April 2005
------------------------------------------------------------------------------

module FlatCurry.FlexRigid(FlexRigidResult(..),getFlexRigid) where

import FlatCurry.Types

--- Datatype for representing a flex/rigid status of an expression.
data FlexRigidResult = UnknownFR | ConflictFR | KnownFlex | KnownRigid

--- Computes the rigid/flex status of a FlatCurry expression.
--- This function checks all cases in this expression.
--- If the expression has rigid as well as flex cases (which cannot
--- be the case for source level programs but might occur after
--- some program transformations), the result ConflictFR is returned.
getFlexRigid :: Expr -> FlexRigidResult
getFlexRigid (Var _) = UnknownFR
getFlexRigid (Lit _) = UnknownFR
getFlexRigid (Comb _ _ args) =
   foldr joinCaseTypes UnknownFR (map getFlexRigid args)
getFlexRigid (Let _ e) = getFlexRigid e
getFlexRigid (Free _ e) = getFlexRigid e
getFlexRigid (Or e1 e2) =
   joinCaseTypes (getFlexRigid e1) (getFlexRigid e2)
getFlexRigid (Case ctype e bs) =
   foldr joinCaseTypes (if ctype==Flex then KnownFlex else KnownRigid)
         (map getFlexRigid (e : map (\(Branch _ be)->be) bs))
getFlexRigid (Typed e _) = getFlexRigid e

joinCaseTypes ConflictFR ConflictFR = ConflictFR
joinCaseTypes ConflictFR UnknownFR  = ConflictFR
joinCaseTypes ConflictFR KnownFlex  = ConflictFR
joinCaseTypes ConflictFR KnownRigid = ConflictFR
joinCaseTypes UnknownFR  ConflictFR = ConflictFR
joinCaseTypes KnownFlex  ConflictFR = ConflictFR
joinCaseTypes KnownRigid ConflictFR = ConflictFR
joinCaseTypes UnknownFR  UnknownFR  = UnknownFR
joinCaseTypes UnknownFR  KnownFlex  = KnownFlex
joinCaseTypes UnknownFR  KnownRigid = KnownRigid
joinCaseTypes KnownFlex  UnknownFR  = KnownFlex
joinCaseTypes KnownFlex  KnownFlex  = KnownFlex
joinCaseTypes KnownFlex  KnownRigid = ConflictFR
joinCaseTypes KnownRigid UnknownFR  = KnownRigid
joinCaseTypes KnownRigid KnownFlex  = ConflictFR
joinCaseTypes KnownRigid KnownRigid = KnownRigid

