--- ----------------------------------------------------------------------------
--- This module contains data types for the classification of (non)determinism
--- and first order/higher order.
---
--- @author  Björn Peemöller, Fabian Skrlac
--- @version May 2014
--- ----------------------------------------------------------------------------
module Classification where

--- Classification of a function to be either deterministic
--- or *potentially* non-deterministic.
--- @cons D  deterministic function
--- @cons ND potentially non-deterministic function
data NDClass = D | ND
  deriving (Eq, Show, Read)


--- Classification of a type (constructor) to be first order or higher order.
--- @cons TypeFO - type whose values can not contain any function
---                or IO action (modulo polymorphism)
--- @cons TypeIO - type whose values may contain an IO action, but not
---                a function (modulo polymorphism)
--- @cons TypeHO - type whose values may contain a function
data TypeHOClass = TypeFO | TypeIO | TypeHO
  deriving (Eq, Show, Read)

--- Return the greater of the two `TypeHOClass`es.
maxTypeHOClass :: TypeHOClass -> TypeHOClass -> TypeHOClass
maxTypeHOClass TypeFO c      = c
maxTypeHOClass TypeIO TypeFO = TypeIO
maxTypeHOClass TypeIO TypeIO = TypeIO
maxTypeHOClass TypeIO TypeHO = TypeHO
maxTypeHOClass TypeHO _      = TypeHO

--- Return the maximal of all given `TypeHOClass`es,
--- defaults to `TypeFO` for an empty list.
maximumTypeHOClass :: [TypeHOClass] -> TypeHOClass
maximumTypeHOClass thcs = foldr maxTypeHOClass TypeFO thcs


--- Classification of a data constructor to be first order or higher order.
--- @cons ConsFO - first  order constructor
--- @cons TypeIO - higher order constructor (contains a function type as its
---                argument type, either directly or wrapped by other type
---                constructors)
data ConsHOClass = ConsFO | ConsHO
  deriving (Eq, Show, Read)

--- Return the greater of the two `ConsHOClass`es.
maxConsHOClass :: ConsHOClass -> ConsHOClass -> ConsHOClass
maxConsHOClass ConsFO c = c
maxConsHOClass ConsHO _ = ConsHO

--- Return the maximal of all given `ConsHOClass`es,
--- defaults to `ConsFO` for an empty list.
maximumConsHOClass :: [ConsHOClass] -> ConsHOClass
maximumConsHOClass chcs = foldr maxConsHOClass ConsFO chcs

--- Convert the higher order information of a type to one for a constructor.
typeToConsHOClass :: TypeHOClass -> ConsHOClass
typeToConsHOClass TypeFO = ConsFO
typeToConsHOClass TypeIO = ConsFO
typeToConsHOClass TypeHO = ConsHO


--- Classification of a function to be first order or higher order.
--- @cons FuncFO      - first order function
--- @cons FuncHORes a - first order function that yields a function
---                     with the arity `a` as its result
--- @cons FuncHO      - higher order function
data FuncHOClass = FuncFO | FuncHORes Int | FuncHO
  deriving (Eq, Show, Read)

--- Return the greater of the two `FuncHOClass`es.
maxFuncHOClass :: FuncHOClass -> FuncHOClass -> FuncHOClass
maxFuncHOClass FuncFO          b             = b
maxFuncHOClass a@(FuncHORes _) FuncFO        = a
maxFuncHOClass (FuncHORes   _) (FuncHORes _) = error "Classification.maxFuncHOClass"
maxFuncHOClass (FuncHORes   _) FuncHO        = FuncHO
maxFuncHOClass FuncHO         _              = FuncHO

--- Return the maximal of all given `FuncHOClass`es,
--- defaults to `FuncFO` for an empty list.
maximumFuncHOClass :: [FuncHOClass] -> FuncHOClass
maximumFuncHOClass fhcs = foldr maxFuncHOClass FuncFO fhcs

--- Convert the higher order information of a type to one for a function.
--- @param ioAsHo - Should the presense of an IO action as an argument
---                 make the function be classified as higher order?
typeToFuncHOClass :: Bool -> TypeHOClass -> FuncHOClass
typeToFuncHOClass _      TypeFO = FuncFO
typeToFuncHOClass ioAsHo TypeIO = if ioAsHo then FuncHO else FuncFO
typeToFuncHOClass _      TypeHO = FuncHO
