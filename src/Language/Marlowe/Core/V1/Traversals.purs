module Language.Marlowe.Core.V1.Traversals where

import Prelude

import Data.Traversable (for)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Case(..)
  , Contract(..)
  , Observation(..)
  , Value(..)
  )

data Visitor f = Visitor
  { onCase :: Case -> f Case
  , onContract :: Contract -> f Contract
  , onObservation :: Observation -> f Observation
  , onValue :: Value -> f Value
  }

defaultVisitor :: forall f. Applicative f => Visitor f
defaultVisitor = Visitor
  { onCase: pure
  , onContract: pure
  , onObservation: pure
  , onValue: pure
  }

traverseContract
  :: forall f. Applicative f => Visitor f -> Contract -> f Contract
traverseContract (Visitor { onContract, onValue, onObservation, onCase }) =
  case _ of
    Close -> onContract Close
    Pay accId payee token value contract -> Pay accId payee token
      <$> onValue value
      <*> onContract contract
    If obs contract1 contract2 -> If
      <$> onObservation obs
      <*> onContract contract1
      <*> onContract contract2
    When cases timeout contract -> When
      <$> for cases onCase
      <*> pure timeout
      <*> onContract contract
    Let valId value contract -> Let valId
      <$> onValue value
      <*> onContract contract
    Assert obs contract -> Assert
      <$> onObservation obs
      <*> onContract contract

traverseCase :: forall f. Functor f => Visitor f -> Case -> f Case
traverseCase (Visitor { onContract }) (Case action contract) =
  Case action <$> onContract contract

traverseObservation
  :: forall f. Applicative f => Visitor f -> Observation -> f Observation
traverseObservation (Visitor { onObservation, onValue }) = case _ of
  AndObs obs1 obs2 -> AndObs
    <$> onObservation obs1
    <*> onObservation obs2
  OrObs obs1 obs2 -> OrObs
    <$> onObservation obs1
    <*> onObservation obs2
  NotObs obs -> NotObs <$> onObservation obs
  ValueGE val1 val2 -> ValueGE
    <$> onValue val1
    <*> onValue val2
  ValueGT val1 val2 -> ValueGT
    <$> onValue val1
    <*> onValue val2
  ValueLE val1 val2 -> ValueLE
    <$> onValue val1
    <*> onValue val2
  ValueLT val1 val2 -> ValueLT
    <$> onValue val1
    <*> onValue val2
  ValueEQ val1 val2 -> ValueEQ
    <$> onValue val1
    <*> onValue val2
  TrueObs -> pure TrueObs
  FalseObs -> pure FalseObs
  chose@(ChoseSomething _) -> pure chose

traverseValue :: forall f. Applicative f => Visitor f -> Value -> f Value
traverseValue (Visitor { onValue, onObservation }) = case _ of
  a@(AvailableMoney _ _) -> pure a
  c@(Constant _) -> pure c
  NegValue value -> NegValue <$> onValue value
  AddValue val1 val2 -> AddValue
    <$> onValue val1
    <*> onValue val2
  SubValue val1 val2 -> SubValue
    <$> onValue val1
    <*> onValue val2
  MulValue val1 val2 -> MulValue
    <$> onValue val1
    <*> onValue val2
  DivValue val1 val2 -> DivValue
    <$> onValue val1
    <*> onValue val2
  c@(ChoiceValue _) -> pure c
  t@TimeIntervalStart -> pure t
  t@TimeIntervalEnd -> pure t
  u@(UseValue _) -> pure u
  Cond obs val1 val2 -> Cond
    <$> onObservation obs
    <*> onValue val1
    <*> onValue val2

-- | Given non recursive vistor create a recursive top to bottom one.
topDownVisitor :: forall f. Monad f => Visitor f -> Visitor f
topDownVisitor (Visitor { onContract, onValue, onCase, onObservation }) = do
  let
    visitor' = Visitor
      { onContract: \c -> onContract c >>= traverseContract visitor'
      , onValue: \c -> onValue c >>= traverseValue visitor'
      , onCase: \c -> onCase c >>= traverseCase visitor'
      , onObservation: \c -> onObservation c >>= traverseObservation visitor'
      }
  visitor'

rewriteContractTopDown
  :: forall f. Monad f => Visitor f -> Contract -> f Contract
rewriteContractTopDown visitor contract = do
  let
    Visitor { onContract } = topDownVisitor visitor
  onContract contract

-- | Flipped version which allows traversing using `for` like infix syntax.
forContractTopDown :: forall f. Monad f => Contract -> Visitor f -> f Contract
forContractTopDown = flip rewriteContractTopDown

-- | Given non recursive vistor create a recursive bottom to top one.
bottomUpVisitor :: forall f. Monad f => Visitor f -> Visitor f
bottomUpVisitor (Visitor { onContract, onValue, onCase, onObservation }) = do
  let
    visitor' = Visitor
      { onContract: \c -> traverseContract visitor' c >>= onContract
      , onValue: \c -> traverseValue visitor' c >>= onValue
      , onCase: \c -> traverseCase visitor' c >>= onCase
      , onObservation: \c -> traverseObservation visitor' c >>= onObservation
      }
  visitor'

rewriteContractBottomUp
  :: forall f. Monad f => Visitor f -> Contract -> f Contract
rewriteContractBottomUp visitor contract = do
  let
    Visitor { onContract } = bottomUpVisitor visitor
  onContract contract

-- | Flipped version which allows traversing using `for` like or infix syntax.
forContractBottomUp :: forall f. Monad f => Contract -> Visitor f -> f Contract
forContractBottomUp = flip rewriteContractBottomUp
