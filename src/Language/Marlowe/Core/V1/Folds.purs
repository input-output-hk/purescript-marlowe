module Language.Marlowe.Core.V1.Folds where

import Prelude

import Control.Monad.State (State, execState, modify)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Case
  , Contract
  , Observation
  , Value
  )
import Language.Marlowe.Core.V1.Traversals
  ( Visitor(..)
  , rewriteContractBottomUp
  , rewriteContractTopDown
  )

-- To avoid confusion with the order of arguments in the step
-- function let's provide a bit more "order agnostic" wrapper.
newtype StepArgs a node = StepArgs
  { node :: node
  , accum :: a
  }

derive instance Newtype (StepArgs a b) _

data Step a = Step
  { stepCase :: Maybe (StepArgs a Case -> a)
  , stepContract :: Maybe (StepArgs a Contract -> a)
  , stepObservation :: Maybe (StepArgs a Observation -> a)
  , stepValue :: Maybe (StepArgs a Value -> a)
  }

defaultStep :: forall a. Step a
defaultStep = Step
  { stepCase: Nothing
  , stepContract: Nothing
  , stepObservation: Nothing
  , stepValue: Nothing
  }

foldingVisitor :: forall a. Step a -> Visitor (State a)
foldingVisitor (Step { stepCase, stepContract, stepObservation, stepValue }) =
  do
    let
      stepCase' :: StepArgs a Case -> a
      stepCase' = fromMaybe (_.accum <<< unwrap) stepCase

      stepContract' :: StepArgs a Contract -> a
      stepContract' = fromMaybe (_.accum <<< unwrap) stepContract

      stepObservation' :: StepArgs a Observation -> a
      stepObservation' = fromMaybe (_.accum <<< unwrap) stepObservation

      stepValue' :: StepArgs a Value -> a
      stepValue' = fromMaybe (_.accum <<< unwrap) stepValue

    Visitor
      { onCase: \c -> do
          _ <- modify (\x -> (stepCase' $ StepArgs { node: c, accum: x }))
          pure c
      , onContract: \c -> do
          _ <- modify (\x -> (stepContract' $ StepArgs { node: c, accum: x }))
          pure c
      , onObservation: \c -> do
          _ <- modify
            (\x -> (stepObservation' $ StepArgs { node: c, accum: x }))
          pure c
      , onValue: \c -> do
          _ <- modify (\x -> (stepValue' $ StepArgs { node: c, accum: x }))
          pure c
      }

-- Top down folding.
foldlContract :: forall a. Step a -> a -> Contract -> a
foldlContract step a cntr = do
  let
    visitor = foldingVisitor step
  execState (rewriteContractTopDown visitor cntr) a

-- Bottom up folding.
foldrContract :: forall a. Step a -> a -> Contract -> a
foldrContract step a cntr = do
  let
    visitor = foldingVisitor step
  execState (rewriteContractBottomUp visitor cntr) a

data MapStep a = MapStep
  { mapCase :: Case -> a
  , mapContract :: Contract -> a
  , mapObservation :: Observation -> a
  , mapValue :: Value -> a
  }

defaultMapStep :: forall a. Monoid a => MapStep a
defaultMapStep = MapStep
  { mapCase: const mempty
  , mapContract: const mempty
  , mapObservation: const mempty
  , mapValue: const mempty
  }

foldMapContract :: forall a. Monoid a => MapStep a -> Contract -> a
foldMapContract (MapStep { mapCase, mapContract, mapObservation, mapValue }) =
  do
    let
      appendAccum :: forall node. (node -> a) -> StepArgs a node -> a
      appendAccum f (StepArgs { node, accum }) = f node <> accum

      step = Step
        { stepCase: Just $ appendAccum mapCase
        , stepContract: Just $ appendAccum mapContract
        , stepObservation: Just $ appendAccum mapObservation
        , stepValue: Just $ appendAccum mapValue
        }
    foldlContract step mempty

-- | Flipped version which allows folding using `for` like or infix syntax.
foldMapContractFlipped :: forall a. Monoid a => Contract -> MapStep a -> a
foldMapContractFlipped = flip foldMapContract
