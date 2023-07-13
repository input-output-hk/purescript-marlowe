module ReadableContract where

import Prelude

import Control.Monad.State (State, evalState, execState, get, gets, put)
import Data.Array (fold, length, partition)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Lens (Lens', assign, modifying)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Set as Set
import Data.Traversable (for, for_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Dodo
  ( Doc
  , break
  , foldWithSeparator
  , indent
  , isEmpty
  , lines
  , plainText
  , print
  , softBreak
  , text
  , twoSpaces
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(..)
  , Party(..)
  , TokenName
  )
import Language.Marlowe.Extended.V1
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Module(..)
  , Payee(..)
  , Timeout
  )
import Marlowe.HasParties (getParties)
import Type.Prelude (Proxy(..))

isRole :: Party -> Boolean
isRole (Role _) = true
isRole _ = false

tokenName :: Party -> Maybe TokenName
tokenName (Role name) = Just name
tokenName _ = Nothing

partyDisplay :: Party -> String
partyDisplay (Role name) = show name
partyDisplay (Address addr) = show addr

type ParticipantInfo =
  { party :: Party
  , description :: Maybe String
  }

type ContractParticipantInfo =
  { addressParties :: Array ParticipantInfo
  , roleParties :: Array ParticipantInfo
  }

partyInfoFromModule :: Module -> ContractParticipantInfo
partyInfoFromModule (Module { metadata, contract }) =
  let
    { yes, no } = partition isRole (Set.toUnfoldable $ getParties contract)
    addressParties =
      no <#> \party -> { party, description: Nothing }
    roleParties =
      yes <#> \party ->
        { party
        , description: do
            name <- tokenName party
            Map.lookup name metadata.roleDescriptions
        }

  in
    { addressParties
    , roleParties
    }

generateParticipantsPreface :: forall a. ContractParticipantInfo -> Doc a
generateParticipantsPreface i = fold
  [ text "The contract has "
  , text (show $ length i.addressParties + length i.roleParties)
  , text " participants: "
  , softBreak
  , foldWithSeparator softBreak
      [ addressPreface
      , rolePreface
      , softBreak
      , partyDisclaimer
      ]
  ]
  where
  addressPreface = indent $ foldWithSeparator softBreak $ i.addressParties <#>
    \a -> fold
      [ text "* The fixed address "
      , text $ partyDisplay a.party
      -- TODO: Add a shorthand if there are no duplicates
      ]
  rolePreface = indent $ foldWithSeparator softBreak $ i.roleParties <#> \r ->
    fold
      [ text "* The "
      , text $ partyDisplay r.party
      , text " role token: "
      , maybe mempty text r.description
      ]
  partyDisclaimer =
    foldWithSeparator (break <> break)
      [ guard (length i.addressParties > 0)
          ( text
              "The participants with a fixed address cannot trade their participation on the contract. Only someone that posseses the private key corresponding to the address can submit the pertinent actions and any payments made to the participant is directly transferred to the address."
          )
      , guard (length i.roleParties > 0)
          ( text
              "Role tokens are distributed by the user that creates the contract and can be freely traded, making the participation dynamic. To provide an input to the contract, the user needs to also submit the token to prove their identity. When a payment is made to a Role participant, the user can use the token to withdraw the funds."
          )
      ]

-- TODO: I think I need to create a datatype with Branch and Inc, and define the
-- showStep as vertical or Horizontal, and to add a way to ignore branching for Close so that we don't
-- leave gaps.
-- Me quede aca
-- El step ref deberia ser un Path o final, y el path es NEA de Branch o Inc
type StepRef = NonEmptyArray Int

showStep :: StepRef -> String
showStep s = NEA.intercalate "." (show <$> s)

firstStep :: StepRef
firstStep = NEA.singleton 1

branchStepH :: StepRef -> StepRef
branchStepH s = NEA.snoc s 1

incStepRefH :: StepRef -> StepRef
incStepRefH s =
  let
    { init, last } = NEA.unsnoc s
  in
    NEA.snoc' init (last + 1)

branchStepV :: StepRef -> StepRef
branchStepV = incStepRefH

incStepRefV :: StepRef -> StepRef
incStepRefV = branchStepH

requiresBranching :: Contract -> Boolean
requiresBranching (If _ _ _) = true
requiresBranching (When [] _ _) = false
requiresBranching (When cs _ Close) = length cs > 1
requiresBranching (When _ _ _) = true
requiresBranching _ = false

showDate :: Timeout -> String
showDate _ = "TODO"

generateFromModule :: forall a. Module -> Doc a
generateFromModule m@(Module { metadata, contract }) = lines
  [ preface
  , softBreak
  , generateParticipantsPreface (partyInfoFromModule m)
  , softBreak
  , beforeContract
  , softBreak
  , foldWithSeparator (break <> break) (buildSteps contract)
  -- , go firstStep contract
  -- , softBreak
  -- , closeDescription
  ]
  where
  preface = lines
    [ text "Contract: " <> text metadata.contractName
    , text metadata.contractShortDescription
    ]

  beforeContract = text "The contract is defined through a series of steps... "
  closeDescription = text
    "Final Step: All the remaining funds are distributed to the participants"

  requiresNewStep :: Contract -> Boolean
  requiresNewStep Close = true
  requiresNewStep (If _ _ _) = true
  requiresNewStep (When _ _ _) = true
  requiresNewStep _ = false

  referIfNeeded s c = if requiresNewStep c then referTo s c else go s c

  referTo _ Close = text "Then the final step is executed"
  referTo s _ = text ("Then the Step " <> showStep s <> " is executed")

  -- TODO: I probably need an intermediate representation to both avoid repetition, allow contracts that don't start with When and better organize this part
  go _ Close = mempty
  go s (Assert _ cont) =
    text
      "If <todo> is not true, the contract will raise a Warning. The static analyzer should be executed on the contract to make sure that no path allows this. "
      <> (referIfNeeded s cont)
  go s (Pay from (Party to) tok val cont) =
    text
      ( "The party " <> partyDisplay from <> " will make a payment of "
          <> show val
          <> " "
          <> show tok
          <> " from his local account to "
          <> partyDisplay to
          <> " wallet. "
      ) <> (referIfNeeded s cont)
  go s (Pay from (Account to) tok val cont) =
    text
      ( "The party " <> partyDisplay from
          <> " will make an internal transfer of "
          <> show val
          <> " "
          <> show tok
          <> " from his local account to "
          <> partyDisplay to
          <> " account. "
      ) <> (referIfNeeded s cont)
  go s (When [] timeout cont) =
    text ("The contract will wait until " <> showDate timeout <> ".") <>
      (referIfNeeded s cont)
  -- go s (When cs timeout cont) =
  go _ _ = text "Hello contract"

type BuildStepState a =
  { currStep :: Doc a
  , currStepRef :: StepRef
  , steps :: NonEmptyArray (Doc a)
  }

_currStepRef :: forall a. Lens' (BuildStepState a) StepRef
_currStepRef = prop (Proxy :: Proxy "currStepRef")

branchStep' :: forall a. State (BuildStepState a) Unit
branchStep' = modifying _currStepRef branchStepV

incStepRef' :: forall a. State (BuildStepState a) Unit
incStepRef' = modifying _currStepRef incStepRefV

buildSteps :: forall a. Contract -> NonEmptyArray (Doc a)
buildSteps rootContract = evalState
  ( do
      declareStep firstStep rootContract
      flushCurrentStep
      gets _.steps
  )
  initialState
  where

  initialState = { currStep: mempty, steps: finalStep, currStepRef: firstStep }
  finalStep = NEA.singleton
    ( text "Final Step:" <> break <> indent
        (text "All the remaining funds are distributed to the participants")
    )

  _currStep = prop (Proxy :: Proxy "currStep")

  declareStep :: StepRef -> Contract -> State (BuildStepState a) Unit
  declareStep _ Close = flushCurrentStep
  declareStep stepRef stepCont = do
    flushCurrentStep
    stepDef <- describeStep stepCont
    assign _currStep $
      text ("Step " <> showStep stepRef <> ":") <> break <> indent (fst stepDef)
    for_ (snd stepDef) (\(contRef /\ cont) -> declareStep contRef cont)

  -- data Contract
  --   = Close
  --   | Pay S.AccountId Payee S.Token Value Contract
  --   | If Observation Contract Contract
  --   | When (Array Case) Timeout Contract
  --   | Let S.ValueId Value Contract
  --   | Assert Observation Contract
  describeActionStep
    :: Case -> State (BuildStepState a) (Doc a /\ StepRef /\ Contract)
  -- TODO Action pattern match
  describeActionStep (Case action c) = do
    -- TODO: maybe refactor order and make incStepRef to return the new one
    stepRef <- gets _.currStepRef
    incStepRef'
    let
      actionMsg = case action of
        (Deposit to from tok val) -> fold
          [ show from
          , " deposits "
          , show val
          , " "
          , show tok
          , " into "
          , show to
          , " account"
          ]
        (Choice (ChoiceId choiceName chooser) bounds) -> fold
          [ show chooser, " chooses ", choiceName, " between ", show bounds ]
        (Notify _) -> "Notify <todo obs>"
    pure $ text ("* " <> actionMsg <> ": " <> goto stepRef c) /\ stepRef /\ c

  goto :: StepRef -> Contract -> String
  goto _ Close = "goto final step"
  goto s _ = "goto " <> showStep s

  describeStep
    :: Contract
    -> State (BuildStepState a) (Doc a /\ Array (StepRef /\ Contract))
  describeStep Close = pure $ text "describe Close" /\ []
  describeStep (Pay from to tok val cont) = do
    contDesc <- describeStep cont
    let
      payMsg = fold
        [ "Pay "
        , show val
        , " "
        , show tok
        , " from "
        , show from
        , " to "
        , show to
        , ". "
        ]
    pure $
      (text payMsg <> fst contDesc) /\ snd contDesc
  -- TODO: Non branching When
  describeStep (When cases _ timeoutCont) = do
    branchStep'
    casesResult <- for cases \cse -> do
      actionDoc /\ stepRef /\ actionCont <- describeActionStep cse
      pure $ actionDoc /\ stepRef /\ actionCont
    timeoutStepRef <- gets _.currStepRef
    -- incStepRef'
    -- timeoutDesc <- describeStep cont
    let
      caseDocs = fst <$> casesResult
      caseConts = (\(_ /\ r /\ c) -> r /\ c) <$> casesResult
    pure $
      ( text "When " <> break
          <> indent
            ( lines
                ( caseDocs <>
                    [ text ("* timeout: " <> goto timeoutStepRef timeoutCont) ]
                )
            )
      ) /\ (caseConts <> [ timeoutStepRef /\ timeoutCont ])
  describeStep c =
    pure $ text "NOOOOOOOO" /\ []

  flushCurrentStep :: State (BuildStepState a) Unit
  flushCurrentStep = do
    { currStep, steps, currStepRef } <- get
    when (not (isEmpty currStep))
      let
        { init, last } = NEA.unsnoc steps
        steps' = NEA.snoc' (init <> [ currStep ]) last
      in
        put { currStep: text "", steps: steps', currStepRef }

printModule :: Module -> String
printModule m = print plainText twoSpaces $ generateFromModule m