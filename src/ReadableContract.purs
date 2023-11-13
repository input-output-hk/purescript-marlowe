module ReadableContract where

import Prelude

import Control.Monad.State (State, evalState, execState, get, gets, put)
import Data.Array (fold, length, partition)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
-- TODO: See if change NonEmptyArray with NonEmptyList that allows for pattern match and O(1) for cons
import Data.Array.NonEmpty as NEA
import Data.Lens (Lens', assign, modifying, use, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
data PathType
  = BranchStep
  | IncStep

derive instance Eq PathType
instance Show PathType where
  show BranchStep = "BranchStep"
  show IncStep = "IncStep"

data StepRef
  = InitialStep
  | StepPath (NonEmptyArray PathType)
  | FinalStep

-- Remove single incs between two branches
normalizeStepRef :: NonEmptyArray PathType -> NonEmptyArray PathType
normalizeStepRef path = fromMaybe path $ NEA.fromFoldable $ removeSingleInc $
  NEA.toUnfoldable path
  where
  removeSingleInc :: List PathType -> List PathType
  removeSingleInc Nil = Nil
  removeSingleInc (x : Nil) = x : Nil
  removeSingleInc (x : y : Nil) = (x : y : Nil)
  removeSingleInc (BranchStep : IncStep : BranchStep : xs) =
    (BranchStep : removeSingleInc (BranchStep : xs))
  removeSingleInc (x : xs) = x : removeSingleInc xs

showStep :: StepRef -> String
showStep InitialStep = "1"
showStep FinalStep = "final step"
showStep (StepPath path) = NEA.intercalate "." (show <$> tmp)
  -- TODO: Remove tmp
  -- showStep (StepPath path) = NEA.intercalate "." (show <$> trace {path, tmp} (\_ -> tmp ))
  where
  tmp = go (NEA.toUnfoldable $ normalizeStepRef path) (NEA.singleton 1)

  go Nil intPath = intPath
  go (h : t) intPath = go t
    ( case h of
        IncStep -> NEA.snoc intPath 0
        BranchStep ->
          let
            { init, last } = NEA.unsnoc intPath
          in
            NEA.snoc' init (last + 1)
    )

firstStep :: StepRef
firstStep = InitialStep

branchStep :: StepRef -> StepRef
branchStep InitialStep = StepPath (NEA.singleton BranchStep)
branchStep (StepPath path) = StepPath (NEA.snoc path BranchStep)
branchStep FinalStep = FinalStep

incStepRef :: StepRef -> StepRef
incStepRef InitialStep = StepPath (NEA.singleton IncStep)
incStepRef (StepPath path) = StepPath (NEA.snoc path IncStep)
incStepRef FinalStep = FinalStep

requiresBranching :: Contract -> Boolean
requiresBranching (If _ _ _) = true
requiresBranching (When [] _ _) = false
requiresBranching (When cs _ Close) = length cs > 1
requiresBranching (When _ _ _) = true
requiresBranching _ = false

showDate :: Timeout -> String
showDate _ = "TODO"

-- Aca me quede con un cambio de como se numeran los steps. Tendria que hacer un Unit test de eso
generateFromModule :: forall a. Module -> Doc a
generateFromModule m@(Module { metadata, contract }) = lines
  [ preface
  , softBreak
  , generateParticipantsPreface (partyInfoFromModule m)
  -- TODO: Add value preface and timeout preface, and token summary (instead of parameter just abbreviate)
  -- TODO: Choices preface?
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
-- branchStep' Close =  pure FinalStep
branchStep' =
  modifying _currStepRef branchStep

incStepRef' :: forall a. Contract -> State (BuildStepState a) StepRef
incStepRef' Close = pure FinalStep
incStepRef' _ = do
  currStep <- use _currStepRef
  assign _currStepRef (incStepRef currStep)
  -- traceM {incCurr: currStep, inchNext: (incStepRef currStep) }
  pure currStep

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

  describeActionStep
    :: Case -> StepRef -> State (BuildStepState a) (Doc a /\ Contract)
  describeActionStep (Case action c) stepRef = do
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
    pure $ text ("* " <> actionMsg <> ": goto " <> showStep stepRef) /\ c

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
  describeStep (Let _ _ cont) = do
    contDesc <- describeStep cont
    let
      letMsg = "Let. "
    pure $ (text letMsg <> fst contDesc) /\ snd contDesc
  describeStep (Assert _ cont) = do
    contDesc <- describeStep cont
    let
      assertMsg = "Assert. "
    pure $ (text assertMsg <> fst contDesc) /\ snd contDesc
  describeStep (If _ thenCont elseCont) = do
    branchStep'
    thenContRef <- incStepRef' thenCont
    elseContRef <- incStepRef' elseCont
    let
      conts = [ thenContRef /\ thenCont, elseContRef /\ elseCont ]
      ifMsg = fold
        [ "If <todo> then goto "
        , showStep thenContRef
        , " else goto "
        , showStep elseContRef

        ]
    pure $ text ifMsg /\ conts
  -- data Contract
  --   = Close
  --   | Pay S.AccountId Payee S.Token Value Contract
  --   | If Observation Contract Contract
  --   | When (Array Case) Timeout Contract
  --   | Let S.ValueId Value Contract
  --   | Assert Observation Contract
  -- TODO: Non branching When
  describeStep (When cases _ timeoutCont) = do
    branchStep'
    casesResult <- for cases \cse -> do
      let Case _ c = cse
      stepRef <- incStepRef' c
      actionDoc /\ actionCont <- describeActionStep cse stepRef
      pure $ actionDoc /\ stepRef /\ actionCont
    timeoutStepRef <- incStepRef' timeoutCont
    let
      caseDocs = fst <$> casesResult
      caseConts = (\(_ /\ r /\ c) -> r /\ c) <$> casesResult
    pure $
      ( text "When " <> break
          <> indent
            ( lines
                ( caseDocs <>
                    [ text ("* timeout: goto " <> showStep timeoutStepRef) ]
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