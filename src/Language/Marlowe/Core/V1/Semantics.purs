module Language.Marlowe.Core.V1.Semantics where

import Prelude

import Data.BigInt.Argonaut (BigInt, fromInt, quot)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, unInstant)
import Data.Foldable (class Foldable, any, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (round)
import Data.Lens (over, set, to, view)
import Data.List (List(..), fromFoldable, reverse, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Accounts
  , Action(..)
  , ApplyAllResult(..)
  , ApplyResult(..)
  , ApplyWarning(..)
  , Bound(..)
  , Case(..)
  , ChosenNum
  , Contract(..)
  , Environment(..)
  , Input(..)
  , InputContent(..)
  , IntervalError(..)
  , IntervalResult(..)
  , Money
  , Observation(..)
  , Party
  , Payee(..)
  , Payment(..)
  , ReduceEffect(..)
  , ReduceResult(..)
  , ReduceStepResult(..)
  , ReduceWarning(..)
  , State(..)
  , TimeInterval(..)
  , Timeout
  , Token(..)
  , TransactionError(..)
  , TransactionInput
  , TransactionOutput(..)
  , TransactionWarning(..)
  , Value(..)
  , _boundValues
  , _choices
  , _minTime
  , _timeInterval
  , asset
  , getAction
  , getInputContent
  , ivFrom
  , ivTo
  )
import Marlowe.Time (unixEpoch)

makeEnvironment :: Instant -> Instant -> Environment
makeEnvironment l h = Environment
  { timeInterval: TimeInterval l h
  }

validInterval :: TimeInterval -> Boolean
validInterval (TimeInterval from to) = from <= to

above :: Instant -> TimeInterval -> Boolean
above v (TimeInterval _ to) = v > to

anyWithin :: forall f. Foldable f => Instant -> f TimeInterval -> Boolean
anyWithin v = any (\(TimeInterval from to) -> v >= from && v <= to)

getEncompassBound :: forall f. Foldable f => Functor f => f Bound -> Bound
getEncompassBound bounds =
  -- NOTE: We can't use the Min/Max semigroup with foldMap and we need to make a fromInt top/bottom
  --       because BigInt doesn't have a Bounded instance. This should be fine in reality
  --       but it could lead to a bug if the lower bound is bigger than the biggest Int or if
  --       the highest bound is lower than the smallest Int.
  -- NOTE': We fold the datastructure twice instead of doing it in a single pass for simplicity
  --        in reality we shouldn't have many bounds, so it should be fine.
  let
    minBound = foldl min (fromInt top) $ map (\(Bound lower _) -> lower) bounds

    maxBound = foldl max (fromInt bottom) $ map (\(Bound _ higher) -> higher)
      bounds
  in
    Bound minBound maxBound

emptyState :: State
emptyState =
  State
    { accounts: Map.empty
    , choices: Map.empty
    , boundValues: Map.empty
    , minTime: unixEpoch
    }

inBounds :: ChosenNum -> Array Bound -> Boolean
inBounds num = any (\(Bound l u) -> num >= l && num <= u)

boundFrom :: Bound -> BigInt
boundFrom (Bound from _) = from

boundTo :: Bound -> BigInt
boundTo (Bound _ to) = to

fixInterval :: TimeInterval -> State -> IntervalResult
fixInterval interval@(TimeInterval from to) (State state)
  | (not <<< validInterval) interval = IntervalError (InvalidInterval interval)
  | state.minTime `above` interval = IntervalError
      (IntervalInPastError state.minTime interval)
  | otherwise =
      let
        -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
        newLow = max from state.minTime

        -- We know high is greater or equal than newLow (prove)
        currentInterval = TimeInterval newLow to

        env = Environment { timeInterval: currentInterval }

        newState = State (state { minTime = newLow })
      in
        IntervalTrimmed env newState

-- EVALUATION
-- | Evaluate a @Value@ to Integer
evalValue :: Environment -> State -> Value -> BigInt
evalValue env state value =
  let
    eval = evalValue env state
  in
    case value of
      AvailableMoney accId token -> moneyInAccount accId token
        (unwrap state).accounts
      Constant integer -> integer
      NegValue val -> negate (eval val)
      AddValue lhs rhs -> eval lhs + eval rhs
      SubValue lhs rhs -> eval lhs - eval rhs
      MulValue lhs rhs -> eval lhs * eval rhs
      DivValue lhs rhs ->
        let
          n = eval lhs
          d = eval rhs
        in
          if d == fromInt 0 then
            fromInt 0
          else
            n `quot` d
      ChoiceValue choiceId -> fromMaybe zero $ Map.lookup choiceId
        (unwrap state).choices
      TimeIntervalStart ->
        BigInt.fromInt $ round $ unwrap $ unInstant $ ivFrom $
          (unwrap env).timeInterval
      TimeIntervalEnd ->
        BigInt.fromInt $ round $ unwrap $ unInstant $ ivTo $
          (unwrap env).timeInterval
      UseValue valId -> fromMaybe zero $ Map.lookup valId
        (unwrap state).boundValues
      Cond cond thn els ->
        if evalObservation env state cond then eval thn else eval els

-- | Evaluate an @Observation@ to Bool
evalObservation :: Environment -> State -> Observation -> Boolean
evalObservation env state obs =
  let
    evalObs = evalObservation env state

    evalVal = evalValue env state
  in
    case obs of
      AndObs lhs rhs -> evalObs lhs && evalObs rhs
      OrObs lhs rhs -> evalObs lhs || evalObs rhs
      NotObs subObs -> not (evalObs subObs)
      ChoseSomething choiceId -> choiceId `Map.member` (unwrap state).choices
      ValueGE lhs rhs -> evalVal lhs >= evalVal rhs
      ValueGT lhs rhs -> evalVal lhs > evalVal rhs
      ValueLT lhs rhs -> evalVal lhs < evalVal rhs
      ValueLE lhs rhs -> evalVal lhs <= evalVal rhs
      ValueEQ lhs rhs -> evalVal lhs == evalVal rhs
      TrueObs -> true
      FalseObs -> false

-- | Pick the first account with money in it
refundOne :: Accounts -> Maybe (Party /\ Token /\ BigInt /\ Accounts)
refundOne accounts = case Map.toUnfoldable accounts of
  Nil -> Nothing
  ((accId /\ token) /\ balance) : rest ->
    if balance > zero then
      Just (accId /\ token /\ balance /\ Map.fromFoldable rest)
    else
      refundOne (Map.fromFoldable rest)

-- | Obtains the amount of money available an account
moneyInAccount :: AccountId -> Token -> Accounts -> BigInt
moneyInAccount accId token accounts = fromMaybe zero
  (Map.lookup (Tuple accId token) accounts)

-- | Sets the amount of money available in an account
updateMoneyInAccount :: AccountId -> Token -> BigInt -> Accounts -> Accounts
updateMoneyInAccount accId token amount =
  if amount <= zero then Map.delete (Tuple accId token)
  else Map.insert (Tuple accId token) amount

{-| Add the given amount of money to an account (only if it is positive).
    Return the updated Map
-}
addMoneyToAccount :: AccountId -> Token -> BigInt -> Accounts -> Accounts
addMoneyToAccount accId token amount accounts =
  let
    balance = moneyInAccount accId token accounts

    newBalance = balance + amount
  in
    if amount <= zero then
      accounts
    else
      updateMoneyInAccount accId token newBalance accounts

{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
giveMoney
  :: AccountId
  -> Payee
  -> Token
  -> BigInt
  -> Accounts
  -> Tuple ReduceEffect Accounts
giveMoney accountId payee token amount accounts =
  let
    newAccounts = case payee of
      Party _ -> accounts
      Account accId -> addMoneyToAccount accId token amount accounts
  in
    Tuple (ReduceWithPayment (Payment accountId payee token amount))
      newAccounts

-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of
  Close -> case refundOne (unwrap state).accounts of
    Just (party /\ token /\ amount /\ newAccounts) ->
      let
        oldState = unwrap state

        newState = wrap (oldState { accounts = newAccounts })
      in
        Reduced ReduceNoWarning
          (ReduceWithPayment (Payment party (Party party) token amount))
          newState
          Close
    Nothing -> NotReduced
  Pay accId payee tok val cont ->
    let
      amountToPay = evalValue env state val
    in
      if amountToPay <= zero then
        let
          warning = ReduceNonPositivePay accId payee tok amountToPay
        in
          Reduced warning ReduceNoPayment state cont
      else
        let
          balance = moneyInAccount accId tok (unwrap state).accounts

          paidAmount = min balance amountToPay

          newBalance = balance - paidAmount

          newAccs = updateMoneyInAccount accId tok newBalance
            (unwrap state).accounts

          warning =
            if paidAmount < amountToPay then
              ReducePartialPay accId payee tok paidAmount amountToPay
            else
              ReduceNoWarning

          (Tuple payment finalAccs) = giveMoney accId payee tok paidAmount
            newAccs

          newState = wrap ((unwrap state) { accounts = finalAccs })
        in
          Reduced warning payment newState cont
  If obs cont1 cont2 ->
    let
      cont = if evalObservation env state obs then cont1 else cont2
    in
      Reduced ReduceNoWarning ReduceNoPayment state cont
  When _ timeout nextContract ->
    let
      startTime = view (_timeInterval <<< to ivFrom) env

      endTime = view (_timeInterval <<< to ivTo) env
    in
      if endTime < timeout then
        NotReduced
      else if timeout <= startTime then
        Reduced ReduceNoWarning ReduceNoPayment state nextContract
      else
        AmbiguousTimeIntervalReductionError
  Let valId val nextContract ->
    let
      evaluatedValue = evalValue env state val

      newState = over _boundValues (Map.insert valId evaluatedValue) state

      warn = case Map.lookup valId (unwrap state).boundValues of
        Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
        Nothing -> ReduceNoWarning
    in
      Reduced warn ReduceNoPayment newState nextContract
  Assert obs cont ->
    let
      warning =
        if evalObservation env state obs then
          ReduceNoWarning
        else
          ReduceAssertionFailed
    in
      Reduced warning ReduceNoPayment state cont

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent startEnv startState startContract =
  let
    reductionLoop
      :: Boolean
      -> Environment
      -> State
      -> Contract
      -> (List ReduceWarning)
      -> (List Payment)
      -> ReduceResult
    reductionLoop reduced env state contract warnings payments =
      case reduceContractStep env state contract of
        Reduced warning effect newState nextContract ->
          let
            newWarnings =
              if warning == ReduceNoWarning then warnings
              else warning : warnings

            newPayments = case effect of
              ReduceWithPayment payment -> payment : payments
              ReduceNoPayment -> payments
          in
            reductionLoop true env newState nextContract newWarnings newPayments
        AmbiguousTimeIntervalReductionError -> RRAmbiguousTimeIntervalError
        -- this is the last invocation of reductionLoop, so we can reverse lists
        NotReduced -> ContractQuiescent reduced (reverse warnings)
          (reverse payments)
          state
          contract
  in
    reductionLoop false startEnv startState startContract mempty mempty

-- | Result of applying an action to a contract.
data ApplyAction
  = AppliedAction ApplyWarning State
  | NotAppliedAction

derive instance genericApplyAction :: Generic ApplyAction _

derive instance eqApplyAction :: Eq ApplyAction

instance showApplyAction :: Show ApplyAction where
  show = genericShow

-- | Try to apply a single input content to a single action.
applyAction :: Environment -> State -> InputContent -> Action -> ApplyAction
applyAction
  env
  state
  (IDeposit accId1 party1 tok1 amount)
  (Deposit accId2 party2 tok2 val) =
  if
    accId1 == accId2 && party1 == party2 && tok1 == tok2 && amount == evalValue
      env
      state
      val then
    let
      warning =
        if amount > zero then ApplyNoWarning
        else ApplyNonPositiveDeposit party2 accId2 tok2 amount
      newAccounts = addMoneyToAccount accId1 tok1 amount (unwrap state).accounts
      newState = wrap ((unwrap state) { accounts = newAccounts })
    in
      AppliedAction warning newState
  else NotAppliedAction
applyAction _ state (IChoice choId1 choice) (Choice choId2 bounds) =
  if choId1 == choId2 && inBounds choice bounds then
    let
      newState = over _choices (Map.insert choId1 choice) state
    in
      AppliedAction ApplyNoWarning newState
  else NotAppliedAction
applyAction env state INotify (Notify obs)
  | evalObservation env state obs = AppliedAction ApplyNoWarning state
applyAction _ _ _ _ = NotAppliedAction

-- | Try to get a continuation from a pair of Input and Case.
getContinuation :: Input -> Case -> Maybe Contract
getContinuation (NormalInput _) (Case _ continuation) = Just continuation
getContinuation
  (MerkleizedInput _ inputContinuationHash continuation)
  (MerkleizedCase _ continuationHash) =
  if inputContinuationHash == continuationHash then Just continuation
  else Nothing
getContinuation _ _ = Nothing

-- | Try to apply an input to a list of cases.
applyCases :: Environment -> State -> Input -> List Case -> ApplyResult
applyCases env state input (headCase : tailCase) =
  let
    inputContent = getInputContent input :: InputContent
    action = getAction headCase :: Action
    maybeContinuation = getContinuation input headCase :: Maybe Contract
  in
    case applyAction env state inputContent action of
      AppliedAction warning newState ->
        case maybeContinuation of
          Just continuation -> Applied warning newState continuation
          Nothing -> ApplyHashMismatch
      NotAppliedAction -> applyCases env state input tailCase
applyCases _ _ _ Nil = ApplyNoMatchError

applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input
  (fromFoldable cases)

applyInput _ _ _ _ = ApplyNoMatchError

convertReduceWarnings :: List ReduceWarning -> List TransactionWarning
convertReduceWarnings Nil = Nil

convertReduceWarnings (first : rest) =
  ( case first of
      ReduceNoWarning -> Nil
      ReduceNonPositivePay accId payee tok amount ->
        (TransactionNonPositivePay accId payee tok amount) : Nil
      ReducePartialPay accId payee tok paid expected ->
        (TransactionPartialPay accId payee tok paid expected) : Nil
      ReduceShadowing valId oldVal newVal ->
        (TransactionShadowing valId oldVal newVal) : Nil
      ReduceAssertionFailed -> TransactionAssertionFailed : Nil
  )
    <> convertReduceWarnings rest

convertApplyWarning :: ApplyWarning -> List TransactionWarning
convertApplyWarning warn = case warn of
  ApplyNoWarning -> Nil
  ApplyNonPositiveDeposit party accId tok amount ->
    (TransactionNonPositiveDeposit party accId tok amount) : Nil

-- | Apply a list of Inputs to the contract
applyAllInputs
  :: Environment -> State -> Contract -> (List Input) -> ApplyAllResult
applyAllInputs startEnv startState startContract startInputs =
  let
    applyAllLoop
      :: Boolean
      -> Environment
      -> State
      -> Contract
      -> List Input
      -> List TransactionWarning
      -> List Payment
      -> ApplyAllResult
    applyAllLoop contractChanged env state contract inputs warnings payments =
      case reduceContractUntilQuiescent env state contract of
        RRAmbiguousTimeIntervalError -> ApplyAllAmbiguousTimeIntervalError
        ContractQuiescent reduced reduceWarns pays curState cont ->
          case inputs of
            Nil ->
              ApplyAllSuccess (contractChanged || reduced)
                (warnings <> (convertReduceWarnings reduceWarns))
                (payments <> pays)
                curState
                cont
            (input : rest) -> case applyInput env curState input cont of
              Applied applyWarn newState nextContract ->
                applyAllLoop true env newState nextContract rest
                  ( warnings <> (convertReduceWarnings reduceWarns)
                      <> (convertApplyWarning applyWarn)
                  )
                  (payments <> pays)
              ApplyNoMatchError -> ApplyAllNoMatchError
              ApplyHashMismatch -> ApplyAllHashMismatch
  in
    applyAllLoop false startEnv startState startContract startInputs mempty
      mempty

isClose :: Contract -> Boolean
isClose Close = true

isClose _ = false

-- | Try to compute outputs of a transaction give its input
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract =
  let
    inputs = (unwrap tx).inputs
  in
    case fixInterval (unwrap tx).interval state of
      IntervalTrimmed env fixState ->
        case applyAllInputs env fixState contract inputs of
          ApplyAllSuccess reduced warnings payments newState cont ->
            if
              not reduced &&
                ( not (isClose contract) ||
                    (Map.isEmpty $ (unwrap state).accounts)
                ) then
              Error TEUselessTransaction
            else
              TransactionOutput
                { txOutWarnings: warnings
                , txOutPayments: payments
                , txOutState: newState
                , txOutContract: cont
                }
          ApplyAllNoMatchError -> Error TEApplyNoMatchError
          ApplyAllAmbiguousTimeIntervalError -> Error
            TEAmbiguousTimeIntervalError
          ApplyAllHashMismatch -> Error TEHashMismatch
      IntervalError error -> Error (TEIntervalError error)

playTrace :: Timeout -> Contract -> List TransactionInput -> TransactionOutput
playTrace initialTime contract inputs =
  go
    { txOutWarnings: Nil
    , txOutPayments: Nil
    , txOutState: set _minTime initialTime emptyState
    , txOutContract: contract
    }
    inputs
  where
  go res Nil = TransactionOutput res
  go res (h : t) =
    let
      transRes = computeTransaction h res.txOutState res.txOutContract
    in
      case transRes of
        TransactionOutput res' -> go
          { txOutWarnings: res.txOutWarnings <> res'.txOutWarnings
          , txOutPayments: res.txOutPayments <> res'.txOutPayments
          , txOutState: res'.txOutState
          , txOutContract: res'.txOutContract
          }
          t
        Error _ -> transRes

moneyInContract :: State -> Money
moneyInContract state =
  foldMapWithIndex
    (\(Tuple _ (Token cur tok)) balance -> asset cur tok balance)
    (unwrap state).accounts

