module Language.Marlowe.Core.Semantics.Gen where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen
  ( class MonadGen
  , chooseFloat
  , chooseInt
  , resize
  , suchThat
  , unfoldable
  )
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Char (fromCharCode)
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foldable (class Foldable)
import Data.Int (rem)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semigroup.Foldable (foldl1)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(..)
  , Address
  , Bound(..)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , CurrencySymbol
  , Input(..)
  , Observation(..)
  , Party(..)
  , Payee(..)
  , Rational(..)
  , TimeInterval(..)
  , Timeout
  , Token(..)
  , TokenName
  , TransactionInput(..)
  , TransactionWarning(..)
  , Value(..)
  , ValueId(..)
  )
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1 as EM
import Partial.Unsafe (unsafePartial)

oneOf
  :: forall m a f
   . Foldable f
  => MonadGen m
  => NonEmpty f (m a)
  -> m a
oneOf = foldl1 Gen.choose

genBigInt :: forall m. MonadGen m => MonadRec m => m BigInt
genBigInt = BigInt.fromInt <$> chooseInt bottom top

genRational :: forall m. MonadGen m => MonadRec m => m Rational
genRational = do
  n <- genBigInt
  d <- genBigInt
  pure $
    if d > zero then
      Rational n d
    else
      Rational (-n) (-d)

genInstant :: forall m. MonadGen m => MonadRec m => m Instant
genInstant = do
  n <- chooseFloat (unwrap (unInstant bottom)) (unwrap (unInstant top))
  pure $ unsafePartial $ fromJust $ instant $ Milliseconds n

genTimeout
  :: forall m
   . MonadGen m
  => MonadRec m
  => m Timeout
genTimeout = genInstant

genValueId
  :: forall m
   . MonadGen m
  => MonadRec m
  => m ValueId
genValueId = ValueId <$> genString

genHexit :: forall m. MonadGen m => m Char
genHexit = oneOf $ lowerAlphaHexDigit :| upperAlphaHexDigit :| [ genDigitChar ]
  where
  lowerAlphaHexDigit = fromMaybe 'a' <$> (fromCharCode <$> chooseInt 97 102)

  upperAlphaHexDigit = fromMaybe 'A' <$> (fromCharCode <$> chooseInt 65 70)

genBase16 :: forall m. MonadGen m => MonadRec m => m String
genBase16 = fromCharArray <$> resize (\s -> s - (s `rem` 2))
  (unfoldable genHexit)

genAlphaNum :: forall m. MonadGen m => MonadRec m => m Char
genAlphaNum = oneOf $ genAlpha :| [ genDigitChar ]

genString :: forall m. MonadGen m => MonadRec m => m String
genString = fromCharArray <$> resize (_ - 1) (unfoldable genAlphaNum)

genPubKey :: forall m. MonadGen m => MonadRec m => m Address
genPubKey = genBase16

genTokenName :: forall m. MonadGen m => MonadRec m => m TokenName
genTokenName = genString

genParty
  :: forall m
   . MonadGen m
  => MonadRec m
  => m Party
genParty = oneOf $ addr :| [ role ]
  where
  addr = Address <$> genPubKey

  role = Role <$> genTokenName

genCurrencySymbol :: forall m. MonadGen m => MonadRec m => m CurrencySymbol
genCurrencySymbol = genBase16

genTimeInterval
  :: forall m. MonadGen m => MonadRec m => m Instant -> m TimeInterval
genTimeInterval gen = do
  from <- gen
  to <- suchThat gen (\v -> v > from)
  pure $ TimeInterval from to

genBound :: forall m. MonadGen m => MonadRec m => m Bound
genBound = do
  from <- genBigInt
  to <- suchThat genBigInt (\v -> v > from)
  pure $ Bound from to

genToken
  :: forall m
   . MonadGen m
  => MonadRec m
  => m Token
genToken = oneOf $ (pure $ Token "" "") :|
  [ Token <$> genCurrencySymbol <*> genTokenName ]

genChoiceId
  :: forall m
   . MonadGen m
  => MonadRec m
  => m ChoiceId
genChoiceId = do
  choiceName <- genString
  choiceOwner <- genParty
  pure $ ChoiceId choiceName choiceOwner

genPayee
  :: forall m
   . MonadGen m
  => MonadRec m
  => m Payee
genPayee = oneOf $ (Account <$> genParty) :|
  [ Party <$> genParty ]

genAction
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Observation)
  => Lazy (m Value)
  => Int
  -> m Action
genAction size =
  oneOf
    $
      ( Deposit <$> genParty <*> genParty
          <*> genToken
          <*> genValue' size
      )
        :|
          [ Choice <$> genChoiceId <*> resize (_ - 1)
              (unfoldable genBound)
          , Notify <$> genObservation' size
          ]

genCase
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Value)
  => Lazy (m Observation)
  => Lazy (m Contract)
  => Int
  -> m Case
genCase size = do
  let
    newSize = size - 1
  action <- genAction newSize
  contract <- genContract' newSize
  pure (Case action contract)

genCases
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Value)
  => Lazy (m Observation)
  => Lazy (m Contract)
  => Int
  -> m (Array Case)
genCases size = resize (_ - 1)
  (unfoldable (genCase size))

genValue
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Value)
  => Lazy (m Observation)
  => m Value
genValue = genValue' 5

genValue'
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Value)
  => Lazy (m Observation)
  => Int
  -> m Value
genValue' size
  | size > 1 =
      defer \_ -> do
        let
          newSize = (size - 1)

          genNewValue = genValue' newSize

          genNewValueIndexed _ = genValue' newSize

        oneOf $ pure TimeIntervalStart
          :|
            ( [ pure TimeIntervalEnd
              , AvailableMoney <$> genParty <*> genToken
              , Constant <$> genBigInt
              , NegValue <$> genNewValue
              , AddValue <$> genNewValueIndexed 1 <*> genNewValueIndexed 2
              , SubValue <$> genNewValueIndexed 1 <*> genNewValueIndexed 2
              , MulValue <$> genNewValueIndexed 1 <*> genNewValueIndexed 2
              , DivValue <$> genNewValueIndexed 1 <*> genNewValueIndexed 2
              , ChoiceValue <$> genChoiceId
              , UseValue <$> genValueId
              , Cond <$> genObservation' newSize
                  <*> genValue' newSize
                  <*> genValue' newSize
              ]
            )
  | otherwise =
      oneOf $ pure TimeIntervalStart
        :|
          [ pure TimeIntervalEnd
          , AvailableMoney <$> genParty <*> genToken
          , Constant <$> genBigInt
          , UseValue <$> genValueId
          ]

genObservation
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Observation)
  => Lazy (m Value)
  => m Observation
genObservation = genObservation' 5

genObservation'
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Observation)
  => Lazy (m Value)
  => Int
  -> m Observation
genObservation' size
  | size > 1 =
      defer \_ ->
        let
          newSize = (size - 1)

          genNewValue = genValue' newSize

          genNewObservationIndexed = genObservation' newSize

          genNewObservation = genObservation' newSize
        in
          oneOf
            $
              ( AndObs <$> genNewObservationIndexed <*>
                  genNewObservationIndexed
              )
                :|
                  [ OrObs <$> genNewObservationIndexed <*>
                      genNewObservationIndexed
                  , NotObs <$> genNewObservation
                  , ChoseSomething <$> genChoiceId
                  , ValueGE <$> genNewValue <*> genNewValue
                  , ValueGT <$> genNewValue <*> genNewValue
                  , ValueLT <$> genNewValue <*> genNewValue
                  , ValueLE <$> genNewValue <*> genNewValue
                  , ValueEQ <$> genNewValue <*> genNewValue
                  ]
  | otherwise = genLeaf
      where
      genLeaf
        :: m Observation
      genLeaf = ChoseSomething <$> genChoiceId

genContract
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Contract)
  => Lazy (m Observation)
  => Lazy (m Value)
  => m Contract
genContract = genContract' 3

genContract'
  :: forall m
   . MonadGen m
  => MonadRec m
  => Lazy (m Contract)
  => Lazy (m Observation)
  => Lazy (m Value)
  => Int
  -> m Contract
genContract' size
  | size > 1 =
      defer \_ ->
        let
          newSize = (size - 1)

          genNewValue = genValue' newSize

          genNewObservation = genObservation' newSize

          genNewContractIndexed _ = genContract' newSize

          genNewContract = genContract' newSize

          genNewTimeout = genTimeout
        in
          oneOf $ pure Close
            :|
              [ Pay <$> genParty
                  <*> genPayee
                  <*> genToken
                  <*> genNewValue
                  <*> genNewContract
              , If <$> genNewObservation <*> genNewContractIndexed 1 <*>
                  genNewContractIndexed 2
              , When <$> genCases newSize <*> genNewTimeout <*> genNewContract
              , Let <$> genValueId <*> genNewValue <*>
                  genNewContract
              , Assert <$> genNewObservation <*> genNewContract
              ]
  | otherwise = genLeaf
      where
      genLeaf
        :: m Contract
      genLeaf = pure Close

----------------------------------------------------------------- Semantics Generators ---------------------------------
genTokenNameValue :: forall m. MonadGen m => MonadRec m => m S.TokenName
genTokenNameValue = genString

genCurrencySymbolValue
  :: forall m. MonadGen m => MonadRec m => m S.CurrencySymbol
genCurrencySymbolValue = genBase16

genTokenValue :: forall m. MonadGen m => MonadRec m => m S.Token
genTokenValue = do
  currencySymbol <- genCurrencySymbolValue
  tokenName <- genTokenName
  pure $ S.Token currencySymbol tokenName

genPartyValue :: forall m. MonadGen m => MonadRec m => m S.Party
genPartyValue = oneOf $ addr :| [ role ]
  where
  addr = S.Address <$> genPubKey

  role = S.Role <$> genTokenNameValue

genPayeeValueCore :: forall m. MonadGen m => MonadRec m => m S.Payee
genPayeeValueCore = oneOf $ (S.Account <$> genPartyValue) :|
  [ S.Party <$> genPartyValue ]

genPayeeValueExtended :: forall m. MonadGen m => MonadRec m => m EM.Payee
genPayeeValueExtended = oneOf $ (EM.Account <$> genPartyValue) :|
  [ EM.Party <$> genPartyValue ]

genValueIdValue :: forall m. MonadGen m => MonadRec m => m S.ValueId
genValueIdValue = S.ValueId <$> genString

genChoiceIdValue :: forall m. MonadGen m => MonadRec m => m S.ChoiceId
genChoiceIdValue = do
  choiceName <- genString
  choiceOwner <- genPartyValue
  pure $ S.ChoiceId choiceName choiceOwner

genInput
  :: forall m
   . MonadGen m
  => MonadRec m
  => m S.Input
genInput =
  oneOf
    $
      ( IDeposit <$> genPartyValue <*> genPartyValue <*> genTokenValue <*>
          genBigInt
      )
        :|
          [ IChoice <$> genChoiceIdValue <*> genBigInt
          , pure INotify
          ]

genTransactionInput
  :: forall m
   . MonadGen m
  => MonadRec m
  => m S.TransactionInput
genTransactionInput = do
  interval <- genTimeInterval genInstant
  inputs <- unfoldable genInput
  pure $ TransactionInput { interval, inputs }

genTransactionWarning
  :: forall m
   . MonadGen m
  => MonadRec m
  => m TransactionWarning
genTransactionWarning =
  oneOf
    $
      ( TransactionNonPositiveDeposit <$> genPartyValue <*> genPartyValue
          <*> genTokenValue
          <*> genBigInt
      )
        :|
          [ TransactionNonPositivePay <$> genPartyValue <*> genPayeeValueCore
              <*> genTokenValue
              <*> genBigInt
          , TransactionPartialPay <$> genPartyValue <*> genPayeeValueCore
              <*> genTokenValue
              <*> genBigInt
              <*> genBigInt
          , TransactionShadowing <$> genValueIdValue <*> genBigInt <*> genBigInt
          ]
