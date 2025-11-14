{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Will where

import Prelude (IO, print, ($))
import Plutus.V2.Ledger.Api (Datum(..), ScriptContext(..), TxInfo(..), PubKeyHash, Validator, POSIXTime, BuiltinData, mkValidatorScript)
import Plutus.V2.Ledger.Contexts (txSignedBy, scriptContextTxInfo)
import Plutus.V1.Ledger.Interval (from, contains)
import PlutusTx (compile, unstableMakeIsData)
import qualified PlutusTx
import PlutusTx.Prelude (Bool(..), traceIfFalse, (&&), (||), error, any, not, elem)

------------------------------------------------------------
-- ON-CHAIN TYPES
------------------------------------------------------------

data WillDatum = WillDatum
    { beneficiaries :: [PubKeyHash]
    , unlockTime    :: POSIXTime
    , claimed       :: Bool
    , partialClaim  :: Bool   -- New: whether partially claimed
    }
PlutusTx.unstableMakeIsData ''WillDatum

newtype WillParam = WillParam { owner :: PubKeyHash }
PlutusTx.unstableMakeIsData ''WillParam
PlutusTx.makeLift ''WillParam

------------------------------------------------------------
-- HELPER FUNCTIONS
------------------------------------------------------------

{-# INLINABLE allSignedBy #-}
allSignedBy :: TxInfo -> [PubKeyHash] -> Bool
allSignedBy info pkhs = all (\p -> txSignedBy info p) pkhs
  where all f [] = True
        all f (x:xs) = f x && all f xs

{-# INLINABLE anySignedBy #-}
anySignedBy :: TxInfo -> [PubKeyHash] -> Bool
anySignedBy info pkhs = any (txSignedBy info) pkhs

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkWillValidator #-}
mkWillValidator :: WillParam -> WillDatum -> () -> ScriptContext -> Bool
mkWillValidator (WillParam ownerPkh) datum _ ctx =
    traceIfFalse "Owner signature missing" signedByOwner &&
    traceIfFalse "No beneficiary signed" signedByBeneficiary &&
    traceIfFalse "Already claimed" notClaimed &&
    traceIfFalse "Too early to unlock funds" deadlineReached &&
    traceIfFalse "Partial claim already done" notPartialClaim
  where
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info ownerPkh

    signedByBeneficiary :: Bool
    signedByBeneficiary = anySignedBy info (beneficiaries datum)

    notClaimed :: Bool
    notClaimed = not (claimed datum)

    notPartialClaim :: Bool
    notPartialClaim = not (partialClaim datum)

    deadlineReached :: Bool
    deadlineReached = contains (from $ unlockTime datum) (txInfoValidRange info)

------------------------------------------------------------
-- WRAPPER
------------------------------------------------------------

{-# INLINABLE wrappedValidator #-}
wrappedValidator :: WillParam -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator p datum _ ctx =
    check $ mkWillValidator p d () c
  where
    d = PlutusTx.unsafeFromBuiltinData datum
    c = PlutusTx.unsafeFromBuiltinData ctx
    check True  = ()
    check False = error ()

------------------------------------------------------------
-- COMPILED PARAMETERISED VALIDATOR GENERATOR
------------------------------------------------------------

validatorCode :: PlutusTx.CompiledCode (WillParam -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [|| wrappedValidator ||])

mkWillInstance :: WillParam -> Validator
mkWillInstance param = mkValidatorScript (validatorCode `PlutusTx.applyCode` PlutusTx.liftCode param)

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = print "✅ Extended Parameterized Will validator compiled successfully!"