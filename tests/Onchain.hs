{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Onchain where

import Prelude (IO, print)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, TxInfo)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, print)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value (AssetClass(..), assetClassValueOf)

------------------------------------------------------------------------------------------
-- | Parameter Type
------------------------------------------------------------------------------------------

data ProgressParam = ProgressParam
    { ppOwner      :: PubKeyHash
    , ppToken      :: Maybe AssetClass
    , ppMilestones :: [BuiltinByteString]
    }
    deriving (Generic)

PlutusTx.makeIsDataIndexed ''ProgressParam [('ProgressParam, 0)]
PlutusTx.makeLift ''ProgressParam

------------------------------------------------------------------------------------------
-- | Datum and Redeemer
------------------------------------------------------------------------------------------

data ProgressDatum = ProgressDatum
    { pdOwner     :: PubKeyHash
    , pdMilestone :: BuiltinByteString
    }
    deriving (Generic)

PlutusTx.makeIsDataIndexed ''ProgressDatum [('ProgressDatum, 0)]

data ProgressRedeemer = ClaimProgress
    deriving (Generic)

PlutusTx.makeIsDataIndexed ''ProgressRedeemer [('ClaimProgress, 0)]

------------------------------------------------------------------------------------------
-- | Validator Logic
------------------------------------------------------------------------------------------

{-# INLINABLE mkProgressValidator #-}
mkProgressValidator :: ProgressParam -> ProgressDatum -> ProgressRedeemer -> ScriptContext -> Bool
mkProgressValidator param datum _ ctx =
    traceIfFalse "Owner signature missing" signedByOwner &&
    traceIfFalse "Milestone not allowed" validMilestone &&
    traceIfFalse "Token not paid to owner" tokenOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info (ppOwner param)

    validMilestone :: Bool
    validMilestone = pdMilestone datum `elem` ppMilestones param

    tokenOk :: Bool
    tokenOk = case ppToken param of
        Nothing   -> True
        Just tkn  -> assetClassValueOf (valuePaidTo info (pdOwner datum)) tkn > 0

------------------------------------------------------------------------------------------
-- | Helper: wrap typed validator to untyped
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: ProgressParam -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator param d r ctx =
    check (mkProgressValidator param
           (unsafeFromBuiltinData d)
           (unsafeFromBuiltinData r)
           (unsafeFromBuiltinData ctx))

------------------------------------------------------------------------------------------
-- | Compile to Validator
------------------------------------------------------------------------------------------

validator :: ProgressParam -> Validator
validator param =
    mkValidatorScript $
      $$(PlutusTx.compile [|| \p -> mkUntypedValidator p ||])
      `PlutusTx.applyCode` PlutusTx.liftCode param

------------------------------------------------------------------------------------------
-- | Example parameter
------------------------------------------------------------------------------------------

exampleParam :: ProgressParam
exampleParam = ProgressParam
    { ppOwner = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
    , ppToken = Nothing
    , ppMilestones = ["HC10", "HC11", "HC12"]
    }

------------------------------------------------------------------------------------------
-- | Main
------------------------------------------------------------------------------------------

main :: IO ()
main = print ("✅ Parameterized Onchain Progress Credential Validator compiled successfully!" :: BuiltinString)