{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------
-- DATUM & REDEEMER
------------------------------------------------------------

data BatcherDatum = BatcherDatum
    { bdEpoch      :: Integer        -- batch epoch
    , bdRulesHash  :: BuiltinByteString -- hash of canonical rules
    , bdIntentsRoot:: BuiltinByteString -- Merkle root of user intents
    , bdSettled    :: Bool
    }
PlutusTx.unstableMakeIsData ''BatcherDatum

data BatcherRedeemer = Batch { clearingData :: BuiltinByteString }
PlutusTx.unstableMakeIsData ''BatcherRedeemer

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE verifyEpoch #-}
verifyEpoch :: BatcherDatum -> Integer -> Bool
verifyEpoch dat currentEpoch = bdEpoch dat == currentEpoch

{-# INLINABLE verifyClearingData #-}
verifyClearingData :: BatcherDatum -> BuiltinByteString -> Bool
verifyClearingData dat clearing =
    -- In a real implementation, this would check the clearingData
    -- deterministically matches intentsRoot & rulesHash
    clearing == bdIntentsRoot dat -- simple placeholder
     
{-# INLINABLE notSettled #-}
notSettled :: BatcherDatum -> Bool
notSettled dat = not (bdSettled dat)

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: BatcherDatum -> BatcherRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    case red of
        Batch clearing ->
            traceIfFalse "batch already settled" (notSettled dat) &&
            traceIfFalse "clearing invalid" (verifyClearingData dat clearing) &&
            traceIfFalse "signed by keeper" (txSignedBy info keeper)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    keeper :: PubKeyHash
    keeper = case txInfoSignatories info of
               [k] -> k
               _   -> traceError "expect exactly one keeper signature"

------------------------------------------------------------
-- UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @BatcherDatum d
        red = unsafeFromBuiltinData @BatcherRedeemer r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkUntyped ||])

------------------------------------------------------------
-- HASH + ADDRESS
------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        bs       = SBS.fromShort short
        builtin  = Builtins.toBuiltin bs
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript CS.PlutusScriptV2 plutusScript)

        addr :: CS.AddressInEra CS.BabbageEra
        addr = C.makeShelleyAddressInEra
                 network
                 (C.PaymentCredentialByScript scriptHash)
                 C.NoStakeAddress
    in T.unpack (C.serialiseAddress addr)

------------------------------------------------------------
-- FILE WRITING + MAIN
------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path v = do
    LBS.writeFile path (Serialise.serialise v)
    putStrLn $ "Validator written: " <> path

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "mevBatcher.plutus" validator

    let vh     = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- MEV Batcher Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"
