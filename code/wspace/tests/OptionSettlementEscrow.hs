{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module OptionSettlementEscrow where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------------

data SeriesParams = SeriesParams
    { spStrike      :: Integer
    , spExpiry      :: POSIXTime
    , spGrace       :: POSIXTime
    , spUnderlier   :: Integer
    , spOptionToken :: Integer
    }
PlutusTx.unstableMakeIsData ''SeriesParams

data EscrowDatum = EscrowDatum
    { edHolder :: PubKeyHash
    , edQty    :: Integer
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data Action = Exercise | Refund
PlutusTx.unstableMakeIsData ''Action

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

{-# INLINABLE signedByHolder #-}
signedByHolder :: TxInfo -> PubKeyHash -> Bool
signedByHolder info holder = txSignedBy info holder

{-# INLINABLE withinTime #-}
withinTime :: POSIXTime -> POSIXTime -> POSIXTime -> Bool
withinTime start end now = Interval.contains (Interval.interval start end) (Interval.singleton now)

{-# INLINABLE afterGracePeriod #-}
afterGracePeriod :: POSIXTime -> POSIXTime -> Bool
afterGracePeriod now expiry = now > expiry

{-# INLINABLE intrinsicPayoff #-}
intrinsicPayoff :: Integer -> Integer -> Integer
intrinsicPayoff underlier strike =
    let diff = underlier - strike
    in if diff > 0 then diff else 0

{-# INLINABLE totalPayoff #-}
totalPayoff :: SeriesParams -> EscrowDatum -> Integer
totalPayoff sp esc = intrinsicPayoff (spUnderlier sp) (spStrike sp) * edQty esc

------------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: SeriesParams -> EscrowDatum -> Action -> ScriptContext -> Bool
mkValidator sp esc action ctx =
    let info = scriptContextTxInfo ctx
        now  = case ivTo (txInfoValidRange info) of
                 UpperBound (Finite t) _ -> t
                 _                        -> traceError "invalid time range"
        holder = edHolder esc
    in case action of
        Exercise ->
            traceIfFalse "holder signature missing" (signedByHolder info holder) &&
            traceIfFalse "outside exercise window" (withinTime (spExpiry sp) (spExpiry sp + spGrace sp) now)
        Refund ->
            traceIfFalse "holder signature missing" (signedByHolder info holder) &&
            traceIfFalse "refund only after grace" (afterGracePeriod now (spExpiry sp))

------------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EscrowDatum d
        red = unsafeFromBuiltinData @Action r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator exampleSeries dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------------
-- On-chain hash and addresses
------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------------
-- Off-chain Bech32
------------------------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------------
-- Example Series
------------------------------------------------------------------------------

exampleSeries :: SeriesParams
exampleSeries = SeriesParams
    { spStrike = 100
    , spExpiry = 1000
    , spGrace = 10
    , spUnderlier = 120
    , spOptionToken = 1
    }

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "optionEscrow.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Option Settlement Escrow Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Option settlement escrow validator generated successfully."
