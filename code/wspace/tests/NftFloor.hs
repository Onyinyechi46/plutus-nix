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

data LoanDatum = LoanDatum
    { ldNFT        :: (CurrencySymbol, TokenName) -- NFT collateral
    , ldBorrower   :: PubKeyHash
    , ldPrincipal  :: Integer    -- in lovelace
    , ldRate       :: Integer    -- interest rate in bps
    , ldLTV        :: Integer    -- Loan-to-value in % (e.g., 50)
    , ldLastAccrual:: POSIXTime
    }
PlutusTx.unstableMakeIsData ''LoanDatum

data LoanAction = Borrow | Repay | Liquidate { oracleFloor :: Integer }
PlutusTx.unstableMakeIsData ''LoanAction

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE nftLocked #-}
nftLocked :: ScriptContext -> (CurrencySymbol, TokenName) -> Bool
nftLocked ctx (cs, tn) =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script"
        Just i  -> valueOf (txOutValue $ txInInfoResolved i) cs tn >= 1

{-# INLINABLE loanHealth #-}
loanHealth :: LoanDatum -> Integer -> Bool
loanHealth dat floorPrice =
    let maxBorrow = floorPrice * 100 `divide` ldLTV dat
    in ldPrincipal dat <= maxBorrow

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LoanDatum -> LoanAction -> ScriptContext -> Bool
mkValidator dat act ctx =
    case act of
        Borrow ->
            traceIfFalse "borrower signature missing" (signedBy (ldBorrower dat) ctx) &&
            traceIfFalse "NFT not locked" (nftLocked ctx (ldNFT dat))
        Repay ->
            traceIfFalse "borrower signature missing" (signedBy (ldBorrower dat) ctx)
        Liquidate { oracleFloor = floorPrice } ->
            traceIfFalse "loan still healthy" (not $ loanHealth dat floorPrice) &&
            traceIfFalse "liquidator must sign" (signedBy keeper ctx)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    keeper :: PubKeyHash
    keeper = case txInfoSignatories info of
               [k] -> k
               _   -> traceError "require exactly one liquidator"

------------------------------------------------------------
-- UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @LoanDatum d
        red = unsafeFromBuiltinData @LoanAction r
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
    writeValidator "nftLending.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- NFT Lending Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"
