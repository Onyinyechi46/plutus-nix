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

data AuctionDatum = AuctionDatum
    { adSeller     :: PubKeyHash
    , adCurrency   :: CurrencySymbol
    , adToken      :: TokenName
    , adMinBid     :: Integer
    , adDeadline   :: POSIXTime
    , adTopBid     :: Integer
    , adTopBidder  :: Maybe PubKeyHash
    , adBidBondBps :: Integer  -- bond in basis points
    }
PlutusTx.unstableMakeIsData ''AuctionDatum

data AuctionRedeemer = Bid Integer PubKeyHash | Settle | Cancel
PlutusTx.unstableMakeIsData ''AuctionRedeemer

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE unPOSIXTime #-}
unPOSIXTime :: POSIXTime -> Integer
unPOSIXTime (POSIXTime n) = n

{-# INLINABLE scriptInputContainsAsset #-}
scriptInputContainsAsset :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsAsset ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no script input"
        Just i  ->
            let v = txOutValue (txInInfoResolved i)
            in valueOf v cs tn >= 1

{-# INLINABLE auctionEnded #-}
auctionEnded :: AuctionDatum -> POSIXTime -> Bool
auctionEnded dat now = unPOSIXTime now >= unPOSIXTime (adDeadline dat)

{-# INLINABLE bidValid #-}
bidValid :: AuctionDatum -> Integer -> Bool
bidValid dat newBid =
    newBid > adTopBid dat + max 1 (adMinBid dat)  -- enforce min increment

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
mkValidator dat redeemer ctx =
    case redeemer of

      -- NEW BID
      Bid amt bidder ->
           traceIfFalse "auction ended" (not $ auctionEnded dat now) &&
           traceIfFalse "bid too low" (bidValid dat amt) &&
           traceIfFalse "asset missing" (scriptInputContainsAsset ctx cs tn)

      -- SETTLE AUCTION
      Settle ->
           traceIfFalse "auction not ended" (auctionEnded dat now) &&
           traceIfFalse "not seller" (txSignedBy info (adSeller dat)) &&
           traceIfFalse "no bids" (adTopBid dat > 0)

      -- CANCEL AUCTION
      Cancel ->
           traceIfFalse "auction not ended" (auctionEnded dat now) &&
           traceIfFalse "not seller" (txSignedBy info (adSeller dat)) &&
           traceIfFalse "bids exist" (adTopBid dat == 0)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs = adCurrency dat
    tn = adToken dat

    now :: POSIXTime
    now =
        case ivFrom (txInfoValidRange info) of
            LowerBound (Finite t) _ -> t
            _ -> traceError "invalid time range"

------------------------------------------------------------
-- UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @AuctionDatum d
        red = unsafeFromBuiltinData @AuctionRedeemer r
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
    writeValidator "englishAuction.plutus" validator

    let vh     = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- English Auction Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"
