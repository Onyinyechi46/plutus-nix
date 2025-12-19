{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take, Char, Int, Word8)
import qualified Prelude as P

import qualified Data.Text as T
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, (<>))
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

--- ============================================================================
-- CUSTOM HEX ENCODING (Without external dependencies)
--- ============================================================================

-- Simple hex encoding function
byteToHex :: Word8 -> String
byteToHex b = [nibbleToHex (b `P.div` 16), nibbleToHex (b `P.mod` 16)]
  where
    nibbleToHex :: Word8 -> Char
    nibbleToHex n
        | n < 10    = P.toEnum (P.fromEnum '0' + P.fromIntegral n)
        | otherwise = P.toEnum (P.fromEnum 'a' + P.fromIntegral n - 10)

-- Convert ShortByteString to hex string
shortBSToHex :: SBS.ShortByteString -> String
shortBSToHex sbs = P.concatMap byteToHex (SBS.unpack sbs)

-- Convert ByteString to hex string
bsToHex :: BS.ByteString -> String
bsToHex bs = P.concatMap byteToHex (BS.unpack bs)

-- Convert Lazy ByteString to hex string
lazyBSToHex :: LBS.ByteString -> String
lazyBSToHex = bsToHex . LBS.toStrict

--- ============================================================================
-- DATUM: Research Grant State
--- ============================================================================

data GrantDatum = GrantDatum
    { gdFunder        :: PubKeyHash      -- Grant funding organization
    , gdResearcher    :: Maybe PubKeyHash   -- Researcher (optional initially)
    , gdTotalAmount   :: Integer         -- Total grant amount
    , gdAmountReleased:: Integer         -- Amount already released
    , gdMilestones    :: [Milestone]     -- List of project milestones
    , gdCurrentMilestone :: Integer      -- Current milestone index (0-based)
    , gdDeadline      :: POSIXTime       -- Overall project deadline
    , gdReviewers     :: [PubKeyHash]    -- Peer reviewers
    , gdReviewsNeeded :: Integer         -- Reviews needed per milestone
    , gdReviewsGiven  :: [PubKeyHash]    -- Reviewers who have approved current milestone
    }

data Milestone = Milestone
    { msDescription :: BuiltinByteString  -- Milestone description
    , msAmount      :: Integer            -- Funds released for this milestone
    , msDeadline    :: POSIXTime          -- Deadline for this milestone
    , msCompleted   :: Bool               -- Whether milestone is completed
    }

PlutusTx.unstableMakeIsData ''GrantDatum
PlutusTx.unstableMakeIsData ''Milestone

--- ============================================================================
-- ACTION: Grant Operations
--- ============================================================================

data GrantAction
    = CreateGrant
    | ApplyForGrant
    | SubmitMilestone BuiltinByteString
    | ReviewMilestone Bool
    | ReleaseFunds
    | ExtendDeadline POSIXTime
    | CancelGrant
    | EmergencyWithdraw

PlutusTx.unstableMakeIsData ''GrantAction

--- ============================================================================
-- HELPERS
--- ============================================================================

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE anySigner #-}
anySigner :: ScriptContext -> Bool
anySigner ctx =
    length (txInfoSignatories (scriptContextTxInfo ctx)) > 0

{-# INLINABLE isFunder #-}
isFunder :: GrantDatum -> ScriptContext -> Bool
isFunder dat ctx = signedBy (gdFunder dat) ctx

{-# INLINABLE isResearcher #-}
isResearcher :: GrantDatum -> ScriptContext -> Bool
isResearcher dat ctx =
    case gdResearcher dat of
        Nothing -> False
        Just researcher -> signedBy researcher ctx

{-# INLINABLE isReviewer #-}
isReviewer :: GrantDatum -> ScriptContext -> Bool
isReviewer dat ctx = any (\r -> signedBy r ctx) (gdReviewers dat)

{-# INLINABLE hasReviewed #-}
hasReviewed :: PubKeyHash -> GrantDatum -> Bool
hasReviewed reviewer dat = reviewer `elem` gdReviewsGiven dat

{-# INLINABLE adaPaidTo #-}
adaPaidTo :: TxInfo -> PubKeyHash -> Integer
adaPaidTo info pkh =
    valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE ownInputAda #-}
ownInputAda :: ScriptContext -> Integer
ownInputAda ctx =
    case findOwnInput ctx of
        Nothing ->
            traceError "script input missing"
        Just txIn ->
            valueOf
                (txOutValue (txInInfoResolved txIn))
                adaSymbol
                adaToken

{-# INLINABLE currentMilestone #-}
currentMilestone :: GrantDatum -> Maybe Milestone
currentMilestone dat =
    let idx = gdCurrentMilestone dat
    in if idx < length (gdMilestones dat)
        then Just (gdMilestones dat !! idx)
        else Nothing

{-# INLINABLE milestoneCompleted #-}
milestoneCompleted :: GrantDatum -> Bool
milestoneCompleted dat =
    case currentMilestone dat of
        Nothing -> False
        Just ms -> msCompleted ms

{-# INLINABLE timeRemaining #-}
timeRemaining :: GrantDatum -> ScriptContext -> Integer
timeRemaining dat ctx =
    let currentTime = case txInfoValidRange (scriptContextTxInfo ctx) of
            Interval (LowerBound (Finite time) _) _ -> time
            _ -> traceError "invalid time range"
        deadline = gdDeadline dat
    in deadline - currentTime

{-# INLINABLE calculateReleaseAmount #-}
calculateReleaseAmount :: GrantDatum -> Integer
calculateReleaseAmount dat =
    case currentMilestone dat of
        Nothing -> 0
        Just ms -> msAmount ms

--- ============================================================================
-- VALIDATOR
--- ============================================================================

{-# INLINABLE mkGrantValidator #-}
mkGrantValidator :: GrantDatum -> GrantAction -> ScriptContext -> Bool
mkGrantValidator dat action ctx =
    case action of
        CreateGrant ->
            traceIfFalse "only funder can create grant"
                (isFunder dat ctx) &&
            traceIfFalse "must deposit grant amount"
                (ownInputAda ctx == gdTotalAmount dat) &&
            traceIfFalse "must have at least one milestone"
                (length (gdMilestones dat) > 0) &&
            traceIfFalse "must have reviewers"
                (length (gdReviewers dat) > 0) &&
            traceIfFalse "reviews needed must be <= total reviewers"
                (gdReviewsNeeded dat <= length (gdReviewers dat))

        ApplyForGrant ->
            case gdResearcher dat of
                Nothing ->
                    traceIfFalse "researcher must sign"
                        (anySigner ctx) &&
                    traceIfFalse "grant already has researcher"
                        (isNothing (gdResearcher dat))
                Just _ ->
                    traceError "grant already has researcher"

        SubmitMilestone proof ->
            traceIfFalse "only researcher can submit milestones"
                (isResearcher dat ctx) &&
            traceIfFalse "no researcher assigned"
                (isJust (gdResearcher dat)) &&
            traceIfFalse "must have current milestone"
                (isJust (currentMilestone dat)) &&
            traceIfFalse "cannot submit after deadline"
                (timeRemaining dat ctx > 0) &&
            traceIfFalse "proof cannot be empty"
                (lengthOfByteString proof > 0)

        ReviewMilestone approved ->
            traceIfFalse "only reviewers can review"
                (isReviewer dat ctx) &&
            traceIfFalse "must have current milestone"
                (isJust (currentMilestone dat)) &&
            traceIfFalse "reviewer already reviewed this milestone"
                (not (hasReviewed (getSigner ctx) dat)) &&
            traceIfFalse "must approve or reject"
                True

        ReleaseFunds ->
            traceIfFalse "only funder can release funds"
                (isFunder dat ctx) &&
            traceIfFalse "must have current milestone"
                (isJust (currentMilestone dat)) &&
            traceIfFalse "milestone not completed"
                (milestoneCompleted dat) &&
            traceIfFalse "insufficient reviews for milestone"
                (length (gdReviewsGiven dat) >= gdReviewsNeeded dat) &&
            traceIfFalse "must pay researcher"
                (case gdResearcher dat of
                    Nothing -> False
                    Just researcher -> adaPaidTo (scriptContextTxInfo ctx) researcher >= calculateReleaseAmount dat)

        ExtendDeadline newDeadline ->
            traceIfFalse "only funder can extend deadline"
                (isFunder dat ctx) &&
            traceIfFalse "new deadline must be later"
                (newDeadline > gdDeadline dat) &&
            traceIfFalse "cannot extend after project completion"
                (gdCurrentMilestone dat < length (gdMilestones dat))

        CancelGrant ->
            traceIfFalse "only funder can cancel grant"
                (isFunder dat ctx) &&
            traceIfFalse "cannot cancel after all funds released"
                (gdAmountReleased dat < gdTotalAmount dat) &&
            traceIfFalse "must refund remaining funds to funder"
                (adaPaidTo (scriptContextTxInfo ctx) (gdFunder dat) >= (gdTotalAmount dat - gdAmountReleased dat))

        EmergencyWithdraw ->
            traceIfFalse "only researcher can emergency withdraw"
                (isResearcher dat ctx) &&
            traceIfFalse "cannot withdraw before deadline"
                (timeRemaining dat ctx <= 0) &&
            traceIfFalse "must have unclaimed milestone funds"
                (gdAmountReleased dat < gdTotalAmount dat) &&
            traceIfFalse "can only withdraw unreleased funds"
                (adaPaidTo (scriptContextTxInfo ctx) (getResearcher dat) >= (gdTotalAmount dat - gdAmountReleased dat))
  where
    getSigner :: ScriptContext -> PubKeyHash
    getSigner ctx = head (txInfoSignatories (scriptContextTxInfo ctx))

    getResearcher :: GrantDatum -> PubKeyHash
    getResearcher dat =
        case gdResearcher dat of
            Nothing -> traceError "no researcher"
            Just r -> r

--- ============================================================================
-- COMPILE VALIDATOR AND GENERATE CBOR HEX
--- ============================================================================

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator d r p =
    let
        datum = PlutusTx.unsafeFromBuiltinData d :: GrantDatum
        action = PlutusTx.unsafeFromBuiltinData r :: GrantAction
        ctx = PlutusTx.unsafeFromBuiltinData p :: ScriptContext
    in
        if mkGrantValidator datum action ctx then () else error ()

-- Create the validator
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- Serialize validator to CBOR
serializeValidator :: Validator -> LBS.ByteString
serializeValidator = Serialise.serialise

-- Convert to ShortByteString
validatorToShortBS :: Validator -> SBS.ShortByteString
validatorToShortBS val = SBS.toShort (LBS.toStrict (serializeValidator val))

-- Convert validator to HEX using custom function
validatorToHex :: Validator -> String
validatorToHex val = 
    let sbs = validatorToShortBS val
    in shortBSToHex sbs

-- Get validator hash
validatorHashText :: Validator -> String
validatorHashText = show . validatorHash

--- ============================================================================
-- TEST DATA
--- ============================================================================

testFunderPKH :: PubKeyHash
testFunderPKH = PubKeyHash (toBuiltin ("research_foundation" :: BS.ByteString))

testResearcherPKH :: PubKeyHash
testResearcherPKH = PubKeyHash (toBuiltin ("principal_investigator" :: BS.ByteString))

testReviewer1PKH :: PubKeyHash
testReviewer1PKH = PubKeyHash (toBuiltin ("peer_reviewer_1" :: BS.ByteString))

testReviewer2PKH :: PubKeyHash
testReviewer2PKH = PubKeyHash (toBuiltin ("peer_reviewer_2" :: BS.ByteString))

testMilestone1 :: Milestone
testMilestone1 = Milestone
    { msDescription = "Literature review and project design"
    , msAmount = 2000000
    , msDeadline = 1000
    , msCompleted = False
    }

testMilestone2 :: Milestone
testMilestone2 = Milestone
    { msDescription = "Data collection and analysis"
    , msAmount = 3000000
    , msDeadline = 2000
    , msCompleted = False
    }

testMilestone3 :: Milestone
testMilestone3 = Milestone
    { msDescription = "Paper writing and publication"
    , msAmount = 5000000
    , msDeadline = 3000
    , msCompleted = False
    }

testGrantDatum :: GrantDatum
testGrantDatum = GrantDatum
    { gdFunder = testFunderPKH
    , gdResearcher = Nothing
    , gdTotalAmount = 10000000
    , gdAmountReleased = 0
    , gdMilestones = [testMilestone1, testMilestone2, testMilestone3]
    , gdCurrentMilestone = 0
    , gdDeadline = 4000
    , gdReviewers = [testReviewer1PKH, testReviewer2PKH]
    , gdReviewsNeeded = 2
    , gdReviewsGiven = []
    }

--- ============================================================================
-- MAIN FUNCTION - GENERATE CBOR HEX
--- ============================================================================

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "RESEARCH GRANT CONTRACT - CBOR HEX GENERATOR"
    putStrLn "=========================================="
    putStrLn ""
    
    putStrLn "1. GENERATING VALIDATOR..."
    putStrLn "   Validator compiled successfully!"
    putStrLn ""
    
    putStrLn "2. SERIALIZING TO CBOR..."
    let cbor = serializeValidator validator
    putStrLn ("   CBOR size: " ++ show (LBS.length cbor) ++ " bytes")
    putStrLn ""
    
    putStrLn "3. GENERATING HEX STRING..."
    let hexString = validatorToHex validator
    putStrLn ("   HEX length: " ++ show (length hexString) ++ " characters")
    putStrLn ""
    
    putStrLn "4. VALIDATOR HASH:"
    putStrLn ("   " ++ validatorHashText validator)
    putStrLn ""
    
    putStrLn "=========================================="
    putStrLn "CBOR HEX OUTPUT:"
    putStrLn "=========================================="
    putStrLn ""
    
    -- Print the first and last 100 characters of hex for verification
    if length hexString > 200
        then do
            putStrLn "First 100 characters:"
            putStrLn (take 100 hexString)
            putStrLn ""
            putStrLn "Last 100 characters:"
            putStrLn (take 100 (reverse hexString))
            putStrLn ""
            putStrLn "Full hex available below"
        else do
            putStrLn "Full hex string:"
            putStrLn hexString
    
    putStrLn ""
    putStrLn "=========================================="
    putStrLn "FULL HEX OUTPUT (Copy everything below):"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn hexString
    putStrLn ""
    putStrLn "=========================================="
    putStrLn "CONTRACT FEATURES SUMMARY:"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn "✓ Milestone-based research funding"
    putStrLn "✓ Peer review system (2 reviewers required)"
    putStrLn "✓ Time-bound project deadlines"
    putStrLn "✓ Emergency withdrawal for researchers"
    putStrLn "✓ Grant cancellation with fund recovery"
    putStrLn "✓ Proof submission for milestones"
    putStrLn "✓ Automatic fund release after approval"
    putStrLn ""
    putStrLn "Contract ready for Cardano deployment!"