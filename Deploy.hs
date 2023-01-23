-- Used to serialize and write to disk the validator.
-- Uses cardano-Api library which is what cardano-cli uses. It contains all funtionality for talking to the node.
-- It has it's own type ScriptData. so we have to convert plutus type Data to ScriptData.Using helper function dataToScriptData

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeJSON
    , writeMintingPolicy
    , writeUnit
    , writeEbutokenPolicy
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..)) --This module enables interactiion with Carado cli
import           Codec.Serialise       (Serialise, serialise) --This module provides functions to serialise and deserialise Haskell values for storage or transmission, to and from lazy ByteString s.
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import           Simplepolicy --gives use the validator

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

-- Writes the Data types to ScriptData Json
-- ScriptDataJsonDetailedSchema used when you want to write json to a file
-- ScriptDataJsonNoSchema used when you want to inline it in the command directly
writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeScript :: Serialise a => FilePath -> a -> IO (Either (FileError ()) ())
writeScript file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise
    -- }}}
-- converts plutus validator to a script then writes it to a file
writeMintingPolicy :: FilePath -> Ledger.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeScript file . Ledger.getMintingPolicy

-- writes unit haskell type for both the redeemer and datum as scriptdata type to the file unit.json
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

-- writing the parameterized validator 
writeEbutokenPolicy :: IO (Either (FileError ()) ())
writeEbutokenPolicy = writeMintingPolicy "testnet/ebupolicy.plutus" policy 
