{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs, TypeSynonymInstances #-}
module Model where

import Yesod
import Data.Time
import Data.Text (Text)
import Database.Persist.MongoDB
import Language.Haskell.TH.Syntax

import Database.Persist
import Database.Persist.Base
import Database.Persist.TH

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as P
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Data.Attoparsec.Number as N
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad (liftM)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }, mkMigrate "migrateAll"] $(persistFile "config/models")

data ChampionEntry = ChampionEntry { ceChampId :: ChampionId
                                   , ceNotes   :: Text
                                   } deriving (Show, Eq, Read)

data SeparatorEntry = SeparatorEntry { sepText :: Text } deriving (Show, Eq, Read)
data TierListEntry = TierListEntryChamp ChampionEntry
                   | TierListEntrySep   SeparatorEntry
                   deriving (Show, Eq, Read)

data TierListData = TierListData { tldEntries :: [TierListEntry] } deriving (Show, Eq, Read)

instance ToJSON ChampionId where
    toJSON = String . pack . show
instance FromJSON ChampionId where
    parseJSON (String text) = 
        case reads (unpack text) of
            ((champId, _):_) -> return champId
            _                -> fail "Could not parse ChampionId"
    parseJSON _ = fail "Could not parse ChampionId"

$(deriveJSON (drop 2) ''ChampionEntry)
$(deriveJSON (drop 3) ''SeparatorEntry)
$(deriveJSON (drop 0) ''TierListEntry)
$(deriveJSON (drop 3) ''TierListData)

instance PersistField TierListData where
    toPersistValue = toPersistValue . toJSON
    fromPersistValue val = fromPersistValue val >>= P.parseEither parseJSON
    sqlType _ = SqlString
    isNullable _ = False

instance PersistField ChampionEntry where
    toPersistValue = toPersistValue . toJSON
    fromPersistValue val = fromPersistValue val >>= P.parseEither parseJSON
    sqlType _ = SqlString
    isNullable _ = False

instance PersistField TierListEntry where
    toPersistValue = toPersistValue . toJSON
    fromPersistValue val = fromPersistValue val >>= P.parseEither parseJSON
    sqlType _ = SqlString
    isNullable _ = False

instance PersistField SeparatorEntry where
    toPersistValue = toPersistValue . toJSON
    fromPersistValue val = fromPersistValue val >>= P.parseEither parseJSON
    sqlType _ = SqlString
    isNullable _ = False

instance PersistField Value where
    toPersistValue (Object obj) = PersistMap . map (\(k,v) -> (k, toPersistValue v)) . M.toList $ obj
    toPersistValue (Array arr)  = PersistList . map toPersistValue . V.toList $ arr
    toPersistValue (String text)= PersistText text
    toPersistValue (Number num) = case num of
                                      N.I int -> PersistInt64 . fromIntegral $ int
                                      N.D double -> PersistDouble double
    toPersistValue (Bool bool)  = PersistBool bool
    toPersistValue (Null)       = PersistNull

    fromPersistValue (PersistText text)     = Right $ String text
    fromPersistValue (PersistByteString bs) = Right . String . E.decodeUtf8 $ bs
    fromPersistValue (PersistInt64 int)     = Right . Number . N.I . fromIntegral $ int
    fromPersistValue (PersistDouble doub)   = Right . Number . N.D $ doub
    fromPersistValue (PersistBool bool)     = Right $ Bool bool
    fromPersistValue (PersistDay day)       = Right . String . pack . show $ day
    fromPersistValue (PersistTimeOfDay time)= Right . String . pack . show $ time
    fromPersistValue (PersistUTCTime utc)   = Right . String . pack . show $ utc
    fromPersistValue (PersistNull)          = Right $ Null
    fromPersistValue (PersistList vals)     = (Array . V.fromList) `liftM` (mapM (fromPersistValue) vals)
    fromPersistValue (PersistMap listPairs) = let parsePair (k,v) = case fromPersistValue v of
                                                                        Right s -> Right (k,s)
                                                                        Left m  -> Left m
                                              in (Object . M.fromList) `liftM` (mapM parsePair listPairs)
    fromPersistValue (PersistObjectId bs)   = Right . String . E.decodeUtf8 $ bs

    sqlType    _ = SqlString
    isNullable _ = False
