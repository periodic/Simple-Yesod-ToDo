{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
module Model where

import Yesod
import Data.Time
import Data.Text (Text)
import Database.Persist.MongoDB
import Language.Haskell.TH.Syntax

import Database.Persist
import Database.Persist.Base
import Database.Persist.TH

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

derivePersistField "ChampionEntry"
derivePersistField "SeparatorEntry"
derivePersistField "TierListEntry"
derivePersistField "TierListData"

