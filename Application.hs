{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withTierList
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger)
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base

-- Import all relevant handler modules here.
import Handler.Champion
import Handler.List
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "TierList" resourcesTierList

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withTierList :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withTierList conf logger f = do
#ifdef PRODUCTION
    s <- static Settings.staticDir
#else
    s <- staticDevel Settings.staticDir
#endif
    dbconf <- withYamlEnvironment "config/mongoDB.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        let h = TierList conf logger s p
        defaultRunner f h

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withTierList
