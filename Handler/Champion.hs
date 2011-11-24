{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.Champion where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery
import Data.Time

getChampionIndexR :: Handler RepHtml
getChampionIndexR = do
    items <- (runDB $ selectList [] []) :: Handler [(ChampionId, Champion)]
    defaultLayout $ do
        setTitle "Champions"
        $(widgetFile "champion/index")


getChampionViewR :: ChampionId -> Handler RepHtml
getChampionViewR id = do
    item <- runDB $ get404 id
    defaultLayout $ do
        setTitle "Champion"
        $(widgetFile "champion/view")

getChampionCreateR :: Handler RepHtml
getChampionCreateR = do
    time <- liftIO getCurrentTime
    ((_, form), enctype) <- generateFormPost (championForm time Nothing)
    defaultLayout $(widgetFile "champion/create")

postChampionCreateR :: Handler RepHtml
postChampionCreateR = do
    time <- liftIO getCurrentTime
    ((result, form), enctype) <- runFormPost (championForm time Nothing)
    case result of
        FormSuccess champion -> do
            id <- runDB $ insert champion
            redirect RedirectTemporary (ChampionViewR id)
        _ -> defaultLayout $(widgetFile "champion/create")


getChampionUpdateR :: ChampionId -> Handler RepHtml
getChampionUpdateR id = do
    time <- liftIO getCurrentTime
    item <- runDB $ get404 id
    ((_, form), enctype) <- generateFormPost (championForm time Nothing)
    defaultLayout $(widgetFile "champion/update")

postChampionUpdateR :: ChampionId -> Handler RepHtml
postChampionUpdateR id = do
    time <- liftIO getCurrentTime
    ((result, form), enctype) <- runFormPost (championForm time Nothing)
    case result of
        FormSuccess champion -> do
            runDB $ replace id champion
            redirect RedirectTemporary (ChampionViewR id)
        _ -> defaultLayout $(widgetFile "champion/create")

getChampionDeleteR :: ChampionId -> Handler RepHtml
getChampionDeleteR id = deleteChampionDeleteR id
postChampionDeleteR :: ChampionId -> Handler RepHtml
postChampionDeleteR id = deleteChampionDeleteR id
deleteChampionDeleteR :: ChampionId -> Handler RepHtml
deleteChampionDeleteR id = do
    runDB $ delete id
    redirect RedirectTemporary ChampionIndexR

-- The form
championForm :: UTCTime -> Maybe Champion -> Html -> Form TierList TierList (FormResult Champion, Widget)
championForm time champion = renderDivs $ Champion
    <$> areq textField "Name" (championName <$> champion)
    <*> pure time
    <*> areq urlField "ImageUrl" (championImageUrl <$> champion)
