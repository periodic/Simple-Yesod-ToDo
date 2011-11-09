{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.Champion where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery

getChampionIndexR :: Handler RepHtml
getChampionIndexR = do
    champs <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Champions"
        $(widgetFile "champion-index")


getChampionViewR :: ChampionId -> Handler RepHtml
getChampionViewR championId = do
    champ <- runDB $ get404 championId
    defaultLayout $ do
        setTitle . toHtml . championName $ champ
        $(widgetFile "champion-view")

getChampionCreateR :: Handler RepHtml
getChampionCreateR = do
    ((_, form), enctype) <- generateFormPost (championForm Nothing)
    defaultLayout $(widgetFile "champion-create")

postChampionCreateR :: Handler RepHtml
postChampionCreateR = do
    ((result, form), enctype) <- runFormPost (championForm Nothing)
    case result of
        FormSuccess champ -> do
            championId <- runDB $ insert champ
            redirect RedirectTemporary (ChampionViewR championId)
        _ -> defaultLayout $(widgetFile "champion-create")


getChampionUpdateR :: ChampionId -> Handler RepHtml
getChampionUpdateR championId = do
    champ <- runDB $ get404 championId
    ((_, form), enctype) <- generateFormPost . championForm $ Just champ
    defaultLayout $(widgetFile "champion-update")

postChampionUpdateR :: ChampionId -> Handler RepHtml
postChampionUpdateR championId = do
    ((result, form), enctype) <- runFormPost (championForm Nothing)
    case result of
        FormSuccess champ -> do
            runDB $ replace championId champ
            redirect RedirectTemporary (ChampionViewR championId)
        _ -> defaultLayout $(widgetFile "champion-create")

getChampionDeleteR :: ChampionId -> Handler RepHtml
getChampionDeleteR championId = deleteChampionDeleteR championId
postChampionDeleteR :: ChampionId -> Handler RepHtml
postChampionDeleteR championId = deleteChampionDeleteR championId
deleteChampionDeleteR :: ChampionId -> Handler RepHtml
deleteChampionDeleteR championId = do
    runDB $ delete championId
    redirect RedirectTemporary ChampionIndexR

-- The form
championForm :: Maybe Champion -> Html -> Form TierList TierList (FormResult Champion, Widget)
championForm champ = renderDivs $ Champion
    <$> areq textField "Name" (championName <$> champ)
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:0" -- 1900 till five years ago
        }) "Created" (championCreated <$> champ)
    <*> areq textField "Image URL" (championImageUrl <$> champ)
