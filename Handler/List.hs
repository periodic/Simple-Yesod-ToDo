{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.List where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery
import Data.Text (Text, unpack)
import Data.Time
import Data.Maybe (catMaybes)

isSeparator :: TierListEntry -> Maybe Text
isSeparator (TierListEntrySep (SeparatorEntry text)) = Just text
isSeparator _                                        = Nothing

isChamp :: TierListEntry -> Maybe ChampionEntry
isChamp (TierListEntryChamp champInfo) = Just champInfo
isChamp _                              = Nothing

getListIndexR :: Handler RepHtml
getListIndexR = do
    lists <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Lists"
        $(widgetFile "list-index")


getListViewR :: ListId -> Handler RepHtml
getListViewR listId = do
    list <- runDB $ get404 listId
    defaultLayout $ do
        setTitle . toHtml . listName $ list
        $(widgetFile "list-view")

getListCreateR :: Handler RepHtml
getListCreateR = do
    ((_, form), enctype) <- generateFormPost (listForm Nothing)
    defaultLayout $(widgetFile "list-create")

postListCreateR :: Handler RepHtml
postListCreateR = do
    ((result, form), enctype) <- runFormPost (listForm Nothing)
    case result of
        FormSuccess list -> do
            listId <- runDB $ insert list
            redirect RedirectTemporary (ListViewR listId)
        _ -> defaultLayout $(widgetFile "list-create")


getListUpdateR :: ListId -> Handler RepHtml
getListUpdateR listId = do
    list <- runDB $ get404 listId
    ((_, form), enctype) <- generateFormPost . listForm $ Just list
    defaultLayout $(widgetFile "list-update")

postListUpdateR :: ListId -> Handler RepHtml
postListUpdateR listId = do
    ((result, form), enctype) <- runFormPost (listForm Nothing)
    case result of
        FormSuccess list -> do
            runDB $ replace listId list
            redirect RedirectTemporary (ListViewR listId)
        _ -> defaultLayout $(widgetFile "list-create")

getListDeleteR :: ListId -> Handler RepHtml
getListDeleteR listId = deleteListDeleteR listId
postListDeleteR :: ListId -> Handler RepHtml
postListDeleteR listId = deleteListDeleteR listId
deleteListDeleteR :: ListId -> Handler RepHtml
deleteListDeleteR listId = do
    runDB $ delete listId
    redirect RedirectTemporary ListIndexR

-- The form
listForm :: Maybe List -> Html -> Form TierList TierList (FormResult List, Widget)
listForm list extra = do
    time <- liftIO getCurrentTime
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (champsRes, champsView) <- mreq championListField "this is not used" Nothing
    let listRes = List <$> nameRes <*> pure time <*> champsRes
    let widget = [whamlet|
    #{extra}
    <p>
        Name: ^{fvInput nameView}
        ^{fvInput champsView}
|]
    return (listRes, widget)

championListField :: Field sub master TierListData
championListField = Field
    { fieldParse = championListParse
    , fieldView  = championListView
    }
    where
        championListParse rawVals = return . Right . Just . TierListData . map (TierListEntryChamp . flip ChampionEntry "") . catMaybes . map maybeId $ rawVals

        maybeId :: Text -> Maybe ChampionId
        maybeId text = case reads (unpack text) of
            ((champId, _):more) -> Just champId
            _                   -> Nothing

        championListView idAttr nameAttr eResult isReq = do
            allChampions <- runDB $ selectList [] []
            let rows = case eResult of
                            Left _ -> map (TierListEntryChamp . flip ChampionEntry "" . fst) allChampions
                            Right (TierListData entries) -> entries
            [whamlet|
<ul id=#{idAttr}>
    $forall row <- rows
        <li>
            <input type=hidden name=#{nameAttr}[]>
                $with champName <- lookup (ceChampId row) allChampions
                    #{champName}
|]

