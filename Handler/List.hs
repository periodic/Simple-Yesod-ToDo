{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.List where

import Foundation
import Control.Applicative
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
    lists <- (runDB $ selectList [] []) :: Handler [(ListId, List)]
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
    champions <- runDB $ selectList [] []
    ((_, form), enctype) <- generateFormPost (listForm champions Nothing)
    defaultLayout $(widgetFile "list-create")

postListCreateR :: Handler RepHtml
postListCreateR = do
    champions <- runDB $ selectList [] []
    ((result, form), enctype) <- runFormPost (listForm champions Nothing)
    case result of
        FormSuccess list -> do
            listId <- runDB $ insert list
            redirect RedirectTemporary (ListViewR listId)
        _ -> defaultLayout $(widgetFile "list-create")


getListUpdateR :: ListId -> Handler RepHtml
getListUpdateR listId = do
    list <- runDB $ get404 listId
    champions <- runDB $ selectList [] []
    ((_, form), enctype) <- generateFormPost . listForm champions $ Just list
    defaultLayout $(widgetFile "list-update")

postListUpdateR :: ListId -> Handler RepHtml
postListUpdateR listId = do
    champions <- runDB $ selectList [] []
    ((result, form), enctype) <- runFormPost (listForm champions Nothing)
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
listForm :: [(ChampionId, Champion)] -> Maybe List -> Html -> Form TierList TierList (FormResult List, Widget)
listForm champions list extra = do
    time <- liftIO getCurrentTime
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (champsRes, champsView) <- mreq (championListField champions) "this is not used" Nothing
    let listRes = List <$> nameRes <*> pure time <*> champsRes
    let widget = [whamlet|
    #{extra}
    <p>
        Name: ^{fvInput nameView}
        ^{fvInput champsView}
|]
    return (listRes, widget)

championListField :: [(ChampionId,Champion)] -> Field sub master TierListData
championListField idNameMap = Field
    { fieldParse = championListParse
    , fieldView  = championListView
    }
    where
        championListParse rawVals = return . Right . Just . TierListData . map (TierListEntryChamp . flip ChampionEntry "") . catMaybes . map maybeId $ rawVals

        maybeId :: Text -> Maybe ChampionId
        maybeId text = case reads (unpack text) of
            ((champId, _):_) -> Just champId
            _                -> Nothing

        getRowText row = case row of
            TierListEntryChamp champData -> maybe "Unknown" championName . lookup (ceChampId champData) $ idNameMap
            TierListEntrySep   sepData   -> sepText sepData
        getRowId row = case row of
            TierListEntryChamp champData -> show $ ceChampId champData
            TierListEntrySep   sepData   -> ""

        championListView idAttr nameAttr eResult isReq = do
            let rows = case eResult of
                            Left _ -> []
                            Right (TierListData entries) -> entries
            [whamlet|
<ul id=#{idAttr}>
    $forall row <- rows
        <li>
            <input type=hidden name=#{nameAttr}[]>
                #{getRowId row} #{getRowText row}
|]

