{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.List where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery

getListIndexR :: Handler RepHtml
getListIndexR = do
    lists <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Lists"
        $(widgetFile "list-index")


getListViewR :: ListId -> Handler RepHtml
getListViewR listId = do
    list <- runDB $ get404 listId
    maybeUser <- runDB $ get (listUser list)
    defaultLayout $ do
        setTitle . toHtml . listName $ list
        $(widgetFile "list-view")

getListCreateR :: Handler RepHtml
getListCreateR = do
    users <- runDB $ selectList [] []
    ((_, form), enctype) <- generateFormPost (listForm users Nothing)
    defaultLayout $(widgetFile "list-create")

postListCreateR :: Handler RepHtml
postListCreateR = do
    users <- runDB $ selectList [] []
    ((result, form), enctype) <- runFormPost (listForm users Nothing)
    case result of
        FormSuccess list -> do
            listId <- runDB $ insert list
            redirect RedirectTemporary (ListViewR listId)
        _ -> defaultLayout $(widgetFile "list-create")


getListUpdateR :: ListId -> Handler RepHtml
getListUpdateR listId = do
    list <- runDB $ get404 listId
    users <- runDB $ selectList [] []
    ((_, form), enctype) <- generateFormPost . listForm users $ Just list
    defaultLayout $(widgetFile "list-update")

postListUpdateR :: ListId -> Handler RepHtml
postListUpdateR listId = do
    users <- runDB $ selectList [] []
    ((result, form), enctype) <- runFormPost (listForm users Nothing)
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
listForm :: [(UserId, User)] -> Maybe List -> Html -> Form TierList TierList (FormResult List, Widget)
listForm users list = renderDivs $ List
    <$> areq textField "Name" (listName <$> list)
    <*> areq (selectField $ map (\(uid, u) -> (userIdent u, uid)) users) "User" (listUser <$> list)
    <*> areq jqueryDayTimeField "Created" (listCreated <$> list)
