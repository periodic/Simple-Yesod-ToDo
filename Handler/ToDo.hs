{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.ToDo where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery

getToDoIndexR :: Handler RepHtml
getToDoIndexR = do
    todos <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "ToDos"
        $(widgetFile "toDo-index")


getToDoViewR :: ToDoId -> Handler RepHtml
getToDoViewR toDoId = do
    todo <- runDB $ get404 toDoId
    defaultLayout $ do
        setTitle . toHtml . toDoName $ todo
        $(widgetFile "toDo-view")

getToDoCreateR :: Handler RepHtml
getToDoCreateR = do
    ((_, form), enctype) <- generateFormPost (toDoForm Nothing)
    defaultLayout $(widgetFile "toDo-create")

postToDoCreateR :: Handler RepHtml
postToDoCreateR = do
    ((result, form), enctype) <- runFormPost (toDoForm Nothing)
    case result of
        FormSuccess todo -> do
            toDoId <- runDB $ insert todo
            redirect RedirectTemporary (ToDoViewR toDoId)
        _ -> defaultLayout $(widgetFile "toDo-create")


getToDoUpdateR :: ToDoId -> Handler RepHtml
getToDoUpdateR toDoId = do
    todo <- runDB $ get404 toDoId
    ((_, form), enctype) <- generateFormPost . toDoForm $ Just todo
    defaultLayout $(widgetFile "toDo-update")

postToDoUpdateR :: ToDoId -> Handler RepHtml
postToDoUpdateR toDoId = do
    ((result, form), enctype) <- runFormPost (toDoForm Nothing)
    case result of
        FormSuccess todo -> do
            runDB $ replace toDoId todo
            redirect RedirectTemporary (ToDoViewR toDoId)
        _ -> defaultLayout $(widgetFile "toDo-create")

getToDoDeleteR :: ToDoId -> Handler RepHtml
getToDoDeleteR toDoId = deleteToDoDeleteR toDoId
postToDoDeleteR :: ToDoId -> Handler RepHtml
postToDoDeleteR toDoId = deleteToDoDeleteR toDoId
deleteToDoDeleteR :: ToDoId -> Handler RepHtml
deleteToDoDeleteR toDoId = do
    runDB $ delete toDoId
    redirect RedirectTemporary ToDoIndexR

-- The form
toDoForm :: Maybe ToDo -> Html -> Form TierList TierList (FormResult ToDo, Widget)
toDoForm todo = renderDivs $ ToDo
    <$> areq textField "Name" (toDoName <$> todo)
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:0" -- 1900 till five years ago
        }) "Created" (toDoCreated <$> todo)
    <*> areq textField "Image URL" (toDoImageUrl <$> todo)
