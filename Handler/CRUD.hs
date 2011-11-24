{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Handler.__DATA__ where

import Foundation
import Control.Applicative
import Yesod.Form.Jquery
import Data.Text (Text)
import Data.Time

get__DATA__IndexR :: Handler RepHtml
get__DATA__IndexR = do
    items <- (runDB $ selectList [] []) :: Handler [(__DATA__Id, __DATA__)]
    defaultLayout $ do
        setTitle "__DATA__s"
        $(widgetFile "__DATALC__/index")


get__DATA__ViewR :: __DATA__Id -> Handler RepHtml
get__DATA__ViewR id = do
    item <- runDB $ get404 id
    defaultLayout $ do
        setTitle "__DATA__"
        $(widgetFile "__DATALC__/view")

get__DATA__CreateR :: Handler RepHtml
get__DATA__CreateR = do
    ((_, form), enctype) <- generateFormPost (__DATALC__Form Nothing)
    defaultLayout $(widgetFile "__DATALC__/create")

post__DATA__CreateR :: Handler RepHtml
post__DATA__CreateR = do
    ((result, form), enctype) <- runFormPost (__DATALC__Form Nothing)
    case result of
        FormSuccess __DATALC__ -> do
            id <- runDB $ insert __DATALC__
            redirect RedirectTemporary (__DATA__ViewR id)
        _ -> defaultLayout $(widgetFile "__DATALC__/create")


get__DATA__UpdateR :: __DATA__Id -> Handler RepHtml
get__DATA__UpdateR id = do
    item <- runDB $ get404 id
    ((_, form), enctype) <- generateFormPost (__DATALC__Form Nothing)
    defaultLayout $(widgetFile "__DATALC__/update")

post__DATA__UpdateR :: __DATA__Id -> Handler RepHtml
post__DATA__UpdateR id = do
    ((result, form), enctype) <- runFormPost (__DATALC__Form Nothing)
    case result of
        FormSuccess __DATALC__ -> do
            runDB $ replace id __DATALC__
            redirect RedirectTemporary (__DATA__ViewR id)
        _ -> defaultLayout $(widgetFile "__DATALC__/create")

get__DATA__DeleteR :: __DATA__Id -> Handler RepHtml
get__DATA__DeleteR id = delete__DATA__DeleteR id
post__DATA__DeleteR :: __DATA__Id -> Handler RepHtml
post__DATA__DeleteR id = delete__DATA__DeleteR id
delete__DATA__DeleteR :: __DATA__Id -> Handler RepHtml
delete__DATA__DeleteR id = do
    runDB $ delete id
    redirect RedirectTemporary __DATA__IndexR

-- The form
__DATALC__Form :: Maybe __DATA__ -> Html -> Form TierList TierList (FormResult __DATA__, Widget)
__DATALC__Form __DATALC__ = renderDivs $ __DATA__
    <*> undefined -- TODO: Fill in the form.
