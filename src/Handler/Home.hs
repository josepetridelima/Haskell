{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

-- static/img/logo.png => img_logo_png
-- imagens baixadas

getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 0

            <img src=@{StaticR img_logo_png}>        
        |] 

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |] 

-- Monad Handler => Back-end
-- Monad Widget => Front-end
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css) 
        toWidgetHead $(juliusFile "templates/home.julius")             
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile"templates/home.hamlet")
             
       