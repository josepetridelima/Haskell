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
                <a href="/clientes">Visualizar e excluir Cliente</a>
                <a href="/produtos">Visualizar e excluir Produto</a>
                <a href="/vendas">Visualizar e excluir Venda</a>
        |] 

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                <a href="/cliente">Cadastrar Cliente</a>
                <a href="/produto">Cadastrar Produto</a>
                <a href="/venda">Cadastrar Venda</a>
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
             
       