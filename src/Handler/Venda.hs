{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Venda where

import Import

formVenda :: Form Venda
formVenda = renderDivs $ Venda
    <$> areq textField "Cliente: " Nothing
    <*> areq textField "Produto: " Nothing
    <*> areq intField  "Data: " Nothing 
    <*> areq intField  "Valor: " Nothing 

getVendaR :: Handler Html
getVendaR = do
    (widget,_) <- generateFormPost formVenda
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/c3.hamlet") 

postVendaR :: Handler Html
postVendaR = do
    ((result,_),_) <- runFormPost formVenda
    case result of
        FormSuccess venda -> do
            runDB $ insert venda
            setMessage [shamlet|
                <div>
                    VENDA INCLUÍDA COM SUCESSO!
            |]
            redirect VendaR
        _ -> redirect HomeR

getPerfilVendR :: VendaId -> Handler Html
getPerfilVendR vid = do
    venda <- runDB $ get404 vid
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/perfilvenda.hamlet")
        
getListaVendR :: Handler Html
getListaVendR = do
    vendas <- runDB $ selectList [] [Asc VendaCliente]
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/vendas.hamlet")

postApagarVendR :: VendaId -> Handler Html
postApagarVendR vid = do
    runDB $ delete vid
    redirect ListaVendR
