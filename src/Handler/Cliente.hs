{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Email: " Nothing
    <*> areq textField "CPF: " Nothing
    
getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/c1.hamlet")

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <div>
                    PARABÉNS, VOCÊ FOI CADASTRADO NA NEWSFEED!
            |]
            redirect ClienteR
        _ -> redirect HomeR
            
getPerfilR :: ClienteId -> Handler Html
getPerfilR cid = do
    cliente <- runDB $ get404 cid
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/perfil.hamlet")
    
getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/clientes.hamlet")
        
postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid
    redirect ListaCliR
