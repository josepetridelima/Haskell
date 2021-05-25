{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import

formProduto :: Form Produto
formProduto = renderDivs $ Produto
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Código: " Nothing
    <*> areq intField  "Peso: " Nothing 
    <*> areq intField  "Volume: " Nothing 

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/c2.hamlet")
        
postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            setMessage [shamlet|
                <div>
                    PRODUTO INCLUÍDO COM SUCESSO!
            |]
            redirect ProdutoR
        _ -> redirect HomeR

getPerfilProdR :: ProdutoId -> Handler Html
getPerfilProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/perfilprod.hamlet")
    
getListaProR :: Handler Html
getListaProR = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/produtos.hamlet")

postApagarProR :: ProdutoId -> Handler Html
postApagarProR pid = do
    runDB $ delete pid
    redirect ListaProR
