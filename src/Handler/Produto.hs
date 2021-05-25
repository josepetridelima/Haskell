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
    <*> areq textField "Cod: " Nothing
    <*> areq intField  "Peso: " Nothing 
    <*> areq intField  "Volume: " Nothing 

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto 
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}

            <h1>
                CADASTRO DE Produto

            <form method=post action=@{ProdutoR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]  

postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            setMessage [shamlet|
                <div>
                    Produto INCLUIDO COM SUCESSO!
            |]
            redirect ProdutoR
        _ -> redirect HomeR

getPerfilProdR :: ProdutoId -> Handler Html
getPerfilProdR cid = do
    produto <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            PAGINA DE #{produtoNome produto}
            
        <h2>
            Cod: #{produtoCod produto}
            
        <h2>
            Peso: #{produtoPeso produto}

        <h2>
            Volume: #{produtoVolume produto}
    |]
--select * from Produto order by nome:
getListaProR :: Handler Html
getListaProR = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $ do
        $(whamletFile "templates/produtos.hamlet")

postApagarProR :: ProdutoId -> Handler Html
postApagarProR cid = do
    runDB $ delete cid
    redirect ListaProR
        
  