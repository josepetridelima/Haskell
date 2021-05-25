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
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}

            <h1>
                CADASTRO DE Venda

            <form method=post action=@{VendaR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]  

postVendaR :: Handler Html
postVendaR = do
    ((result,_),_) <- runFormPost formVenda
    case result of
        FormSuccess venda -> do
            runDB $ insert venda
            setMessage [shamlet|
                <div>
                    Venda INCLUIDA COM SUCESSO!
            |]
            redirect VendaR
        _ -> redirect HomeR

getPerfilVendR :: VendaId -> Handler Html
getPerfilVendR cid = do
    venda <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            PAGINA DE #{vendaCliente venda}
            
        <h2>
            Produto: #{vendaProduto venda}
            
        <h2>
            Data: #{vendaData venda}

        <h2>
            Valor: #{vendaValor venda}
    |]
--select * from Venda order by cliente:
getListaVendR :: Handler Html
getListaVendR = do
    vendas <- runDB $ selectList [] [Asc VendaCliente]
    defaultLayout $ do
        $(whamletFile "templates/vendas.hamlet")

postApagarVendR :: VendaId -> Handler Html
postApagarVendR cid = do
    runDB $ delete cid
    redirect ListaVendR
        
  