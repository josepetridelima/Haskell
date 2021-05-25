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
    <*> areq textField "Cpf: " Nothing
    <*> areq intField  "Idade: " Nothing 
    <*> areq textField  "Endereco: " Nothing 

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost formCliente 
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}

            <h1>
                CADASTRO DE CLIENTE

            <form method=post action=@{ClienteR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]  

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <div>
                    CLIENTE INCLUIDO COM SUCESSO!
            |]
            redirect ClienteR
        _ -> redirect HomeR

getPerfilR :: ClienteId -> Handler Html
getPerfilR cid = do
    cliente <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            PAGINA DE #{clienteNome cliente}
            
        <h2>
            CPF: #{clienteCpf cliente}
            
        <h2>
            Idade: #{clienteIdade cliente}

        <h2>
            Endereco: #{clienteEndereco cliente}
    |]
--select * from cliente order by nome:
getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $ do
        $(whamletFile "templates/clientes.hamlet")

postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid
    redirect ListaCliR
        
  