{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuarios json
   email Text
   senha Text
   nome  Text
   sobreNome Text
   deriving Show

Saldo json
   userId Usuarios
   saldo Double
   UniqueUsuarios userId
   deriving Show
   
Produtos json
   descricao Text
   valor Double
   deriving Show
   
Vendas json
   userId Usuarios
   prodId Produtos
   total Double
   dataVenda Date
   UniqueUsuariosProdutos userId prodId
   deriving Show
   
Historico json
   userId Usuarios
   valorComprado Double
   dinheiroGasto Double
   dataCompra Date
   UniqueUsuarios userId
   deriving Show

|]

mkYesod "Pagina" [parseRoutes|

/ HomeR GET
/login LoginR GET
/usuario CadastroR GET POST

--/cadastro UserR GET POST OPTIONS 
--/cadastro/action/#ClientesId ActionR GET PUT DELETE
--/produto ProdutoR GET POST
--/venda VendaR POST
--/venda/check/#ClientesId VendaCliR GET
|]
------------------------------------------------------
--Configuração do BD
instance YesodPersist Pagina where
   
   authRoute _ = Just LoginR
   
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
       
isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
        
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "Admin" -> Authorized
        Just _ -> Unauthorized "Você tem que ser autorizado"
------------------------------------------------------
--Link para o site > https://haskel-cloned-danielhpassos.c9users.io/
------------------------------------------------------

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    
    

postCadastroR :: Handler ()
postCadastroR = do
    usuarios <- requireJsonBody :: Handler Usuarios
    runDB $ insert usuarios
    sendResponse (object [pack "resp" .= pack "Cadastro realizado com sucesso!"])

getCadastroR :: Handler Html
getCadastroR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  
  [whamlet|
  <div>
      <div class="navbar navbar-inverse">
          <div class="row">
              <div class="col-md-10">
              <div class="col-md-2 pull-right">
                  <form class="form-inline" role="form" style="padding-top:1%;">
                      <div class="form-group">
                          <button class="btn btn-default glyphicon glyphicon-off" style="border-radius:5px;" id="btn">
  <article class="container">
      <section>
          <div class="row">
              <div class="col-md-2">
              <div class="col-md-8">
                  <form class="form-horizontal" role="form" method="POST" #form>
                      <div class="form-group">
                          Nome: <input type="text" class="form-control" #nome name="nome">
                      <div class="form-group">
                          Sobrenome: <input type="text" class="form-control" #sobreNome name="sobreNome">
                      <div class="form-group">
                          E-Mail: <input type="text" class="form-control" #email name="email">
                      <div class="form-group">
                          Senha: <input type="password" class="form-control" #pwd name="pwd">
                      <div class="form-group">
                          Repita a senha: <input type="password" class="form-control" #pwdConfirm>
                      <div class="form-group">
                          <button #btn class="btn btn-default">Cadastrar
              <div class="col-md-2">
  <footer>
  
  
  |]
getLoginR :: Handler Html
getLoginR  = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  
  [whamlet|
  <div>
      <div class="navbar navbar-inverse">
          <div class="row">
              <div class="col-md-7">
                  <div class="pull-left form-group">
                      <a href="@{CadastroR}" role="button" class="btn btn-default">Não tenho cadastro
              <div class="col-md-5">
                  <form class="form-inline" role="form" style="padding-top:1%;">
                      <div class="form-group has-feedback">
                          <input type="text" class="form-control" id="login" name="login" placeholder="Login">
                          <span class="glyphicon glyphicon-user form-control-feedback">
                      <div class="form-group has-feedback">
                          <input type="text" class="form-control" id="pwd" placeholder="Senha">
                          <span class="glyphicon glyphicon-lock form-control-feedback">
                      <div class="form-group">
                          <button class="btn btn-default glyphicon glyphicon-off" style="border-radius:5px;" id="btn">
  <article class="container">
      <section>
  <footer>
                
                  
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").on("click",function(e){e.preventDefault();});
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "{}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#email").val()},{"nome":$("#usuario").val()}),
                                      
                 success: function(data) {
                     alert(data.resp);
                     $("#email").val("");
                     $("#senha").val("");
                 }
            })
         });
     }
  |]



connStr = "dbname=d7tngusljsj07g host=ec2-54-163-240-97.compute-1.amazonaws.com user=gdecjykupajwsm password=vhX9rbdjMj3-z6j47hvjWjsUNb port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)

