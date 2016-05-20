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

Creditos json
    userId Usuarios
    saldo Double
    deriving Show
    
Produtos json
   descricao Text
   valor Double
   deriving Show
   
Vendas json
   userId Usuarios
   prodId Produtos
   total Double
   UniqueClientesProdutos userId prodId
|]

mkYesod "Pagina" [parseRoutes|
/ LoginR GET POST
--/cadastro UserR GET POST OPTIONS 
--/cadastro/action/#ClientesId ActionR GET PUT DELETE
--/produto ProdutoR GET POST
--/venda VendaR POST
--/venda/check/#ClientesId VendaCliR GET
|]
------------------------------------------------------
--Configuração do BD
instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

getLoginR :: Handler Html
getLoginR  = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  
  [whamlet| 
<div class="container">
    <form class="form">
        <div class="form-group">
            Login: <input type="text" #email class="form-control">
            Senha: <input type="text" #senha class="form-control">
            <br />
            <button #btn class="btn btn-primary btn-md">OK
        
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").on("click",function(e){e.preventDefault();});
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{UserR}",
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

getUserR :: Handler ()
getUserR = do
    allClientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allClientes])

postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

getActionR :: ClientesId -> Handler ()
getActionR cid = do
    cliente <- runDB $ get404 cid
    sendResponse $ toJSON cliente

deleteActionR :: ClientesId -> Handler ()
deleteActionR cid = do
    runDB $ delete cid
    sendResponse (object [pack "resp" .= pack "DELETED"])
    
putActionR :: ClientesId -> Handler ()
putActionR cid = do
    cliente <- requireJsonBody :: Handler Clientes
    runDB $ update cid [ClientesNome =. clientesNome cliente]
    sendResponse (object [pack "resp" .= pack "UPDATE"])

optionsUserR :: Handler ()
optionsUserR = addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

getProdutoR :: Handler ()
getProdutoR = do
    allProd <- runDB $ selectList [] [Asc ProdutozValor]
    sendResponse (object [pack "data" .= fmap toJSON allProd])
    
postProdutoR :: Handler ()
postProdutoR = do
    prod <- requireJsonBody :: Handler Produtoz
    runDB $ insert prod
    sendResponse (object [pack "resp" .= pack "CREATED"])

postVendaR :: Handler ()
postVendaR = do
    venda <- requireJsonBody :: Handler ClientesProdutos
    runDB $ insert venda
    sendResponse (object [pack "resp" .= pack "CREATED"])

getVendaCliR :: ClientesId -> Handler ()    
getVendaCliR cid = do
    xs <- runDB $ (rawSql (pack $ "SELECT ??, ??, ?? FROM produtoz  \ 
        \ INNER JOIN clientes_produtos ON produtoz.id=clientes_produtos.prid \ 
        \ INNER JOIN clientes ON  clientes.id=clientes_produtos.clid \
        \ WHERE clientes_produtos.clid = " ++ (show $ fromSqlKey cid)) []) :: Handler [(Entity Produtoz,Entity ClientesProdutos,Entity Clientes)]
    sendResponse (object [pack "data" .= fmap (toJSON . (\(p,_,_) -> p)) xs])

connStr = "dbname=d7tngusljsj07g host=ec2-54-163-240-97.compute-1.amazonaws.com user=gdecjykupajwsm password=vhX9rbdjMj3-z6j47hvjWjsUNb port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)

