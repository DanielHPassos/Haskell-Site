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
Clientes json
   nome Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/cadastro UserR POST OPTIONS
/cadastro/action/#ClientesId ActionR GET PUT DELETE
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------
postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

getActionR :: ClientesId -> Handler ()
getActionR cid = do
    cliente <- runDB $ get404 cid
    sendResponse $ toJSON cliente

putActionR :: ClientesId -> Handler ()
putActionR cid = do
    cliente <- requireJsonBody :: Handler Clientes
    runDB $ update cid [ClientesNome =. clientesNome cliente]
    sendResponse (object [pack "resp" .= pack "UPDATE"])


deleteActionR :: ClientesId -> Handler ()
deleteActionR cid = do
    runDB $ delete cid
    sendResponse (object [pack "resp" .= pack "DELETED"])
    
optionsUserR :: Handler ()
optionsUserR = addHeader "Access-Control-Allow-Methods" "POST, OPTIONS, DELETE"

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


