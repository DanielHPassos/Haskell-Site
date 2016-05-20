{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET POST
/link LinkR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet|
    <h1> Ola Mundo
    <button onclick="teste()"> OK
|]>> toWidget [cassius |
    h1
        color:red;
|]>> toWidget [julius|
    function teste(){
        alert("OK");
    }
|]

postHomeR :: Handler Html
postHomeR = defaultLayout $ [whamlet|
    Ola do POST
|]

getLinkR :: Handler Html
getLinkR = defaultLayout $ [whamlet|
    <a href={@HomeR}>Home
|] 
main :: IO ()
main = warp 3000 HelloWorld

--                             stack web -- web(nome da página, no caso, web)  -- < compilar página
--                             curl -v -x GET https://haskel-cloned                -- renderizar page
--                              sempre que usa html usa
--                              sempre que usa css lucius ou cassius              <<< os tres usam toWidget
--                              sempre que usa javascript julius

>> toWidget [lucius |
    h1{
        color:red;
    }