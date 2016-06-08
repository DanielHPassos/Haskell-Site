{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Yesod.Static 
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{getStatic :: Static, connPool :: ConnectionPool}

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
   dataVenda Text
   UniqueUsuariosProdutos userId prodId
   deriving Show
   
Historico json
   userId Usuarios
   valorComprado Double
   dinheiroGasto Double
   dataCompra Text
   UniqueUsuariosH userId
   deriving Show

|]

staticFiles "static"

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/cadastro CadastroR GET POST
/autor AutorR GET
/adm AdmR GET
/produtos ProdutoR GET
/sobre SobreR GET
/login LoginR GET
/logout LogoutR GET
/static StaticR Static getStatic


|]


------------------------------------------------------
--Configuração do BD
instance YesodPersist Pagina where
   
   --authRoute _ = Just LoginR
   
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

             
------------------------------------------------------
--Link para o site > https://haskel-cloned-danielhpassos.c9users.io/
-- https://github.com/yesodweb/yesod/wiki/Cookbook-file-upload-saving-files-to-server
------------------------------------------------------
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
           addStylesheet $ StaticR estilo_css
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
    <body class="center clearfix" style="background:#F2F2F2;">
        <header>
           <h1> <img src="http://i.imgur.com/1o358Mn.png" height="100" width"100">
        <section class="chamada">
            <h2>Que tal fazer compras com moeda virtual?
            <h3>Sim, aqui isso é possível :D
        <section class="container">
            <div class="comprar">
                <img src="http://i.imgur.com/3ywsUnv.jpg" width="200" height="200" alt="Comprar">
                <h3>Compra
                <p>Com cada compra, você ganhará créditos para a próxima compra!.
            <div class="pesquisar">
                <img src="http://i.imgur.com/xPavGFH.jpg" width="200" height="200" alt="Pesquisa">
                <h3>Pesquisa
                <p>Pesquisa de itens para comprar! Cada iten pesquisado ficará salvo.
            <div class="dinheiro">
                <img src="http://i.imgur.com/HhiWXVy.jpg" width="200" height="200" alt="DimDim">
                <h3>Grana
                <p>Dinheiro virtual para efetuar a compra, quanto mais créditos, melhor !!
        <footer>
            <p> Análise e Desenvolvimento de Sistemas - 6º ciclo
        <nav> 
            <ul> 
                <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
                <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
                <li><a href="#">SemIdeia
   
    |]

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
  addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
  addStylesheet  $ StaticR estilo_css
  
  [whamlet|
  <div>
      <div class="navbar navbar-inverse">
          <div class="row">
              <div class="col-md-10">
              <div class="col-md-2 pull-right">
                  <div class="form-inline" role="form" style="padding-top:1%;">
                      <div class="form-group">
                          <button class="btn btn-default glyphicon glyphicon-off" style="border-radius:5px;" id="btn">
                          
  <body class="clearfix" style="background: #F2F2F2;">
   <section class="chamada" style="height:150px !important;">
        <h2 style="padding-left:150px !important;margin-top:50px !important;"> Cadastro
        <h3 style="padding-left:150px !important;"> Realize seu cadastro e divirta-se 
    <header>
   <section style="height:450px !important;" class="container">
        <div align="center">
            <div class="form-group">
                  <div class="img">
                       <img src="http://i.imgur.com/1xtzm4B.png" height="380" width="330" style="alignment-adjust:central;margin-right:25px !important;"> 
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
        
        <nav> 
           <ul> 
               <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
               <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
               <li><a href="#">SemIdeia
  |]
  
  
getAdmR :: Handler Html
getAdmR = defaultLayout $ do
    addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
    addStylesheet  $ StaticR estilo_css

    [whamlet|
     <body class="center clearfix" style="background:#F2F2F2;">
    <h1> Área Restrita !!
    <h3> Apenas administradores estão autorizados 
    <header>
  <nav> 
           <ul> 
               <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
               <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
               <li><a href="#">SemIdeia
  |]
  
  
  
getLogoutR :: Handler Html
getLogoutR = defaultLayout $ do
    addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
    addStylesheet  $ StaticR estilo_css

    [whamlet|
     <body class="center clearfix" style="background:#F2F2F2;">
    <h2> Obrigado por acessar o BitShop
    <h3> Volte sempre !  
    <header>
  <nav> 
           <ul> 
               <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
               <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
               <li><a href="#">SemIdeia
  |]
  
getAutorR :: Handler Html
getAutorR = defaultLayout $ do
    addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
    addStylesheet  $ StaticR estilo_css

    [whamlet|
     <body class="center clearfix" style="background:#F2F2F2;">
    <h1> :)
  <nav> 
           <ul> 
               <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
               <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
               <li><a href="#">SemIdeia
  |]
  
  
getSobreR :: Handler Html
getSobreR = defaultLayout $ do
    addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
    addStylesheet  $ StaticR estilo_css

    [whamlet|
     <body class="center clearfix" style="background:#F2F2F2;">
    <h1> Sobre o BitShop 
  <nav> 
           <ul> 
               <li><a href="https://aula-5-isabelabg.c9users.io/sobre">Sobre
               <li><a href="https://aula-5-isabelabg.c9users.io/autor">Autores
               <li><a href="#">SemIdeia
  |]
  
  
getProdutoR :: Handler Html
getProdutoR = defaultLayout $ do
    addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"
    addStylesheet  $ StaticR estilo1_css
    addStylesheet  $ StaticR style_css
    addStylesheet  $ StaticR jquery_jscrollpane_css
    

   
    [whamlet|
    <head>
        <script type="text/javascript" src="@{StaticR jquery_contentcarousel_js}" >
        <script type="text/javascript" src="@{StaticR jquery_mousewheel_js}" >
    <body class="center clearfix" style="background:#F2F2F2;">
    <header>
   
        <h1> Produtos 
    
    
  <section class="chamada">
     <h2> Bem vindo ! 
         <h3> Esta é a página de compra de produtos , aproveite ! 
  <section class="caixaprincipal">
          <div id="ca-container" class="ca-container">
              <div class="ca-wrapper">
                  <div class="ca-item ca-item-1">
                      <div class="ca-item-main">
                          <img src="http://i.imgur.com/0rhO05a.jpg" height="220" width="220">
                          <h3> Estojo PacMan 
                          <h4>
                          <span class="ca-quote"> <img src"http://i.imgur.com/DLcGf87.png";>
                           <span> Estojo com a estampa do jogo "PacMan" 
                              <a href="#" class="ca-more"> Leia Mais 
          <div class="ca-content-wrapper">
              <div class="ca-content">
                  <h6> Estojo PacMan 
                      <a href="#" class="ca-close">close
                      <div class="ca-content-text">
                          <p> Descrição do produto: Estojo com 1 compartimento,fechos em zíper com puxadores personalizados.
                          <p> Composição   100% poliéster.
                          <p> Cores:  Preto, Vermelho e Branco
                          <p> Dimensões:  22x8x4cm.
                          <p> Preço: ILS 28,00
                          <button style="background:#103754; border:#103754;"> <img src="http://i.imgur.com/FjU6yum.png" height="35">
                                  
    <div class="ca-item ca-item-2">
                      <div class="ca-item-main">
                          <img src="http://i.imgur.com/HVmItOe.jpg" height="220" width="220">
                          <h3> Pikachu
                          <h4>
                          <span class="ca-quote"> <img src"http://i.imgur.com/DLcGf87.png";>
                           <span> Estojo com a estampa do jogo "PacMan" 
                              <a href="#" class="ca-more"> Leia Mais 
          <div class="ca-content-wrapper">
              <div class="ca-content">
                  <h6> Estojo PacMan 
                      <a href="#" class="ca-close">close
                      <div class="ca-content-text">
                          <p> Descrição do produto: Estojo com 1 compartimento,fechos em zíper com puxadores personalizados.
                          <p> Composição   100% poliéster.
                          <p> Cores:  Preto, Vermelho e Branco
                          <p> Dimensões:  22x8x4cm.
                          <p> Preço: ILS 28,00
                          <button style="background:#103754; border:#103754;"> <img src="http://i.imgur.com/FjU6yum.png" height="35">
                                  
    
  <nav> 
           <ul> 
               <li><a href="#">Sobre
               <li><a href="#">Autores
               <li><a href="#">SemIdeia
  |]
  
  
  
getLoginR :: Handler Html
getLoginR  = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  addStylesheetRemote "http://necolas.github.io/normalize.css/+Sans:300,400,600,700,800"

  
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
       t@(Static settings) <- static "static"
       warp 8080 (Pagina t pool)


