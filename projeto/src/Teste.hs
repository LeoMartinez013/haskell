{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Teste where

import Foundation
import Yesod.Core

getTesteR :: Handler Html
getTesteR = defaultLayout $ do
    setTitle "Pagina de teste"
    [whamlet|
        <p>
            Hello World.
        <p>
            Paginação
            <a href=@{Pagina1}>
    |]