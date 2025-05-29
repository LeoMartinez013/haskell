{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PaginaUm where

import Foundation
import Yesod.Core

getPaginaUmR :: Handler Html
getPaginaUmR = defaultLayout $ do
    setTitle "Pagina Um"
    [whamlet|
        <p>
            Pagina 1
    |]