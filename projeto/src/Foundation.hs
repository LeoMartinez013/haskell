{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core

data App = App


mkYesodData "App" $(parseRoutesFile "routes.yesodroutes") -- Geração das rotas para o app

instance Yesod App
