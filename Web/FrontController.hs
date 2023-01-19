module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Static
import Web.Controller.Register
import Web.Controller.Login

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @RegisterController
        , parseRoute @LoginController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
