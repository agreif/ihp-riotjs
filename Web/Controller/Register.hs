module Web.Controller.Register where

import Web.Controller.Prelude
import Web.View.Layout (riotLayout)
import Web.View.Riot.Main
import Application.Helper.RiotData
import qualified Data.Aeson.KeyMap as KM

instance Controller RegisterController where
  beforeAction = do
    setLayout riotLayout

  action GetRegisterPageAction =
    render MainView {dataUrl = urlTo GetRegisterPageDataAction}

  action GetRegisterPageDataAction = do
    translations <- translate
    renderJson (toJSON $ RiotData
                { pages = Pages
                  { register = Just $ RegisterPage
                    { form = RegisterForm
                      { params = RegisterFormParams "a" "b" "c"
                      , errors = RegisterFormErrors [] [] []
                      }
                    , loginUrl = urlTo GetLoginPageAction
                    , getLoginDataUrl = urlTo GetLoginPageDataAction
                    }
                  , login = Nothing
                  }
                , translations = translations
                }
               )
    where
      translate :: IO (KM.KeyMap Text)
      translate = languageMap
                    [ Translation {en = "Forgot your password", de = "Forgot your password"}
                    , Translation {en = "Password", de = "Password"}
                    , Translation {en = "Register", de = "Register"}
                    ]
