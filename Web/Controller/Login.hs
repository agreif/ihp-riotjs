module Web.Controller.Login where

import Web.Controller.Prelude
import Web.View.Layout (riotLayout)
import Web.View.Riot.Main
import Application.Helper.RiotData
import qualified Data.Aeson.KeyMap as KM

instance Controller LoginController where
  beforeAction = do
    setLayout riotLayout

  action GetLoginPageAction =
    render MainView {dataUrl = urlTo GetLoginPageDataAction}

  action GetLoginPageDataAction = do
    translations <- translate
    renderJson (toJSON $ RiotData
                { pages = Pages
                  { register = Nothing
                  , login = Just $ LoginPage
                    { form = LoginForm
                      { params = LoginFormParams "a" "b"
                      , errors = LoginFormErrors [] []
                      , postDataUrl = ""
                      }
                    , registerUrl = urlTo GetRegisterPageAction
                    , getRegisterDataUrl = urlTo GetRegisterPageDataAction
                    }
                  }
                , translations = translations
                }
               )
    where
      translate :: IO (KM.KeyMap Text)
      translate = languageMap
                  [ Translation {en = "Forgot your password", de = "Passwort vergessen"}
                  , Translation {en = "Password", de = "Passwort"}
                  , Translation {en = "Register", de = "Registrieren"}
                  ]








data Post = Post {text :: Text}

instance ToJSON Post where
  toJSON post = object ["text" .= post.text]
