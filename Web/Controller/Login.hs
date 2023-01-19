module Web.Controller.Login where

import IHP.ModelSupport
import qualified IHP.AuthSupport.Authentication as Auth
import Web.Controller.Prelude
import Web.View.Layout (riotLayout)
import Web.View.Riot.Main
import Application.Helper.RiotData
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.List as L

instance Controller LoginController where
  beforeAction = do
    setLayout riotLayout

  action GetLoginPageAction =
    render MainView {dataUrl = urlTo GetLoginPageDataAction}

  action GetLoginPageDataAction = do
    riotData <- getRiotData Nothing
    renderJson $ toJSON riotData

  action PostLoginDataAction = do
    riotData <- getRiotData Nothing
    renderJson $ toJSON riotData

getRiotData :: (?context::ControllerContext) => Maybe LoginSubmitForm -> IO RiotData
getRiotData maybeForm = do
    translations <- translate
    return $
      RiotData
      { pages = Pages
        { register = Nothing
        , login = Just $ LoginPage
          { form = LoginForm
            { params = LoginFormParams
              { login = ""
              , password = ""
              }
            , errors = LoginFormErrors
              { login = failures "login"
              , password = failures "password"
              , misc = failures "misc"
              }
            , postDataUrl = urlTo PostLoginDataAction
            }
          , registerUrl = urlTo GetRegisterPageAction
          , getRegisterDataUrl = urlTo GetRegisterPageDataAction
          }
        }
      , translations = translations
      }
    where
      failures key = case maybeForm of
                       Just form -> L.map (\(_, v) -> v.message) $ L.filter (\(k, _) -> k == key) form.meta.annotations
                       otherwise -> []
      translate :: IO (KM.KeyMap Text)
      translate = languageMap
        [ Translation {en = "Forgot your password", de = "Passwort vergessen"}
        , Translation {en = "Password", de = "Passwort"}
        , Translation {en = "Register", de = "Registrieren"}
        ]

data LoginSubmitForm'  = LoginSubmitForm {id :: (Id' "loginSubmitForms"), login :: Text, password :: Text, meta :: MetaBag} deriving (Eq, Show)
instance InputValue LoginSubmitForm where inputValue = IHP.ModelSupport.recordToInputValue
type LoginSubmitForm = LoginSubmitForm'
type instance GetTableName (LoginSubmitForm' ) = "loginSubmitForms"
type instance GetModelByTableName "loginSubmitForms" = LoginSubmitForm
type instance GetModelName (LoginSubmitForm' ) = "LoginSubmitForm"
type instance PrimaryKey "loginSubmitForms" = UUID
instance Record LoginSubmitForm where
    {-# INLINE newRecord #-}
    newRecord = LoginSubmitForm def def def def
instance Default (Id' "loginSubmitForms") where def = Id def
instance SetField "id" (LoginSubmitForm' ) (Id' "loginSubmitForms") where
    {-# INLINE setField #-}
    setField newValue (LoginSubmitForm id login password meta) =
        LoginSubmitForm newValue login password (meta { touchedFields = "id" : touchedFields meta })
instance SetField "login" (LoginSubmitForm' ) Text where
    {-# INLINE setField #-}
    setField newValue (LoginSubmitForm id login password meta) =
        LoginSubmitForm id newValue password (meta { touchedFields = "login" : touchedFields meta })
instance SetField "password" (LoginSubmitForm' ) Text where
    {-# INLINE setField #-}
    setField newValue (LoginSubmitForm id login password meta) =
        LoginSubmitForm id login newValue (meta { touchedFields = "password" : touchedFields meta })
instance SetField "meta" (LoginSubmitForm' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (LoginSubmitForm id login password meta) =
        LoginSubmitForm id login password newValue
