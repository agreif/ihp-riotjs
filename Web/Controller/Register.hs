module Web.Controller.Register where

import IHP.ModelSupport
import qualified IHP.AuthSupport.Authentication as Auth
import Web.Controller.Prelude
import qualified Web.Controller.Login (getRiotData)
import Web.View.Layout (riotLayout)
import Web.View.Riot.Main
import Application.Helper.RiotData
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.List as L

instance Controller RegisterController where
  beforeAction = do
    setLayout riotLayout

  action GetRegisterPageAction =
    render MainView {dataUrl = urlTo GetRegisterPageDataAction}

  action GetRegisterPageDataAction = do
    riotData <- getRiotData Nothing
    renderJson $ toJSON riotData

  action PostRegisterDataAction = do
    let form = newRecord @RegisterSubmitForm
    riotData <- form
                |> buildForm
                |> ifValid
                \case
                  Left form -> getRiotData $ Just form
                  Right form -> do
                    buildUser form >>= createRecord
                    Web.Controller.Login.getRiotData Nothing
    renderJson $ toJSON riotData
    where
      buildForm form =
        form
        |> fill @["login", "email", "password"]
        |> validateField #login (hasMinLength 2)
        |> validateField #email nonEmpty
        |> validateField #password nonEmpty
      buildUser form = do
        passwordHash <- Auth.hashPassword form.password
        return $
          newRecord @User
          |> set #login form.login
          |> set #email form.email
          |> set #passwordHash passwordHash


getRiotData :: (?context :: ControllerContext) => Maybe RegisterSubmitForm -> IO RiotData
getRiotData maybeForm = do
  translations <- translate
  return $
    RiotData
    { pages = Pages
      { register = Just $ RegisterPage
        { form = RegisterForm
          { params = RegisterFormParams
            { login = ""
            , email = ""
            , password = ""
            }
          , errors = RegisterFormErrors
            { login = failures "login"
            , email = failures "email"
            , password = failures "password"
            , misc = failures "misc"
            }
          , postDataUrl = urlTo PostRegisterDataAction
          }
        , loginUrl = urlTo GetLoginPageAction
        , getLoginDataUrl = urlTo GetLoginPageDataAction
        }
      , login = Nothing
      }
    , translations = translations
    }
    where
      failures key = case maybeForm of
                       Just form -> L.map (\(_, v) -> v.message) $ L.filter (\(k, _) -> k == key) form.meta.annotations
                       otherwise -> []
      translate :: IO (KM.KeyMap Text)
      translate = languageMap
        [ Translation {en = "Forgot your password", de = "Forgot your password"}
        , Translation {en = "Password", de = "Password"}
        , Translation {en = "Register", de = "Register"}
        ]

data RegisterSubmitForm'  = RegisterSubmitForm {id :: (Id' "registerSubmitForms"), login :: Text, email :: Text, password :: Text, meta :: MetaBag} deriving (Eq, Show)
instance InputValue RegisterSubmitForm where inputValue = IHP.ModelSupport.recordToInputValue
type RegisterSubmitForm = RegisterSubmitForm'
type instance GetTableName (RegisterSubmitForm' ) = "registerSubmitForms"
type instance GetModelByTableName "registerSubmitForms" = RegisterSubmitForm
type instance GetModelName (RegisterSubmitForm' ) = "RegisterSubmitForm"
type instance PrimaryKey "registerSubmitForms" = UUID
instance Record RegisterSubmitForm where
    {-# INLINE newRecord #-}
    newRecord = RegisterSubmitForm def def def def  def
instance Default (Id' "registerSubmitForms") where def = Id def
instance SetField "id" (RegisterSubmitForm' ) (Id' "registerSubmitForms") where
    {-# INLINE setField #-}
    setField newValue (RegisterSubmitForm id login email password meta) =
        RegisterSubmitForm newValue login email password (meta { touchedFields = "id" : touchedFields meta })
instance SetField "login" (RegisterSubmitForm' ) Text where
    {-# INLINE setField #-}
    setField newValue (RegisterSubmitForm id login email password meta) =
        RegisterSubmitForm id newValue email password (meta { touchedFields = "login" : touchedFields meta })
instance SetField "email" (RegisterSubmitForm' ) Text where
    {-# INLINE setField #-}
    setField newValue (RegisterSubmitForm id login email password meta) =
        RegisterSubmitForm id login newValue password (meta { touchedFields = "email" : touchedFields meta })
instance SetField "password" (RegisterSubmitForm' ) Text where
    {-# INLINE setField #-}
    setField newValue (RegisterSubmitForm id login email password meta) =
        RegisterSubmitForm id login email newValue (meta { touchedFields = "password" : touchedFields meta })
instance SetField "meta" (RegisterSubmitForm' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (RegisterSubmitForm id login email password meta) =
        RegisterSubmitForm id login email password newValue
