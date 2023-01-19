module Application.Helper.RiotData where

import IHP.ControllerPrelude

import qualified Data.Aeson.KeyMap as KM

-- RIOT DATA

data RiotData = RiotData
  { pages :: Pages,
    translations :: KM.KeyMap Text
  }
instance ToJSON RiotData where
  toJSON o = object
    [ "pages" .= o.pages
    , "translations" .= o.translations
    ]

data Pages = Pages
  { register :: Maybe RegisterPage
  , login :: Maybe LoginPage
  }
instance ToJSON Pages where
  toJSON o = object
    [ "register" .= o.register
    , "login" .= o.login
    ]

data Translation = Translation
  { en :: Text
  , de :: Text
  }
instance IsLabel "translationDe" (Translation -> Text) where
    fromLabel = de
instance IsLabel "translationEn" (Translation -> Text) where
    fromLabel = en


-- REGISTER PAGE

data RegisterPage = RegisterPage
  { form :: RegisterForm
  , loginUrl :: Text
  , getLoginDataUrl :: Text
  }
instance ToJSON RegisterPage where
  toJSON o = object
    [ "form" .= o.form
    , "loginUrl" .= o.loginUrl
    , "getLoginDataUrl" .= o.getLoginDataUrl
    ]

data RegisterForm = RegisterForm
  { params :: RegisterFormParams
  , errors :: RegisterFormErrors
  , postDataUrl :: Text
  }
instance ToJSON RegisterForm where
  toJSON o = object
    [ "params" .= o.params
    , "errors" .= o.errors
    , "postDataUrl" .= o.postDataUrl
    ]

data RegisterFormParams = RegisterFormParams
  { login :: Text
  , email :: Text
  , password :: Text
  }
instance ToJSON RegisterFormParams where
  toJSON o = object
    [ "login" .= o.login
    , "email" .= o.email
    , "password" .= o.password
    ]

data RegisterFormErrors = RegisterFormErrors
  { login :: [Text]
  , email :: [Text]
  , password :: [Text]
  , misc :: [Text]
  }
instance ToJSON RegisterFormErrors where
  toJSON o = object
    [ "login" .= o.login
    , "email" .= o.email
    , "password" .= o.password
    , "misc" .= o.misc
    ]

-- LOGIN PAGE

data LoginPage = LoginPage
  { form :: LoginForm
  , registerUrl :: Text
  , getRegisterDataUrl :: Text
  }
instance ToJSON LoginPage where
  toJSON o = object
    [ "form" .= o.form
    , "registerUrl" .= o.registerUrl
    , "getRegisterDataUrl" .= o.getRegisterDataUrl
    ]

data LoginForm = LoginForm
  { params :: LoginFormParams
  , errors :: LoginFormErrors
  , postDataUrl :: Text
  }
instance ToJSON LoginForm where
  toJSON o = object
    [ "params" .= o.params
    , "errors" .= o.errors
    , "postDataUrl" .= o.postDataUrl
    ]

data LoginFormParams = LoginFormParams
  { login :: Text
  , password :: Text
  }
instance ToJSON LoginFormParams where
  toJSON o = object
    [ "login" .= o.login
    , "password" .= o.password
    ]

data LoginFormErrors = LoginFormErrors
  { login :: [Text]
  , password :: [Text]
  , misc :: [Text]
  }
instance ToJSON LoginFormErrors where
  toJSON o = object
    [ "login" .= o.login
    , "password" .= o.password
    , "misc" .= o.misc
    ]

