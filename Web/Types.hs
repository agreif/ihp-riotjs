module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RegisterController
  = GetRegisterPageAction
  | GetRegisterPageDataAction
  | PostRegisterDataAction
  deriving (Eq, Show, Data)

data LoginController
  = GetLoginPageAction
  | GetLoginPageDataAction
  | PostLoginDataAction
  deriving (Eq, Show, Data)
