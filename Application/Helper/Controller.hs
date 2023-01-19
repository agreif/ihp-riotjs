module Application.Helper.Controller where

import IHP.ControllerPrelude

import qualified Data.Aeson.Key as K
import qualified Data.List as L
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Media as M

import Application.Helper.RiotData

-- Here you can add functions which are available in all your controllers

languageMap :: (?context :: ControllerContext) => [Translation] -> IO (KM.KeyMap Text)
languageMap translations = do
  language <- userLanguage
  let translator = case language of
                     EN -> #translationEn
                     DE -> #translationDe
  return $
    KM.fromList $ L.map (\tr -> ( K.fromText tr.en, translator tr)) translations

data UserLanguage = DE | EN

userLanguage :: (?context :: ControllerContext) => IO UserLanguage
userLanguage = return language
  where
    defaultLanguage :: UserLanguage
    defaultLanguage = EN
    language :: UserLanguage
    language =
      maybe
      defaultLanguage
      (\header -> maybe
                  defaultLanguage
                  id
                  (M.mapAcceptLanguage languageTuples header)
      )
      (getHeader "Accept-Language")
    languageTuples :: [(M.Language, UserLanguage)]
    languageTuples = [ ("en", EN)
                     , ("de", DE)]
