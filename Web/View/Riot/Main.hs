module Web.View.Riot.Main where

import Web.View.Prelude

data MainView = MainView {dataUrl :: Text}

instance View MainView where
    html MainView {..} = [hsx| <body-tag data-url={dataUrl}></body-tag> |]
