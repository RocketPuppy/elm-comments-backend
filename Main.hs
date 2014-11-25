{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Types
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad.IO.Class
import Data.Acid
import Network.HTTP.Types.Status

main = do
    acid <- openLocalState (Articles Map.empty)
    scotty 3000 $ do
        get "/articles" $ do
            titles <- liftIO $ query acid GetTitles
            json titles
        get "/article" $ do
            title <- Title <$> param "title"
            article <- liftIO $ query acid (GetArticleWithTitle title)
            case article of
                Just a -> json a
                Nothing -> status status404
        post "/articles" $ do
            article <- jsonData
            liftIO $ update acid (NewArticle article)
        post "/comments" $ do
            title <- Title <$> param "title"
            index <- read <$> param "index"
            comment <- jsonData
            liftIO $ update acid (PostComment title index comment)
