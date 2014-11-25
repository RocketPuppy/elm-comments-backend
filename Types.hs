{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies, TemplateHaskell, OverloadedStrings #-}
module Types where

import Data.Acid
import qualified Data.Map.Strict as Map
import Data.SafeCopy
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH

data Comment = Comment { author :: Author, message :: String }
    deriving (Typeable)
newtype Title = Title String
    deriving (Typeable, Eq, Ord)
newtype Author = Author String
    deriving (Typeable)

data Paragraph = Paragraph [Comment] String
    deriving (Typeable)

data Article = Article Author Title [Paragraph]
    deriving (Typeable)

newtype Articles = Articles (Map.Map Title Article)
    deriving (Typeable)

getMap :: Articles -> Map.Map Title Article
getMap (Articles m) = m

addComment :: Int -> [Paragraph] -> Comment -> [Paragraph]
addComment paragraphIndex paras comment = paras'
    where (heads, tails) = splitAt paragraphIndex paras
          ((Paragraph comments p), tails') = (head tails, tail tails)
          paras' = heads ++ [Paragraph (comments ++ [comment]) p] ++ tails'

$(deriveSafeCopy 0 'base ''Title)
$(deriveSafeCopy 0 'base ''Author)
$(deriveSafeCopy 0 'base ''Comment)
$(deriveSafeCopy 0 'base ''Paragraph)
$(deriveSafeCopy 0 'base ''Article)
$(deriveSafeCopy 0 'base ''Articles)

$(deriveToJSON defaultOptions ''Article)
$(deriveJSON defaultOptions ''Title)
$(deriveJSON defaultOptions ''Comment)
$(deriveToJSON defaultOptions ''Paragraph)
$(deriveJSON defaultOptions ''Author)

instance FromJSON Article where
    parseJSON (Object v) = Article <$> (Author <$> v .: "author") <*> (Title <$> v .: "title") <*> (map (Paragraph []) . lines <$> v.: "text")
    parseJSON _ = mzero

newArticle :: Article -> Update Articles ()
newArticle a@(Article _ title _) = modify (Articles . Map.insert title a . getMap)

getTitles :: Query Articles [Title]
getTitles = Map.keys . getMap <$> ask

getArticleWithTitle :: Title -> Query Articles (Maybe Article)
getArticleWithTitle title = Map.lookup title . getMap <$> ask

postComment :: Title -> Int -> Comment -> Update Articles ()
postComment title paragraphIndex comment = do
    map <- getMap <$> get
    case (Map.lookup title map) of
        Just (Article author' title paras) -> put (Articles (Map.insert title (Article author' title (addComment paragraphIndex paras comment)) map))
        Nothing -> put (Articles map)

$(makeAcidic ''Articles ['newArticle, 'getArticleWithTitle, 'postComment, 'getTitles])
