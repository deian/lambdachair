{-# LANGUAGE OverloadedStrings #-}
module LambdaChair.Models (
    User(..)
  , PaperId, Paper(..)
  , ReviewId, Review(..)
  ) where

import           Prelude hiding (lookup)
import           Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Hails.Data.Hson
import           Hails.Web
import           Hails.Database
import           Hails.Database.Structured

--
--
--

data User = User { userName     :: UserName
                 , userFullName :: Text
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    usr  <- lookup "_id" doc
    name <- lookup "name" doc
    return User { userName = usr
                , userFullName = name }

  toDocument u = 
        [ "_id"  -: userName u
        , "name" -: userFullName u ]

  recordCollection _ = "pc"

--
--
--

type PaperId  = Maybe ObjectId

data Paper = Paper { paperId        :: PaperId
                   , paperOwner     :: UserName
                   , paperTitle     :: Text
                   , paperAuthors   :: Text
                   , paperAbstract  :: Text
                   , paperBody      :: L8.ByteString
                   , paperConflicts :: [UserName]
                   } deriving (Show, Eq)

--
--
--

type ReviewId = Maybe ObjectId

data Review = Review { reviewId     :: ReviewId
                     , reviewPaper  :: PaperId
                     , reviewAuthor :: UserName
                     , reviewBody   :: Text
                     } deriving (Show, Eq)

