{-# LANGUAGE OverloadedStrings #-}
module LambdaChair.Models (
    User(..)
  , PaperId, getPaperId, Paper(..)
  , ReviewId, getReviewId, Review(..)
  ) where

import           Prelude hiding (lookup)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import           Hails.Data.Hson
import           Hails.Web
import           Hails.Database
import           Hails.Database.Structured

--
-- Committee members
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
-- Papers
--

type PaperId  = Maybe ObjectId

getPaperId :: Paper -> ObjectId
getPaperId = fromJust . paperId

data Paper = Paper { paperId        :: PaperId
                   , paperOwners    :: [UserName]
                   , paperTitle     :: Text
                   , paperAuthors   :: Text
                   , paperAbstract  :: Text
                   , paperBody      :: S8.ByteString
                   , paperConflicts :: [UserName]
                   } deriving (Show, Eq)

--
-- Reviews
--

type ReviewId = Maybe ObjectId

getReviewId :: Review -> ObjectId
getReviewId = fromJust . reviewId

data Review = Review { reviewId     :: ReviewId
                     , reviewPaper  :: ObjectId
                     , reviewAuthor :: UserName
                     , reviewBody   :: Text
                     } deriving (Show, Eq)
