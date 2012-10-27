{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module LambdaChair.Controllers where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Maybe

import           Control.Monad

import           LIO

import           Hails.Data.Hson (labeledRequestToHson)
import           Hails.Database (insert_, save, select)
import           Hails.Database.Structured
import           Hails.HttpServer.Types
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import           Network.HTTP.Types

import           LambdaChair.Policy
import           LambdaChair.Models
import           LambdaChair.Views

import LIO.TCB
import LIO.Labeled.TCB

server :: Application
server = mkRouter $ do
  routeName "pc" pcController
  routeName "paper" paperController


pcController :: RESTController
pcController = do
  REST.index $ withUserOrDoAuth $ \usr -> do
    pc <- liftLIO . withLambdaChairPolicy $ findAll $ select [] "pc"
    return $ respondHtml $ indexPC usr pc
  REST.new $ withUserOrDoAuth_ $ return . respondHtml $ newPC
  REST.create $ withUserOrDoAuth_ $ do
    ldoc <- labeledRequestToHson `liftM` request
    usr <- getHailsUser
    liftLIO . withLambdaChairPolicy $ insert_ "pc" ldoc
    return $ redirectTo "/pc"
  REST.show $ redirectTo "/pc"
  REST.edit $ withUserOrDoAuth_ $ do
    (Just uid) <- queryParam "id"
    mpost <- liftLIO . withLambdaChairPolicy $ findBy "pc" "_id" uid
    return $ maybe notFound (respondHtml . editPC) mpost
  REST.update $ withUserOrDoAuth_ $ do
    ldoc <- labeledRequestToHson `liftM` request
    liftLIO . withLambdaChairPolicy $ save "pc" ldoc
    return $ redirectTo "/pc"

paperController :: RESTController
paperController = do
  REST.index $ withUserOrDoAuth $ \usr -> do
    return . okHtml $ L8.pack . T.unpack $ usr
  REST.new $ withUserOrDoAuth $ \usr -> do
    pc <- liftLIO . withLambdaChairPolicy $ findAll $ select [] "pc"
    return $ respondHtml $ newPaper usr pc

--
-- Utils
--

withUserOrDoAuth_ = withUserOrDoAuth . const
