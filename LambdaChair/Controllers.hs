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
import           LIO.DCLabel

import           Hails.Data.Hson (ObjectId, labeledRequestToHson, (-:))
import           Hails.Database (insert_, save, select)
import           Hails.Database.Structured hiding (findAll, findAllP)
import           Hails.HttpServer.Types
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as Frank
import           Network.HTTP.Types

import           LambdaChair.Policy
import           LambdaChair.Models
import           LambdaChair.Views

import LIO.TCB
import LIO.Labeled.TCB

server :: Application
server = mkRouter $ do
  routeTop $ do
    muser <- getHailsUser
    return . respondHtml . welcome $ muser
  -- program chair
  routeName "pc" pcController
  -- papers
  routeName "papers" papersController
  Frank.get "papers/:id/download" $ do
    (Just pid) <- queryParam "id"
    liftLIO . withLambdaChairPolicy $ do
      mpaper <- findBy "papers" "_id" (read . S8.unpack $ pid :: ObjectId)
      return $ case mpaper of
        Just paper | isJust (paperBody paper)->
          let file = fromJust . paperBody $ paper
          in ok (S8.pack . T.unpack  $ fileContentType file) 
                (L8.fromStrict $ fileContent file)
        _  -> notFound
  -- reviews
  routeName "reviews" reviewsController

pcController :: RESTController
pcController = do
  REST.index $ withUserOrDoAuth $ \usr -> do
    pc <- liftLIO . withLambdaChairPolicy $ findAll $ select [] "pc"
    return $ respondHtml $ indexPC usr pc
  REST.new $ withUserOrDoAuth_ $ return . respondHtml $ newPC
  REST.create $ withUserOrDoAuth_ $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLambdaChairPolicy $ insert_ "pc" ldoc
    return $ redirectTo "/pc"
  REST.show $ redirectTo "/pc"
  REST.edit $ withUserOrDoAuth_ $ do
    (Just uid) <- queryParam "id"
    mpost <- liftLIO . withLambdaChairPolicy $ findBy "pc" "_id" uid
    return $ maybe notFound (respondHtml . editPC) mpost
  REST.update $ withUserOrDoAuth_ $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLambdaChairPolicy $ save "pc" ldoc
    return $ redirectTo "/pc"

papersController :: RESTController
papersController = do
  REST.index $ withUserOrDoAuth_ $ do
    ps <- liftLIO . withLambdaChairPolicy $ findAll $ select [] "papers"
    return $ respondHtml $ indexPapers ps
  REST.new $ withUserOrDoAuth $ \usr -> do
    pc <- liftLIO . withLambdaChairPolicy $ findAll $ select [] "pc"
    return $ respondHtml $ newPaper usr pc
  REST.create $ withUserOrDoAuth_ $ do
    req <- request >>= unlabel
    ldoc <- request >>= labeledRequestToHson

    doc <- unlabel ldoc
    liftLIO . ioTCB . putStrLn . show $ doc

    liftLIO . withLambdaChairPolicy $ do
      lrec <- fromLabeledDocument ldoc
      insertLabeledRecord (lrec :: DCLabeled Paper)
    return $ redirectTo "/papers"
  REST.show $ withUserOrDoAuth $ \usr -> do
    (Just pid) <- queryParam "id"
    (mpaper, revs) <- liftLIO . withLambdaChairPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      mp <- findBy "papers" "_id" _id
      revs <- findAll $ select ["paper" -: _id] "reviews"
      revs' <- forM revs $ \rev -> do
        pcM <- findBy "pc" "_id" $ reviewAuthor rev
        return $ maybe rev (\n -> rev { reviewAuthor = userFullName n }) pcM
      return (mp, revs')
    return $ maybe notFound (respondHtml . showPaper usr revs) mpaper
  REST.edit $ withUserOrDoAuth_ $ do
    (Just pid) <- queryParam "id"
    (mpaper, pc) <- liftLIO . withLambdaChairPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      mpaper <- findBy "papers" "_id" _id
      pc <- findAll $ select [] "pc"
      return (mpaper, pc)
    return $ maybe notFound (respondHtml . editPaper pc) mpaper
  REST.update $ withUserOrDoAuth_ $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLambdaChairPolicy $ do
      lrec <- fromLabeledDocument ldoc
      saveLabeledRecord (lrec :: DCLabeled Paper)
      rec <- unlabel lrec
      return $ redirectTo $ "/papers/" ++ show (getPaperId rec)

reviewsController :: RESTController
reviewsController = do
  REST.new $ withUserOrDoAuth $ \usr -> do
    (Just pid) <- queryParam "id"
    mpaper <- liftLIO . withLambdaChairPolicy $
                findBy "papers" "_id" (read . S8.unpack $ pid :: ObjectId)
    return $ maybe notFound (respondHtml . newReview usr) mpaper
  REST.create $ withUserOrDoAuth_ $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLambdaChairPolicy $ do
      lrec <- fromLabeledDocument ldoc
      insertLabeledRecord (lrec :: DCLabeled Review)
      rec <- unlabel lrec
      return $ redirectTo $ "/papers/" ++ show (reviewPaper rec)
  REST.edit $ withUserOrDoAuth_ $ do
    (Just rid) <- queryParam "id"
    liftLIO . withLambdaChairPolicy $ do
      mrev <- findBy "reviews" "_id" (read . S8.unpack $ rid :: ObjectId)
      case mrev of
        Nothing -> return notFound
        Just rev -> do
          mpaper <- findBy "papers" "_id" $ reviewPaper rev
          return $ maybe notFound (respondHtml . editReview rev) mpaper
  REST.update $ withUserOrDoAuth_ $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLambdaChairPolicy $ do
      lrec <- fromLabeledDocument ldoc
      saveLabeledRecord (lrec :: DCLabeled Review)
      rec <- unlabel lrec
      doc <- unlabel ldoc
      return $ redirectTo $ "/papers/" ++ show (reviewPaper rec)

--
-- Utils
--

withUserOrDoAuth_ = withUserOrDoAuth . const
