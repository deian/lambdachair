{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , OverloadedStrings #-}
module LambdaChair.Policy ( LambdaChairPolicy
                          , withLambdaChairPolicy
                          -- * Rexport hails interface with groups
                          , findAll, findAllP
                          ) where

import           Prelude hiding (lookup)
import           Data.Maybe
import           Data.Monoid
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable

import           Control.Monad

import           LIO
import           LIO.DCLabel
import           Hails.Database
import           Hails.PolicyModule
import           Hails.PolicyModule.Groups
import           Hails.PolicyModule.DSL

import           Hails.Database.Structured hiding (findAll, findAllP)

import           LambdaChair.Models

-- | Internal mappend policy. The type constructor should not be
-- exported to avoid leaking the privilege.
data LambdaChairPolicy = LambdaChairPolicyTCB DCPriv
                   deriving Typeable

instance PolicyModule LambdaChairPolicy where
   initPolicyModule priv = do
     setPolicy priv $ do
       database $ do
         readers ==> True
         writers ==> True
         admins  ==> this

      --
       collection "pc" $ do
         access $ do
           readers ==> True
           writers ==> True
         clearance $ do
           secrecy   ==> this
           integrity ==> True
         document $ \doc -> do
           let (Just u) = fromDocument doc
           readers ==> True
           writers ==> (T.unpack $ userName u) \/ root \/ this
      --
       collection "papers" $ do
         access $ do
           readers ==> True
           writers ==> True
         clearance $ do
           secrecy   ==> this
           integrity ==> True
         document $ \doc -> do
           let (Just p) = fromDocument doc
               owners = map T.unpack $ paperOwners p
               pc = "#commitee_member" :: String
           readers ==> foldr (\/) (pc \/ root \/ this) owners
           writers ==> foldr (\/) (root \/ this) owners
      --
       collection "reviews" $ do
         access $ do
           readers ==> True
           writers ==> True
         clearance $ do
           secrecy   ==> this
           integrity ==> True
         document $ \doc -> do
           r <- fromDocument doc
           let author = T.unpack $ reviewAuthor r
               rid = "#reviewId=" ++ (maybe "" show $ reviewId r)
               rpid = "#reviewPaperId=" ++ (show $ reviewPaper r)
           readers ==> author \/ rid \/ rpid \/ root \/ this
           writers ==> author \/ root \/ this
         field "paper" searchable
      --

     return $ LambdaChairPolicyTCB priv
       where this = privDesc priv
             root = principal "root"

instance DCLabeledRecord LambdaChairPolicy Paper where
  endorseInstance _ = LambdaChairPolicyTCB emptyPriv

instance DCLabeledRecord LambdaChairPolicy Review where
  endorseInstance _ = LambdaChairPolicyTCB emptyPriv

withLambdaChairPolicy :: DBAction a -> DC a
withLambdaChairPolicy act = withPolicyModule $
  \(LambdaChairPolicyTCB _) -> act


--
-- Goups
--

instance Groups LambdaChairPolicy where
  groupsInstanceEndorse = LambdaChairPolicyTCB emptyPriv
  groups _ p pgroup = case () of
    _ | group == "#commitee_member" -> do
      pc <- findAllP p $ select [] "pc"
      return $ map (toPrincipal . userName) pc
    _ | reviewPaperId `S8.isPrefixOf` group -> do
      pc <- (map userName) `liftM` (findAllP p $ select [] "pc")
      let _id = read . S8.unpack $ S8.drop (S8.length reviewPaperId) group
      mpaper <- findBy "papers" "_id" (_id :: ObjectId)
      case mpaper of
        Nothing -> return [pgroup]
        Just paper -> return . map toPrincipal $ pc List.\\ paperConflicts paper
    _ -> return [pgroup]
    where group = principalName pgroup
          toPrincipal = principal . T.unpack
          reviewPaperId = "#reviewPaperId="

--
-- Port over Hails interface to use groups
--

-- | Same as 'findWhereP', but uses groups when retrieving document.
findWhereWithGroupP :: (DCRecord a, MonadDB m) => DCPriv -> Query -> m (Maybe a)
findWhereWithGroupP p query  = liftDB $ do
  mldoc <- findOneP p query
  c <- liftLIO $ getClearance
  case mldoc of
    Just ldoc' -> do ldoc <- labelRewrite (undefined :: LambdaChairPolicy) ldoc'
                     if canFlowToP p (labelOf ldoc) c 
                       then fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                       else return Nothing
    _ -> return Nothing

-- | Same as Hails\' 'findAll', but uses groups
findAll :: (DCRecord a, MonadDB m)
        => Query -> m [a]
findAll = findAllP emptyPriv

-- | Same as Hails\' 'findAllP', but uses groups
findAllP :: (DCRecord a, MonadDB m)
         => DCPriv -> Query -> m [a]
findAllP p query = liftDB $ do
  cursor <- findP p query
  cursorToRecords cursor []
  where cursorToRecords cur docs = do
          mldoc <- nextP p cur
          case mldoc of
            Just ldoc' -> do
              ldoc <- labelRewrite (undefined :: LambdaChairPolicy) ldoc'
              c <- liftLIO $ getClearance
              if canFlowTo (labelOf ldoc) c
                then do md <- fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                        cursorToRecords cur $ maybe docs (:docs) md
                 else cursorToRecords cur docs
            _ -> return $ reverse docs

--
-- DCRecord instances
--

instance DCRecord Paper where
  fromDocument doc = do
    let pid  = lookupObjId "_id" doc
        owners =  fromMaybe [] $ lookup  "owners" doc
    title    <- lookup_ "title" doc
    authors  <- lookup_ "authors" doc
    abstract <- lookup_ "abstract" doc
    let body = case lookup "paper" doc of
                 (Just (d :: BsonDocument)) -> do
                   fn <- lookup "fileName" d
                   ct <- lookup "fileContentType" d
                   c  <- lookup "fileContent" d
                   return $ File { fileName        = fn
                                 , fileContentType = ct
                                 , fileContent     = unBinary c }
                 _ -> Nothing
        conflicts  = fromMaybe [] $ lookup "conflicts" doc
    return Paper { paperId        = pid
                 , paperOwners    = owners
                 , paperTitle     = title
                 , paperAuthors   = authors
                 , paperAbstract  = abstract
                 , paperBody      = body
                 , paperConflicts = conflicts }
      where lookup_ n d = return $ fromMaybe T.empty $ lookup n d
                

  toDocument p = 
    let pid = paperId p
        pre = if isJust pid
               then ["_id" -: fromJust pid]
               else []
        body = maybe [] (\f -> ["paper" -: toDoc f]) $ paperBody p
        toDoc f= [ "fileName" -: fileName f
                 , "fileContentType" -: fileContentType f 
                 , "fileContent" -: Binary (fileContent f)
                 ] :: BsonDocument
    in pre ++ [ "owners"    -: paperOwners p
              , "title"     -: paperTitle p
              , "authors"   -: paperAuthors p
              , "abstract"  -: paperAbstract p
              , "conflicts" -: paperConflicts p ] ++ body

  findWhereP = findWhereWithGroupP

  recordCollection _ = "papers"


instance DCRecord Review where
  fromDocument doc = do
    let rid  = lookupObjId "_id" doc
    author <- lookup "author" doc
    paper  <- lookupObjId "paper" doc
    body   <- lookup_ "body" doc
    return Review { reviewId     = rid
                  , reviewPaper  = paper
                  , reviewAuthor = author
                  , reviewBody   = body }
      where lookup_ n d = return $ fromMaybe T.empty $ lookup n d
                
  toDocument r = 
    let rid = reviewId r
        pre = if isJust rid
               then ["_id" -: fromJust rid]
               else []
    in pre ++ [ "paper"    -: reviewPaper r
              , "author"   -: reviewAuthor r
              , "body"     -: reviewBody r ]

  findWhereP = findWhereWithGroupP

  recordCollection _ = "reviews"


lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads

emptyPriv :: DCPriv
emptyPriv = mempty
