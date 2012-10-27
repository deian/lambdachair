{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module LambdaChair.Policy ( LambdaChairPolicy
                          , withLambdaChairPolicy
                          ) where

import           Data.Maybe
import qualified Data.Text as T
import           Data.Typeable

import           LIO
import           LIO.DCLabel
import           Hails.Database
import           Hails.PolicyModule
import           Hails.PolicyModule.DSL

import           Hails.Database.Structured

import           LambdaChair.Models

-- | Internal mappend policy. The type constructor should not be
-- exported to avoid leaking the privilege.
data LambdaChairPolicy = LambdaChairPolicyTCB DCPriv
                   deriving Typeable

instance PolicyModule LambdaChairPolicy where
   initPolicyModule priv = do
     setPolicy priv $ do
       database $ do
         readers ==> anybody
         writers ==> anybody
         admins  ==> this

      --
       collection "pc" $ do
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ \doc -> do
           let (Just u) = fromDocument doc
           readers ==> anybody
           writers ==> (T.unpack $ userName u) \/ root \/ this
       --

     return $ LambdaChairPolicyTCB priv
       where this = privDesc priv
             root = principal "root"

withLambdaChairPolicy :: DBAction a -> DC a
withLambdaChairPolicy act = withPolicyModule $ \(LambdaChairPolicyTCB noPrivs) -> act
