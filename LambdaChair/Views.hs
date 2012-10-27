{-# LANGUAGE OverloadedStrings #-}
module LambdaChair.Views where

import Prelude hiding (div, span, head, id)

import LambdaChair.Models

import Control.Monad
import qualified Data.Text as T
import Data.Maybe
import Hails.Web hiding (body)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)
import Text.Blaze.Html.Renderer.Utf8

respondHtml content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title "LambdaChair"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
  body $ do
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.min.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

--
-- Paper related
--

newPaper :: UserName -> [User] -> Html
newPaper usr pc = do
  h1 $ "Submit a new paper"
  form ! action "/papers" ! method "POST" $ do
    input ! type_ "hidden" ! name "_id" ! value (toValue $ usr)
    div $ do
      label ! for "title" $ "Title:"
      input ! type_ "text" ! name "title" ! id "title"
            ! placeholder "Rooter: A Methodology for the Typical Unification of Access Points and Redundancy"
    div $ do
      label ! for "author" $ "Authors:"
      input ! type_ "text" ! name "author" ! id "author" 
            ! placeholder "Jeremy Stribling, Daniel Aguayo and Maxwell Krohn"
    div $ do
      label ! for "abstract" $ "Abstract:"
      textarea ! name "abstract" ! id "abstract" $ ""
    div $ do
      label ! for "paper" $ "Upload paper:"
      input ! type_ "file" ! name "paper" ! id "paper"
    div $ do
      "Program committee members in conflict:"
      forM_ pc $ \u -> do
        let id_ = toValue $ "conflict-"++ (T.unpack $ userName u)
        label ! class_ "checkbox" $ do
          input ! type_ "checkbox" ! name "conflict" ! id id_
                ! value (toValue $ userName u)
          toHtml $ userFullName u
    p $ input ! type_ "submit" ! value "Submit paper"

--
-- PC related
--

newPC :: Html
newPC = do
  h1 $ "New PC member"
  form ! action "/pc" ! method "POST" $ do
    div $ do
      label ! for "_id" $ "User name:"
      input ! type_ "text" ! name "_id" ! id "_id"
            ! placeholder "ron_swanson"
    div $ do
      label ! for "name" $ "Full Name:"
      input ! type_ "text" ! name "name" ! id "name" 
            ! placeholder "Ron Swanson"
    p $ input ! type_ "submit" ! value "Create user"

indexPC :: UserName -> [User] -> Html
indexPC usr pc = do
  h1 $ "Commitee members"
  div $ do
    table ! class_ "table table-condensed" $ do
      thead $ tr $ do
        th "User Name"
        th "Full Name"
        td ""
      tbody $ do
        forM_ pc $ \u -> tr $ do
          td $ do
            toHtml $ userName u
          td $ toHtml $ userFullName u
          td $ if usr `elem` ["root", userName u]
                 then a ! href (toValue $ "/pc/" ++ (T.unpack $ userName u) ++ "/edit")
                        ! class_ "icon-wrench" $ ""
                 else ""
  div ! class_ "form-actions" $ do
    a ! href "/pc/new" ! class_ "btn btn-primary btn-small" $ do
      span ! class_ "icon-plus icon-white" $ ""
      " Add new member"

editPC :: User -> Html
editPC u = do
  h1 $ toHtml $ userName u
  form ! action "/pc" ! method "POST" $ do
    input ! type_ "hidden" ! name "_method" ! value "PUT"
    input ! type_ "hidden" ! name "_id" ! value (toValue $ userName u)
    div $ do
      label ! for "name" $ "Full Name:"
      input ! type_ "text" ! name "name" ! id "name" 
            ! value (toValue $ userFullName u)
    p $ input ! type_ "submit" ! value "Update user"
