{-# LANGUAGE OverloadedStrings #-}
module LambdaChair.Views where

import Prelude hiding (div, span, head, id)

import LambdaChair.Models

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid (mempty)
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
     div ! class_ "navbar navbar-fixed-top navbar-inverse" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href "/" ! class_ "brand" $ preEscapedToHtml ("&#955;Chair" :: Text)
           ul ! class_ "nav" $ do
             li $ a ! href "/pc" $ "Program Commitee"
             li $ a ! href "/papers" $ "Papers"
             li $ a ! href "/papers/new" $ do
              span ! class_ "icon-plus icon-white" $ ""
              " Submit paper"
     div ! class_ "container" $ content
     script ! src "/static/js/jquery.min.js" $ ""
     script ! src "/static/js/bootstrap.min.js" $ ""

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

--
-- Welcome
--

welcome :: Maybe UserName -> Html
welcome muser = do
  h1 $ "Welcome..."
  

--
-- Paper related
--

newPaper :: UserName -> [User] -> Html
newPaper usr pc = do
  h1 $ "Submit a new paper"
  form ! action "/papers" ! method "POST" {-! enctype "multipart/form-data" -}$ do
    input ! type_ "hidden" ! name "owners[]" ! value (toValue $ usr)
    div $ do
      label ! for "title" $ "Title:"
      input ! type_ "text" ! name "title" ! id "title"
            ! placeholder "Rooter: A Methodology for the Typical Unification of Access Points and Redundancy"
    div $ do
      label ! for "authors" $ "Authors:"
      input ! type_ "text" ! name "authors" ! id "authors" 
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
          input ! type_ "checkbox" ! name "conflict[]" ! id id_
                ! value (toValue $ userName u)
          toHtml $ userFullName u
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Submit paper"

editPaper :: [User] -> Paper -> Html
editPaper pc paper = do
  h1 $ "Edit paper"
  form ! action "/papers" ! method "POST" {-! enctype "multipart/form-data" -}$ do
    input ! type_ "hidden" ! name "_method" ! value "PUT" 
    input ! type_ "hidden" ! name "_id" 
          ! value (toValue $ show $ getPaperId paper)
    forM_ (paperOwners paper) $ \usr ->
      input ! type_ "hidden" ! name "owners[]" ! value (toValue $ usr)
    div $ do
      label ! for "title" $ "Title:"
      input ! type_ "text" ! name "title" ! id "title"
            ! value (toValue $ paperTitle paper)
    div $ do
      label ! for "authors" $ "Authors:"
      input ! type_ "text" ! name "authors" ! id "authors" 
            ! value (toValue $ paperAuthors paper)
    div $ do
      label ! for "abstract" $ "Abstract:"
      textarea ! name "abstract" ! id "abstract" $ toHtml $ paperAbstract paper
    div $ do
      label ! for "paper" $ "Upload paper:"
      input ! type_ "file" ! name "paper" ! id "paper"
    div $ do
      "Program committee members in conflict:"
      forM_ pc $ \u -> do
        let id_ = toValue $ "conflict-"++ (T.unpack $ userName u)
        label ! class_ "checkbox" $ do
          input ! type_ "checkbox" ! name "conflicts[]" ! id id_
                ! value (toValue $ userName u)
                ! if userName u `elem` paperConflicts paper
                    then checked (toValue True)
                    else mempty
          toHtml $ userFullName u
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Update paper"

indexPapers :: [Paper] -> Html
indexPapers ps = do
  h1 $ "Submitted Papers"
  div $ do
    ul ! class_ "nav nav-pills nav-stacked" $ do
      li $ do
        forM_ ps $ \p -> tr $ do
          let mk x = a ! href (toValue $ "/papers/" ++ show (getPaperId p)) $ x
          mk $ do
            toHtml $ paperTitle p
            " by "
            toHtml $ paperAuthors p

showPaper :: UserName -> [Review] -> Paper -> Html
showPaper usr reviews paper = do
  h1 $ toHtml $ paperTitle paper
  h4 $ toHtml $ paperAuthors paper
  div $ blockquote $ p $ toHtml $ paperAbstract paper
  div $ do
    a ! href (toValue $ "/papers/" ++ (show $ getPaperId paper) ++ "/download")
      ! class_ "btn btn-small" $ do
      span ! class_ "icon-eye-open" $ ""
      " View paper"
    unless (haveReview || isOwner) $ do
      " "
      a ! href (toValue $ "/reviews/new?id=" ++ (show $ getPaperId paper))
        ! class_ "btn btn-primary btn-small" $ do
        span ! class_ "icon-pencil icon-white" $ ""
        " Write review"
    when isOwner $ do
      " "
      a ! href (toValue $ "/papers/" ++ (show $ getPaperId paper) ++ "/edit")
        ! class_ "btn btn-primary btn-small" $ do
        span ! class_ "icon-edit icon-white" $ ""
        " Edit paper"
  hr 
  div $ forM_ (zip [1..] reviews) $ \(nr, rev) -> do
    h2 $ toHtml $ "Review #" ++ show nr
    h4 $ do "by "
            toHtml $ reviewAuthor rev
    div $ blockquote $ p $ toHtml $ reviewBody rev
    when (reviewAuthor rev == usr) $ do
      a ! href (toValue $ "/reviews/" ++ (show $ getReviewId rev) ++ "/edit")
        ! class_ "btn btn-primary btn-small" $ do
        span ! class_ "icon-pencil icon-white" $ ""
        " Edit review"
    where haveReview = any ( (==usr) . reviewAuthor) reviews
          isOwner = usr `elem` paperOwners paper


--
-- Reviews related
--

newReview :: UserName -> Paper -> Html
newReview usr paper = do
  h1 $ toHtml $ "Reviewing " ++ T.unpack (paperTitle paper)
  form ! action "/reviews" ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! value (toValue $ usr)
    input ! type_ "hidden" ! name "paper" 
          ! value (toValue . show . getPaperId $ paper)
    div $ do
      label ! for "body" $ "Review:"
      textarea ! name "body" ! id "body" $ ""
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Save review"

editReview :: Review -> Paper -> Html
editReview rev paper = do
  h1 $ toHtml $ "Reviewing " ++ T.unpack (paperTitle paper)
  form ! action "/reviews" ! method "POST" $ do
    input ! type_ "hidden" ! name "_method" ! value "PUT"
    input ! type_ "hidden" ! name "_id"
          ! value (toValue $ show $ getReviewId rev)
    input ! type_ "hidden" ! name "author"
          ! value (toValue $ reviewAuthor rev)
    input ! type_ "hidden" ! name "paper"
          ! value (toValue $ show $ reviewPaper rev)
    div $ do
      label ! for "body" $ "Review:"
      textarea ! name "body" ! id "body" $ toHtml $ reviewBody rev
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Update review"

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
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Create user"

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
                        $ do span ! class_ "icon-wrench" $ ""
                             " Edit"
                 else ""
  when (usr == "root") $ 
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
    div ! class_ "form-actions btn-group" $ do
      input ! type_ "submit" ! class_ "btn-primary" ! value "Update user"
