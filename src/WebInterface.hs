-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WebInterface (renderPage, viewIndex, viewProject) where

import Control.Monad (forM_, unless, void)
import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import Prelude hiding (id, div, head, span)
import Text.Blaze ((!), toValue)
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 (Html, a, body, div, docTypeHtml, h1, h2, head, meta, p, span, style, title, toHtml)
import Text.Blaze.Html5.Attributes (class_, charset, content, href, id, name)

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Format as Text

import Git (Branch (..), Sha (..))
import Project (ProjectInfo, ProjectState, Push (..))

import qualified Git
import qualified Project

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

-- TODO: Minify this css at inclusion time.
stylesheet :: Text
stylesheet = $(embedStringFile "static/style.css")

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "robots" ! content "noindex, nofollow"
    title $ toHtml pageTitle
    style $ toHtml stylesheet
  body $
    div ! id "content" $
      bodyHtml

-- Render an "owner/repo" link.
viewProjectInfo :: ProjectInfo -> Html
viewProjectInfo info =
  let
    owner = Project.owner info
    repo  = Project.repository info
    repoUrl  = format "/{}/{}" [owner, repo]
  in
    p $ do
      a ! href (toValue repoUrl) $ do
        toHtml owner
        void "\x2009/\x2009" -- U+2009 is a thin space.
        toHtml repo

-- Renders the body html for the index page.
viewIndex :: [ProjectInfo] -> Html
viewIndex infos =
  let
  in do
    h1 "Hoff"
    h2 "About"
    p $ do
      void "Hoff is a gatekeeper for your commits. See "
      a ! href "https://github.com/ruuda/hoff" $ "github.com/ruuda/hoff"
      void " for more information."
    h2 "Tracked repositories"
    mapM_ viewProjectInfo infos

-- Renders the body html for the status page of a project.
viewProject :: ProjectInfo -> ProjectState -> Html
viewProject info state =
  let
    owner = Project.owner info
    repo  = Project.repository info
    ownerUrl = format "https://github.com/{}" [owner]
    repoUrl  = format "https://github.com/{}/{}" (owner, repo)
  in do
    h1 $ do
      a ! href (toValue ownerUrl) $ toHtml owner
      void "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ toHtml repo

    viewProjectQueues info state

-- Render the html for the queues in a project, excluding the header and footer.
viewProjectQueues :: ProjectInfo -> ProjectState -> Html
viewProjectQueues info state = do
  let
    building = Project.pushesStarted state
    pending = Project.pushesPending state
    succeeded = Project.pushesSucceeded state
    failed = Project.pushesFailed state

  h2 "Building"
  if null building
    then p "There are no builds in progress at the moment."
    else viewList viewPush info building

  unless (null pending) $ do
    h2 "Pending"
    viewList viewPush info pending

  unless (null succeeded) $ do
    h2 "Succeeded"
    viewList viewPush info succeeded

  unless (null failed) $ do
    h2 "Failed"
    viewList viewPush info failed

-- Renders the contents of a list item with a link to the commit log.
viewPush :: ProjectInfo -> Push -> Html
viewPush info (Push (Sha sha) ref commitTitle author) =
  let
    Branch unqualifiedRef = Git.unqualify ref
    url = format "https://github.com/{}/{}/commits/{}"
      (Project.owner info, Project.repository info, sha)
  in do
    a ! href (toValue url) $ toHtml $ Text.concat [sha, " ", unqualifiedRef, " ", commitTitle]
    span ! class_ "review" $ toHtml $ Text.append "Authored by " author

-- Render all pull requests in the list with the given view function.
viewList
  :: (ProjectInfo -> Push -> Html)
  -> ProjectInfo
  -> [Push]
  -> Html
viewList view info pushes = forM_ pushes (p . view info)
