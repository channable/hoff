-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WebInterface (renderPage, viewIndex, viewProject) where

import Control.Monad (forM_, void)
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
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Format as Text

import Git (Branch (..), Sha (..))
import Project (BuildStatus, ProjectInfo, ProjectState, Push (..))

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
    h1 "Bob"
    h2 "About"
    p $ do
      void "Bob the build server. See "
      a ! href "https://github.com/channable/hoff" $ "github.com/channable/hoff"
      void " for more information."
    h2 "Tracked repositories"
    mapM_ viewProjectInfo infos

-- Renders the body html for the status page of a project.
viewProject :: ProjectInfo -> ProjectState -> Html
viewProject info state =
  let
    owner = Project.owner info
    repo  = Project.repository info
    repoUrl  = format "https://github.com/{}/{}" (owner, repo)
  in do
    h1 $ do
      a ! href "/" $ toHtml ("bob" :: Text)
      void "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ toHtml repo

    viewProjectQueues info state

-- Render the html for the queues in a project, excluding the header and footer.
viewProjectQueues :: ProjectInfo -> ProjectState -> Html
viewProjectQueues info state = do
  let
    pushes = Project.pushesWithStatus state

  h2 "Builds"
  if null pushes
    then p "There are no builds yet."
    else viewList viewPush info pushes

-- Extract the summary line of a commit, possibly truncated to 52 characters.
commitSummary :: Text -> Text
commitSummary message =
  let
    summaryLine = List.head $ Text.lines message
  in
    if Text.length summaryLine <= 52
      then summaryLine
      else Text.append (Text.take 52 summaryLine) "…"

-- Renders the contents of a list item with a link to the commit log.
viewPush :: ProjectInfo -> Push -> BuildStatus -> Html
viewPush info (Push (Sha sha) ref message author) status =
  let
    Branch unqualifiedRef = Git.unqualify ref
    commitUrl = format "https://github.com/{}/{}/commits/{}"
      (Project.owner info, Project.repository info, sha)
    branchUrl = format "https://github.com/{}/{}/tree/{}"
      (Project.owner info, Project.repository info, unqualifiedRef)
    logsUrl = format "/{}/{}/{}"
      (Project.owner info, Project.repository info, sha)
    statusClass = case status of
      Project.Pending -> "build pending"
      Project.Started -> "build started"
      Project.Succeeded -> "build succeeded"
      Project.Failed -> "build failed"
  in
    p ! class_  statusClass $ do
      a ! href (toValue logsUrl) $ toHtml $ commitSummary message
      span ! class_ "detail" $ do
        a ! href (toValue branchUrl) $ toHtml unqualifiedRef
        " · "
        a ! href (toValue commitUrl) $ toHtml $ Text.take 12 sha
      span ! class_ "detail" $ toHtml $ Text.append "Authored by " author

-- Render all pull requests in the list with the given view function.
viewList
  :: (ProjectInfo -> Push -> BuildStatus -> Html)
  -> ProjectInfo
  -> [(Push, BuildStatus)]
  -> Html
viewList view info pushes = forM_ pushes (uncurry $ view info)
