-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module WebInterface (
  renderPage,
  viewIndex,
  viewProject,
  viewOwner,
  stylesheet,
  stylesheetUrl,

  -- * The following are only exported for testing
  ClassifiedPullRequests (..),
  classifiedPullRequests,
)
where

import Control.Monad (forM_, unless, void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Bifunctor (second)
import Data.ByteArray.Encoding (Base (Base64, Base64URLUnpadded), convertToBase)
import Data.FileEmbed (embedStringFile)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (showVersion)
import Text.Blaze (toValue, (!))
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 (
  Html,
  a,
  body,
  div,
  docTypeHtml,
  h1,
  h2,
  h3,
  head,
  link,
  meta,
  p,
  preEscapedToHtml,
  script,
  span,
  title,
  toHtml,
 )
import Text.Blaze.Html5.Attributes (charset, class_, content, href, id, name, onclick, rel)
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)
import Prelude hiding (div, head, id, span)

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Format (format)
import Git (Sha (..))
import Project (
  Approval (..),
  BuildStatus (..),
  IntegrationStatus (..),
  MergeCommand (..),
  Owner,
  ProjectInfo,
  ProjectState,
  PullRequest (integrationStatus),
  speculativelyFailedPullRequests,
  summarize,
 )
import Types (PullRequestId (..), Username (..))

import Project qualified

import Paths_hoff (version)

-- TODO: Minify this css at inclusion time.
stylesheet :: Text
stylesheet = $(embedStringFile "static/style.css")

stylesheetDigest :: Digest SHA256
stylesheetDigest = hash $ encodeUtf8 stylesheet

-- Render a digest to a text in a given base.
showAs :: Base -> Digest a -> Text
showAs base = decodeUtf8 . convertToBase base

stylesheetUrlDigest :: Text
stylesheetUrlDigest = showAs Base64URLUnpadded stylesheetDigest

stylesheetBase64Digest :: Text
stylesheetBase64Digest = showAs Base64 stylesheetDigest

-- URL to the Google Fonts stylesheet. The family parameter is a pipe-separated
-- list of font families. The display parameter corresponds to the CSS
-- font-display property.
googlefontsUrl :: Text
googlefontsUrl = "https://fonts.googleapis.com/css?family=Source+Sans+Pro&display=swap"

-- URL to host the stylesheet at. Including a digest in this URL means we
-- can set the @Cache-Control: immutable@ header to facilitate caching.
-- That's both less wasteful and easier to implement than 304 responses
-- and ETag headers.
stylesheetUrl :: Text
stylesheetUrl = "/style/" <> stylesheetUrlDigest <> ".css"

jsScript :: Text
jsScript = $(embedStringFile "static/script.js")

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "robots" ! content "noindex, nofollow"
    title $ toHtml pageTitle
    link ! rel "stylesheet" ! href (toValue googlefontsUrl)
    link ! rel "stylesheet" ! href (toValue stylesheetUrl) ! integrity (toValue $ "sha256-" <> stylesheetBase64Digest)
  body $ do
    autoRefreshToggle
    div ! id "content" $ do
      bodyHtml
      p ! class_ "version" $ "Hoff v" <> toHtml (showVersion version)
    script $ preEscapedToHtml jsScript

autoRefreshToggle :: Html
autoRefreshToggle =
  div ! class_ "autoRefresh" $ do
    span ! id "autoRefreshToggle" ! onclick "toggleAutoRefresh();" $ "toggle autoRefresh"

-- Integrity attribute for subresource integrity. Blaze doesn't have
-- this yet, but this is what their implementation would look like.
integrity :: AttributeValue -> Attribute
integrity = attribute "integrity" " integrity=\""

-- Render an "owner/repo" link.
viewProjectInfo :: ProjectInfo -> Html
viewProjectInfo info =
  let
    owner = Project.owner info
    repo = Project.repository info
    ownerUrl = format "/{}" [owner]
    repoUrl = format "/{}/{}" [owner, repo]
  in
    p $ do
      a ! href (toValue ownerUrl) $ (toHtml owner)
      void "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ (toHtml repo)

-- Renders the body html for the index page.
viewIndex :: [ProjectInfo] -> Html
viewIndex infos =
  let
  in  do
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
    repo = Project.repository info
    ownerUrl = format "/{}" [owner]
    repoUrl = format "https://github.com/{}/{}" (owner, repo)
  in
    do
      h1 $ do
        a ! class_ "back" ! href "/" $ void "«"
        a ! href (toValue ownerUrl) $ toHtml owner
        void "\x2009/\x2009" -- U+2009 is a thin space.
        a ! href (toValue repoUrl) $ toHtml repo

      viewProjectQueues info state

viewOwner :: Owner -> [(ProjectInfo, ProjectState)] -> Html
viewOwner owner projects = do
  let ownerUrl = format "https://github.com/{}" [owner]
  h1 $ do
    a ! class_ "back" ! href "/" $ void "«"
    a ! href (toValue ownerUrl) $ toHtml owner
  viewGroupedProjectQueues projects

-- | This record structure contains pull requests classified
--   by each corresponding section in the UI:
--   building, failed, approved or awaiting.
--
-- Use 'classifiedPullRequests' to construct this structure
-- from a 'ProjectState'.
data ClassifiedPullRequests = ClassifiedPullRequests
  { building
    , failed
    , approved
    , awaiting
      :: [(PullRequestId, PullRequest, Project.PullRequestStatus)]
  }
  deriving (Eq, Show)

-- | Given a 'ProjectState', classifies pull requests into
--   the four sections of the UI (building, failed, approved or awaiting)
--   in a 'ClassifiedPullRequests' record.
classifiedPullRequests :: ProjectState -> ClassifiedPullRequests
classifiedPullRequests state =
  ClassifiedPullRequests
    { building = sortPrs $ filterPrs prPending ++ speculativelyFailed
    , failed = sortPrs $ realFailed
    , approved = sortPrs $ filterPrs (== Project.PrStatusApproved)
    , awaiting = reverse $ filterPrs (== Project.PrStatusAwaitingApproval)
    }
 where
  allFailed = filterPrs prFailed
  realFailed = filter (\(pid, _, _) -> pid `notElem` speculativelyFailedIds) allFailed
  speculativelyFailed = filter (\(pid, _, _) -> pid `elem` speculativelyFailedIds) allFailed
  speculativelyFailedIds = speculativelyFailedPullRequests state
  sortPrs = sortOn (\(_, pr, _) -> approvalOrder <$> Project.approval pr)
  filterPrs predicate = filter (\(_, _, status) -> predicate status) pullRequests
  pullRequests = Project.classifyPullRequests state

-- Render the html for the queues in a project, excluding the header and footer.
viewProjectQueues :: ProjectInfo -> ProjectState -> Html
viewProjectQueues info state = do
  let ClassifiedPullRequests{..} = classifiedPullRequests state
  h2 "Building"
  if null building
    then p "There are no builds in progress at the moment."
    else viewList viewPullRequestWithApproval info building

  unless (null approved) $ do
    h2 "Approved"
    viewList viewPullRequestWithApproval info approved

  unless (null failed) $ do
    h2 "Failed"
    viewList viewPullRequestWithApproval info failed

  unless (null awaiting) $ do
    h2 "Awaiting approval"
    viewList viewPullRequest info awaiting

-- Render the html for the queues in a project, excluding the header and footer.
viewGroupedProjectQueues :: [(ProjectInfo, ProjectState)] -> Html
viewGroupedProjectQueues projects = do
  let
    prs = map (second classifiedPullRequests) projects
    only what = filter (not . null . snd) $ map (second what) prs
    onlyBuilding = only building
    onlyApproved = only approved
    onlyFailed = only failed
    onlyAwaiting = only awaiting

  h2 "Building"
  if null onlyBuilding
    then p "There are no builds in progress at the moment."
    else mapM_ (uncurry $ viewList' viewPullRequestWithApproval) onlyBuilding

  unless (null onlyApproved) $ do
    h2 "Approved"
    mapM_ (uncurry $ viewList' viewPullRequestWithApproval) onlyApproved

  unless (null onlyFailed) $ do
    h2 "Failed"
    mapM_ (uncurry $ viewList' viewPullRequestWithApproval) onlyFailed

  unless (null onlyAwaiting) $ do
    h2 "Awaiting approval"
    mapM_ (uncurry $ viewList' viewPullRequest) onlyAwaiting
 where
  viewList'
    :: (ProjectInfo -> PullRequestId -> PullRequest -> Html)
    -> ProjectInfo
    -> [(PullRequestId, PullRequest, status)]
    -> Html
  viewList' view info prs = do
    h3 (toHtml $ Project.repository info)
    forM_ prs $ \(prId, pr, _) -> p $ view info prId pr

-- Renders the contents of a list item with a link for a pull request.
viewPullRequest :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequest info pullRequestId pullRequest = do
  a ! href (toValue $ pullRequestUrl info pullRequestId) $ toHtml $ Project.title pullRequest
  span ! class_ "prId" $ toHtml $ prettyPullRequestId pullRequestId

viewPullRequestWithApproval :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequestWithApproval info prId pullRequest = do
  viewPullRequest info prId pullRequest
  case Project.approval pullRequest of
    Just Approval{approver = Username username, approvedFor = approvalType, approvalRetriedBy = retriedBy} -> do
      span ! class_ "review" $ do
        -- Show approver
        toHtml $ format "Approved for {} by " [approvedAction]
        -- TODO: Link to approval comment, not just username.
        a ! href (toValue $ profileUrl username) $ toHtml username
        -- If the merge was retried, also show who triggered the retry
        forM_ retriedBy $ \(Username retrierUsername) -> do
          toHtml (" (retried by " :: Text)
          a ! href (toValue $ profileUrl retrierUsername) $ toHtml retrierUsername
          toHtml (")" :: Text)
        -- Show build info
        case integrationStatus pullRequest of
          Integrated sha buildStatus -> do
            let
              formatStatus status = case status of
                (BuildStarted ciUrl) -> ciLink ciUrl "🟡"
                (BuildFailed (Just ciUrl)) -> ciLink ciUrl "❌"
                BuildSucceeded -> ciLink (commitUrl info sha) "✅"
                _ -> pure ()
              latestStatus = summarize buildStatus
            span " | "
            case buildStatus of
              Project.AnyCheck status -> formatStatus status
              Project.SpecificChecks statuses -> mapM_ (formatStatus . snd) $ Map.toList statuses
            a ! href (toValue $ commitUrl info sha) $ toHtml $ prettySha sha
            case latestStatus of
              (BuildStarted ciUrl) -> span " | " >> ciLink ciUrl "CI build"
              (BuildFailed (Just ciUrl)) -> span " | " >> ciLink ciUrl "CI build"
              _ -> pure ()
          Conflicted _ _ -> span "  | " >> span "❗ Conflicted"
          IncorrectBaseBranch -> span "  | " >> span "❗ Incorrect base branch"
          Promote _ _ -> span "  | " >> span "Awaiting promotion"
          PromoteAndTag{} -> span "  | " >> span "Awaiting promotion"
          -- Promotions are not actually shown in the interface
          -- as a PR is deleted right after it is promoted.
          -- The case is here so we cover all branches
          -- (and so we are notified in case we add a new constructor).
          Promoted -> span "  | " >> span "🔷 Promoted"
          NotIntegrated -> pure ()
          Outdated -> pure ()
     where
      approvedAction = Project.displayMergeCommand (Approve approvalType)
      profileUrl = Text.append "https://github.com/"
      ciLink url text = do
        a ! href (toValue url) $ text
        span " "
    Nothing ->
      error $
        "Tried to render approval link for pull request "
          ++ (show prId)
          ++ " which was not approved. This is a programming error."

-- Render all pull requests in the list with the given view function.
viewList
  :: (ProjectInfo -> PullRequestId -> PullRequest -> Html)
  -> ProjectInfo
  -> [(PullRequestId, PullRequest, status)]
  -> Html
viewList view info prs = forM_ prs $ \(prId, pr, _) -> p $ view info prId pr

-- | Formats a pull request URL
pullRequestUrl :: ProjectInfo -> PullRequestId -> Text
pullRequestUrl info (PullRequestId n) =
  format
    "https://github.com/{}/{}/pull/{}"
    ( Project.owner info
    , Project.repository info
    , n
    )

commitUrl :: ProjectInfo -> Sha -> Text
commitUrl info (Sha sha) =
  format
    "https://github.com/{}/{}/commit/{}"
    ( Project.owner info
    , Project.repository info
    , sha
    )

-- | Textual rendering of a PullRequestId as #number
prettyPullRequestId :: PullRequestId -> String
prettyPullRequestId (PullRequestId n) = "#" <> show n

-- | Textual rendering of a Sha with just the first 7 characters
prettySha :: Sha -> Text
prettySha (Sha sha) = Text.take 7 sha

prFailed :: Project.PullRequestStatus -> Bool
prFailed Project.PrStatusFailedConflict = True
prFailed Project.PrStatusEmptyRebase = True
prFailed Project.PrStatusWrongFixups = True
prFailed Project.PrStatusIncorrectBaseBranch = True
prFailed (Project.PrStatusFailedBuild _) = True
prFailed _ = False

prPending :: Project.PullRequestStatus -> Bool
prPending Project.PrStatusBuildPending = True
prPending (Project.PrStatusBuildStarted _) = True
-- PrStatusIntegrated here means that the PR successfully built
-- but it has not been promoted to master yet for either of two reasons:
-- 1. this is the split-second between receiving the status and promoting;
-- 2. this PR is not at the head of the merge train,
--    we are waiting for the build status of the previous PR.
prPending Project.PrStatusIntegrated = True
-- A speculative conflict means that the PR is also still "building".
-- The conflict may have well been fault of a previous PR that will eventually
-- fail.  At that moment, this PR will be reintegrated automatically.
prPending Project.PrStatusSpeculativeConflict = True
prPending _ = False
