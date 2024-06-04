-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (buildServer) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem, signalTSem, waitTSem)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (badRequest400, notFound404, noContent204, notImplemented501, serviceUnavailable503)
import Web.Scotty (ActionM, ScottyM, body, captureParam, get, header, jsonData, notFound, post, raw, scottyApp, setHeader, status, text)
import Web.Scotty.Internal.Types (RoutePattern(Literal))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import Configuration (TlsConfiguration)
import Project (ProjectInfo (ProjectInfo), ProjectState, Owner)

import qualified Configuration as Config
import qualified Github
import qualified WebInterface

-- Router for the web server.
router
  :: [ProjectInfo]
  -> Text
  -> (Github.WebhookEvent -> ActionM ())
  -> (ProjectInfo -> Maybe (IO ProjectState))
  -> (Owner -> IO [(ProjectInfo, ProjectState)])
  -> ScottyM ()
router infos ghSecret serveEnqueueEvent getProjectState getOwnerState = do
  get  "/"                 $ serveIndex infos
  get  styleRoute          $ serveStyles
  post "/hook/github"      $ withSignatureCheck ghSecret $ serveGithubWebhook serveEnqueueEvent
  get  "/hook/github"      $ serveWebhookDocs
  get  "/:owner"           $ serveWebInterfaceOwner getOwnerState
  get  "/:owner/:repo"     $ serveWebInterfaceProject getProjectState
  get  "/api/:owner/:repo" $ serveAPIproject getProjectState
  notFound                 $ serveNotFound

styleRoute :: RoutePattern
styleRoute = Literal $ LT.fromStrict WebInterface.stylesheetUrl

-- Checks the signature (encoded as hexadecimal characters in 'hexDigest') of
-- the message, given the secret, and the actual message bytes.
isSignatureValid :: Text -> Text -> ByteString -> Bool
isSignatureValid secret hexDigest message =
  let actualHmac   = hmac (encodeUtf8 secret) message :: HMAC SHA1
      binaryDigest = Base16.decode $ encodeUtf8 hexDigest
  in  case binaryDigest of
        -- If the hexDigest was not hexadecimal, is was definitely not valid
        Left _ -> False
        Right x -> case digestFromByteString x of
          -- The HMAC type implements a constant-time comparison.
          Just expectedDigest -> (HMAC expectedDigest) == actualHmac
          -- If the hexDigest was not a valid hexadecimally-encoded digest,
          -- the signature was definitely not valid.
          Nothing -> False

-- The X-Hub-Signature header value is prefixed with "sha1=", and then the
-- digest in hexadecimal. Strip off that prefix, and ensure that it has the
-- expected value.
extractHexDigest :: Text -> Maybe Text
extractHexDigest value =
  let (prefix, hexDigest) = Text.splitAt 5 value
  in case prefix of
    "sha1=" -> Just hexDigest
    _       -> Nothing

withSignatureCheck :: Text -> ActionM () -> ActionM ()
withSignatureCheck secret bodyAction = do
  maybeHubSignature <- header "X-Hub-Signature"
  let maybeHexDigest = fmap LT.toStrict maybeHubSignature >>= extractHexDigest
  -- Compute the HMAC as from the body, encode as hexadecimal characters in a
  -- bytestring. Scotty reads headers as Text, so convert the header to a byte
  -- string as well.
  case maybeHexDigest of
    Nothing -> do
      status badRequest400
      text "missing or malformed X-Hub-Signature header"
    Just hexDigest -> do
      bodyBytes <- body
      if isSignatureValid secret hexDigest (LBS.toStrict bodyBytes)
        then bodyAction
        else do
          status badRequest400
          text "signature does not match, is the secret set up properly?"

serveGithubWebhook :: (Github.WebhookEvent -> ActionM ()) -> ActionM ()
serveGithubWebhook serveEnqueueEvent = do
  eventName <- header "X-GitHub-Event"
  case eventName of
    Just "pull_request" -> do
      payload <- jsonData :: ActionM Github.PullRequestPayload
      serveEnqueueEvent $ Github.PullRequest payload
    Just "issue_comment" -> do
      payload <- jsonData :: ActionM Github.CommentPayload
      serveEnqueueEvent $ Github.Comment payload
    Just "pull_request_review" -> do
      payload <- jsonData :: ActionM Github.CommentPayload
      serveEnqueueEvent $ Github.Comment payload
    Just "status" -> do
      payload <- jsonData :: ActionM Github.CommitStatusPayload
      serveEnqueueEvent $ Github.CommitStatus payload
    Just "push" -> do
      payload <- jsonData :: ActionM Github.PushPayload
      serveEnqueueEvent $ Github.Push payload
    Just "ping" ->
      serveEnqueueEvent $ Github.Ping
    Just anEventName -> do
      requestId <- header "X-GitHub-Hook-ID" >>= \mbRequestId ->
        case mbRequestId of
          Just requestId -> pure requestId
          Nothing        -> pure "REQUEST_ID_MISSING"

      -- Manually append "\n" to ensure line buffering for thread-safe logging.
      liftIO $ LT.putStr $
        "Ignored event: " <> anEventName <>
        " - Request ID: " <> requestId <> "\n"

      -- Send a 204 (NoContent) to prevent GitHub interpreting it as an error.
      status noContent204

    Nothing -> do
      status notImplemented501
      text "hook ignored, the event type is not supported"

-- Handles replying to the client when a GitHub webhook is received.
serveTryEnqueueEvent :: (Github.WebhookEvent -> IO Bool)
                     -> Github.WebhookEvent
                     -> ActionM ()
serveTryEnqueueEvent tryEnqueueEvent event = do
  -- Enqueue the event if the queue is not full. Don't block if the queue is
  -- full: instead we don't want to enqueue the event and tell the client to
  -- retry in a while.
  enqueued <- liftIO $ tryEnqueueEvent event
  if enqueued
    then text "hook received"
    else do
      status serviceUnavailable503
      text "webhook event queue full, please try again in a few minutes"

serveWebhookDocs :: ActionM ()
serveWebhookDocs = do
  status badRequest400
  text "expecting POST request at /hook/github"

serveIndex :: [ProjectInfo] -> ActionM ()
serveIndex infos = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  let title = "Hoff"
  raw $ WebInterface.renderPage title $ WebInterface.viewIndex infos

serveStyles :: ActionM ()
serveStyles = do
  setHeader "Content-Type" "text/css; charset=utf-8"
  -- Chrome does not support `immutable`, so also set a long max age.
  setHeader "Cache-Control" "public, max-age=31536000, immutable"
  text $ LT.fromStrict WebInterface.stylesheet

serveWebInterfaceOwner :: (Owner -> IO [(ProjectInfo, ProjectState)]) -> ActionM ()
serveWebInterfaceOwner getOwnerState = do
  owner <- captureParam "owner"
  states <- liftIO $ getOwnerState owner
  setHeader "Content-Type" "text/html; charset=utf-8"
  let title = owner
  raw $ WebInterface.renderPage title $ WebInterface.viewOwner owner states

serveWebInterfaceProject :: (ProjectInfo -> Maybe (IO ProjectState)) -> ActionM ()
serveWebInterfaceProject getProjectState = do
  owner <- captureParam "owner"
  repo  <- captureParam "repo"
  let info = ProjectInfo owner repo
  case getProjectState info of
    Nothing -> do
      status notFound404
      text "not found"
    Just getState -> do
      state <- liftIO $ getState
      setHeader "Content-Type" "text/html; charset=utf-8"
      let title = Text.concat [owner, "/", repo]
      raw $ WebInterface.renderPage title $ WebInterface.viewProject info state


serveAPIproject :: (ProjectInfo -> Maybe (IO ProjectState)) -> ActionM ()
serveAPIproject getProjectState = do
  owner <- captureParam "owner"
  repo  <- captureParam "repo"
  let info = ProjectInfo owner repo
  case getProjectState info of
    Nothing -> do
      status notFound404
      text "not found"
    Just getState -> do
      state <- liftIO $ getState
      setHeader "Content-Type" "application/json; charset=utf-8"
      raw $ Aeson.encode state

serveNotFound :: ActionM ()
serveNotFound = do
  status notFound404
  text "not found"

warpSettings :: Int -> IO () -> Warp.Settings
warpSettings port beforeMainLoop
  = Warp.setPort port
  $ Warp.setBeforeMainLoop beforeMainLoop
  $ Warp.defaultSettings

warpTlsSettings :: TlsConfiguration -> Warp.TLSSettings
warpTlsSettings config =
  Warp.tlsSettings (Config.certFile config) (Config.keyFile config)

-- Runs the a server with TLS if a TLS config was provided, or a normal http
-- server otherwise. Behaves identical to Warp.runSettings after passing the
-- TLS configuration.
runServerMaybeTls :: Maybe TlsConfiguration
                  -> Warp.Settings
                  -> Wai.Application
                  -> IO ()
runServerMaybeTls maybeTlsConfig =
  case maybeTlsConfig of
    Just tlsConfig -> Warp.runTLS $ warpTlsSettings tlsConfig
    Nothing -> Warp.runSettings

-- Runs a webserver at the specified port. When GitHub webhooks are received,
-- an event will be added to the event queue. Returns a pair of two IO
-- operations: (runServer, blockUntilReady). The first should be used to run
-- the server, the second may be used to wait until the server is ready to
-- serve requests.
buildServer :: Int
            -> Maybe TlsConfiguration
            -> [ProjectInfo]
            -> Text
            -> (Github.WebhookEvent -> IO Bool)
            -> (ProjectInfo -> Maybe (IO ProjectState))
            -> (Owner -> IO [(ProjectInfo, ProjectState)])
            -> IO (IO (), IO ())
buildServer port tlsConfig infos ghSecret tryEnqueueEvent getProjectState getOwnerState = do
  -- Create a semaphore that will be signalled when the server is ready.
  readySem <- atomically $ newTSem 0
  let signalReady     = atomically $ signalTSem readySem
      blockUntilReady = atomically $ waitTSem readySem

  let
    -- Make Warp signal the semaphore when it is ready to serve requests.
    settings = warpSettings port signalReady

    serveEnqueueEvent :: Github.WebhookEvent -> ActionM ()
    serveEnqueueEvent = serveTryEnqueueEvent tryEnqueueEvent

  -- Build the Scotty app, but do not start serving yet, as that would never
  -- return, so we wouldn't have the opportunity to return the 'blockUntilReady'
  -- function to the caller.
  app <- scottyApp $ router infos ghSecret serveEnqueueEvent getProjectState getOwnerState
  let runServer = runServerMaybeTls tlsConfig settings app

  -- Return two IO actions: one that will run the server (and never return),
  -- and one that blocks until 'readySem' is signalled from the server.
  return (runServer, blockUntilReady)
