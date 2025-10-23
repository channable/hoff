-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
{-# LANGUAGE OverloadedStrings #-}

-- This file contains sort-of end-to-end tests for the server. A real server is
-- run, and the events is produces are tested, but they are not fed into an
-- actual event loop. The purpose of these tests it not to test the *logic* of
-- the server (e.g. proper parsing of the request payload), there are unit tests
-- for that.

module ServerSpec (serverSpec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (tryReadTBQueue)
import Control.Monad (replicateM_)
import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple (Response)
import Network.HTTP.Types.Header (Header, HeaderName, hContentType)
import Network.HTTP.Types.Status (badRequest400, noContent204, notFound404, ok200, serviceUnavailable503)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.ByteString.Char8 qualified as ByteString.Strict
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Network.HTTP.Simple qualified as Http

import Server (buildServer)

import Github qualified
import Logic (
  EventQueue,
  newEventQueue,
 )
import Logic qualified as Project
import Project qualified

-- Bring a tiny bit of sense into the Haskell string type madness.
type LazyByteString = ByteString.Lazy.ByteString
type StrictByteString = ByteString.Strict.ByteString

testPort :: Int
testPort = 5273

testHost :: String
testHost = "http://localhost:" ++ (show testPort)

-- Performs an http get request. The host is prepended to the url automatically.
httpGet :: String -> IO (Response LazyByteString)
httpGet url = Http.httpLBS (Http.parseRequest_ (testHost ++ url))

-- Performs an http post request. The host is prepended to the url automatically.
httpPost :: String -> [Header] -> LazyByteString -> IO (Response LazyByteString)
httpPost url headers body = Http.httpLBS r
 where
  r =
    Http.setRequestHeaders headers
      . Http.setRequestMethod "POST"
      . Http.setRequestBodyLBS body
      $ Http.parseRequest_ (testHost ++ url)

hGithubEvent :: HeaderName
hGithubEvent = "X-GitHub-Event"

hGithubSignature :: HeaderName
hGithubSignature = "X-Hub-Signature" -- Not a typo, really 'Hub', not 'GitHub'.

testSecret :: Text
testSecret = "N6MAC41717"

-- Why three different string types? The secret is Text, which will be encoded
-- as utf-8 to provide the key for the mac. The message is the data to be
-- posted, and http-conduit expects a lazy byte string. The result must be put
-- in a http header, and the http-types package chose to use strict bytestrings
-- for those. So yeah, it's a mess.
computeSignature :: Text -> LazyByteString -> StrictByteString
computeSignature secret message =
  let digest = hmac (encodeUtf8 secret) (ByteString.Lazy.toStrict message) :: HMAC SHA1
  in  ByteString.Strict.pack $ "sha1=" ++ show (hmacGetDigest digest)

-- Peforms an http post request for an event with the given body payload. The
-- host is prepended to the url automatically. (Also, three different string
-- types in one signature ... please ecosystem, can we sort this out?)
httpPostGithubEvent :: String -> StrictByteString -> LazyByteString -> IO (Response LazyByteString)
httpPostGithubEvent url eventName body =
  let
    signature = computeSignature testSecret body
    headers =
      [ (hContentType, "application/json")
      , (hGithubEvent, eventName)
      , (hGithubSignature, signature)
      ]
  in
    httpPost url headers body

-- Pops one event from the queue, assuming there is already an event there. This
-- does not block and wait for an event to arrive, because that could make tests
-- deadlock in case an event is never pushed.
popQueue :: Github.EventQueue -> IO Github.WebhookEvent
popQueue = fmap fromJust . atomically . tryReadTBQueue

isPullRequestEvent :: Github.WebhookEvent -> Bool
isPullRequestEvent event = case event of
  Github.PullRequest _ -> True
  _ -> False

isPushEvent :: Github.WebhookEvent -> Bool
isPushEvent event = case event of
  Github.Push _ -> True
  _ -> False

isCommitStatusEvent :: Github.WebhookEvent -> Bool
isCommitStatusEvent event = case event of
  Github.CommitStatus _ -> True
  _ -> False

withServer :: (Github.EventQueue -> Logic.EventQueue -> IO ()) -> IO ()
withServer body = do
  -- Create a github and project event queue with a capacity of 5 events.
  ghQueue <- Github.newEventQueue 5
  projectQueue <- Logic.newEventQueue 5

  let
    info = Project.ProjectInfo "deckard" "voight-kampff"
    tryEnqueue = Github.tryEnqueueEvent ghQueue

    enqueueProjectQueue projectInfo event
      | projectInfo == info = Project.tryEnqueueEvent projectQueue event
      | otherwise = return False

    -- Fake the project state, always return the empty state,
    -- if the project exists.
    getProjectState forProject =
      if forProject == info
        then Just $ pure Project.emptyProjectState
        else Nothing

    getOwnerState forOwner
      | forOwner == Project.owner info = pure [(info, Project.emptyProjectState)]
      | otherwise = pure []

    getHealth = return True

  -- Start the server on the test port, wait until it is ready to handle
  -- requests, and then run the body with access to the queue.
  (runServer, blockUntilReady) <-
    buildServer testPort Nothing [info] testSecret tryEnqueue enqueueProjectQueue getProjectState getOwnerState getHealth
  withAsync runServer $ \_ -> do
    blockUntilReady
    body ghQueue projectQueue
  return ()

serverSpec :: Spec
serverSpec = do
  -- Note: these tests cannot be run in parallel, because the server must
  -- listen on a fixed port. We could give every test its own port, but that
  -- would complicate things unnecessarily.

  describe "The webhook server" $ do
    it "serves something at the root" $
      withServer $ \_ghQueue _projectQueue -> do
        response <- httpGet "/"
        let statusCode = Http.getResponseStatus response
        statusCode `shouldBe` ok200

    it "serves 'not found' at a non-existing url" $
      withServer $ \_ghQueue _projectQueue -> do
        response <- httpGet "/bogus/url"
        let statusCode = Http.getResponseStatus response
        statusCode `shouldBe` notFound404

    it "responds with 'bad request' to a GET for a webhook url" $
      withServer $ \_ghQueue _projectQueue -> do
        response <- httpGet "/hook/github"
        let statusCode = Http.getResponseStatus response
        statusCode `shouldBe` badRequest400

    it "accepts a pull_request webhook" $
      withServer $ \ghQueue _projectQueue -> do
        payload <- ByteString.Lazy.readFile "tests/data/pull-request-payload.json"
        response <- httpPostGithubEvent "/hook/github" "pull_request" payload
        event <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        Http.getResponseBody response `shouldBe` "hook received"
        Http.getResponseStatus response `shouldBe` ok200
        event `shouldSatisfy` isPullRequestEvent

    it "accepts a push webhook" $
      withServer $ \ghQueue _projectQueue -> do
        payload <- ByteString.Lazy.readFile "tests/data/push-payload.json"
        response <- httpPostGithubEvent "/hook/github" "push" payload
        event <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        Http.getResponseBody response `shouldBe` "hook received"
        Http.getResponseStatus response `shouldBe` ok200
        event `shouldSatisfy` isPushEvent

    it "accepts a (commit) status webhook" $
      withServer $ \ghQueue _projectQueue -> do
        payload <- ByteString.Lazy.readFile "tests/data/status-payload.json"
        response <- httpPostGithubEvent "/hook/github" "status" payload
        event <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        Http.getResponseBody response `shouldBe` "hook received"
        Http.getResponseStatus response `shouldBe` ok200
        event `shouldSatisfy` isCommitStatusEvent

    it "serves 503 service unavailable when the queue is full" $
      withServer $ \ghQueue _projectQueue -> do
        payload <- ByteString.Lazy.readFile "tests/data/pull-request-payload.json"

        -- The first five responses should be accepted, which will fill up the
        -- queue (that has a capacity of 5 in these tests).
        replicateM_ 5 $ do
          resp <- httpPostGithubEvent "/hook/github" "pull_request" payload
          Http.getResponseStatus resp `shouldBe` ok200

        -- The next request should therefore be denied.
        resp6 <- httpPostGithubEvent "/hook/github" "pull_request" payload
        Http.getResponseStatus resp6 `shouldBe` serviceUnavailable503

        -- After popping one event, a new request should be allowed.
        _ <- popQueue ghQueue
        resp7 <- httpPostGithubEvent "/hook/github" "pull_request" payload
        Http.getResponseStatus resp7 `shouldBe` ok200

    it "requires an X-Hub-Signature header to be present for webhook calls" $
      withServer $ \_ghQueue _projectQueue -> do
        let headers =
              [ (hContentType, "application/json")
              , (hGithubEvent, "pull_request")
              ]
        response <- httpPost "/hook/github" headers ("{}" :: LazyByteString)
        let
          status = Http.getResponseStatus response
          msg = Http.getResponseBody response
        status `shouldBe` badRequest400
        msg `shouldBe` "missing or malformed X-Hub-Signature header"

    it "requires an X-Hub-Signature header to be valid for webhook calls" $
      withServer $ \_ghQueue _projectQueue -> do
        let headers =
              [ (hContentType, "application/json")
              , (hGithubEvent, "pull_request")
              , -- Provivide the header, but put bogus in it.
                (hGithubSignature, "sha1=not even hexadecimal")
              ]
        response <- httpPost "/hook/github" headers ("{}" :: LazyByteString)
        let
          status = Http.getResponseStatus response
          msg = Http.getResponseBody response
        status `shouldBe` badRequest400
        msg `shouldBe` "signature does not match, is the secret set up properly?"

        -- Now try again, but with the "sha1=" prefix in the signature.
        let headers' = (hGithubSignature, "badc0ffee") : (take 2 headers)
        response' <- httpPost "/hook/github" headers' ("{}" :: LazyByteString)
        let
          status' = Http.getResponseStatus response'
          msg' = Http.getResponseBody response'
        status' `shouldBe` badRequest400
        msg' `shouldBe` "missing or malformed X-Hub-Signature header"

    it "continues serving after receiving an invalid webhook" $
      withServer $ \ghQueue _projectQueue -> do
        -- First send an invalid webhook with a bad payload.
        let badPayload = "this is definitely not valid json"
        badResponse <- httpPostGithubEvent "/hook/github" "status" badPayload
        Http.getResponseStatus badResponse `shouldBe` badRequest400
        -- Now sends a valid one, and verify that an event arrives.
        goodPayload <- ByteString.Lazy.readFile "tests/data/status-payload.json"
        goodResponse <- httpPostGithubEvent "/hook/github" "status" goodPayload
        event <- popQueue ghQueue
        Http.getResponseStatus goodResponse `shouldBe` ok200
        event `shouldSatisfy` isCommitStatusEvent

    it "serves 204 no content for unknown webhooks" $
      withServer $ \_ghQueue _projectQueue -> do
        payload <- ByteString.Lazy.readFile "tests/data/pull-request-payload.json"
        -- Send a webhook event with correct signature, but bogus event name.
        response <- httpPostGithubEvent "/hook/github" "launch_missiles" payload
        let status = Http.getResponseStatus response
        status `shouldBe` noContent204

    it "pauses the train when asked to" $
      withServer $ \_ghQueue projectQueue -> do
        response <- httpPost "/pause/deckard/voight-kampff" [] ""
        let status = Http.getResponseStatus response
        status `shouldBe` ok200
        event <- Project.dequeueEvent projectQueue
        event `shouldBe` Just Project.Pause

    it "unpauses the train when asked to" $
      withServer $ \_ghQueue projectQueue -> do
        response <- httpPost "/resume/deckard/voight-kampff" [] ""
        let status = Http.getResponseStatus response
        status `shouldBe` ok200
        event <- Project.dequeueEvent projectQueue
        event `shouldBe` Just Project.Resume
