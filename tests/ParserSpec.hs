{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import qualified Data.Text as Text
import qualified Test.QuickCheck as QC

import Configuration (ProjectConfiguration (..), TriggerConfiguration (..))
import Parser (ParseResult (..), parseMergeCommand)
import Project (ApprovedFor (..), DeployEnvironment (..), MergeCommand (..), MergeWindow (..))

parserSpec :: Spec
parserSpec = do
  describe "Parser" $ do
    describe "merge commands" $ do
      it "can parse 'merge'" $
        dummyParse "@bot merge" `shouldBe`
          Success (Approve Merge, AnyDay)

      it "can parse 'merge on friday'" $
        dummyParse "@bot merge on friday" `shouldBe`
          Success (Approve Merge, OnFriday)

      it "can parse 'merge as hotfix'" $
        dummyParse "@bot merge as hotfix" `shouldBe`
          Success (Approve Merge, DuringFeatureFreeze)

    describe "tag commands" $ do
      it "can parse 'merge and tag'" $
        dummyParse "@bot merge and tag" `shouldBe`
          Success (Approve MergeAndTag, AnyDay)

      it "can parse a merge window after 'tag'" $ do
        dummyParse "@bot merge and tag on friday" `shouldBe`
          Success (Approve MergeAndTag, OnFriday)

    describe "deploy commands" $ do
      let oneEnvProject = dummyProject { deployEnvironments = Just ["foo"] }
      let oneEnvParse = parseMergeCommand oneEnvProject dummyTrigger

      it "selects the environment implicitly when there is only one" $
        oneEnvParse "@bot merge and deploy" `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "foo"), AnyDay)

      it "allows the environment to be specified when there is only one" $
        oneEnvParse "@bot merge and deploy to foo" `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "foo"), AnyDay)

      it "can parse a merge window after an implicit environment" $
        oneEnvParse "@bot merge and deploy as hotfix" `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "foo"), DuringFeatureFreeze)

      it "allows the environment to be specified when there are multiple" $
        dummyParse "@bot merge and deploy to staging" `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "staging"), AnyDay)

      it "fails when the environment is not specified and ambiguous" $
        dummyParse "@bot merge and deploy" `shouldBe`
          ParseError
            "comment:1:22:\n  |\n1 | @bot merge and deploy\n  |                      ^\n\
            \Merge and deploy has been deprecated. Please use merge and deploy to <environment>\n\
            \where <environment> is one of production, staging\n"

      let noEnvProject = dummyProject { deployEnvironments = Nothing }
      let noEnvParse = parseMergeCommand noEnvProject dummyTrigger

      it "fails when there are no deploy environments" $
        noEnvParse "@bot merge and deploy" `shouldBe`
          ParseError
            "comment:1:22:\n  |\n1 | @bot merge and deploy\n  |                      ^\n\
            \No deployment environments have been configured.\n"

      -- Do we still correctly parse merge windows after a deploy environment
      it "can parse a merge window after an explicit environment" $
        dummyParse "@bot merge and deploy to production on friday" `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "production"), OnFriday)

    describe "retry commands" $ do
      it "can parse 'retry'" $
        dummyParse "@bot retry" `shouldBe`
          Success (Retry, AnyDay)

      it "can parse a merge window after 'retry'" $
        dummyParse "@bot retry on friday" `shouldBe`
          Success (Retry, OnFriday)

    describe "misc features" $ do
      it "ignores messages without the comment prefix" $
        dummyParse "merge and deploy to production" `shouldBe`
          Ignored

      it "accepts commands at the end of other comments" $
        dummyParse "LGTM, @bot merge" `shouldBe`
          Success (Approve Merge, AnyDay)

      it "accepts comments after a command if there is a newline" $
        dummyParse "@bot merge\nLGTM" `shouldBe`
          Success (Approve Merge, AnyDay)

      it "rejects following a command with another command" $
        dummyParse "@bot merge, @bot merge and tag" `shouldBe`
          ParseError
            "comment:1:13:\n  |\n1 | @bot merge, @bot merge and tag\n  |             ^\n\
            \Merge commands may not be followed by anything other than a punctuation character \
            \('.', ',', '!', '?', ':', ';').\n"

      it "rejects following an invalid command with a valid command" $
        dummyParse "@bot looks good to me, @bot merge" `shouldBe`
          ParseError
            "comment:1:6:\n  |\n1 | @bot looks good to me, @bot merge\n  |      ^^^^^\n\
            \unexpected \"looks\"\nexpecting \"merge\", \"retry\", or white space\n"

      it "parses case insensitively" $
        dummyParse "@bot MeRgE aNd TaG oN fRiDaY" `shouldBe`
          Success (Approve MergeAndTag, OnFriday)

      prop "allows trailing punctuation" $
        let genSuffix = Text.pack <$> QC.listOf (QC.elements ".,!?:;")
         in QC.forAll genSuffix $ \suffix ->
              dummyParse ("@bot merge" <> suffix) ==
                Success (Approve Merge, AnyDay)

      it "understands HTML comments" $
        dummyParse
          "@bot <!-- hi --> merge <!-- there --> and <!-- this --> deploy\
          \ <!-- is --> to <!-- a --> production <!-- secret --> on <!-- message --> friday"
          `shouldBe`
          Success (Approve $ MergeAndDeploy (DeployEnvironment "production"), OnFriday)

      -- Giving silly commands to the bot is a highly-valued feature ;)
      it "allows HTML comment chicanery" $
        dummyParse "<!--\n@bot merge\n--> @bot YEET" `shouldBe`
          Success (Approve Merge, AnyDay)

{-
      TODO I would like to change the parser to be able to recognise the hoff
      ignore messages at any point and just bail. We can do that by using the
      recovery / deferred errors for normal parse errors and immediately giving
      up when we see an error of our custom "ignore me" type.

      it "understands ignore comments at the beginning" $ do
        dummyParse "<!-- hoff: ignore --> @bot merge" `shouldBe`
          Ignored

      it "understands ignore comments not at the beginning" $ do
        dummyParse "@ops merge and tag <!-- hoff: ignore -->" `shouldBe`
          Ignored
-}

dummyParse :: Text -> ParseResult (MergeCommand, MergeWindow)
dummyParse = parseMergeCommand dummyProject dummyTrigger

dummyProject :: ProjectConfiguration
dummyProject =
  ProjectConfiguration
    { owner               = "owner"
    , repository          = "repository"
    , branch              = "branch"
    , testBranch          = "test-branch"
    , checkout            = "/dev/null"
    , stateFile           = "/dev/null"
    , checks              = Nothing
    , deployEnvironments  = Just ["production", "staging"]
    }

dummyTrigger :: TriggerConfiguration
dummyTrigger =
  TriggerConfiguration
    { commentPrefix = "@bot" }
