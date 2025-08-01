{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Data.Text qualified as Text
import Test.QuickCheck qualified as QC

import Configuration (ProjectConfiguration (..), TriggerConfiguration (..))
import Parser (ParseResult (..), parseMergeCommand)
import Project (
  ApprovedFor (..),
  DeployEnvironment (..),
  DeploySubprojects (..),
  MergeCommand (..),
  MergeWindow (..),
  Priority (..),
 )

parserSpec :: Spec
parserSpec = do
  describe "Parser" $ do
    describe "merge commands" $ do
      it "can parse 'merge'" $
        dummyParse "@bot merge"
          `shouldBe` Success (Approve Merge, AnyDay, Normal)

      it "can parse 'merge on friday'" $
        dummyParse "@bot merge on friday"
          `shouldBe` Success (Approve Merge, OnFriday, Normal)

      it "can parse 'merge as hotfix'" $
        dummyParse "@bot merge as hotfix"
          `shouldBe` Success (Approve Merge, DuringFeatureFreeze, Normal)

      it "can parse 'with priority'" $
        dummyParse "@bot merge with priority"
          `shouldBe` Success (Approve Merge, AnyDay, High)

      it "can parse 'with priority' on friday" $
        dummyParse "@bot merge on friday with priority"
          `shouldBe` Success (Approve Merge, OnFriday, High)

    describe "tag commands" $ do
      it "can parse 'merge and tag'" $
        dummyParse "@bot merge and tag"
          `shouldBe` Success (Approve MergeAndTag, AnyDay, Normal)

      it "can parse a merge window after 'tag'" $ do
        dummyParse "@bot merge and tag on friday"
          `shouldBe` Success (Approve MergeAndTag, OnFriday, Normal)

    describe "deploy commands" $ do
      let oneEnvProject = dummyProject{deployEnvironments = Just ["foo"]}
      let oneEnvParse = parseMergeCommand oneEnvProject dummyTrigger

      it "selects the environment implicitly when there is only one" $
        oneEnvParse "@bot merge and deploy"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "foo"), AnyDay, Normal)

      it "allows the environment to be specified when there is only one" $
        oneEnvParse "@bot merge and deploy to foo"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "foo"), AnyDay, Normal)

      it "can parse a merge window after an implicit environment" $
        oneEnvParse "@bot merge and deploy as hotfix"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "foo"), DuringFeatureFreeze, Normal)

      it "can parse a priority after an implicit environment" $
        oneEnvParse "@bot merge and deploy with priority"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "foo"), AnyDay, High)

      it "allows the environment to be specified when there are multiple" $
        dummyParse "@bot merge and deploy to staging"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "staging"), AnyDay, Normal)

      it "fails when the environment is not specified and ambiguous" $
        dummyParse "@bot merge and deploy"
          `shouldBe` ParseError
            "comment:1:22:\n  |\n1 | @bot merge and deploy\n  |                      ^\n\
            \Merge and deploy has been deprecated. Please use merge and deploy to <environment>\n\
            \where <environment> is one of production, staging\n"

      let noEnvProject = dummyProject{deployEnvironments = Nothing}
      let noEnvParse = parseMergeCommand noEnvProject dummyTrigger

      it "fails when there are no deploy environments" $
        noEnvParse "@bot merge and deploy"
          `shouldBe` ParseError
            "comment:1:22:\n  |\n1 | @bot merge and deploy\n  |                      ^\n\
            \No deployment environments have been configured.\n"

      it "can parse a merge window after an explicit environment" $
        dummyParse "@bot merge and deploy to production on friday"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "production"), OnFriday, Normal)

      it "can parse a priority after an explicit environment" $
        dummyParse "@bot merge and deploy to production with priority"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "production"), AnyDay, High)

      it "allows a specific subproject to be deployed" $
        dummyParse "@bot merge and deploy aaa to production"
          `shouldBe` Success (Approve $ MergeAndDeploy (OnlySubprojects ["aaa"]) (DeployEnvironment "production"), AnyDay, Normal)

      it "allows many specific subprojects to be deployed" $
        dummyParse "@bot merge and deploy aaa, bbb to production"
          `shouldBe` Success (Approve $ MergeAndDeploy (OnlySubprojects ["aaa", "bbb"]) (DeployEnvironment "production"), AnyDay, Normal)

      it "fails when an unknown subproject is specified" $
        dummyParse "@bot merge and deploy ccc to production"
          `shouldBe`
          -- I'm not super happy with this error message, it _should_ say that we expect to see
          -- either 'to', or the name of a known subproject.
          ParseError
            "comment:1:23:\n  |\n1 | @bot merge and deploy ccc to production\n\
            \  |                       ^^\nunexpected \"cc\"\nexpecting \"to\" or white space\n"

      it "allows subprojects to be specified with an implicit environment" $
        oneEnvParse "@bot merge and deploy aaa"
          `shouldBe` Success (Approve $ MergeAndDeploy (OnlySubprojects ["aaa"]) (DeployEnvironment "foo"), AnyDay, Normal)

      it "allows subprojects, an implicit environment, and a merge window" $
        oneEnvParse "@bot merge and deploy bbb on friday"
          `shouldBe` Success (Approve $ MergeAndDeploy (OnlySubprojects ["bbb"]) (DeployEnvironment "foo"), OnFriday, Normal)

      let prefixProject =
            dummyProject
              { deployEnvironments = Just ["foo", "fooooooo"]
              , deploySubprojects = Just ["bar", "barrrrrr"]
              }
      let prefixParse = parseMergeCommand prefixProject dummyTrigger

      it "allows environment names to be prefixes of each other" $
        prefixParse "@bot merge and deploy to fooooooo"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "fooooooo"), AnyDay, Normal)

      it "allows subproject names to be prefixes of each other" $
        prefixParse "@bot merge and deploy barrrrrr to fooooooo"
          `shouldBe` Success (Approve $ MergeAndDeploy (OnlySubprojects ["barrrrrr"]) (DeployEnvironment "fooooooo"), AnyDay, Normal)

    describe "retry commands" $ do
      it "can parse 'retry'" $
        dummyParse "@bot retry"
          `shouldBe` Success (Retry, AnyDay, Normal)

      it "can parse a merge window after 'retry'" $
        dummyParse "@bot retry on friday"
          `shouldBe` Success (Retry, OnFriday, Normal)

    describe "misc features" $ do
      it "ignores messages without the comment prefix" $
        dummyParse "merge and deploy to production"
          `shouldBe` Ignored

      it "accepts commands at the end of other comments" $
        dummyParse "LGTM, @bot merge"
          `shouldBe` Success (Approve Merge, AnyDay, Normal)

      it "accepts comments after a command if there is a newline" $
        dummyParse "@bot merge\nLGTM"
          `shouldBe` Success (Approve Merge, AnyDay, Normal)

      it "rejects following a command with another command" $
        dummyParse "@bot merge, @bot merge and tag"
          `shouldBe` ParseError
            "comment:1:13:\n  |\n1 | @bot merge, @bot merge and tag\n  |             ^\n\
            \Merge commands may not be followed by anything other than a punctuation character \
            \('.', ',', '!', '?', ':', ';').\n"

      it "rejects following an invalid command with a valid command" $
        dummyParse "@bot looks good to me, @bot merge"
          `shouldBe` ParseError
            "comment:1:6:\n  |\n1 | @bot looks good to me, @bot merge\n  |      ^^^^^\n\
            \unexpected \"looks\"\nexpecting \"merge\", \"retry\", or white space\n"

      it "parses case insensitively" $
        dummyParse "@bot MeRgE aNd TaG oN fRiDaY"
          `shouldBe` Success (Approve MergeAndTag, OnFriday, Normal)

      prop "allows trailing punctuation" $
        let genSuffix = Text.pack <$> QC.listOf (QC.elements ".,!?:;")
        in  QC.forAll genSuffix $ \suffix ->
              dummyParse ("@bot merge" <> suffix)
                == Success (Approve Merge, AnyDay, Normal)

      it "understands HTML comments" $
        dummyParse
          "@bot <!-- hi --> merge <!-- there --> and <!-- this --> deploy\
          \ <!-- is --> to <!-- a --> production <!-- secret --> on <!-- message --> friday"
          `shouldBe` Success (Approve $ MergeAndDeploy EntireProject (DeployEnvironment "production"), OnFriday, Normal)

      -- Giving silly commands to the bot is a highly-valued feature ;)
      it "allows HTML comment chicanery" $
        dummyParse "<!--\n@bot merge\n--> @bot YEET"
          `shouldBe` Success (Approve Merge, AnyDay, Normal)

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

dummyParse :: Text -> ParseResult (MergeCommand, MergeWindow, Priority)
dummyParse = parseMergeCommand dummyProject dummyTrigger

dummyProject :: ProjectConfiguration
dummyProject =
  ProjectConfiguration
    { owner = "owner"
    , repository = "repository"
    , branch = "branch"
    , testBranch = "test-branch"
    , checkout = "/dev/null"
    , stateFile = "/dev/null"
    , checks = Nothing
    , deployEnvironments = Just ["production", "staging"]
    , deploySubprojects = Just ["aaa", "bbb"]
    , safeForFriday = Just True
    }

dummyTrigger :: TriggerConfiguration
dummyTrigger =
  TriggerConfiguration
    { commentPrefix = "@bot"
    }
