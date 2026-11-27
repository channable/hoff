{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Parser2 (
  parseComment,
  Comment(..),
  Command(..),
) where

import Data.Functor (($>))
import Data.Text (Text)
import Text.Earley (Grammar, list, rule, terminal, fullParses, (<?>), namedToken, satisfy)
import Text.Earley qualified as Earley
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Applicative.Combinators ((<|>))

import Project (ApprovedFor (..), DeployEnvironment (..), DeploySubprojects (..), MergeCommand (..), MergeWindow (..), Priority (..))

data Comment
  = Skippable
  | HasCommand Command
  deriving Show

data Command
  = Command MergeCommand MergeWindow Priority
  deriving Show

parseComment :: Text -> Either (Earley.Report Token [Token]) Comment
parseComment = parse commentGrammar

parse :: (forall r. Grammar r (Prod r a)) -> Text -> Either (Earley.Report Token [Token]) a
parse grammar input =
  let
    parser = Earley.parser grammar
    tokens = tokenize input
  in case fullParses parser tokens of
    ([], report) -> Left report
    (c : _, _) -> Right c

tokenize :: Text -> [Token]
tokenize =
  -- WIP(daan): deal with commas
  Text.words

type Token = Text

type Prod r a = Earley.Prod r Token Token a

commentGrammar :: Grammar r (Prod r Comment)
commentGrammar =
  -- WIP(daan): parse skippable
  -- WIP(daan): parse prefix, trigger word, and suffix
  fmap HasCommand <$> commandGrammar

commandGrammar :: Grammar r (Prod r Command)
commandGrammar = do
  mergeCommand <- mergeCommandGrammar

  pure $ Command
    <$> mergeCommand
    <*> mergeWindow
    <*> priority

mergeCommandGrammar :: Grammar r (Prod r MergeCommand)
mergeCommandGrammar = do
  approvedFor <- approvedForGrammar

  pure $
    Approve <$> approvedFor <|> "retry" $> Retry

approvedForGrammar :: Grammar r (Prod r ApprovedFor)
approvedForGrammar = do
  deploySubprojects <- deploySubprojectsGrammar

  let mergeAndDeploy =
        MergeAndDeploy <$> deploySubprojects <*> deployEnvironment

  pure $
    "merge"
      *> ( pure Merge
            <|> "and" *> ( "deploy" *> mergeAndDeploy <|> "tag" $> MergeAndTag )
        )

deploySubprojectsGrammar :: Grammar r (Prod r DeploySubprojects)
deploySubprojectsGrammar = do
  onlySubprojects <- sepBy1 (namedToken ",") subprojectName

  pure $
    pure EntireProject
      <|> OnlySubprojects <$> onlySubprojects

subprojectName :: Prod r Text
subprojectName = identifier <?> "subproject name"

deployEnvironment :: Prod r DeployEnvironment
deployEnvironment =
  pure (DeployEnvironment "")
    <|> "to"
    *> (DeployEnvironment <$> identifier <?> "environment name")

identifier :: Prod r Token
identifier =
  satisfy $ \tok ->
    not (Text.null tok) && not (Text.any (== ',') tok)

mergeWindow :: Prod r MergeWindow
mergeWindow =
  pure AnyDay
    <|> list ["on", "friday"] $> OnFriday
    <|> list ["as", "hotfix"] $> DuringFeatureFreeze

priority :: Prod r Priority
priority =
  pure Normal
    <|> list ["with", "priority"] $> High

sepBy1 :: Earley.Prod r e t sep -> Earley.Prod r e t a -> Grammar r (Earley.Prod r e t [a])
sepBy1 sep item = do
  rec
    prod <- rule $
      (List.singleton <$> item)
        <|> ((:) <$> item <*> (sep *> prod))

  pure prod
