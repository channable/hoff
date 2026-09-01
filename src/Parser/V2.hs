{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}

module Parser.V2 (parseCommand) where

import Data.Text (Text)
import Text.Megaparsec (Parsec, choice, takeWhile1P, (<?>), sepBy1, try, MonadParsec (notFollowedBy), satisfy)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Void (Void)
import Text.Megaparsec.Char (hspace1, string)
import Control.Monad.Permutations (runPermutation, Permutation, toPermutationWithDefault)
import Control.Applicative (Alternative(..))
import Data.Char (isLetter)
import qualified Data.Text as Text

data Command
  = MergeCommand Merge
  | RetryCommand Retry
  deriving (Show, Eq)

{-
merge

merge and deploy

merge and tag

merge and tag and deploy

merge and deploy only A, B
XX merge and deploy A, B

merge and deploy to production

merge and deploy to production only A
merge and deploy only A to production

merge and deploy on friday only A
merge and deploy only A on friday to production
merge on friday and deploy

merge as hotfix
merge on friday as hotfix

-}

data Merge = Merge
  { andDeploy :: Maybe Deploy
  , andTag :: Bool
  , withoutDeploy :: Bool
  , commonFlags :: CommonFlags
  }
  deriving (Show, Eq)

data Deploy = Deploy
  { toEnvironment :: Maybe Text
  , onlySubprojects :: [Text]
  }
  deriving (Show, Eq)

newtype Retry = Retry
  { commonFlags :: CommonFlags
  }
  deriving (Show, Eq)

data CommonFlags = CommonFlags
  { onFriday :: Bool
  , asHotfix :: Bool
  , withPriority :: Bool
  }
  deriving (Show, Eq)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    hspace1
    empty
    (Lexer.skipBlockComment "<!--" "-->")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

word :: Text -> Parser Text
word w =
  lexeme $ string w <* notFollowedBy (satisfy isWordChar)

phrase :: Text -> Parser [Text]
phrase ws =
  try (traverse word $ Text.words ws) <?> show ws

-- Prefer this over @toPermutation (optional p)@, as that won't behave as you'd expect.
toPermutationOptional :: Parser a -> Permutation Parser (Maybe a)
toPermutationOptional p =
  toPermutationWithDefault Nothing (Just <$> p)

toPermutationFlag :: Parser a -> Permutation Parser Bool
toPermutationFlag p =
  toPermutationWithDefault False (True <$ p)

parseCommand :: Parser Command
parseCommand =
  choice
    [ MergeCommand <$> parseMerge
    , RetryCommand <$> parseRetry
    ]

parseMerge :: Parser Merge
parseMerge = do
  _ <- word "merge"
  runPermutation $ do
    andDeploy <- toPermutationOptional parseDeploy
    andTag <- toPermutationFlag parseAndTag
    withoutDeploy <- toPermutationFlag parseWithoutDeploy
    commonFlags <- parseCommonFlags

    pure Merge
      { andDeploy
      , andTag
      , withoutDeploy
      , commonFlags
      }

parseDeploy :: Parser Deploy
parseDeploy = do
  _ <- phrase "and deploy"
  runPermutation $ do
    toEnvironment <- toPermutationOptional parseToEnvironment
    onlySubprojects <- toPermutationWithDefault [] parseOnlySubprojects
    pure Deploy{toEnvironment, onlySubprojects}

parseToEnvironment :: Parser Text
parseToEnvironment = do
  _ <- word "to"
  parseName <?> "environment name"

parseOnlySubprojects :: Parser [Text]
parseOnlySubprojects = do
  _ <- word "only"
  sepBy1 (parseName <?> "subproject name") (symbol ",")

parseAndTag :: Parser [Text]
parseAndTag =
  phrase "and tag"

parseWithoutDeploy :: Parser [Text]
parseWithoutDeploy =
   phrase "without deploy" <|> phrase "without deploying"

parseRetry :: Parser Retry
parseRetry = do
  _ <- word "retry"
  runPermutation $ do
    commonFlags <- parseCommonFlags
    pure Retry{commonFlags}

parseCommonFlags :: Permutation Parser CommonFlags
parseCommonFlags = do
  onFriday <- toPermutationFlag $ phrase "on friday"
  asHotfix <- toPermutationFlag $ phrase "as hotfix"
  withPriority <- toPermutationFlag $ phrase "with priority"

  pure CommonFlags{onFriday, asHotfix, withPriority}

parseName :: Parser Text
parseName =
  lexeme $ takeWhile1P (Just "name") isWordChar

isWordChar :: Char -> Bool
isWordChar c = isLetter c || c `elem` ['-', '_']
