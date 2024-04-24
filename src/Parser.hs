{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (intercalate, intersperse)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, (<|>))

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (skipBlockComment)

import Configuration (ProjectConfiguration (..), TriggerConfiguration (..),
                      knownEnvironments, knownSubprojects)
import Project (ApprovedFor (..), DeployEnvironment (..), DeploySubprojects (..),
                MergeCommand (..), MergeWindow (..), Priority (..))

-- | Internal result type for parsing a merge command, which allows the
-- consumer of `parseMergeCommand` to inspect the reason why a message
-- was considered invalid.
data ParseResult a
  -- | The parser found a valid prefix and a valid command.
  = Success a
  -- | The parser found a valid prefix, but no valid command. This contains a
  -- (multiline) error message describing the problem with the command. When
  -- displaying the error in a GitHub comment, it should be shown as monospace
  -- text as it may use whitespace for alignment and it may also contain
  -- markdown from the original comment.
  | ParseError Text
  -- | The parser decided to ignore the message because it did
  -- not contain a valid prefix.
  | Ignored
  deriving (Eq, Show)

-- Checks whether the parse result was valid.
isSuccess :: ParseResult a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

type Parser = Parsec Void Text

-- | A comment that can be added to a message that will cause it to not parse
-- the message for merge commands. This is parsed in 'shouldIgnoreComment'. Note
-- the trailing line feed here. GitHub's markdown parser won't parse markdown if
-- it's on the same line as an HTML comment:
--
-- https://github.com/channable/hoff/issues/227
hoffIgnoreComment :: Text
hoffIgnoreComment = "<!-- Hoff: ignore -->\n"

-- | Helper to parse over whitespace and HTML comments. There must be at least
-- one group of whitespace consumed, although this does not need to be at the
-- start or end of the series of spaces and comments.
pSpace1 :: Parser ()
pSpace1 =
  P.choice
    [ P.hspace1 *> pSpace
    , P.hidden (P.skipBlockComment "<!--" "-->") *> pSpace1
    ]

pSpace :: Parser ()
pSpace =
  P.skipMany $ P.choice
    [ P.hspace1
    , P.hidden (P.skipBlockComment "<!--" "-->")
    ]

-- Helper to parse a string, case insensitively, and ignoring excess spaces
-- between words. Also allows line breaks.
pString :: Text -> Parser ()
pString =
  sequence_
    . intersperse pSpace1
    . fmap (void . P.string')
    . Text.words

-- | Parse a comma-separated list of items. The list must be non-empty
pCommaList1 :: Parser a -> Parser [a]
pCommaList1 item =
  P.sepBy1 item (P.try (pSpace *> P.single ',' *> pSpace))

-- | Checks if a comment contains 'hoffIgnoreComment', matching case
-- insensitively and allowing variations in whitespace. This is used to prevent
-- feedback cycles when Hoff repeats part of a message posted by the user. This
-- is parsed this way instead of simple string matching to be a bit more
-- reliable if users start using this as well.
shouldIgnoreComment :: Text -> Bool
shouldIgnoreComment = cvtParseResult . P.parse pComment "comment"
  where
    cvtParseResult :: Either (ParseErrorBundle Text Void) Bool -> Bool
    cvtParseResult = fromRight False

    pComment :: Parser Bool
    pComment = (True <$ pHoffIgnoreComment)
      <|> (P.anySingle *> pComment)
      <|> pure False

    -- This parses 'hoffIgnoreComment' while being as lenient in terms of
    -- whitespace usage and casing as possible.
    pHoffIgnoreComment :: Parser ()
    pHoffIgnoreComment = P.string "<!--" *> P.space *> pString "Hoff: ignore" <* P.space <* P.string "-->"

-- | Parse a PR comment for a merge command. The parsing is done
-- case-insensitively and duplicate whitespace is ignored. The 'ParseResult'
-- indicates whether:
--
--   1) The comment contains a properly parsed merge command.  To reduce
--      ambiguity, the command should be either on its own line or at the end of
--      a sentence (which may end with a period, comma, exclamation mark, or
--      question mark). If the line contains other text that cannot be parsed
--      then this is treated as an error.
--   2) The comment did contain the merge command's prefix but the text folowing
--      it was not a valid merge command according to the rules mentioned above.
--   3) The comment did not contain the command prefix at all and should be
--      ignored.
--
-- If the trigger prefix is "@hoffbot", a command "@hoffbot merge" would
-- indicate the `Merge` approval type.
parseMergeCommand :: ProjectConfiguration -> TriggerConfiguration -> Text -> ParseResult (MergeCommand, MergeWindow, Priority)
parseMergeCommand projectConfig triggerConfig = cvtParseResult . P.parse pComment "comment"
  where
    cvtParseResult :: Either (ParseErrorBundle Text Void) (Maybe a) -> ParseResult a
    cvtParseResult (Right (Just result)) = Success result
    cvtParseResult (Right Nothing) = Ignored
    cvtParseResult (Left err) = ParseError (Text.pack $ P.errorBundlePretty err)

    -- This parser maintains a lot of the odd parser's behavior in that in folds
    -- repeated whitespace into one, matches case insensitively, and strips the
    -- prefix (but not the environment names) before matching.
    commandPrefix :: Text
    commandPrefix = Text.strip $ commentPrefix triggerConfig

    largestMatchFirst :: [Text] -> [Text]
    largestMatchFirst = List.sortOn (Down . Text.length)

    -- No whitespace stripping or case folding is performed here to be
    -- consistent with how environments are handled.
    subprojects :: [Text]
    subprojects = largestMatchFirst $ knownSubprojects projectConfig

    -- No whitespace stripping or case folding is performed here since they are
    -- also matched verbatim elsewhere in Hoff.
    environments :: [Text]
    environments = largestMatchFirst $ knownEnvironments projectConfig

    -- The punctuation characters that are allowed at the end of a merge
    -- command. This doesn't use the included punctuation predicate because that
    -- includes many more classes of punctuation, like quotes.
    allowedPunctuation :: [Char]
    allowedPunctuation = ".,!?:;"

    -- The error message printed when a comand is not terminated correctly.
    commentSuffixError :: String
    commentSuffixError = "Merge commands may not be followed by anything other than a punctuation character ("
      <> intercalate ", " (map show allowedPunctuation)
      <> ")."

    -- The error message printed when using 'merge and deploy' with no
    -- configured deployment environments.
    noDeployEnvironmentsError :: String
    noDeployEnvironmentsError = "No deployment environments have been configured."

    -- The error message printed when using 'merge and deploy' with no specified environment
    noDeployEnvironmentSpecifiedError :: String
    noDeployEnvironmentSpecifiedError = "Merge and deploy has been deprecated. Please use merge and deploy to <environment>\n" ++
      "where <environment> is one of " ++ intercalate ", " (map Text.unpack environments)

    -- This parser succeeds if it either successfully parses the comment for a
    -- merge command, in which case it returns @Just (approval, mergeWindow)@,
    -- or when the comment doesn't contain any merge command, in which case the
    -- parser returns @Nothing@. The prefix is matched greedily in 'pCommand',
    -- so if the comment contains an invaild command followed by a valid command
    -- an error will be returned based on that earlier invalid command.
    pComment :: Parser (Maybe (MergeCommand, MergeWindow, Priority))
    pComment = (Just <$> pCommand)
      <|> (P.anySingle *> pComment)
      <|> pure Nothing

    -- Parse a full merge command. Does not consume any input if the prefix
    -- could not be matched fully.
    pCommand :: Parser (MergeCommand, MergeWindow, Priority)
    pCommand = P.try pCommandPrefix *> pSpace1 *> (pApprovalCommand <|> pRetryCommand) <* pSpace <* pCommandSuffix

    -- Parse the (normalized) command prefix. Matched non-greedily in 'pCommand'
    -- using 'P.try'.
    pCommandPrefix :: Parser ()
    pCommandPrefix = void $ P.string' commandPrefix

    -- Commands may be terminated by one or more (common) punctuation
    -- characters, one or more whitespace characters, and either the end of a
    -- line or the end of the input.
    pCommandSuffix :: Parser ()
    pCommandSuffix =
      P.many (P.oneOf allowedPunctuation)
      *> pSpace
      *> (void P.eol <|> P.eof <|> fail commentSuffixError)

    -- Parse the actual merge approval command following the command prefix. The
    -- merge window is either @ on friday@ or empty.
    --
    -- NOTE: Since @ on friday@ starts with a space, additional whitespace at
    --       the end of 'pMergeApproval' should not already have been consumed.
    --       This is a bit tricky, and using 'P.hspace' instead of 'P.hspace1'
    --       in 'pMergeWindow' would allow @mergeon friday@ which is also not
    --       desirable.
    pApprovalCommand :: Parser (MergeCommand, MergeWindow, Priority)
    pApprovalCommand = (,,) . Approve <$> pMergeApproval <*> pMergeWindow <*> pPriority

    pRetryCommand :: Parser (MergeCommand, MergeWindow, Priority)
    pRetryCommand = (Retry,,) <$> (P.string' "retry" *> pMergeWindow) <*> pPriority

    -- We'll avoid unnecessary backtracking here by parsing the common prefixes.
    -- Note that 'P.try' is used sparingly here. It's mostly used when parsing
    -- whitespace plus another word. Backtracking should be limited to trying
    -- difference branches since it will otherwise destroy the nice error
    -- messages megaparsec gives us, and the parser will instead error out in
    -- 'pCommandSuffix' which would be confusing.
    --
    -- When the comment isn't folowed by @ and @ this is treated as a plain
    -- merge command.
    pMergeApproval :: Parser ApprovedFor
    pMergeApproval = P.string' "merge" *> P.option Merge pMergeAnd

    -- NOTE: As mentioned above, only the @ and @ part will backtrack. This is
    --       needed so a) the custom error message in pDeploy works and b) so
    --       'merge on friday' can be parsed correctly.
    pMergeAnd :: Parser ApprovedFor
    pMergeAnd = P.try (pSpace1 *> P.string' "and" *> pSpace1) *> (pTag <|> pDeploy)

    -- Parses @merge and tag@ commands.
    pTag :: Parser ApprovedFor
    pTag = MergeAndTag <$ P.string' "tag"

    -- Parses @merge and deploy[ to <environment>]@ commands.
    pDeploy :: Parser ApprovedFor
    pDeploy = do
      void (P.string' "deploy")
      MergeAndDeploy <$> pDeploySubprojects <*> pDeployToEnvironment

    pSubproject :: Parser Text
    pSubproject = P.choice (fmap P.string subprojects)

    pDeploySubprojects :: Parser DeploySubprojects
    pDeploySubprojects
      | null subprojects
      = pure EntireProject

      | otherwise
      = -- Without the try this could consume the space and break 'to <env>' and merge windows
        P.try (pSpace1 *> (OnlySubprojects <$> pCommaList1 pSubproject))
          <|> pure EntireProject

    -- This parser is run directly after parsing "deploy", so it may need to
    -- parse a space character first since specifying a deployment environment
    -- is optional. The reason for splitting this up from 'pDeploy' like this is
    -- so we can have a nicer error message when no environments have been
    -- configured.
    pDeployToEnvironment :: Parser DeployEnvironment
    pDeployToEnvironment
      | null environments
      = fail noDeployEnvironmentsError

      | otherwise
      = -- Without the try this could consume the space and break 'merge and deploy on friday'
        P.try (pSpace1 *> P.string' "to" *> pSpace1 *> P.choice pDeployEnvironments)
          <|> defaultEnvironment

    -- The default environment to deploy to on a "merge and deploy". This
    -- behavior is deprecated when more than one environment is set, and will
    -- return an error in that case.
    defaultEnvironment :: Parser DeployEnvironment
    defaultEnvironment
      | [environment] <- environments = pure (DeployEnvironment environment)
      | otherwise = fail noDeployEnvironmentSpecifiedError

    -- NOTE: This uses 'P.string' instead of 'P.string'' to avoid case folding,
    --       since environment names are also matched case sensitively elsewhere
    pDeployEnvironments :: [Parser DeployEnvironment]
    pDeployEnvironments = map (fmap DeployEnvironment . P.string) environments

    -- Parses the optional @ on friday@ command suffix. Since this starts with a
    -- space, it's important that the last run parser has not yet consumed it.
    pMergeWindow :: Parser MergeWindow
    pMergeWindow =
      (OnFriday <$ P.try (pSpace1 *> pString "on friday"))
        <|> (DuringFeatureFreeze <$ P.try (pSpace1 *> pString "as hotfix"))
        <|> pure AnyDay

    pPriority :: Parser Priority
    pPriority = High <$ P.try (pSpace1 *> pString "with priority") <|> pure Normal
