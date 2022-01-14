module AlembicRebase where

import Control.Exception (Exception (..), SomeException (..), catch)
import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))

import qualified Data.Map as M
import qualified Language.Python.Common.AST as Python
import qualified Language.Python.Version3.Parser as Python

data AlembicRebaseError
  = AlembicRebasePythonFileError FilePath String
  | AlembicRebaseUnknownVariable FilePath String
  | AlembicRebaseInvalidRevisionId FilePath String
  | AlembicRebaseUnexpectedNone FilePath
  | AlembicRebaseExistingBaseNotFound
  | AlembicRebaseInternalError String
  | AlembicRebaseConflictingRevisions [RevisionId]
  deriving (Eq, Show)

loadPythonFile :: FilePath -> IO (Either AlembicRebaseError Python.ModuleSpan)
loadPythonFile pythonFilePath = do
  pythonFile <- (Right <$> readFile pythonFilePath)
    `catch` \e@(SomeException _) -> (pure (Left $ AlembicRebasePythonFileError pythonFilePath (displayException e)))
  let parseResult = pythonFile >>= \contents ->
        either (\e -> Left $ AlembicRebasePythonFileError pythonFilePath $ show e) Right $ Python.parseModule contents ""
  pure (fmap fst parseResult)

newtype RevisionId = RevisionId String
  deriving (Eq, Ord, Show)

instance IsString RevisionId where
  fromString = RevisionId

mkRevisionId :: FilePath -> String -> Either AlembicRebaseError RevisionId
mkRevisionId fp s =
  let
    clean '\'' = False
    clean '\"' = False
    clean _ = True
    cleaned = filter clean s
  in
  if all isAlphaNum cleaned then
    Right (RevisionId cleaned)
  else
    Left (AlembicRebaseInvalidRevisionId fp s)

data AlembicRevision = AlembicRevision
  { alembicRevisionFile :: FilePath
  , alembicCurrentRevision :: RevisionId
  , alembicDownRevision :: Maybe RevisionId
  } deriving (Show, Eq)

alembicRevisions :: FilePath -> Python.ModuleSpan -> Either AlembicRebaseError AlembicRevision
alembicRevisions fp (Python.Module statements) = do
  currentRevision
    <- findAssignVariable fp "revision" statements
    >>= extractRevisionId fp
    >>= maybe (Left $ AlembicRebaseUnexpectedNone fp) pure
  downRevision <- findAssignVariable fp "down_revision" statements >>= extractRevisionId fp
  pure (AlembicRevision
    { alembicRevisionFile = fp
    , alembicCurrentRevision = currentRevision
    , alembicDownRevision = downRevision
    })

extractRevisionId :: Show annot => FilePath -> Python.Expr annot -> Either AlembicRebaseError (Maybe RevisionId)
extractRevisionId fp (Python.Strings strings _) = Just <$> mkRevisionId fp (concat strings)
extractRevisionId _ (Python.None _) = pure Nothing
extractRevisionId fp expr = Left (AlembicRebaseInvalidRevisionId fp (show expr))


findAssignVariable :: FilePath -> String -> [Python.Statement annot] -> Either AlembicRebaseError (Python.Expr annot)
findAssignVariable fp name statements =
  case mapMaybe (getAssignVariable name) statements of
    [] -> Left (AlembicRebaseUnknownVariable fp name)
    -- Following the Python standard that any variable overwrites a previous variable
    xs -> Right (last xs)

getAssignVariable :: String -> Python.Statement annot -> Maybe (Python.Expr annot)
getAssignVariable name (Python.Assign [Python.Var ident _] val _) | Python.ident_string ident == name = Just val
getAssignVariable _ _ = Nothing

-- | Make a dependency graph where the down_revision is the key, pointing to the revision
-- Can be used to quickly get the alembic above the current one
upPathRevision :: [AlembicRevision] -> Either AlembicRebaseError (M.Map RevisionId RevisionId)
upPathRevision = dependencyGraph alembicDownRevision (Just . alembicCurrentRevision)

downPathRevision :: [AlembicRevision] -> Either AlembicRebaseError (M.Map RevisionId RevisionId)
downPathRevision = dependencyGraph (Just . alembicCurrentRevision) alembicDownRevision

duplicates :: Eq a => [a] -> [a]
duplicates = go []
  where
    go xs [] = xs
    go xs (y:ys)
      | y `elem` ys = go (y:xs) ys
      | otherwise = go xs ys

dependencyGraph
  :: (AlembicRevision -> Maybe RevisionId)
  -> (AlembicRevision -> Maybe RevisionId)
  -> [AlembicRevision]
  -> Either AlembicRebaseError (M.Map RevisionId RevisionId)
dependencyGraph keyF valueF revisions
  | not $ null $ duplicates $ mapMaybe keyF revisions = Left (AlembicRebaseConflictingRevisions $ duplicates $ mapMaybe keyF revisions)
  | otherwise = pure $ M.fromList (mapMaybe (\r -> tup (keyF r, valueF r)) revisions)
  where
    tup (Just x, Just y) = Just (x, y)
    tup _ = Nothing

findLast :: M.Map RevisionId RevisionId -> Maybe (RevisionId, RevisionId)
findLast m = case M.toList m of
  [] -> Nothing
  (firstPair : _) -> go firstPair
  where
    go (key, value) = case M.lookup key m of
      -- Cannot find what this key references? This is the last key in that case
      Nothing -> Just (key, value)
      Just newKey -> go (newKey, key)

data RebaseInstructions = RebaseInstructions
  { rebaseInstructionsFile :: FilePath
  , rebaseInstructionOld :: RevisionId
  , rebaseInstructionNew :: RevisionId
  } deriving (Show, Eq)

rebaseInstructions :: [AlembicRevision] -> [AlembicRevision] -> Either AlembicRebaseError (Maybe RebaseInstructions)
rebaseInstructions _ [] = pure Nothing -- No rebase necessary if there are no new alembics
rebaseInstructions existingAlembics newAlembics = do

  _ <- downPathRevision existingAlembics
  existingPath <- upPathRevision existingAlembics

  -- Take the path upwards until no other alembics can be found, that is the latest existing alembic
  latestExistingAlembic <- case findLast existingPath of
    Nothing -> Left AlembicRebaseExistingBaseNotFound
    Just (revision, _) -> pure revision
  newPath <- downPathRevision newAlembics
  _ <- upPathRevision newAlembics

  (newestFirstAlembicDown, newestFirstAlembicCur) <- case findLast newPath of
    -- This should not occur, because we should have at least one alembic
    Nothing -> Left (AlembicRebaseInternalError "Cannot find newest alembic")
    Just revisions -> pure revisions

  -- The alembic is already rebased, nothing to do for us
  if latestExistingAlembic == newestFirstAlembicDown then
    pure Nothing
  else do
    newestAlembicFile <-
      case find (\x -> alembicCurrentRevision x == newestFirstAlembicCur) newAlembics of
        Nothing -> Left (AlembicRebaseInternalError $ "Cannot find revision id " <> show newestFirstAlembicCur)
        Just revision -> pure $ alembicRevisionFile revision
    pure $ Just $ RebaseInstructions
      { rebaseInstructionsFile = newestAlembicFile
      , rebaseInstructionOld = newestFirstAlembicDown
      , rebaseInstructionNew = latestExistingAlembic
      }
