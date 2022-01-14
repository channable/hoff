{-# LANGUAGE OverloadedStrings #-}
module AlembicRebaseSpec where

import Test.Hspec
import Data.Either (isRight)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose, hPutStr)

import qualified AlembicRebase as AR

alembicSpec :: Spec
alembicSpec = do
  describe "Python file parsing" $ do
    it "parses python files" $ do
      pythonFile <- AR.loadPythonFile "tests/data/alembic-with-down.py"
      isRight pythonFile `shouldBe` True
    it "parses the alembic file with a down revision" $ do
      let filepath = "tests/data/alembic-with-down.py"
      ePythonModule <- AR.loadPythonFile filepath
      case ePythonModule >>= AR.alembicRevisions filepath of
        Left e -> fail (show e)
        Right revision -> do
          revision `shouldBe` AR.AlembicRevision
            { AR.alembicRevisionFile = filepath
            , AR.alembicCurrentRevision = AR.RevisionId "8173ca21b11d"
            , AR.alembicDownRevision = Just (AR.RevisionId "db6fced84dcd")
            }
    it "parses the alembic file with no down revision" $ do
      let filepath = "tests/data/alembic-first.py"
      ePythonModule <- AR.loadPythonFile filepath
      case ePythonModule >>= AR.alembicRevisions filepath of
        Left e -> fail (show e)
        Right revision -> do
          revision `shouldBe` AR.AlembicRevision
            { AR.alembicRevisionFile = filepath
            , AR.alembicCurrentRevision = AR.RevisionId "8173ca21b11d"
            , AR.alembicDownRevision = Nothing
            }

  describe "Alembic rebasing" $ do
    let noDownRevision = AR.AlembicRevision "file1.py" "1" Nothing
    it "is fine with no alembics" $
      AR.rebaseInstructions [] [] `shouldBe` Right Nothing
    it "is fine with no down revision on existing alembics" $
      AR.rebaseInstructions [noDownRevision] [] `shouldBe` Right Nothing
    it "is not fine with no down revision on new alembics" $
      AR.rebaseInstructions [] [noDownRevision] `shouldBe` Left AR.AlembicRebaseExistingBaseNotFound
    let alembic2 = AR.AlembicRevision "file2" "2" (Just "1")
    let alembic3 = AR.AlembicRevision "file3" "3" (Just "2")
    let alembic4 = AR.AlembicRevision "file4" "4" (Just "3")
    it "should detect an already rebased alembic" $ do
      -- The entire chain
      AR.rebaseInstructions [noDownRevision, alembic2, alembic3] [alembic4] `shouldBe` Right Nothing
      -- No down revisions
      AR.rebaseInstructions [alembic2, alembic3] [alembic4] `shouldBe` Right Nothing
      -- Multiple new
      AR.rebaseInstructions [noDownRevision, alembic2] [alembic3, alembic4] `shouldBe` Right Nothing
    let alembic5 = AR.AlembicRevision "file5" "5" (Just "2")
    it "should rebase a new alembic" $
      AR.rebaseInstructions [noDownRevision, alembic2, alembic3, alembic4] [alembic5]
        `shouldBe` (Right $ Just $
          AR.RebaseInstructions
            { AR.rebaseInstructionsFile = "file5"
            , AR.rebaseInstructionOld = "2"
            , AR.rebaseInstructionNew = "4"
            })
    let alembic6 = AR.AlembicRevision "file6" "6" (Just "5")
    it "should rebase multiple new alembic" $
      AR.rebaseInstructions [noDownRevision, alembic2, alembic3, alembic4] [alembic5, alembic6]
        `shouldBe` (Right $ Just $
          AR.RebaseInstructions
            { AR.rebaseInstructionsFile = "file5"
            , AR.rebaseInstructionOld = "2"
            , AR.rebaseInstructionNew = "4"
            })
    it "should rebase non-linear alembics" $
      AR.rebaseInstructions [noDownRevision, alembic2, alembic3] [alembic5, alembic6]
        `shouldBe` (Right $ Just $
          AR.RebaseInstructions
            { AR.rebaseInstructionsFile = "file5"
            , AR.rebaseInstructionOld = "2"
            , AR.rebaseInstructionNew = "3"
            })
    it "should fail on missing alembics" $
      AR.rebaseInstructions [noDownRevision] [alembic5, alembic6]
        `shouldBe` (Left AR.AlembicRebaseExistingBaseNotFound)
    let alembic3wrong = AR.AlembicRevision "file3" "3" (Just "2")
    it "should break with conflicting revisions" $
      AR.rebaseInstructions [noDownRevision, alembic2, alembic3, alembic4, alembic3wrong] [alembic5, alembic6]
        `shouldBe` (Left $ AR.AlembicRebaseConflictingRevisions ["3"])
    it "should break with conflicting revisions in new alembics" $
      AR.rebaseInstructions [noDownRevision, alembic2, alembic4] [alembic3, alembic3wrong, alembic5, alembic6]
        `shouldBe` (Left $ AR.AlembicRebaseConflictingRevisions ["3"])

    it "should correctly execute the instructions" $ do
      let contents = "hello\n1\n2\n3"
      withSystemTempFile "rebase_file.py" $
        \fp h -> do
          hPutStr h contents
          hClose h
          let rebaseInstructions = AR.RebaseInstructions
                { AR.rebaseInstructionsFile = fp
                , AR.rebaseInstructionOld = "2"
                , AR.rebaseInstructionNew = "4"
                }
          res <- AR.executeRebaseInstructions rebaseInstructions
          res `shouldBe` Nothing
          newContents <- readFile fp
          newContents `shouldBe` "hello\n1\n4\n3"
    it "should correctly rewrite multiple entries the instructions" $ do
      let contents = "hello, we are rewriting 2\n1\ndown_revision='2'\n3"
      withSystemTempFile "rebase_file.py" $
        \fp h -> do
          hPutStr h contents
          hClose h
          let rebaseInstructions = AR.RebaseInstructions
                { AR.rebaseInstructionsFile = fp
                , AR.rebaseInstructionOld = "2"
                , AR.rebaseInstructionNew = "4"
                }
          res <- AR.executeRebaseInstructions rebaseInstructions
          res `shouldBe` Nothing
          newContents <- readFile fp
          newContents `shouldBe` "hello, we are rewriting 4\n1\ndown_revision='4'\n3"
