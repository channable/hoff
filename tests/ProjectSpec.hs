{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ProjectSpec (projectSpec) where

import Control.Monad (forM_)
import Test.Hspec (Spec, describe, shouldBe, shouldNotBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, genericShrink, withMaxSuccess, (==>))
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances.Text ()

import Data.Map.Strict qualified as Map

import Project qualified

projectSpec :: Spec
projectSpec = do
  describe "Project.subMapByOwner" $ do
    prop "the ord instance guarentees owners are grouped together" $ withMaxSuccess 100 $ \(owners, repositories) -> do
      let
        projectInfos = [Project.ProjectInfo owner repo | owner <- owners, repo <- repositories]
        projectInfoMap = Map.fromList (map (\p -> (p, Project.repository p)) projectInfos)

      forM_ owners $ \owner -> do
        Project.subMapByOwner owner projectInfoMap
          `shouldBe` Map.filterWithKey (\key _ -> Project.owner key == owner) projectInfoMap

  describe "Project.summarize" $ do
    prop "Ensure successes are always overshadowed by other statuses" $ withMaxSuccess 1000 $ \statuses -> do
      let
        checkMap = Map.fromList statuses
        outstanding = Project.SpecificChecks checkMap
      any ((/= Project.BuildSucceeded) . snd) (Map.toList checkMap) ==> do
        Project.summarize outstanding `shouldNotBe` Project.BuildSucceeded
    prop "Ensure failures always overshadow other statuses" $ withMaxSuccess 1000 $ \statuses -> do
      let
        checkMap = Map.fromList statuses
        outstanding = Project.SpecificChecks checkMap
        isFailure (Project.BuildFailed _) = True
        isFailure _ = False
      any (isFailure . snd) (Map.toList checkMap) ==> do
        Project.summarize outstanding `shouldSatisfy` isFailure

instance Arbitrary Project.Check where
  arbitrary = Project.Check <$> arbitrary
instance Arbitrary Project.BuildStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink
