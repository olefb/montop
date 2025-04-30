{-# LANGUAGE ScopedTypeVariables #-}

module ZipperTests (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.List as L

import qualified Zipper as Z

spec :: Spec
spec = do
    describe "mkZipper" $ do
        it "returns Nothing for an empty list" $
            Z.mkZipper ([] :: [Int]) `shouldSatisfy` isNothing

        it "returns Just for a non-empty list" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                isJust (Z.mkZipper xs)

        it "focuses on the first element for a non-empty list" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                in Z.focus z == head xs

    describe "zipperToList" $ do
        it "mkZipper . zipperToList returns the original zipper" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                    -- fromJust is safe here because zipperToList always gives non-empty
                    z' = fromJust $ Z.mkZipper (Z.zipperToList z)
                in z == z'

        it "zipperToList . mkZipper returns the original list" $
             property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                in Z.zipperToList z == xs

    describe "zipperUp / zipperDown" $ do
        it "zipperUp is idempotent at the top" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                    zTop = L.foldl' (flip ($)) z (replicate (length xs + 5) Z.zipperUp)
                in Z.zipperUp zTop == zTop

        it "zipperDown is idempotent at the bottom" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                    zBottom = L.foldl' (flip ($)) z (replicate (length xs + 5) Z.zipperDown)
                in Z.zipperDown zBottom == zBottom

        it "zipperDown . zipperUp returns original zipper (if not at top)" $
             property $ \(xs :: [Int]) -> length xs > 1 ==>
                let z = fromJust $ Z.mkZipper xs
                    zDown = Z.zipperDown z
                    zDownUp = Z.zipperUp zDown
                in zDown /= z && zDownUp == z

        it "zipperUp . zipperDown returns original zipper (if not at bottom)" $
             property $ \(NonEmpty xs :: NonEmptyList Int) ->
                forAll (choose (0, length xs - 1)) $ \steps ->
                    let zInitial = fromJust $ Z.mkZipper xs
                        z = L.foldl' (flip ($)) zInitial (replicate steps Z.zipperDown)
                    in not (null (Z.after z)) ==> Z.zipperUp (Z.zipperDown z) == z

    describe "zipperFocusIndex" $ do
        it "returns 0 after mkZipper" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                let z = fromJust $ Z.mkZipper xs
                in Z.zipperFocusIndex z == 0

        it "increases by 1 after zipperDown (if possible)" $
             property $ \(xs :: [Int]) -> length xs > 1 ==>
                let z = fromJust $ Z.mkZipper xs
                    idx1 = Z.zipperFocusIndex z
                    zDown = Z.zipperDown z
                    idx2 = Z.zipperFocusIndex zDown
                in idx2 == idx1 + 1

        it "decreases by 1 after zipperUp (if possible)" $
             property $ \(xs :: [Int]) -> length xs > 1 ==>
                let z = fromJust $ Z.mkZipper xs
                    zDown = Z.zipperDown z -- Move down first so up is possible
                    idx1 = Z.zipperFocusIndex zDown
                    zDownUp = Z.zipperUp zDown
                    idx2 = Z.zipperFocusIndex zDownUp
                in idx1 > 0 && idx2 == idx1 - 1

    describe "findAndFocus" $ do
        it "focuses on the element if found" $
            property $ \(NonEmpty xs :: NonEmptyList Int) ->
                forAll (elements xs) $ \target ->
                    let z = fromJust $ Z.mkZipper xs
                        zFocused = Z.findAndFocus (== target) z
                    in Z.focus zFocused == target

        it "doesn't change the zipper if element is not found" $
             property $ \(NonEmpty xs :: NonEmptyList Int) (target :: Int) ->
                target `notElem` xs ==>
                    let z = fromJust $ Z.mkZipper xs
                        zFocused = Z.findAndFocus (== target) z
                    in zFocused == z