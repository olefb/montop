module Zipper
  ( Zipper(..)
  , mkZipper
  , zipperUp
  , zipperDown
  , zipperFocus
  , zipperToList
  , findAndFocus
  , mapZipper
  , zipperPageDown
  , zipperPageUp
  , zipperFocusIndex
  ) where

import Data.List (findIndex, length)

-- | A Zipper for a list, maintaining a focused element.
-- Invariant: A Zipper cannot be constructed for an empty list.
-- 'before' contains elements before the focus, in reverse order (head is closest to focus).
-- 'after' contains elements after the focus, in normal order (head is closest to focus).
data Zipper a = Zipper
    { before :: [a] -- Elements before focus, reversed
    , focus  :: a   -- The focused element
    , after  :: [a] -- Elements after focus
    } deriving (Show, Eq)

-- | Creates a zipper from a list, focusing on the first element.
-- Returns Nothing if the list is empty.
mkZipper :: [a] -> Maybe (Zipper a)
mkZipper []     = Nothing
mkZipper (x:xs) = Just (Zipper [] x xs)

-- | Moves the focus up one element. Returns the original zipper if already at the top.
zipperUp :: Zipper a -> Zipper a
zipperUp z@(Zipper [] _ _) = z -- Already at the top
zipperUp (Zipper (b:bs) f as) = Zipper bs b (f:as)

-- | Moves the focus down one element. Returns the original zipper if already at the bottom.
zipperDown :: Zipper a -> Zipper a
zipperDown z@(Zipper _ _ []) = z -- Already at the bottom
zipperDown (Zipper bs f (a:as)) = Zipper (f:bs) a as

-- | Returns the currently focused element.
zipperFocus :: Zipper a -> a
zipperFocus = focus

-- | Reconstructs the original list from the zipper.
zipperToList :: Zipper a -> [a]
zipperToList (Zipper bs f as) = reverse bs ++ [f] ++ as

-- | Searches for an element matching the predicate and focuses it.
-- If not found, returns the original zipper.
findAndFocus :: (a -> Bool) -> Zipper a -> Zipper a
findAndFocus p z =
    let lst = zipperToList z
    in case findIndex p lst of
         Nothing -> z -- Not found, return original
         Just idx ->
           -- Calculate initial relative index (target index - current index)
           let currentIdx = zipperFocusIndex z
               relativeIdx = idx - currentIdx
           in go relativeIdx z -- Start recursion with relative index
    where
        -- go takes the relative steps needed (-ve = up, +ve = down)
        go :: Int -> Zipper a -> Zipper a
        go 0 z' = z' -- Target reached
        -- Use '_' for the unused focus element 'f'
        go n z'@(Zipper bs _ as)
          | n < 0 && not (null bs) = go (n + 1) (zipperUp z')   -- Need to move up
          | n > 0 && not (null as) = go (n - 1) (zipperDown z') -- Need to move down
          | otherwise              = z' -- Target is current focus or cannot move further

-- | Apply a function to every element in the zipper
mapZipper :: (a -> b) -> Zipper a -> Zipper b
mapZipper f (Zipper bs foc aft) = Zipper (map f bs) (f foc) (map f aft)

-- | Helper function to apply a function n times
applyN :: Int -> (a -> a) -> a -> a
applyN n f x | n <= 0 = x
             | otherwise = applyN (n-1) f (f x)

-- | Moves the focus up by 'n' elements, stopping at the top.
zipperPageUp :: Int -> Zipper a -> Zipper a
zipperPageUp n = applyN n zipperUp

-- | Moves the focus down by 'n' elements, stopping at the bottom.
zipperPageDown :: Int -> Zipper a -> Zipper a
zipperPageDown n = applyN n zipperDown

-- | Returns the 0-based index of the focus element in the list representation.
zipperFocusIndex :: Zipper a -> Int
zipperFocusIndex (Zipper bs _ _) = Data.List.length bs -- Since 'before' is reversed, its length is the index.