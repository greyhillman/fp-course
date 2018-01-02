{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> FilePath -> IO (List Chars)
anagrams xs filename = help <$> (readFile filename)
    where
        help contents = filter (\x -> x `elem` (lines contents)) $ permutations xs

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase Nil Nil = True
equalIgnoringCase _   Nil = False
equalIgnoringCase Nil _   = False
equalIgnoringCase (l:.ls) (r:.rs) = toLower l == toLower r && equalIgnoringCase ls rs
