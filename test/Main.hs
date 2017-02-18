{-# Language ScopedTypeVariables #-}

import Test.HUnit
import TestCvsParser

main :: IO ()
main = do
    counts :: Counts <- runTestTT TestCvsParser.tests
    let x = show counts in
        return ()
