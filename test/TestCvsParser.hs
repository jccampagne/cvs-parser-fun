module TestCvsParser (tests) where

import Test.HUnit
import CvsParser
import Text.ParserCombinators.Parsec


sampleCommit1 =
    "----------------------------\n\
     \revision 1.3\n\
     \date: 2017-02-18 13:21:47 +0000;  author: jc;  state: Exp;  lines: +2 -0;  commitid: puTKUR3zNxli5rGz;\n\
     \Edit b\n"
testParseCommit =
    let e = Right $ CvsCommit "1.3" "2017-02-18 13:21:47 +0000" in
    let r = parse parseCommit "parse commit revision" sampleCommit1 in
    TestCase $ assertEqual "parseCommit" e r

tests :: Test
tests =
    TestList [ TestLabel "parseCommit" testParseCommit
             ]
