module ATest where

import Test.HUnit
import Parser

dd = "read x ; if ((y + 1) == x) then write y else skip"
dc = "if ((y + 1) == x) then write y ; skip else skip"
dr = "skip ; skip"

snippet s = case parseStat s of 
    Right _ -> True
    Left _ -> False

aTest :: Test
aTest = TestCase (assertBool "first" (snippet dd))

bTest = TestCase (assertBool "first" (snippet dc))

cTest = TestCase (assertBool "first" (snippet dr))