{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem9 where

import Bird.Parser
import Data.IntMap.Strict (findMax)

{-------------------------------------------------------------------------------

CS:3820 Fall 2020 Problem of the Week, Week 9
=============================================

This problem applies parser combinators to several parsing problems.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 9-1
-----------

The first problem is to parse strings of equal numbers of 'a's and 'b's.  For
example, the strings "aaabbb" and "ab" should parse, while the string "aaaabb"
does not.  You should include the empty string as having an equal number of 'a's
and 'b's.  Note that your parser may not consume all the input!  For example,
given a string "aaabbbb", your parser should consume "aaabbb" and leave the
final 'b' on the input.

-------------------------------------------------------------------------------}
--Compare function for recognizing values a and b
balanced :: Parser [Char]
balanced = option "" (do --Executes by comparing values and assigning the respective list to a temp variable
                x <- many(satisfy(== 'a')) --Check to see if value is satisfied by comparing to expected value 'a'
                y <- manyn(length x)(satisfy(== 'b')) --Check to see if value is satisfied by comparing to expected value 'b'
                return(x ++ y)) --Appends list x and y


-- >>> runParser balanced "aaabbb"
-- [("aaabbb","")]

-- >>> runParser balanced "aaaaaaabbbbbbbbb"
-- [("aaaaaaabbbbbbb","bb")]

-- >>> runParser balanced "ccdd"
-- [("","ccdd")]

-- >>> runParser balanced "aaaaaabbbbb"                          
-- [("","aaaaaabbbbb")]

{-------------------------------------------------------------------------------

Problem 9-2
-----------

The second problem is to parse strings of equal numbers of 'a's, 'b's, and 'c's.
For example, the strings "abc", "aaabbbccc", and "" all parse, while the string
"aaabbcc" and "aaabbbcc" do not.  This problem is broken down into two parts.
The first part is to write a parser combinator `manyn` that parses a fixed
number of repetitions of its input parser.  For example, the parser `manyn 3
(char 'a')` would succeed on input "aaa", but fail on input "aa".  The second
part is to write a parser `balanced3` that parses strings of a's, b's and c's.

-------------------------------------------------------------------------------}

manyn :: Int -> Parser a -> Parser [a]
manyn 0 p = return [] --Base case for n == 0
manyn n p = do --Execute
            a <- p --Assign value p to variable a
            b <- manyn (n-1) p --Assigns n-1 and p to variable b
            return(a:b) --Returns a as first element(base) then all values stored in b

--Function is the same as balanced but adds a third temp list to compare for c values
balanced3 :: Parser [Char]
balanced3 = option "" (do --Executes by comparing values and assigning the respective list to a temp variable
                    x <- many(satisfy(== 'a')) --Check to see if value is satisfied by comparing to expected value 'a'
                    y <- manyn(length x)(satisfy(== 'b')) --Check to see if value is satisfied by comparing to expected value 'b'
                    z <- manyn(length x)(satisfy(== 'c')) --Check to see if value is satisfied by comparing to expected value 'c'
                    return (x ++ y ++ z)) --Appends list x, y, and z together

-- >>> runParser balanced3 "aaabbbccc"
-- [("aaabbbccc","")]

-- >>> runParser balanced3 "aaaabbbccc"
-- []

-- >>> runParser balanced3 "aaabbbcccc"
-- [("aaabbbccc","c")]

-- >>> runParser balanced3 "dddeeefff"
-- [("","dddeeefff")]

{-------------------------------------------------------------------------------

Problem 9-3
-----------

The final task is to define a parser for floating point numbers, following this
pattern:

    (-)?{digit}*(.{digit}+)?

That is, the following are all valid examples of float point numbers:

    4
    4.1
    -4
    -4.1
    .2
    -.34
    -0
    -0.0

The following are not

    4.
    --4
    -
    -.

Your parser should return the value parsed, as a Haskell `Double`

-------------------------------------------------------------------------------}

--Helper function to parse right side of the float including the decimal syntax
rightInt :: Parser Double
rightInt = do --Execute
    parseDecimal <- many1(satisfy(== '.')) --Checks for decimal in input
    x <- many1 digit --Puts ints in temp list x
    return(newRightValue x / 10) --Takes the new right values that are now currently in x and divides them by 10
    where newRightValue [] = 0 --Base case for empty list
          newRightValue (y:ys) = fromIntegral y + newRightValue ys / 10 --Ties integrated value y with all of the new right values

--Helper function to parse left side of the float
leftInt :: Parser Double
leftInt = do --Execute
    x <- many1 digit --Puts ints in temp list x
    return(newLeftValue(reverse x)) --Reverses and returns the new left value of the float
    where newLeftValue [] = 0 --Base case for empty list
          newLeftValue (y:ys) = fromIntegral y + 10 * newLeftValue ys --Ties integrated value y with all of the new left values multiplied by 10

--Helper function to reassemble both sides of the float
bothSides :: Parser Double
bothSides = do --Execute
    x <- leftInt --Assigns the values from the left int helper function to x
    y <- rightInt --Assigns the values from the right int helper function to y
    return(x+y) --Combines the left and the right from left to right

float :: Parser Double
float = do --Execute (essentially negation function)
    negateInt <- option id (string "-" >> return negate) --Searches for a hyphen, if detected, negates the int
    outsideInts <- bothSides <|> rightInt <|> leftInt --compiles reassembled value 
    return(negateInt outsideInts) --Negates the int and returns it

-- >>> runParser float "25"
-- [(25.0,"")]

-- >>> runParser float "2.5"
-- [(2.5,"")]

-- >>> runParser float ".5"
-- [(0.5,"")]

-- >>> runParser float "-2.5"
-- [(-2.5,"")]

-- >>> runParser float "-.2551"
-- [(-0.2551,"")]

-- >>> runParser float "4."
-- [(4.0,".")]

-- >>> runParser float "-ab3"
-- []