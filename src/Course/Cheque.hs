{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

import Course.Parser
import Course.MoreParser

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded)

instance Show Digit where
    show = hlist . showDigit

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _ = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars amount = unwrapResult $ output <$> parse dollarParser amount
    where
        output ("1", "01") = "one dollar and one cent"
        output ("1", right) = "one dollar and " ++ toEnglish right ++ " cents"
        output (left, right) = toEnglish left ++ " dollars and " ++ toEnglish right ++ " cents"

toEnglish :: Chars -> Chars
toEnglish xs = unwords $ reverse $ filter (not . isEmpty) $ map help $ zip (reverse $ groupDigits xs) illion
    where
        help ((D3 Zero Zero Zero), _) = Nil
        help (x, Nil) = showDigit3 x
        help (x, y) = showDigit3 x ++ (' ':.Nil) ++ y

showDigit3 :: Digit3 -> Chars
showDigit3 (D1 x) = showDigit x
showDigit3 (D2 Zero x) = showDigit x
showDigit3 (D2 One Zero) = "ten"
showDigit3 (D2 One One) = "eleven"
showDigit3 (D2 One Two) = "twelve"
showDigit3 (D2 One Three) = "thirteen"
showDigit3 (D2 One Four) = "fourteen"
showDigit3 (D2 One Five) = "fifteen"
showDigit3 (D2 One Six) = "sixteen"
showDigit3 (D2 One Seven) = "seventeen"
showDigit3 (D2 One Eight) = "eighteen"
showDigit3 (D2 One Nine) = "nineteen"
showDigit3 (D2 Two Zero) = "twenty"
showDigit3 (D2 Two x) = "twenty-" ++ showDigit x
showDigit3 (D2 Three Zero) = "thirty"
showDigit3 (D2 Three x) = "thirty-" ++ showDigit x
showDigit3 (D2 Four Zero) = "forty"
showDigit3 (D2 Four x) = "forty-" ++ showDigit x
showDigit3 (D2 Five Zero) = "fifty"
showDigit3 (D2 Five x) = "fifty-" ++ showDigit x
showDigit3 (D2 Six Zero) = "sixty"
showDigit3 (D2 Six x) = "sixty-" ++ showDigit x
showDigit3 (D2 Seven Zero) = "seventy"
showDigit3 (D2 Seven x) = "seventy-" ++ showDigit x
showDigit3 (D2 Eight Zero) = "eighty"
showDigit3 (D2 Eight x) = "eighty-" ++ showDigit x
showDigit3 (D2 Nine Zero) = "ninety"
showDigit3 (D2 Nine x) = "ninety-" ++ showDigit x
showDigit3 (D3 Zero x y) = showDigit3 (D2 x y)
showDigit3 (D3 x Zero Zero) = showDigit x ++ " hundred"
showDigit3 (D3 x y z) = showDigit x ++ " hundred and " ++ showDigit3 (D2 y z)

unsafeToDigit :: Char -> Digit
unsafeToDigit = help . fromChar
    where
        help (Full x) = x
        help Empty = undefined

groupDigits :: Chars -> List Digit3
groupDigits = foldRight help Nil
    where
        help x Nil = D1 (unsafeToDigit x) :. Nil
        help x ((D1 y) :. xs) = D2 (unsafeToDigit x) y :. xs
        help x ((D2 y z) :. xs) = (D3 (unsafeToDigit x) y z) :. xs
        help x xs = D1 (unsafeToDigit x) :. xs

unwrapResult :: ParseResult a -> a
unwrapResult (Result _ x) = x
unwrapResult _ = undefined

dollarParser :: Parser (Chars, Chars)
dollarParser = convert <$> list1 digit <*> maybeP (is '.') <*> maybeP digit <*> maybeP digit <* eof
    where
        convert ds Empty _ _ = (ds, "00")
        convert ds (Full _) Empty Empty = (ds, "00")
        convert ds (Full _) (Full x) Empty = (ds, x:.'0':.Nil)
        convert ds (Full _) (Full x) (Full y) = (ds, x:.y:.Nil)

maybeP :: Parser a -> Parser (Optional a)
maybeP p = Full <$> p ||| valueParser Empty

maybesToList :: List (Optional a) -> List a
maybesToList Nil = Nil
maybesToList (Empty:.xs) = maybesToList xs
maybesToList ((Full x):.xs) = x :. maybesToList xs
