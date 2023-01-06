-- This parser for Scheme numbers has been copied from Husk Scheme.
-- https://github.com/justinethier/husk-scheme/blob/a39da0b385597264d3e5e11c09a907eef3d3db42/hs-src/Language/Scheme/Parser.hs
--
-- TODO: Write a custom parser for Scheme numbers which is closer to the
-- R7RS formal specification provided in section 7.1 (Formal Syntax).
--
-- Copyright (c) 2010 Justin Ethier
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

module SchemeDoc.Parser.Number (number) where

import Numeric
import Data.Complex
import Data.Ratio
import SchemeDoc.Types
import Text.ParserCombinators.Parsec

import qualified Data.Char as DC

-- Parse an integer in octal notation, base 8
parseOctalNumber :: Parser Sexp
parseOctalNumber = do
  _ <- try (string "#o")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01234567")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readOct num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readOct num)
     _ -> pzero

-- Parse an integer in binary notation, base 2
parseBinaryNumber :: Parser Sexp
parseBinaryNumber = do
  _ <- try (string "#b")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readInt 2 (`elem` "01") DC.digitToInt num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readInt 2 (`elem` "01") DC.digitToInt num)
     _ -> pzero

-- Parse an integer in hexadecimal notation, base 16
parseHexNumber :: Parser Sexp
parseHexNumber = do
  _ <- try (string "#x")
  sign <- many (oneOf "-")
  num <- many1 (digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readHex num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readHex num)
     _ -> pzero

-- Parser for Integer, base 10
parseDecimalNumber :: Parser Sexp
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 digit
  if (length sign) > 1
     then pzero
     else return $ (Number . read) $ sign ++ num

-- Parser for a base 10 Integer that will also
--  check to see if the number is followed by
--  an exponent (scientific notation). If so,
--  the integer is converted to a float of the
--  given magnitude.
parseDecimalNumberMaybeExponent :: Parser Sexp
parseDecimalNumberMaybeExponent = do
  num <- parseDecimalNumber
  parseNumberExponent num

-- Parse an integer in any base
parseNumber :: Parser Sexp
parseNumber = parseDecimalNumberMaybeExponent <|>
              parseHexNumber <|>
              parseBinaryNumber <|>
              parseOctalNumber <?>
              "Unable to parse number"

-- Parse a floating point number
parseRealNumber :: Parser Sexp
parseRealNumber = do
  sign <- many (oneOf "-+")
  num <- many digit
  _ <- char '.'
  frac <- many1 digit
  let dec = if not (null num)
               then num ++ "." ++ frac
               else "0." ++ frac
  f <- case (length sign) of
     0 -> return $ Float $ fst $ head (Numeric.readFloat dec)
          -- Bit of a hack, but need to support the + sign as well as the minus.
     1 -> if sign == "-" 
             then return $ Float $ (*) (-1.0) $ fst $ head (Numeric.readFloat dec)
             else return $ Float $ fst $ head (Numeric.readFloat dec)
     _ -> pzero
  parseNumberExponent f

--  Parse the exponent section of a floating point number
--   in scientific notation. Eg "e10" from "1.0e10"
parseNumberExponent :: Sexp -> Parser Sexp
parseNumberExponent n = do 
  expnt <- many $ oneOf "Ee"
  case (length expnt) of
    0 -> return n
    1 -> do
      num <- try parseDecimalNumber
      case num of
        Number nexp -> buildResult n nexp
        _ -> pzero
    _ -> pzero
 where 
  buildResult (Number num) nexp = return $ Float $ (fromIntegral num) * (10 ** (fromIntegral nexp))
  buildResult (Float num) nexp = return $ Float $ num * (10 ** (fromIntegral nexp))
  buildResult _ _ = pzero

-- Parse a rational number
parseRationalNumber :: Parser Sexp
parseRationalNumber = do
  pnumerator <- parseDecimalNumber
  case pnumerator of
    Number n -> do
      _ <- char '/'
      sign <- many (oneOf "-")
      num <- many1 digit
      if (length sign) > 1
         then pzero
         else do
             let pdenominator = read $ sign ++ num
             if pdenominator == 0
                then return $ Number 0 -- TODO: Prevents a div-by-zero error, but not really correct either
                else return $ Rational $ n % pdenominator
    _ -> pzero

-- Parse a complex number
parseComplexNumber :: Parser Sexp
parseComplexNumber = do
  lispreal <- (try parseRealNumber <|> try parseRationalNumber <|> parseDecimalNumber)
  let real = case lispreal of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0
  _ <- char '+'
  lispimag <- (try parseRealNumber <|> try parseRationalNumber <|> parseDecimalNumber)
  let imag = case lispimag of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0 -- Case should never be reached
  _ <- char 'i'
  return $ Complex $ real :+ imag

-- Parse a number
number :: Parser Sexp
number = try parseComplexNumber
     <|> try parseRationalNumber
     <|> try parseRealNumber
     <|> try parseNumber
