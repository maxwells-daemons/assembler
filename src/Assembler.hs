{-# LANGUAGE BinaryLiterals #-}
module Assembler where

import qualified Data.Map.Strict as Map
import Data.Char (toUpper)
import Data.Bits
import Data.Either
import Data.Int (Int8)
import Data.Map.Strict ((!))
import Data.Maybe
import Data.Word
import Numeric (showHex)
import System.IO
import Text.Parsec

import Opcodes

-- Top-level interface
assemble :: String -> Either ParseError [String]
assemble text = runParser (many (line vars labels)) 0 "(assembler)" text
  where allLines = lines text
        codeLines = filter isCode allLines
        vars = getVars allLines
        labels = getLabels codeLines

-- Code to extract variables and labels ahead of time
getVars :: [String] -> IntMap
getVars lines = Map.fromList (zip names [0..])
  where parseResults = map (parse variable "") lines
        names = rights parseResults

getLabels :: [String] -> IntMap
getLabels lines = snd (foldl accumulateLabel (0, Map.empty) lines)

accumulateLabel :: (Int, IntMap) -> String -> (Int, IntMap)
accumulateLabel (lines, labels) str = case (parse labelP "" str) of
    Left _ -> (lines + 1, labels)
    Right label -> (lines, Map.insert label lines labels)

-- Program element parsers
type StringParser = Parsec String () String

variable = (many1 alphaNum) <* (many1 space) <* string "DB" :: StringParser
labelP =  many1 alphaNum <* char ':' :: StringParser
comment = many1 (char ';') :: StringParser
nonCode = eof <|>
    (spaces *> (comment <|>  (try variable)) *> (manyTill anyChar eof) *> eof)

-- Instruction parsers
type InstructionParser = Parsec String Int String

line :: IntMap -> IntMap -> InstructionParser
line vars labels = (++) <$>
    (option "" (content vars labels)) <*> (manyTill anyChar (char '\n'))

content :: IntMap -> IntMap -> InstructionParser
content vars labels = try (labelI labels)
                  <|> try (variableI vars)
                  <|> try (absoluteJump labels)
                  <|> try nullary
                  <|> try (absoluteData vars)
                  <|> try immediateValue
                  <|> try (relativeJump labels)

anyKey :: (Map.Map String b) -> InstructionParser
anyKey codemap = choice [try (string x <* (space
                                       <|> char ';'
                                       <|> endOfLine
                                       <|> char ':')) | x <- Map.keys codemap]

nullary :: InstructionParser
nullary = do
    line <- getState
    putState (line + 1)
    let lineno = hex line 4

    many space
    opcode <- anyKey nullaryCodes
    let hexcode = (hex (nullaryCodes ! opcode) 4)

    return $ lineno ++ "  " ++ hexcode ++ "; " ++ opcode

absoluteData :: IntMap -> InstructionParser
absoluteData vars = do
    line <- getState
    putState (line + 1)
    let lineno = hex line 4

    many space
    opcode <- anyKey absoluteDataCodes
    let hexcode = (hex (absoluteDataCodes ! opcode) 2)

    innerSpace <- many1 space
    var <- anyKey vars
    let varAddr = hex (vars ! var) 2

    return $ lineno ++ "  " ++ hexcode ++ varAddr ++ "; " ++ opcode ++
        innerSpace ++ var

immediateValue :: InstructionParser
immediateValue = do
    line <- getState
    putState (line + 1)
    let lineno = hex line 4

    many space
    opcode <- anyKey immediateValueCodes
    let hexcode = (hex (immediateValueCodes ! opcode) 2)

    innerSpace <- many1 space
    char '$'
    literal <- many1 hexDigit
    let num = (read literal) :: Int

    return $ lineno ++ "  " ++ hexcode ++ (hex num 2) ++ "; " ++ opcode ++
        innerSpace ++ "$" ++ literal

relativeJump :: IntMap -> InstructionParser
relativeJump labels = do
    line <- getState
    putState (line + 1)
    let lineno = hex line 4

    many space
    opcode <- anyKey relativeJumpCodes
    let hexcode = (hex (relativeJumpCodes ! opcode) 2)

    innerSpace <- many1 space

    let labelParsers = [try (string x <* (space <|> char ';' <|> char '\n'))
                        | x <- Map.keys labels] :: [InstructionParser]

    label <- choice labelParsers
    let labelLine = labels ! label
    let offset = (fromIntegral $ labelLine - line - 1) :: Int8

    -- Convert through unsigned binary representation to handle negatives
    let unsignedOffset = (fromIntegral offset) :: Word8
    let hexOffset = hex unsignedOffset 2

    return $ lineno ++ "  " ++ hexcode ++ hexOffset ++ "; " ++ opcode ++
        innerSpace ++ label

absoluteJump :: IntMap -> InstructionParser
absoluteJump labels = do
    line <- getState
    putState (line + 1)
    let lineno = hex line 4

    many space
    string "JMP"

    innerSpace <- many1 space
    label <- anyKey labels
    let labelLine = labels ! label
    let address = (fromIntegral labelLine) :: Word16
    let hexcode = hex (address `setBit` 15 `setBit` 14) 4

    return $ lineno ++ "  " ++ hexcode ++ "; " ++ "JMP" ++
        innerSpace ++ label

variableI :: IntMap -> InstructionParser
variableI vars = do
    name <- anyKey vars
    let address = hex (vars ! name) 2
    return $ "; " ++ address ++ indent ++ "??" ++ indent ++ name

labelI :: IntMap -> InstructionParser
labelI labels = do
    name <- anyKey labels
    let address = hex (labels ! name) 4
    return $ ';' : address ++ "   " ++ name ++ ":"

-- Utilities
type IntMap = Map.Map String Int

hex :: (Integral a, Show a) => a -> Int -> String
hex num len =
    let rep = map toUpper ((showHex num) "")
        padding = len - length rep
     in (replicate padding '0') ++ rep

indent = "    "

doesParse :: (Parsec String () ()) -> String -> Bool
doesParse parser str = isRight (parse parser "" str)

isCode :: String -> Bool
isCode str = not (doesParse nonCode str)
