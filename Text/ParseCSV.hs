{-# Language OverloadedStrings #-}

-- Copyright 2012 UserEvents, Inc.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--    http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- Contributors:
--    Robin Bate Boerop <me@robinbb.com>

module Text.ParseCSV
   ( CSV
   , parseCSV
   ) where

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*), (*>), many)
import Data.Attoparsec.Text
import Data.Text (Text, concat)
import Data.Text hiding (takeWhile)

type CSV = [[Text]]

lineEnd :: Parser ()
lineEnd =
   (char '\n' >> return ()) <|> (string "\r\n" >> return ())
   <?> "end of line"

unquotedField :: Parser Text
unquotedField =
   takeWhile (\c -> c /= ',' && c /= '\n' && c /= '\r' && c /= '"')
   <?> "unquoted field"

insideQuotes :: Parser Text
insideQuotes =
   concat <$> many (dquotes <|> takeWhile (/= '"'))
   <?> "inside of double quotes"
   where
      dquotes =
         string "\"\"" >> return "\""
         <?> "paired double quotes"

quotedField :: Parser Text
quotedField =
   char '"' *> insideQuotes <* char '"'
   <?> "quoted field"

field :: Parser Text
field =
   quotedField <|> unquotedField
   <?> "field"

line :: Parser [Text]
line =
   field `sepBy1` char ','
   <?> "line"

csv :: Parser CSV
csv =
   line `sepBy1` lineEnd <* endOfInput
   <?> "CSV"

parseCSV :: Text -> Either String CSV
parseCSV =
   parseOnly csv
