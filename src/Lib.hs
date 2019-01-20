{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Text.XML.HXT.Core 
import Data.Text (Text, pack, unpack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Applicative

secret = "Ilovechoco123"

someFunc :: IO ()
someFunc = do
    conn <- connect defaultConnectInfo {
        connectHost = "kanjidb.postgres.database.azure.com",
        connectUser = "rashadg1030@kanjidb",
        connectPassword = secret,
        connectDatabase = "kanjidb"
    }

    kanjis <- runKanjiParser
    result <- executeMany conn "insert into kanjiTbl (literal, grade, strokes, jaon, jakun, def, nanori) values (?,?,?,?,?,?,?)" kanjis
    print result
    --return result

-- Kanji Data Type --
data Kanji = Kanji {
    literal :: Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: Text, -- "onyomi" of the kanji
    jaKun :: Text, -- "kunyomi" of the kanji
    def :: Text, -- definitions of the kanji
    nanori :: Text -- readings of the kanji used in names
} deriving (Generic, Show)

-- hash list of text to bytestring and store
-- write about this

instance ToJSON Kanji

instance FromJSON Kanji

instance ToRow Kanji 
-- Run Parser --
runKanjiParser :: IO [Kanji] 
runKanjiParser = do 
    kanjis <- runX (parseXML "kanjidic2.xml" >>> getKanjis)
    return kanjis

-- Parser --
getKanjis = atTag "kanjidic2" >>>
    proc root -> do
        char <- atTag "character" -< root 
        literal <- content <<< atTag "literal" -< char
        grade <- withDefault (content <<< atTag "grade") "0" -< char
        strokes <- withDefault (content <<< atTag "stroke_count") "0" -< char
        jaOn <- listA (content <<< atAttrVal "r_type" "ja_on") -< char
        jaKun <- listA (content <<< atAttrVal "r_type" "ja_kun") -< char
        def <- listA (content <<< atTag "meaning") -< char
        nanori <- listA (content <<< atTag "nanori") -< char
        returnA -< Kanji {
            literal = pack literal,
            grade = read grade,
            strokes = read strokes,
            jaOn = bling jaOn,
            jaKun = bling jaKun,
            def = bling def,
            nanori = bling nanori
        }

-- Helper Functions --
bling :: [String] -> Text
bling []     = ""
bling (x:xs) = (pack x) <> "|" <> bling xs

strip :: Text -> [String]
strip = (splitWhere '|') . unpack
        where
            splitWhere :: Char -> String -> [String]
            splitWhere _ "" = []
            splitWhere c s = first' $ gop c ([], "", s)

first' :: (a, b, c) -> a
first' (x, _, _) = x

gop :: Char -> ([String], String, String) -> ([String], String, String)
gop c (words, acc, "")           = (words, "", "")
gop c (words, acc, next@(h2:t2)) = if c == h2 then gop c (words ++ [acc], "", t2) else gop c (words, acc ++ [h2], t2)

parseXML file = readDocument [ withValidate no, 
                               withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)

atAttrVal a v = deep (isElem >>> hasAttrValue a (\x -> x == v))

content = getChildren >>> getText
