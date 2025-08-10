{-# LANGUAGE OverloadedStrings #-}

module Gitrea.Store.Object (
  parseTree
  , parseCommit
  , parseObject
  , toCommit
  , Commit(..)
  , Object(..)
  , ObjectType(..)
  , Tree(..)
  , TreeEntry(..)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Gitrea.Common (eitherToMaybe, ObjectId)

data ObjectType = BTree | BCommit | BTag | BBlob deriving (Eq)

instance Show ObjectType where
  show BTree = "tree"
  show BCommit = "commit"
  show BTag = "tag"
  show BBlob = "blob"

data Object = Object {
  getBlobContent :: B.ByteString
  , objType :: ObjectType
  , sha :: ObjectId
} deriving (Eq, Show)

data Identity = Author B.ByteString B.ByteString | Commiter B.ByteString B.ByteString deriving (Eq, Show)

data Tree = Tree {
  getObjectId :: ObjectId
  , getEntries :: [TreeEntry]
} deriving (Eq, Show)

data TreeEntry = TreeEntry {
  getMode :: C.ByteString
  , getPath :: C.ByteString
  , getBlobSha :: C.ByteString
} deriving (Eq, Show)

data Commit = Commit {
  getTree :: B.ByteString
  , getParents :: [B.ByteString]
  , getSha :: B.ByteString
  , getAuthor :: Identity
  , getCommiter :: Identity
  , getMessage :: B.ByteString
} deriving (Eq, Show)

toCommit :: Object -> Maybe Commit
toCommit (Object content BCommit _) = parseCommit content
toCommit _ = Nothing

parseObject :: ObjectId -> C.ByteString -> Maybe Object
parseObject sha1 obj = eitherToMaybe $ parseOnly (objParser sha1) obj

objParser :: ObjectId -> Parser Object
objParser sha1 = do
  objType' <- string "commit" <|> string "tree" <|> string "blob" <|> string "tag"
  char ' '
  _size <- takeWhile isDigit
  nul
  content <- takeByteString
  return $ Object content (obj objType') sha1
  where obj "commit" = BCommit
        obj "tree" = BTree
        obj "tag" = BTag
        obj "blob" = BBlob
        obj _ = error "Invalid object type"

parseTree :: ObjectId -> C.ByteString -> Maybe Tree
parseTree sha' input = eitherToMaybe $ parseOnly (treeParser sha') input

parseCommit :: C.ByteString -> Maybe Commit
parseCommit = eitherToMaybe . parseOnly commitParser

treeParser :: ObjectId -> Parser Tree
treeParser sha' = do
  entries <- many' treeEntryParser
  return $ Tree sha' entries

treeEntryParser :: Parser TreeEntry
treeEntryParser = do
  mode <- takeTill (== ' ')
  space
  path <- takeTill (== '\0')
  nul
  sha' <- take 20
  return $ TreeEntry mode path sha'

commitParser :: Parser Commit
commitParser = do
  tree <- "tree " .*> take 40
  space
  parents <- many' parseParentCommit
  author <- "author " .*> parsePerson
  space
  commiter <- "committer " .*> parsePerson
  space
  space
  message <- takeByteString
  let author' = Author (getPersonName author) (getPersonEmail author)
      commiter' = Commiter (getPersonName commiter) (getPersonEmail commiter)
  return $ Commit tree parents B.empty author' commiter' message

parseParentCommit :: Parser C.ByteString
parseParentCommit = do
  parent <- "parent " .*> take 40
  space
  return parent

parsePerson :: Parser Person
parsePerson = do
  name <- takeWhile (/= '<')
  email <- "<" .*> takeWhile (/= '>') <*. ">"
  date <- takeTill (== '\n')
  return $ Person name email date

data Person = Person {
  getPersonName :: B.ByteString
  , getPersonEmail :: B.ByteString
  , getDate :: B.ByteString
} deriving (Show, Eq)

nul :: Parser Char
nul = satisfy (== '\0')