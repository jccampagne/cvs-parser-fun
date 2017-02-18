module CvsParser where

import Text.ParserCombinators.Parsec

data CvsCommit =
    CvsCommit { cCommitRevision :: String
              , cCommitDate     :: String
              , cCommitAuthor   :: String
              , cCommitState    :: String
              -- , cCommitLines    :: (Int, Int)
              -- , cCommitId       :: String
              -- , cCommitLog      :: String
              }
    deriving (Eq, Show)

parseEOL :: Parser ()
parseEOL = do
    string "\n"
    return ()

parseCommitSeperator :: Parser ()
parseCommitSeperator = do
    string "----------------------------\n"
    return ()

parseCommitRevision :: Parser String
parseCommitRevision = do
    string "revision "
    revision <- many1 $ noneOf "\n"
    parseEOL
    return revision

parseCommitDate :: Parser String
parseCommitDate = do
    string "date: "
    d <- many1 (noneOf ";") <?> "missing ; in date"
    return d

parseCommitAuthor :: Parser String
parseCommitAuthor = do
    string "author: "
    d <- many1 (noneOf ";") <?> "missing ; in author"
    return d


parseSemicolonSpaces :: Parser ()
parseSemicolonSpaces = do
    char ';'
    many1 (char ' ')
    return ()

parseCommitState :: Parser String
parseCommitState = do
    string "state: "
    s <- many1 (noneOf ";")
    return s

parseCommit :: Parser CvsCommit
parseCommit = do
    parseCommitSeperator             <?> "missing dash seperator?"
    revision  <- parseCommitRevision <?> "missing revision?"
    date      <- parseCommitDate     <?> "missing date?"
    parseSemicolonSpaces             <?> "missing ;?"
    author    <- parseCommitAuthor   <?> "missing author?"
    parseSemicolonSpaces             <?> "missing ;?"
    state     <- parseCommitState    <?> "missing state?"
    return $ CvsCommit revision date author state

