module CvsParser where

import Text.ParserCombinators.Parsec

data CvsCommit =
    CvsCommit { cCommitRevision :: String
              , cCommitDate     :: String
              -- , cCommitAuthor   :: String
              -- , cCommitState    :: String
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

parseCommit :: Parser CvsCommit
parseCommit = do
    parseCommitSeperator <?> "no sep?"
    revision <- parseCommitRevision <?> "no revision?"
    date <- parseCommitDate <?> "no date?"

    return $ CvsCommit revision date

