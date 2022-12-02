{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module HDL.Parser
    ( Pin(..)
    , Part(..)
    , Conn(..)
    , ConnSide(..)
    , ChipImpl(..)
    , Chip(..)
    , loadChip
    , loadAllChips
    , connTargetName
    , connValueName
    , pinName
    , connSideName) where

import           Control.Applicative (Applicative(liftA2), (<|>))
import           Control.Monad (void)
import           Data.Text (Text, pack)
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, between, choice, errorBundlePretty
                                , many, parse, try)
import           Text.Megaparsec.Char (char, digitChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Map (Map)
import qualified Data.Map as M
import           System.Directory (listDirectory)
import           System.FilePath (takeBaseName, takeExtension)

-- Basic types
type Name = Text

data Pin = PinSingle Name
         | PinMulti Name Int
  deriving (Show)

data ConnSide = Id Name
              | Index Name Int
              | Range Name Int Int
  deriving (Show)

data Conn = Conn { connTarget :: ConnSide, connValue :: ConnSide }
  deriving (Show)

data Part = Part { partName :: Name, partConns :: [Conn] }
  deriving (Show)

data ChipImpl = Parts [Part]
              | BuiltIn Name
  deriving (Show)

data Chip = Chip { chipName :: Name
                 , chipIns :: [Pin]
                 , chipOuts :: [Pin]
                 , chipImpl :: ChipImpl
                 }
  deriving (Show)

-- Helper
pinName :: Pin -> Name
pinName = \case
  PinSingle n  -> n
  PinMulti n _ -> n

connTargetName :: Conn -> Name
connTargetName = \case
  Conn (Id n) _        -> n
  Conn (Index n _) _   -> n
  Conn (Range n _ _) _ -> n

connValueName :: Conn -> Name
connValueName = \case
  Conn _ (Id n)        -> n
  Conn _ (Index n _)   -> n
  Conn _ (Range n _ _) -> n

connSideName :: ConnSide -> Name
connSideName (Id n) = n
connSideName (Index n _) = n
connSideName (Range n _ _) = n

-- Parser
type Parser = Parsec Void Text

skipComment :: Parser ()
skipComment =
  L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol skipComment

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipComment

identifier :: Parser Text
identifier = pack
  <$> lexeme
    (liftA2 (:) (letterChar <|> char '_')
     $ many (choice [letterChar, digitChar, char '_']))

number :: Parser Int
number = lexeme L.decimal

-- IN a, b[10]; OUT out, a[10];
parsePinDecl :: Parser Pin
parsePinDecl = do
  name <- identifier
  (PinMulti name <$> brackets number) <|> pure (PinSingle name)

parseConn :: Parser Conn
parseConn = do
  liftA2 Conn parseSide (symbol "=" *> parseSide)
  where
    parseSide = do
      name <- identifier
      choice
        [ try
          $ uncurry (Range name)
          <$> brackets (liftA2 (,) (number <* symbol "..") number)
        , try $ Index name <$> brackets number
        , pure $ Id name]

parsePart :: Parser Part
parsePart = liftA2 Part identifier
  $ parens (liftA2 (:) parseConn (many $ symbol "," *> parseConn))

parseChip :: Parser Chip
parseChip = do
  void skipComment
  void $ symbol "CHIP"
  name <- identifier
  braces
    $ do
      ins <- pins "IN"
      outs <- pins "OUT"
      impl <- BuiltIn name <$ (symbol "BUILTIN" *> symbol name *> symbol ";")
        <|> (symbol "PARTS:" *> (Parts <$> many (parsePart <* symbol ";")))
      return $ Chip name ins outs impl
  where
    pins typ = symbol typ
      *> liftA2 (:) parsePinDecl (many $ symbol "," *> parsePinDecl)
      <* symbol ";"

loadChip :: String -> IO Chip
loadChip name = do
  content <- readFile $ "chips/" <> name <> ".hdl"
  case parse parseChip name (pack content) of
    Right chip -> return chip
    Left e     -> fail (errorBundlePretty e)

loadAllChips :: IO (Map Name Chip)
loadAllChips = do
  fs <- listDirectory "chips"
  foldr
    (\f acc -> do
       chip <- loadChip $ takeBaseName f
       M.insert (pack . takeBaseName $ f) chip <$> acc)
    (pure M.empty)
    (filter ((==) ".hdl" . takeExtension) fs)