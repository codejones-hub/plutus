{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common functions for parsers of UPLC, PLC, and PIR.

module PlutusCore.ParserCommon where

import           Data.Char                  (isAlphaNum)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified PlutusCore                 as PLC
import           PlutusCore.Error           (ParseError (..))
import qualified PlutusCore.Parsable        as PLC
import           PlutusPrelude
import           Text.Megaparsec            hiding (ParseError, State, parse)
import qualified Text.Megaparsec            as Parsec
import           Text.Megaparsec.Char       (char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char.Lexer as Lex

import           Control.Monad.State        (MonadState (get, put), StateT, evalStateT)

import           Data.Proxy                 (Proxy (Proxy))


newtype ParserState = ParserState { identifiers :: M.Map T.Text PLC.Unique }
    deriving (Show)

-- data ParseError = UnknownBuiltinType T.Text
--                 | BuiltinTypeNotAStar T.Text
--                 | InvalidConstant T.Text T.Text
--                 deriving (Eq, Ord, Show)

-- instance HasErrorCode (ParseError ann) where
--       errorCode UnknownBuiltinType {}  = ErrorCode 5
--       errorCode BuiltinTypeNotAStar {} = ErrorCode 52
--       errorCode InvalidBuiltinConstant {}     = ErrorCode 4

type Error ann = Parsec.ParseError Char (ParseError ann)

instance ShowErrorComponent (ParseError ann) where
    showErrorComponent (LexErr str) =
        "Lexing error: " ++ str
    showErrorComponent (Unexpected tk) =
        "Unexpected token " ++ show tk
    showErrorComponent (UnknownBuiltinType ann ty) =
        "Unknown built-in type at " ++ show ann ++ " : " ++ T.unpack ty
    showErrorComponent (BuiltinTypeNotAStar ann ty) =
        "Expected a type of kind star (to later parse a constant), but got: "
        ++ T.unpack ty
        ++ " at "
        ++ show ann
    showErrorComponent (UnknownBuiltinFunction ann fn) =
        "Unknown built-in type at " ++ show ann ++ " : " ++ T.unpack fn
    showErrorComponent (InvalidBuiltinConstant ann ty con) =
        "Invalid constant at "
        ++ show ann
        ++ " : "
        ++ T.unpack con
        ++ " of type "
        ++ T.unpack ty

topSourcePos :: SourcePos
topSourcePos = initialPos "top"

initial :: ParserState
initial = ParserState M.empty

-- | Return the unique identifier of a name.
-- If it's not in the current parser state, map the name to a fresh id
-- and add it to the state. Used in the Name parser.
intern :: (MonadState ParserState m, PLC.MonadQuote m)
    => T.Text -> m PLC.Unique
intern n = do
    st <- get
    case M.lookup n (identifiers st) of
        Just u -> return u
        Nothing -> do
            fresh <- PLC.freshUnique
            let identifiers' = M.insert n fresh $ identifiers st
            put $ ParserState identifiers'
            return fresh

type Parser ann = ParsecT (ParseError ann) T.Text (StateT ParserState PLC.Quote)
instance (Stream s, PLC.MonadQuote m) => PLC.MonadQuote (ParsecT e s m)

parse :: Parser ann a -> String -> T.Text -> Either (ParseErrorBundle T.Text (ParseError ann)) a
parse p file str = PLC.runQuote $ parseQuoted p file str

parseQuoted :: Parser ann a -> String -> T.Text -> PLC.Quote
                   (Either (ParseErrorBundle T.Text (ParseError ann)) a)
parseQuoted p file str = flip evalStateT initial $ runParserT p file str

whitespace :: Parser ann ()
whitespace = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockCommentNested "{-" "-}")

lexeme :: Parser ann a -> Parser ann a
lexeme = Lex.lexeme whitespace

symbol :: T.Text -> Parser ann T.Text
symbol = Lex.symbol whitespace

lparen :: Parser ann T.Text
lparen = symbol "("
rparen :: Parser ann T.Text
rparen = symbol ")"

lbracket :: Parser ann T.Text
lbracket = symbol "["
rbracket :: Parser ann T.Text
rbracket = symbol "]"

lbrace :: Parser ann T.Text
lbrace = symbol "{"
rbrace :: Parser ann T.Text
rbrace = symbol "}"

inParens :: Parser ann a -> Parser ann a
inParens = between lparen rparen

inBrackets :: Parser ann a -> Parser ann a
inBrackets = between lbracket rbracket

inBraces :: Parser ann a-> Parser ann a
inBraces = between lbrace rbrace

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

stringLiteral :: Parser ann String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- | Create a parser that matches the input word and returns its source position.
-- This is for attaching source positions to parsed terms/programs.
-- getSourcePos is not cheap, don't call it on matching of every token.
wordPos ::
    -- | The word to match
    T.Text -> Parser ann SourcePos
wordPos w = lexeme $ try $ getSourcePos <* symbol w

builtinFunction :: (Bounded fun, Enum fun, Pretty fun) => Parser fun ann
builtinFunction = lexeme $ choice $ map parseBuiltin [minBound .. maxBound]
    where parseBuiltin builtin = try $ string (display builtin) >> pure builtin

version :: Parser ann (PLC.Version SourcePos)
version = lexeme $ do
    p <- getSourcePos
    x <- Lex.decimal
    void $ char '.'
    y <- Lex.decimal
    void $ char '.'
    PLC.Version p x y <$> Lex.decimal

name :: Parser ann PLC.Name
name = lexeme $ try $ do
    void $ lookAhead letterChar
    str <- takeWhileP (Just "identifier") isIdentifierChar
    PLC.Name str <$> intern str

tyName :: Parser ann PLC.TyName
tyName = PLC.TyName <$> name

-- | Turn a parser that can succeed without consuming any input into one that fails in this case.
enforce :: Parser ann a -> Parser ann a
enforce p = do
    (input, x) <- match p
    guard . not $ T.null input
    pure x

-- | Consume a chunk of text and either stop on whitespace (and consume it) or on a \')\' (without
-- consuming it). We need this for collecting a @Text@ to pass it to 'PLC.parse' in order to
-- parse a built-in type or a constant. This is neither efficient (because of @manyTill anySingle@),
-- nor future-proof (what if some future built-in type has parens in its syntax?). A good way of
-- resolving this would be to turn 'PLC.parse' into a proper parser rather than just a function
-- from @Text@ - this will happen as SCP-2251 gets done.
-- Note that this also fails on @(con string \"yes (no)\")@ as well as @con unit ()@, so it really
-- should be fixed somehow.
-- (For @con unit ()@, @kwxm suggested replacing it with @unitval@ or @one@ or *)
closedChunk :: Parser ann T.Text
closedChunk = T.pack <$> manyTill anySingle end where
    end = enforce whitespace <|> void (lookAhead $ char ')')

-- | Parse a type tag by feeding the output of 'closedChunk' to 'PLC.parse'.
builtinTypeTag
    :: forall uni ann.
    PLC.Parsable (PLC.SomeTypeIn (PLC.Kinded uni))
    => Parser ann (PLC.SomeTypeIn (PLC.Kinded uni))
builtinTypeTag = do
    uniText <- closedChunk
    case PLC.parse uniText of
        Nothing  -> customFailure $ UnknownBuiltinType @ann uniText
        Just uni -> pure uni

-- | Parse a constant by parsing a type tag first and using the type-specific parser of constants.
-- Uses 'PLC.parse' under the hood for both types and constants.
-- @kwxm: this'll have problems with some built-in constants like strings and characters.
-- The existing parser has special cases involving complicated regular expressions
-- to deal with those (see Lexer.x), but things got more complicated recently when
-- @effectfully added built-in lists and pairs that can have other constants
-- nested inside them...We're probably still going to need special parsers
-- for things like quoted strings that can contain escape sequences.
-- @thealmarty will hopefully deal with these in SCP-2251.
constant
    :: forall uni ann.
       ( PLC.Parsable (PLC.SomeTypeIn (PLC.Kinded uni))
       , PLC.Closed uni, uni `PLC.Everywhere` PLC.Parsable
       )
    => Parser ann (PLC.Some (PLC.ValueOf uni))
constant = do
    -- We use 'match' for remembering the textual representation of the parsed type tag,
    -- so that we can show it in the error message if the constant fails to parse.
    (uniText, PLC.SomeTypeIn (PLC.Kinded uni)) <- match builtinTypeTag
    -- See Note [Decoding universes].
    case PLC.checkStar @uni uni of
        Nothing -> customFailure $ BuiltinTypeNotAStar ann uniText
        Just PLC.Refl -> do
            conText <- closedChunk
            case PLC.bring (Proxy @PLC.Parsable) uni $ PLC.parse conText of
                Nothing  -> customFailure $ InvalidBuiltinConstant @ann uniText conText
                Just con -> pure . PLC.Some $ PLC.ValueOf uni con

