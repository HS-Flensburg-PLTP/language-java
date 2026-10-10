{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-unused-binds #-}
module Language.Java.Lexer (L1(..), Token(..), lexer) where

import Numeric
import Data.Char
}

%wrapper "posn"

$digit       = [0-9]
$digitUnder  = [0-9_]
$nonzero     = [1-9]
$octdig      = [0-7]
$hexdig      = [0-9A-Fa-f]
$hexdigUnder = [0-9A-Fa-f_]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$javaLetter = [a-zA-Z\_\$]
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

-- result type is TokResult
tokens  :-

    $white+         ;
    @comm           ;

    abstract        { plainTok KW_Abstract     }
    assert          { plainTok KW_Assert       }
    boolean         { plainTok KW_Boolean      }
    break           { plainTok KW_Break        }
    byte            { plainTok KW_Byte         }
    case            { plainTok KW_Case         }
    catch           { plainTok KW_Catch        }
    char            { plainTok KW_Char         }
    class           { plainTok KW_Class        }
    const           { plainTok KW_Const        }
    continue        { plainTok KW_Continue     }
    default         { plainTok KW_Default      }
    do              { plainTok KW_Do           }
    double          { plainTok KW_Double       }
    else            { plainTok KW_Else         }
    enum            { plainTok KW_Enum         }
    extends         { plainTok KW_Extends      }
    final           { plainTok KW_Final        }
    finally         { plainTok KW_Finally      }
    float           { plainTok KW_Float        }
    for             { plainTok KW_For          }
    goto            { plainTok KW_Goto         }
    if              { plainTok KW_If           }
    implements      { plainTok KW_Implements   }
    import          { plainTok KW_Import       }
    instanceof      { plainTok KW_Instanceof   }
    int             { plainTok KW_Int          }
    interface       { plainTok KW_Interface    }
    long            { plainTok KW_Long         }
    native          { plainTok KW_Native       }
    new             { plainTok KW_New          }
    package         { plainTok KW_Package      }
    private         { plainTok KW_Private      }
    protected       { plainTok KW_Protected    }
    public          { plainTok KW_Public       }
    record          { plainTok KW_Record       }
    return          { plainTok KW_Return       }
    short           { plainTok KW_Short        }
    static          { plainTok KW_Static       }
    strictfp        { plainTok KW_Strictfp     }
    super           { plainTok KW_Super        }
    switch          { plainTok KW_Switch       }
    synchronized    { plainTok KW_Synchronized }
    this            { plainTok KW_This         }
    throw           { plainTok KW_Throw        }
    throws          { plainTok KW_Throws       }
    transient       { plainTok KW_Transient    }
    try             { plainTok KW_Try          }
    void            { plainTok KW_Void         }
    volatile        { plainTok KW_Volatile     }
    while           { plainTok KW_While        }

    0               { plainTok (IntTok 0)        }
    0 [lL]          { plainTok (LongTok 0)       }
    0 $digitUnder+               { \p s -> parseNum p readOct s IntTok }
    0 $digitUnder+ [lL]          { \p s -> parseNum p readOct (init s) LongTok }
    $nonzero $digitUnder*        { \p s -> parseNum p reads s IntTok }
    $nonzero $digitUnder* [lL]   { \p s -> parseNum p reads (init s) LongTok }
    0 [xX] $hexdig $hexdigUnder*         { \p s -> parseNum p readHex (drop 2 s) IntTok }
    0 [xX] $hexdig $hexdigUnder* [lL]    { \p s -> parseNum p readHex (init (drop 2 s)) LongTok }

    $digit $digitUnder* \. $digit? $digitUnder* @exponent? [dD]?    { \p s -> parseNum p readFloat (dropDoubleSuffix s) DoubleTok }
            \. $digit $digitUnder* @exponent? [dD]?                 { \p s -> parseNum p readFloat ('0':(dropDoubleSuffix s)) DoubleTok }
    $digit $digitUnder* \. $digit? $digitUnder* @exponent? [fF]     { \p s -> parseNum p readFloat (init s) FloatTok }
            \. $digit $digitUnder* @exponent? [fF]                  { \p s -> parseNum p readFloat ('0':(init s)) FloatTok }
    $digit $digitUnder* @exponent                                   { \p s -> parseNum p readFloat s DoubleTok }
    $digit $digitUnder* @exponent? [dD]                             { \p s -> parseNum p readFloat (init s) DoubleTok }
    $digit $digitUnder* @exponent? [fF]                             { \p s -> parseNum p readFloat (init s) FloatTok }
    0 [xX] $hexdig? $hexdigUnder* \.? $hexdig? $hexdigUnder* @pexponent [dD]?     { \p s -> readHexExp p (drop 2 (dropDoubleSuffix s)) DoubleTok }
    0 [xX] $hexdig? $hexdigUnder* \.? $hexdig? $hexdigUnder* @pexponent [fF]      { \p s -> readHexExp p (drop 2 (init s)) FloatTok }

    true            { plainTok (BoolTok True)    }
    false           { plainTok (BoolTok False)   }

    ' (@charEscape | ~[\\\']) '               { readCharTok }

    \" (@charEscape | ~[\\\"])* \"            { readStringTok }

    null            { plainTok NullTok }

    $javaLetter $javaLetterOrDigit*     { \p s -> L (mkPos p) $ Right $ IdentTok s }

    \(              { plainTok OpenParen       }
    \)              { plainTok CloseParen      }
    \[              { plainTok OpenSquare      }
    \]              { plainTok CloseSquare     }
    \{              { plainTok OpenCurly       }
    \}              { plainTok CloseCurly      }
    \;              { plainTok SemiColon       }
    \,              { plainTok Comma           }
    \.              { plainTok Period          }
    "->"            { plainTok LambdaArrow     }
    "::"            { plainTok MethodRefSep    }

    "="             { plainTok Op_Equal        }
    ">"             { plainTok Op_GThan        }
    "<"             { plainTok Op_LThan        }
    "!"             { plainTok Op_Bang         }
    "~"             { plainTok Op_Tilde        }
    "?"             { plainTok Op_Query        }
    ":"             { plainTok Op_Colon        }
    "=="            { plainTok Op_Equals       }
    "<="            { plainTok Op_LThanE       }
    ">="            { plainTok Op_GThanE       }
    "!="            { plainTok Op_BangE        }
    "&&"            { plainTok Op_AAnd         }
    "||"            { plainTok Op_OOr          }
    "++"            { plainTok Op_PPlus        }
    "--"            { plainTok Op_MMinus       }
    "+"             { plainTok Op_Plus         }
    "-"             { plainTok Op_Minus        }
    "*"             { plainTok Op_Star         }
    "/"             { plainTok Op_Slash        }
    "&"             { plainTok Op_And          }
    "|"             { plainTok Op_Or           }
    "^"             { plainTok Op_Caret        }
    "%"             { plainTok Op_Percent      }
    "<<"            { plainTok Op_LShift       }
    "+="            { plainTok Op_PlusE        }
    "-="            { plainTok Op_MinusE       }
    "*="            { plainTok Op_StarE        }
    "/="            { plainTok Op_SlashE       }
    "&="            { plainTok Op_AndE         }
    "|="            { plainTok Op_OrE          }
    "^="            { plainTok Op_CaretE       }
    "%="            { plainTok Op_PercentE     }
    "<<="           { plainTok Op_LShiftE      }
    ">>="           { plainTok Op_RShiftE      }
    ">>>="          { plainTok Op_RRShiftE     }
    "@"             { plainTok Op_AtSign       }


{

type GenTokResult a = L (Either String a)
type TokResult = GenTokResult Token

plainTok :: Token -> AlexPosn -> String -> TokResult
plainTok t p _ = L (mkPos p) $ Right t

dropDoubleSuffix :: String -> String
dropDoubleSuffix s =
  case reverse s of
    'd':rest -> reverse rest
    'D':rest -> reverse rest
    _ -> s

removeUnder :: String -> String
removeUnder = filter (/= '_')

parseNum :: (Eq a, Num a) => AlexPosn -> (String -> [(a, String)]) -> String -> (a -> b) -> GenTokResult b
parseNum p f s mkTok =
  case f (removeUnder s) of
    [(x, "")] -> L (mkPos p) (Right (mkTok x))
    _ -> lexicalError p $ "invalid numeric literal: " ++ s

readHexExp :: (Floating a, Eq a) => AlexPosn -> String -> (a -> Token) -> TokResult
readHexExp p initial mkTok =
    case readHex (removeUnder initial) of
      [(m, suf)] ->
          case suf of
            q:s | toLower q == 'p' ->
                case parseNum p readHex s id of
                  L _ (Right n) -> L (mkPos p) (Right (mkTok (m ** n)))
                  _  -> lexicalError p $ "invalid numeric literal: " ++ initial
            _  -> lexicalError p $ "invalid numeric literal: " ++ initial
      _ -> lexicalError p $ "invalid numeric literal: " ++ initial

readCharTok :: AlexPosn -> String -> TokResult
readCharTok p s =
    L (mkPos p) $
    case convChar (dropQuotes s) of
      Left err -> Left err
      Right (c:_) -> Right (CharTok c)
      Right [] -> Left ("invalid char literal " ++ s)

readStringTok :: AlexPosn -> String -> TokResult
readStringTok p s =
  L (mkPos p) $
  case convChar (dropQuotes s) of
    Left err -> Left err
    Right s -> Right (StringTok s)

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> Either String String
convChar s = loop s ""
  where
    loop ('\\':'u':s@(d1:d2:d3:d4:s')) acc =
      -- TODO: this is the wrong place for handling unicode escapes
      -- according to the Java Language Specification. Unicode escapes can
      -- appear anywhere in the source text, and are best processed
      -- before lexing.
      if all isHexDigit [d1,d2,d3,d4]
      then loop s' (toEnum (read ['0','x',d1,d2,d3,d4]) : acc)
      else Left $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
    loop ('\\':'u':s) _ =
      Left $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
    loop ('\\':c:s) acc =
      if isOctDigit c
      then convOctal maxRemainingOctals
      else case c of
             'b' -> loop s ('\b' : acc)
             'f' -> loop s ('\f' : acc)
             'n' -> loop s ('\n' : acc)
             'r' -> loop s ('\r' : acc)
             't' -> loop s ('\t' : acc)
             '\'' -> loop s ('\'' : acc)
             '\\' -> loop s ('\\' : acc)
             '"' -> loop s ('"' : acc)
             _ -> Left $ "bad escape \"\\" ++ c:"\""
      where maxRemainingOctals =
              if c <= '3' then 2 else 1
            convOctal n =
              let octals = takeWhile isOctDigit $ take n s
                  noctals = length octals
                  toChar = toEnum . fst . head . readOct
              in loop (drop noctals s) (toChar (c:octals) : acc)
    loop ("\\") _ =
      Left ("bad escape \"\\\"")
    loop (x:s) acc = loop s (x : acc)
    loop "" acc = Right (reverse acc)

lexicalError :: AlexPosn -> String -> GenTokResult a
lexicalError p s = L (mkPos p) (Left ("lexical error: " ++ s))

data L a = L Pos a
  deriving (Show, Eq)

{-|
  The 'L1' datatype takes the position of a token 'Pos' and the length of that token as Int.
  Unlike using 'L' the length can be used to calculate the end location of the token in the parser.
 -}
data L1 a = L1 Pos Int a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

mkPos :: AlexPosn -> Pos
mkPos (AlexPn _ l c) = (l,c)

data Token
    -- Keywords
    = KW_Abstract
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Record
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period
    | LambdaArrow
    | MethodRefSep

    -- Literals
    | IntTok  Integer
    | LongTok Integer
    | DoubleTok Double
    | FloatTok Double
    | CharTok Char
    | StringTok String
    | BoolTok Bool
    | NullTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | Op_Equal
    | Op_GThan
    | Op_LThan
    | Op_Bang
    | Op_Tilde
    | Op_Query
    | Op_Colon
    | Op_Equals
    | Op_LThanE
    | Op_GThanE
    | Op_BangE
    | Op_AAnd
    | Op_OOr
    | Op_PPlus
    | Op_MMinus
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_And
    | Op_Or
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_PlusE
    | Op_MinusE
    | Op_StarE
    | Op_SlashE
    | Op_AndE
    | Op_OrE
    | Op_CaretE
    | Op_PercentE
    | Op_LShiftE
    | Op_RShiftE
    | Op_RRShiftE
    | Op_AtSign
  deriving (Show, Eq)

lexer :: String -> Either (Int, Int, String) [L1 Token]
lexer str = go (alexStartPos, '\n', [], str) []
  where
    go :: AlexInput -> [L1 Token] -> Either (Int, Int, String) [L1 Token]
    go inp@(p, _, _, str) acc =
      case alexScan inp 0 of
        AlexEOF -> Right (reverse acc)
        AlexSkip inp' len -> go inp' acc
        AlexToken inp' len act ->
            case act p (take len str) of
              L (line, col) (Left err) -> Left (line, col, err)
              L pos (Right x) -> go inp' (L1 pos len x : acc)
        AlexError (p, _, _, _) ->
          let (line, col) = mkPos p
          in Left (line, col, "lexical error")

}
