{
module Language.LEFDEF.Lexer
    ( Lexer (..)
    , Token (..)
    , lexer
    ) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.LEFDEF.Tokens
}

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

@nul_eof = \0 $any*

@preprocessor = \# .* @newline

$ident_start = [a-zA-Z_\@]
@ident_part  = [^\ ]
$const_part  = [A-Z_]

$digit     = [0-9]
$hex_digit = [0-9a-fA-F]
$sign      = [\+\-]

@int_suffix  = [uU][lL]? | [lL][uU]?
@real_suffix = [fFdDmM]
@exponent    = [eE] $sign? $digit+

@simple_escape  = \\ [0abfnrtv\'\"\\]
@hex_escape     = \\x $hex_digit{1,4}
@unicode_escape = \\u $hex_digit{4} | \\U $hex_digit{8}
@escapes        = @simple_escape | @hex_escape | @unicode_escape

@character          = [^\'\\] | @escapes
@string_character   = [^\"\\] | @escapes
@verbatim_character = $any # \" | \"\"

@history = [^\;] | @newline


tokens :-

$white+       ;
@comment      ;
@nul_eof      ;
@preprocessor ;

HISTORY @history* { textTok (Tok_History . T.drop 7) }


-- T.T
\; ;

\(             { constTok Tok_Lparen    }
\)             { constTok Tok_Rparen    }

\-             { constTok Tok_Minus     }
\+             { constTok Tok_Plus      }

\*             { constTok Tok_Star      }


-- Keywords
SPECIALNETS        { constTok Tok_Specialnets }
ROUTED             { constTok Tok_Routed   }
NEW                { constTok Tok_New      }
NET                { constTok Tok_Net      }
NETS               { constTok Tok_Nets      }
PINS               { constTok Tok_Pins      }
PLACED             { constTok Tok_Placed    }
COMPONENTS         { constTok Tok_Components }
TRACKS             { constTok Tok_Tracks    }
FIXED              { constTok Tok_Fixed     }
SOURCE             { constTok Tok_Source    }
SIGNAL             { constTok Tok_Signal    }
DIST               { constTok Tok_Dist      }
DO                 { constTok Tok_Do        }
BY                 { constTok Tok_By        }
ROW                { constTok Tok_Row       }
STEP               { constTok Tok_Step      }
DIEAREA            { constTok Tok_Diearea   }
DESIGN             { constTok Tok_Design    }
END                { constTok Tok_End       }
LIBRARY            { constTok Tok_Library   }
VERSION            { constTok Tok_Version   }
NAMESCASESENSITIVE { constTok Tok_Namescasesensitive }
BUSBITCHARS        { constTok Tok_BusBitChars }
DIVIDERCHAR        { constTok Tok_DividerChar }
UNITS              { constTok Tok_Units       }
DISTANCE           { constTok Tok_Distance    }
ON                 { constTok Tok_On          }
OFF                { constTok Tok_Off         }
MICRONS	           { constTok Tok_Microns     }
USEMINSPACING	   { constTok Tok_UseMinSpacing }
OBS	               { constTok Tok_Obs }
PIN	               { constTok Tok_Pin }
CLEARANCEMEASURE   { constTok Tok_ClearanceMeasure }
MANUFACTURINGGRID  { constTok Tok_ManufacturingGrid }
LAYER            { constTok Tok_Layer }
TYPE             { constTok Tok_Type }
SPACING          { constTok Tok_Spacing }
DIRECTION        { constTok Tok_Direction }
PITCH            { constTok Tok_Pitch }
OFFSET           { constTok Tok_Offset }
THICKNESS        { constTok Tok_Thickness }
PATH             { constTok Tok_Path }
HEIGHT           { constTok Tok_Height }
WIDTH            { constTok Tok_Width }
RESISTANCE       { constTok Tok_Resistance }
EDGECAPACITANCE  { constTok Tok_EdgeCapacitance }
CAPACITANCE      { constTok Tok_Capacitance }
VIA              { constTok Tok_Via }
VIAS             { constTok Tok_Vias }
RECT             { constTok Tok_Rect }
VIARULE	         { constTok Tok_ViaRule }
TO               { constTok Tok_To }
BY               { constTok Tok_By }
SAMENET          { constTok Tok_Samenet }
ENCLOSURE        { constTok Tok_Enclosure }
SPACINGTABLE     { constTok Tok_Spacingtable }
PARALLELRUNLENGTH { constTok Tok_Parallelrunlength }
OVERHANG         { constTok Tok_Overhang }
METALOVERHANG	 { constTok Tok_MetalOverhang }
SITE             { constTok Tok_Site }
SYMMETRY         { constTok Tok_Symmetry }
STACK            { constTok Tok_Stack }
CLASS            { constTok Tok_Class }
SIZE             { constTok Tok_Size }
MACRO            { constTok Tok_Macro }
FOREIGN	         { constTok Tok_Foreign }
ORIGIN           { constTok Tok_Origin }
USE              { constTok Tok_Use }
SHAPE            { constTok Tok_Shape }
PORT             { constTok Tok_Port }
LIBRARY          { constTok Tok_Library }
INPUT            { constTok Tok_Input }
OUTPUT           { constTok Tok_Output }
INOUT            { constTok Tok_Inout }
HORIZONTAL       { constTok Tok_Horizontal }
VERTICAL         { constTok Tok_Vertical }
POWER            { constTok Tok_Power }
power            { constTok Tok_Power }
GROUND           { constTok Tok_Ground }
ground           { constTok Tok_Ground }
POLYGON          { constTok Tok_Polygon }
RANGE            { constTok Tok_Range }
ANALOG           { constTok Tok_Analog }
CLOCK            { constTok Tok_Clock }
DATABASE         { constTok Tok_Database }
ANTENNAPARTIALMETALAREA      { constTok Tok_Antennapartialmetalarea }
ANTENNAPARTIALMETALSIDEAREA  { constTok Tok_Antennapartialmetalsidearea }
ANTENNAGATEAREA              { constTok Tok_Antennagatearea }
ANTENNADIFFAREA              { constTok Tok_Antennadiffarea }
GCELLGRID        { constTok Tok_Gcellgrid }

-- Integer literals
\-    $digit+     @int_suffix? { textTok Tok_Number }
      $digit+     @int_suffix? { textTok Tok_Number }

-- Real literals
\-  $digit+ \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
    $digit+ \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
            \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
               $digit+ @exponent  @real_suffix? { textTok Tok_Number }
               $digit+            @real_suffix  { textTok Tok_Number }

-- Character / String literals
\" @string_character* \"     { textTok (Tok_String . T.drop 1 . T.init)   }

-- Identifiers
$ident_start @ident_part*      { textTok Tok_Identifier }

{
wrap :: (str -> tok) -> AlexPosn -> str -> Lexer tok
wrap f (AlexPn _ line col) s = L (line, col) (f s)

constTok = wrap . const
bstrTok f = wrap (f . T.encodeUtf8)
textTok = wrap

lexer :: String -> T.Text -> [Lexer Token]
lexer file text = go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> act pos (T.take len cs) : go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
}
