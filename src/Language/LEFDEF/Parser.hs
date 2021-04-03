{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.LEFDEF.Parser where

import Data.Text (Text)
import Text.Parsec hiding (option, optional)
import Text.Parsec.String (GenParser)
import Text.Parsec.Pos
import qualified Data.Attoparsec.Text as A

import Language.LEFDEF.Lexer
import Language.LEFDEF.Syntax



type Parser = GenParser (Lexer Token) ()


boolean :: Parser Bool
boolean = True <$ on_ <|> False <$ off_ <?> "boolean"


decimal :: Parser Decimal
decimal = do
  t <- number
  case A.parseOnly (A.scientific) t of
    Left ex -> fail ex
    Right r -> pure $! r
  <?> "decimal"


integer :: Parser Integer
integer = do
  t <- number
  case A.parseOnly (A.signed A.decimal) t of
     Left ex -> fail ex
     Right r -> pure $! r
  <?> "integer"



maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
    showT (L _ t) = show t
    posT  (L x _) = pos2sourcePos x
    testT (L _ t) = test t
    pos2sourcePos (l, c) = newPos "" l c


identifier :: Parser Name
identifier = maybeToken q
  where q (Tok_Identifier t) = Just t
        q _ = Nothing

number :: Parser Text
number = maybeToken q
  where q (Tok_Number t) = Just t
        q _ = Nothing

stringLiteral :: Parser Text
stringLiteral = maybeToken q
  where q (Tok_String t) = Just t
        q _ = Nothing


p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing

end_ = p Tok_End
library_ = p Tok_Library
version_ = p Tok_Version
edgecapacitance_ = p Tok_EdgeCapacitance
capacitance_ = p Tok_Capacitance
spacingtable_ = p Tok_Spacingtable
parallelrunlength_ = p Tok_Parallelrunlength
samenet_ = p Tok_Samenet
antennadiffarea_ = p Tok_Antennadiffarea
antennagatearea_ = p Tok_Antennagatearea
antennapartialmetalsidearea_ = p Tok_Antennapartialmetalsidearea
antennapartialmetalarea_ = p Tok_Antennapartialmetalarea
resistance_ = p Tok_Resistance
height_ = p Tok_Height
width_ = p Tok_Width
offset_ = p Tok_Offset
pitch_ = p Tok_Pitch
direction_ = p Tok_Direction
spacing_ = p Tok_Spacing
type_ = p Tok_Type
layer_ = p Tok_Layer
units_ = p Tok_Units
dividerchar_ = p Tok_DividerChar
microns_ = p Tok_Microns
database_ = p Tok_Database
busbitchars_ = p Tok_BusBitChars
namescasesensitive_ = p Tok_Namescasesensitive
pin_ = p Tok_Pin
obs_ = p Tok_Obs
useminspacing_ = p Tok_UseMinSpacing
rect_ = p Tok_Rect
metaloverhang_ = p Tok_MetalOverhang
by_ = p Tok_By
to_ = p Tok_To
viarule_ = p Tok_ViaRule
manufacturinggrid_ = p Tok_ManufacturingGrid
clearancemeasure_ = p Tok_ClearanceMeasure
symmetry_ = p Tok_Symmetry
stack_ = p Tok_Stack
class_ = p Tok_Class
size_ = p Tok_Size
site_ = p Tok_Site
use_ = p Tok_Use
origin_ = p Tok_Origin
foreign_ = p Tok_Foreign
macro_ = p Tok_Macro
on_ = p Tok_On
off_ = p Tok_Off
via_ = p Tok_Via
overhang_ = p Tok_Overhang
enclosure_ = p Tok_Enclosure
path_ = p Tok_Path
port_ = p Tok_Port
shape_ = p Tok_Shape
input_ = p Tok_Input
output_ = p Tok_Output
inout_ = p Tok_Inout
horizontal_ = p Tok_Horizontal
vertical_ = p Tok_Vertical
thickness_ = p Tok_Thickness
power_ = p Tok_Power
ground_ = p Tok_Ground
polygon_ = p Tok_Polygon
range_ = p Tok_Range
analog_ = p Tok_Analog
clock_ = p Tok_Clock
signal_ = p Tok_Signal
specialnets_ = p Tok_Specialnets
routed_ = p Tok_Routed
star_ = p Tok_Star
new_ = p Tok_New
net_ = p Tok_Net
nets_ = p Tok_Nets
row_ = p Tok_Row
pins_ = p Tok_Pins
placed_ = p Tok_Placed
fixed_ = p Tok_Fixed
plus_ = p Tok_Plus
source_ = p Tok_Source
dist_ = p Tok_Dist
minus_ = p Tok_Minus
components_ = p Tok_Components
tracks_ = p Tok_Tracks
do_ = p Tok_Do
step_ = p Tok_Step
diearea_ = p Tok_Diearea
lparen_ = p Tok_Lparen
rparen_ = p Tok_Rparen
design_ = p Tok_Design
distance_ = p Tok_Distance
gcellgrid_ = p Tok_Gcellgrid
vias_ = p Tok_Vias

