{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.DEF.Parser where

import Control.Applicative (optional)
import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, replicateM)
import Text.Parsec hiding (option, optional)
import Text.Parsec.String (GenParser)
import Text.Parsec.Pos
import Prelude hiding (null)

import Language.DEF.Lexer
import Language.DEF.Syntax



type Parser = GenParser (Lexer Token) ()


parseDEF :: Text -> Either ParseError DEF
parseDEF = parse def [] . lexer []


def :: Parser DEF
def = DEF
  <$> many1 option
  <*> many history
  <*> dieArea
  <*> many row
  <*> many tracks
  <*> many gcellgrid
  <*> (foldMap id <$> optional vias)
  <*> components
  <*> pins
  <*> nets
  <*> (foldMap id <$> optional specialnets)
  <*  endDesign
  <?> "def"


section :: Parser () -> Parser a -> Parser (Vector a)
section title parser = do
    title
    k <- fromIntegral <$> integer
    v <- replicateM k $ minus_ *> parser
    end_
    title
    pure v


dieArea :: Parser DieArea
dieArea = diearea_ >> DieArea
  <$> tuple double 
  <*> tuple double
  <?> "die_area"


tuple :: Parser a -> Parser (a, a)
tuple f = between lparen_ rparen_ ((,) <$> f <*> f) 
  <?> "tuple"


row :: Parser Row
row = row_ >> Row
  <$> ident
  <*> ident
  <*> integer
  <*> integer
  <*> orientation
  <*> (do_ *> integer)
  <*> (by_ *> integer)
  <*> (step_ *> integer)
  <*> integer
  <?> "row"


tracks :: Parser Tracks
tracks = tracks_ >> Tracks
  <$> xy
  <*> double
  <*> (do_ *> integer)
  <*> (step_ *> double)
  <*> (layer_ *> many ident)
  <?> "track"


gcellgrid :: Parser Gcellgrid
gcellgrid = gcellgrid_ >> Gcellgrid
  <$> xy
  <*> double
  <*> (do_ *> integer)
  <*> (step_ *> integer)
  <?> "gcellgrid"


xy :: Parser XY
xy = ident <?> "xy"


vias :: Parser (Vector Via)
vias = section vias_ via <?> "vias"


via :: Parser Via
via = Via
  <$> ident
  <*> (plus_ *> sepBy rect plus_)
  <?> "via"


rect :: Parser Rect
rect = rect_ >> Rect
  <$> ident
  <*> tuple double
  <*> tuple double
  <?> "rect"


components :: Parser (Vector Component)
components = section components_ component <?> "components"


component :: Parser Component
component = Component
  <$> ident
  <*> ident
  <*> optional (plus_ *> placed)
  <*  optional (plus_ *> source_ *> dist_)
  <?> "component"


placed :: Parser Placed
placed
  =   (placed_ >> Placed
      <$> tuple double
      <*> orientation)
  <|> (fixed_ >> Fixed
      <$> tuple double
      <*> orientation)
  <?> "placed"


orientation :: Parser Orient
orientation = ident <?> "orientation"


pins :: Parser (Vector Pin)
pins = section pins_ pin <?> "pins"


layer :: Parser Layer
layer = layer_ >> Layer
  <$> ident
  <*> tuple integer
  <*> tuple integer
  <?> "layer"


pin :: Parser Pin
pin = Pin
  <$> ident
  <*> (optional plus_ *> optional (net_ *> ident))
  <*> (optional plus_ *> optional direction <* optional plus_ <* optional use_ <* optional signal_)
  <*> (optional plus_ *> optional layer)
  <*> (optional plus_ *> optional placed)
  <?> "pin"


direction :: Parser Direction
direction = direction_ *> (Input <$ input_ <|> Output <$ output_)
  <?> "direction"


nets :: Parser (Vector Net)
nets = section nets_ net <?> "nets"


specialnets :: Parser (Vector Specialnet)
specialnets = section specialnets_ specialnet <?> "specialnets"


net :: Parser Net
net = Net
  <$> ident
  <*> many (lparen_ *> contact <* rparen_)
  <*> (optional plus_ *> optional routed)
  <?> "net"


specialnet :: Parser Specialnet
specialnet = Specialnet
  <$> ident
  <*> (optional plus_ *> optional routed) 
  <?> "specialnet"


contact :: Parser Contact
contact
  =   Left  <$> (pin_ *> ident)
  <|> Right <$> ((,) <$> ident <*> ident)
  <?> "contact"


routed :: Parser Routed
routed = routed_ >> Routed
  <$> sepBy1 segment new_
  <?> "routed"


segment :: Parser (Segment Integer)
segment = Seg
  <$> ident
  <*> optional integer
  <*> tuple integer
  <*> many (tuple (Nothing <$ star_ <|> Just <$> integer))
  <*> optional ident 
  <?> "segment"


option :: Parser Option
option
  =   version
  <|> cases
  <|> design
  <|> bitChars
  <|> divideChars
  <|> units
  <|> useMinSpacing
  <|> clearanceMeasure
  <|> manufacturingGrid
  <?> "option"


design :: Parser Option
design = design_ >> Design <$> ident
    <?> "design"

version :: Parser Option
version = version_ >> Version <$> double
    <?> "version"

cases :: Parser Option
cases = namescasesensitive_ >> Cases <$> boolean
    <?> "cases"

bitChars :: Parser Option
bitChars = busbitchars_ >> BitChars <$> stringLiteral
    <?> "bit_chars"

divideChars :: Parser Option
divideChars = dividerchar_ >> DivideChar <$> stringLiteral
    <?> "divide_char"

units :: Parser Option
units = units_ >> Units <$> distanceList
    <?> "units"

distanceList :: Parser DistanceList
distanceList = distance_ >> microns_ >> DistanceList <$> integer
    <?> "distance_list"

useMinSpacing :: Parser Option
useMinSpacing = useminspacing_ >> (obs_ <|> pin_) >> UseMinSpacing <$> boolean
    <?> "use_min_spacing"

clearanceMeasure :: Parser Option
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> ident
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> double
    <?> "manufacturing_grid"


history :: Parser History
history = History <$> maybeToken q <?> "history"
  where q (Tok_History t) = Just t
        q _ = Nothing


endDesign :: Parser ()
endDesign = end_ *> design_
  <?> "end_design"


boolean :: Parser Bool
boolean = True <$ on_ <|> False <$ off_ <?> "boolean"


double :: Parser Double
double = do
    t <- number
    case (T.head t, T.findIndex (== '.') t) of
      ('-', Nothing) -> pure $ fromIntegral $ negate $ numberValue $ T.tail t
      (_, Nothing) -> pure $ fromIntegral $ numberValue t
      ('-', Just i)
         | (a, b) <- T.splitAt i t -> pure
         $ fromIntegral (negate $ numberValue $ T.tail a)
         - fractionValue (T.tail b)
      (_, Just i)
         | (a, b) <- T.splitAt i t -> pure
         $ fromIntegral (numberValue a)
         + fractionValue (T.tail b)
  <?> "double"


integer :: Parser Integer
integer = do
    t <- number
    case T.head t of
      '-' -> pure $ fromIntegral $ negate $ numberValue $ T.tail t
      _   -> pure $ fromIntegral $ numberValue t
  <?> "integer"


numberValue :: Text -> Int
numberValue = T.foldl (\ x c -> 10 * x + digitToInt c) 0


fractionValue :: Text -> Double
fractionValue
    = uncurry (/)
    . bimap fromIntegral fromIntegral
    . T.foldl (\ (s, p) d -> (p * digitToInt d + s, p * 10)) (0, 1)
    . T.dropWhile (== '0')
    . T.reverse


maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident  t) = Just t
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
end_ = p Tok_End
lparen_ = p Tok_Lparen
rparen_ = p Tok_Rparen
library_ = p Tok_Library
version_ = p Tok_Version
capacitance_ = p Tok_Capacitance
resistance_ = p Tok_Resistance
width_ = p Tok_Width
offset_ = p Tok_Offset
pitch_ = p Tok_Pitch
direction_ = p Tok_Direction
spacing_ = p Tok_Spacing
signal_ = p Tok_Signal
type_ = p Tok_Type
layer_ = p Tok_Layer
units_ = p Tok_Units
design_ = p Tok_Design
dividerchar_ = p Tok_DividerChar
microns_ = p Tok_Microns
distance_ = p Tok_Distance
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
vias_ = p Tok_Vias
overhang_ = p Tok_Overhang
path_ = p Tok_Path
port_ = p Tok_Port
shape_ = p Tok_Shape
input_ = p Tok_Input
output_ = p Tok_Output
inout_ = p Tok_Inout
horizontal_ = p Tok_Horizontal
vertical_ = p Tok_Vertical
power_ = p Tok_Power
ground_ = p Tok_Ground
gcellgrid_ = p Tok_Gcellgrid
