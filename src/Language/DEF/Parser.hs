
module Language.DEF.Parser where

import Control.Applicative (optional)
import Data.Foldable
import Data.Text (Text)
import Data.Vector (Vector, replicateM)
import Text.Parsec hiding (option, optional)
import Prelude hiding (null)

import Language.LEFDEF.Lexer
import Language.LEFDEF.Parser
import Language.DEF.Syntax



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
  <$> tuple decimal
  <*> tuple decimal
  <?> "die_area"


tuple :: Parser a -> Parser (a, a)
tuple f = between lparen_ rparen_ ((,) <$> f <*> f) 
  <?> "tuple"


row :: Parser Row
row = row_ >> Row
  <$> identifier
  <*> identifier
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
  <*> decimal
  <*> (do_ *> integer)
  <*> (step_ *> decimal)
  <*> (layer_ *> many identifier)
  <?> "track"


gcellgrid :: Parser Gcellgrid
gcellgrid = gcellgrid_ >> Gcellgrid
  <$> xy
  <*> decimal
  <*> (do_ *> integer)
  <*> (step_ *> integer)
  <?> "gcellgrid"


xy :: Parser XY
xy = identifier <?> "xy"


vias :: Parser (Vector Via)
vias = section vias_ via <?> "vias"


via :: Parser Via
via = Via
  <$> identifier
  <*> (plus_ *> sepBy rect plus_)
  <?> "via"


rect :: Parser Rect
rect = rect_ >> Rect
  <$> identifier
  <*> tuple decimal
  <*> tuple decimal
  <?> "rect"


components :: Parser (Vector Component)
components = section components_ component <?> "components"


component :: Parser Component
component = Component
  <$> identifier
  <*> identifier
  <*> optional (plus_ *> placed)
  <*  optional (plus_ *> source_ *> dist_)
  <?> "component"


placed :: Parser Placed
placed
  =   (placed_ >> Placed
      <$> tuple decimal
      <*> orientation)
  <|> (fixed_ >> Fixed
      <$> tuple decimal
      <*> orientation)
  <?> "placed"


orientation :: Parser Orientation
orientation = identifier <?> "orientation"


pins :: Parser (Vector Pin)
pins = section pins_ pin <?> "pins"


layer :: Parser Layer
layer = layer_ >> Layer
  <$> identifier
  <*> tuple integer
  <*> tuple integer
  <?> "layer"


pin :: Parser Pin
pin = Pin
  <$> identifier
  <*> (optional plus_ *> optional (net_ *> identifier))
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
  <$> identifier
  <*> many (lparen_ *> contact <* rparen_)
  <*> (optional plus_ *> optional routed)
  <?> "net"


specialnet :: Parser Specialnet
specialnet = Specialnet
  <$> identifier
  <*> (optional plus_ *> optional routed) 
  <?> "specialnet"


contact :: Parser Contact
contact
  =   Left  <$> (pin_ *> identifier)
  <|> Right <$> ((,) <$> identifier <*> identifier)
  <?> "contact"


routed :: Parser Routed
routed = routed_ >> Routed
  <$> sepBy1 segment new_
  <?> "routed"


segment :: Parser (Segment Integer)
segment = Seg
  <$> identifier
  <*> optional integer
  <*> tuple integer
  <*> many (tuple (Nothing <$ star_ <|> Just <$> integer))
  <*> optional identifier
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
design = design_ >> Design <$> identifier
    <?> "design"

version :: Parser Option
version = version_ >> Version <$> decimal
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
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> identifier
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> decimal
    <?> "manufacturing_grid"


history :: Parser History
history = History <$> maybeToken q <?> "history"
  where q (Tok_History t) = Just t
        q _ = Nothing


endDesign :: Parser ()
endDesign = end_ *> design_
  <?> "end_design"

