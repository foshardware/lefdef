{-# LANGUAGE OverloadedStrings #-}

module Language.DEF.Builder where

import Data.Foldable
import Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat hiding (Fixed)
import Data.Vector (Vector)

import Language.DEF.Syntax



defaultOptions :: Maybe Ident -> [Option]
defaultOptions ident =
  [ Version 5.6
  , Cases True
  , DivideChar "/"
  , BitChars "<>"
  ] ++
  [ Design i | i <- toList ident ] ++
  [ Units $ DistanceList 100 ]



printDEF :: DEF -> IO ()
printDEF = Text.putStr . toLazyText . builderDEF


builderDEF :: DEF -> Builder
builderDEF (DEF options histories area rows trs gcellgrids vias components pins nets specialnets)
   = foldMap option options
  <> newline
  <> foldMap history histories
  <> newline
  <> dieArea area
  <> newline
  <> foldMap row rows
  <> newline
  <> foldMap tracks trs
  <> newline
  <> foldMap gcellgrid gcellgrids
  <> newline
  <> section "VIAS" via vias
  <> newline
  <> section "COMPONENTS" component components
  <> newline
  <> section "PINS" pin pins
  <> newline
  <> section "NETS" net nets
  <> newline
  <> section "SPECIALNETS" specialnet specialnets
  <> newline
  <> "END DESIGN"
  <> newline


section :: Builder -> (a -> Builder) -> Vector a -> Builder
section _ _ xs | null xs = mempty
section title builder xs
   = title <> " " <> decimal (length xs) <> " ;" <> newline
  <> foldMap builder xs
  <> "END " <> title <> newline


history :: History -> Builder
history (History string)
   = "HISTORY"
  <> fromText string
  <> ";" <> newline


dieArea :: DieArea -> Builder
dieArea (DieArea (x1, y1) (x2, y2))
   = "DIEAREA"
  <> " ( " <> realFloat x1 <> " " <> realFloat y1 <> " )"
  <> " ( " <> realFloat x2 <> " " <> realFloat y2 <> " )"
  <> " ;"
  <> newline


option :: Option -> Builder
option    (Version x) = "VERSION " <> realFloat x <> " ;" <> newline
option (Cases x) | x = "NAMESCASESENSITIVE ON ;" <> newline
option      (Cases x) = "NAMESCASESENSITIVE OFF ;" <> newline
option (DivideChar x) = "DIVIDERCHAR \"" <> fromText x <> "\" ;" <> newline
option   (BitChars x) = "BUSBITCHARS \"" <> fromText x <> "\" ;" <> newline
option     (Design x) = "DESIGN " <> fromText x <> " ;" <> newline
option      (Units x) = "UNITS DISTANCE MICRONS " <> distanceList x <> " ;" <> newline
option _ = mempty


distanceList :: DistanceList -> Builder
distanceList (DistanceList x) = decimal x


row :: Row -> Builder
row (Row a b x y o c d e f)
  = "ROW " <> fromText a <> " " <> fromText b
  <> " " <> decimal x <> " " <> decimal y
  <> " " <> fromText o <> " DO " <> decimal c <> " BY " <> decimal d
  <> " STEP " <> decimal e <> " " <> decimal f
  <> " ;" <> newline


tracks :: Tracks -> Builder
tracks (Tracks xy a b c ls)
   = "TRACKS "  <> fromText xy <> " " <> realFloat a
   <> " DO "    <> decimal b
   <> " STEP "  <> realFloat c
   <> " LAYER" <> foldMap (mappend " " . fromText) ls
   <> " ;" <> newline


gcellgrid :: Gcellgrid -> Builder
gcellgrid (Gcellgrid xy a b c)
  = "GCELLGRID " <> fromText xy <> " " <> realFloat a
  <> " DO " <> decimal b
  <> " STEP " <> decimal c
  <> " ;" <> newline


via :: Via -> Builder
via (Via i rs)
  = "- " <> fromText i
  <> foldMap (mappend " + " . rect) rs
  <> " ;" <> newline


rect :: Rect -> Builder
rect (Rect l (x1, y1) (x2, y2))
  = "RECT " <> fromText l
  <> " ( " <> realFloat x1 <> " " <> realFloat y1 <> " )"
  <> " ( " <> realFloat x2 <> " " <> realFloat y2 <> " )"


component :: Component -> Builder
component (Component a b Nothing)
   = "- " <> fromText a <> " " <> fromText b 
   <> " ;" <> newline
component (Component a b (Just placed))
   = "- " <> fromText a <> " " <> fromText b 
   <> " + " <> placedExpression placed
   <> " ;" <> newline


placedExpression :: Placed -> Builder
placedExpression (Placed (x, y) o)
  = "PLACED" <> " ( " <> realFloat x <> " " <> realFloat y <> " ) " <> fromText o
placedExpression (Fixed (x, y) o)
  = "FIXED" <> " ( " <> realFloat x <> " " <> realFloat y <> " ) " <> fromText o
placedExpression Unplaced
  = "UNPLACED"



pin :: Pin -> Builder
pin (Pin a net dir layer placed)
  = "- " <> fromText a
  <> foldMap (mappend " + NET " . fromText) net
  <> foldMap (mappend " + " . directionExpression) dir
  <> foldMap (mappend (newline <> "  + ") . layerExpression) layer
  <> foldMap (mappend (newline <> "  + ") . placedExpression) placed
  <> " ;" <> newline


directionExpression :: Direction -> Builder
directionExpression Input  = "DIRECTION INPUT"
directionExpression Output = "DIRECTION OUTPUT"
directionExpression InputOutput = "DIRECTION INOUT"


layerExpression :: Layer -> Builder
layerExpression (Layer l (x1, y1) (x2, y2))
   = "LAYER " <> fromText l
  <> " ( " <> decimal x1 <> " " <> decimal y1 <> " )"
  <> " ( " <> decimal x2 <> " " <> decimal y2 <> " )"



net :: Net -> Builder
net (Net n cs rt)
   = "- " <> fromText n
  <> mconcat [ newline <> "  ( " <> either p q c <> " )" | c <- cs ]
  <> foldMap (mappend (newline <> "+ ") . routedExpression) rt
  <> " ;" <> newline
  where p = mappend "PIN " . fromText
        q (x, y) = fromText x <> " " <> fromText y


specialnet :: Specialnet -> Builder
specialnet (Specialnet n rt)
   = "- " <> fromText n
  <> foldMap (mappend (newline <> "+ ") . routedExpression) rt
  <> " ;" <> newline
  where p = mappend "PIN " . fromText
        q (x, y) = fromText x <> " " <> fromText y


routedExpression :: Routed -> Builder
routedExpression (Routed (x : xs))
   = "ROUTED " <> segExpression x
  <> foldMap (mappend (newline <> "  NEW ") . segExpression) xs
routedExpression _ = mempty


segExpression :: Segment Integer -> Builder
segExpression (Seg l n (x1, y1) xs i)
   = fromText l
  <> foldMap (mappend " " . decimal) n
  <> " ( " <> decimal x1 <> " " <> decimal y1 <> " )"
  <> foldMap (\ (x2, y2) -> " ( " <> maybe "*" decimal x2 <> " " <> maybe "*" decimal y2 <> " )") xs
  <> foldMap (mappend " " . fromText) i


newline :: Builder
newline = fromString "\n"

