{-# LANGUAGE OverloadedStrings #-}

module Language.DEF.Builder where

import Data.Foldable
import Data.Text.Lazy hiding (length, null)
import Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat hiding (Fixed)

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
builderDEF (DEF options area _ tracks components pins nets specialnets)
   = foldMap optionStatement options
  <> newline
  <> dieAreaStatement area
  <> newline
  <> foldMap trackStatement tracks
  <> newline
  <> "COMPONENTS " <> decimal (length components) <> " ;" <> newline
  <> foldMap componentStatement components
  <> "END COMPONENTS" <> newline
  <> newline
  <> "PINS " <> decimal (length pins) <> " ;" <> newline
  <> foldMap pinStatement pins
  <> "END PINS" <> newline
  <> newline
  <> "NETS " <> decimal (length nets) <> " ;" <> newline
  <> foldMap netStatement nets
  <> "END NETS" <> newline
  <> newline
  <> (if null specialnets then mempty else
     "SPECIALNETS " <> decimal (length specialnets) <> " ;" <> newline
  <> foldMap specialnetStatement specialnets
  <> "END SPECIALNETS" <> newline
  <> newline)

  <> "END DESIGN"
  <> newline


dieAreaStatement :: DieArea -> Builder
dieAreaStatement (DieArea (x1, y1) (x2, y2))
   = "DIEAREA"
  <> " ( " <> realFloat x1 <> " " <> realFloat y1 <> " )"
  <> " ( " <> realFloat x2 <> " " <> realFloat y2 <> " )"
  <> " ;"
  <> newline


optionStatement :: Option -> Builder
optionStatement    (Version x) = "VERSION " <> realFloat x <> " ;" <> newline
optionStatement (Cases x) | x = "NAMESCASESENSITIVE ON ;" <> newline
optionStatement      (Cases x) = "NAMESCASESENSITIVE OFF ;" <> newline
optionStatement (DivideChar x) = "DIVIDERCHAR \"" <> fromText x <> "\" ;" <> newline
optionStatement   (BitChars x) = "BUSBITCHARS \"" <> fromText x <> "\" ;" <> newline
optionStatement     (Design x) = "DESIGN " <> fromText x <> " ;" <> newline
optionStatement      (Units x) = "UNITS DISTANCE MICRONS " <> distanceList x <> " ;" <> newline
optionStatement _ = mempty


distanceList :: DistanceList -> Builder
distanceList (DistanceList x) = decimal x


trackStatement :: Track -> Builder
trackStatement (Track xy a b c l)
   = "TRACKS "  <> fromText xy <> " " <> realFloat a
   <> " DO "    <> decimal b
   <> " STEP "  <> realFloat c
   <> " LAYER " <> fromText l
   <> " ;" <> newline



componentStatement :: Component -> Builder
componentStatement (Component a b Nothing)
   = "- " <> fromText a <> " " <> fromText b 
   <> " ;" <> newline
componentStatement (Component a b (Just placed))
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



pinStatement :: Pin -> Builder
pinStatement (Pin a net _ layer placed)
  = "- " <> fromText a
  <> foldMap (mappend " + NET " . fromText) net
  <> foldMap (mappend (newline <> "  + ") . layerExpression) layer
  <> foldMap (mappend (newline <> "  + ") . placedExpression) placed
  <> " ;" <> newline


layerExpression :: Layer -> Builder
layerExpression (Layer l (x1, y1) (x2, y2))
   = "LAYER " <> fromText l
  <> " ( " <> decimal x1 <> " " <> decimal y1 <> " )"
  <> " ( " <> decimal x2 <> " " <> decimal y2 <> " )"



netStatement :: Net -> Builder
netStatement (Net n cs rt)
   = "- " <> fromText n
  <> mconcat [ newline <> "  ( " <> either p q c <> " )" | c <- cs ]
  <> foldMap (mappend (newline <> "+ ") . routedExpression) rt
  <> " ;" <> newline
  where p = mappend "PIN " . fromText
        q (x, y) = fromText x <> " " <> fromText y


specialnetStatement :: Specialnet -> Builder
specialnetStatement (Specialnet n rt)
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

