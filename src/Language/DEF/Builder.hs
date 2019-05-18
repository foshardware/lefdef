{-# LANGUAGE OverloadedStrings #-}

module Language.DEF.Builder where

import Data.Text.Lazy hiding (length)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat

import Language.DEF.Syntax


buildDEF :: DEF -> Text 
buildDEF (DEF options area tracks components pins nets) = toLazyText
   $ foldMap optionStatement options
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



pinStatement :: Pin -> Builder
pinStatement (Pin a net layer placed)
  = "- " <> fromText a
  <> maybe mempty (mappend " + NET " . fromText) net
  <> maybe mempty (mappend (newline <> "  + ") . layerExpression) layer
  <> maybe mempty (mappend (newline <> "  + ") . placedExpression) placed
  <> " ;" <> newline


layerExpression :: Layer -> Builder
layerExpression (Layer l (x1, y1) (x2, y2))
   = "LAYER " <> fromText l
  <> " ( " <> decimal x1 <> " " <> decimal y1 <> " )"
  <> " ( " <> decimal x2 <> " " <> decimal y2 <> " )"



netStatement :: Net -> Builder
netStatement (Net n cs)
   = "- " <> fromText n
  <> mconcat [ newline <> "  ( " <> either p q c <> " )" | c <- cs ]
  <> " ;" <> newline
  where p = mappend "PIN " . fromText
        q (x, y) = fromText x <> " " <> fromText y


newline :: Builder
newline = fromString "\n"

