
module Language.LEF.Parser where

import Control.Applicative (optional)
import Control.Monad
import Data.Text (Text)
import Text.Parsec hiding (option, optional)
import Prelude hiding (null)

import Language.LEFDEF.Lexer
import Language.LEFDEF.Parser
import Language.LEF.Syntax



parseLEF :: Text -> Either ParseError LEF
parseLEF = parse lef [] . lexer []


lef :: Parser LEF
lef = LEF
  <$> many1 option
  <*> many layer
  <*> many via
  <*> many viaRule
  <*> spacing
  <*> many site
  <*> many1 macro
  <*  endLibrary
  <?> "lef"


option :: Parser Option
option
  =   version
  <|> cases
  <|> bitChars
  <|> divideChars
  <|> units
  <|> useMinSpacing
  <|> clearanceMeasure
  <|> manufacturingGrid
  <?> "option"


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
units = units_ >> Units <$> databaseList <* end_ <* units_
    <?> "units"

databaseList :: Parser DatabaseList
databaseList = database_ >> microns_ >> DatabaseList <$> integer
    <?> "database_list"

useMinSpacing :: Parser Option
useMinSpacing = useminspacing_ >> (obs_ <|> pin_) >> UseMinSpacing <$> boolean
    <?> "use_min_spacing"

clearanceMeasure :: Parser Option
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> ident
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> double
    <?> "manufacturing_grid"

layer :: Parser Layer
layer = Layer
  <$> layerName
  <*> many layerOption
  <*> (end_ *> ident)
  <?> "layer"

layerName :: Parser LayerName
layerName = layer_ *> ident <?> "layer_name"

layerOption :: Parser LayerOption
layerOption
  =   Type         <$> (type_        *> ident ) 
  <|> LayerSpacing <$> (spacing_     *> double) <*> many spacingOption
  <|> Direction    <$> (direction_   *> layerDirection)
  <|> Pitch        <$> (pitch_       *> double)
  <|> Offset       <$> (offset_      *> double <* optional double)
  <|> Thickness    <$> (thickness_   *> double)
  <|> Height       <$> (height_      *> double)
  <|> Width        <$> (width_       *> double)
  <|> Resistance   <$> (resistance_  *> optional ident) <*> double
  <|> SpacingTable <$> (spacingtable_ *> spacingTable)
  <|> Capacitance  <$> (capacitance_ *> ident ) <*> double
  <|> EdgeCapacitance <$> (edgecapacitance_ *> double)
  <?> "layer_option"

spacingOption :: Parser SpacingOption
spacingOption
  =   Range <$> (range_ *> double) <*> double
  <?> "spacing_option"

via :: Parser Via
via = via_ >> Via
  <$> viaName
  <*> many viaLayer
  <*> (end_ *> ident)
  <?> "via"

viaName :: Parser ViaName
viaName = ViaName <$> ident <*> ident <?> "via_name"

viaLayer :: Parser ViaLayer
viaLayer = ViaLayer
  <$> viaLayerName
  <*> many viaRect
  <?> "via_layer"

viaLayerName :: Parser ViaLayerName
viaLayerName = layer_ *> ident <?> "via_layer_name"

viaRect :: Parser ViaRect
viaRect = rect_ >> ViaRect
  <$> double
  <*> double
  <*> double
  <*> double
  <?> "via_rect"

viaRule :: Parser ViaRule
viaRule = ViaRule
  <$> viaRuleName
  <*> many viaRuleLayer
  <*> (end_ *> ident)
  <?> "via_rule"

viaRuleName :: Parser ViaRuleName
viaRuleName = viarule_ >> ViaRuleName
  <$> ident
  <*> ident
  <?> "via_rule_name"

viaRuleLayer :: Parser ViaRuleLayer
viaRuleLayer = ViaRuleLayer
  <$> viaRuleLayerName
  <*> many viaRuleLayerOption
  <?> "via_rule_layer"

viaRuleLayerName :: Parser ViaRuleLayerName
viaRuleLayerName = layer_ *> ident <?> "via_rule_layer_name"

viaRuleLayerOption :: Parser ViaRuleLayerOption
viaRuleLayerOption
  =   ViaRuleLayerOptionDirection     <$> (direction_ *> layerDirection)
  <|> ViaRuleLayerOptionWidth         <$> (width_     *> double) <*> (to_ *> double)
  <|> ViaRuleLayerOptionSpacing       <$> (spacing_   *> double) <*> (by_ *> double)
  <|> ViaRuleLayerOptionEnclosure     <$> (enclosure_ *> double) <*> double
  <|> ViaRuleLayerOptionOverhang      <$> (overhang_  *> double)
  <|> ViaRuleLayerOptionMetalOverhang <$> (metaloverhang_ *> double)
  <|> ViaRuleLayerOptionRect          <$> (rect_ *> double) <*> double <*> double <*> double
  <?> "via_rule_layer_option"


spacing :: Parser Spacing
spacing = spacing_ >> Spacing
  <$> many samenet
  <*  (end_ *> spacing_)
  <?> "spacing"

samenet :: Parser Samenet
samenet = samenet_ >> Samenet
  <$> ident
  <*> ident
  <*> double
  <*  optional stack_
  <?> "samenet" 


site :: Parser Site
site = Site
  <$> siteName
  <*> many siteOption
  <*> (end_ *> ident)
  <?> "site"

siteName :: Parser SiteName
siteName = site_ *> ident <?> "site_name"

siteOption :: Parser SiteOption
siteOption
  =   SiteClass    <$> (class_    *> ident )
  <|> SiteSymmetry <$> (symmetry_ *> ident ) <*> optional ident
  <|> SiteSize     <$> (size_     *> double) <*> (by_ *> double)
  <?> "site_option"

macro :: Parser Macro
macro = Macro
  <$> macroName
  <*> many macroOption
  <*> (end_ *> ident)
  <?> "macro"

macroName :: Parser MacroName
macroName = macro_ *> ident <?> "macro_name"

pinUsage :: Parser PinUsage
pinUsage
  =   Analog <$ analog_
  <|> Clock  <$ clock_
  <|> Ground <$ ground_
  <|> Power  <$ power_
  <|> Signal <$ signal_
  <?> "pin_usage"

macroOption :: Parser MacroOption
macroOption
  =   MacroClass    <$> (class_    *> ident ) <*> optional ident
  <|> MacroForeign  <$> (foreign_  *> ident ) <*> double <*> double
  <|> MacroOrigin   <$> (origin_   *> double) <*> double
  <|> MacroSize     <$> (size_     *> double) <*> (by_ *> double)
  <|> MacroSymmetry <$> (symmetry_ *> ident ) <*> optional ident <*> optional ident
  <|> MacroSite     <$> (site_     *> ident )
  <|> MacroPin      <$> (pin_ *> ident) <*> many macroPinOption <*> (end_ *> ident)
  <|> MacroObs      <$> (obs_ *> many macroObsInfo) <* end_
  <?> "macro_option"

macroObsInfo :: Parser MacroObsInfo
macroObsInfo
  =   MacroObsLayer <$> (layer_ *> ident) <*> many (polygon_ *> many1 double)
  <|> MacroObsRect  <$> (rect_ *> double) <*> double <*> double <*> double
  <?> "macro_obs_info"

macroPinOption :: Parser MacroPinOption
macroPinOption
  =   MacroPinName      <$> (pin_ *> ident)
  <|> MacroPinUse       <$> (use_ *> pinUsage)
  <|> MacroPinDirection <$> (direction_ *> portDirection) <*> optional ident
  <|> MacroPinShape     <$> (shape_ *> ident)
  <|> MacroPinPort      <$> (port_  *> many macroPinPortInfo) <* end_
  <|> MacroPinAntennaPartialMetalArea     <$> (antennapartialmetalarea_ *> double) <*> (layer_ *> ident)
  <|> MacroPinAntennaPartialMetalSideArea <$> (antennapartialmetalsidearea_ *> double) <*> (layer_ *> ident)
  <|> MacroPinAntennaGateArea             <$> (antennagatearea_ *> double)
  <|> MacroPinAntennaDiffArea             <$> (antennadiffarea_ *> double)
  <?> "macro_pin_option"

macroPinPortInfo :: Parser MacroPinPortInfo
macroPinPortInfo
  =   MacroPinPortLayer <$> (layer_ *> ident ) <*> many (polygon_ *> many1 double)
  <|> MacroPinPortRect  <$> (rect_  *> double) <*> double <*> double <*> double
  <|> MacroPinPortClass <$> (class_ *> ident )
  <|> MacroPinPortWidth <$> (width_ *> double)
  <|> MacroPinPortPath  <$> (path_  *> double) <*> double <*> double <*> double
  <?> "macro_pin_port_info"

endLibrary :: Parser ()
endLibrary = end_ *> library_

portDirection :: Parser PortDirection
portDirection
  =   InputOutput <$ inout_
  <|> Output <$ output_
  <|> Input  <$ input_
  <?> "port_direction"

layerDirection :: Parser LayerDirection
layerDirection
  =   Horizontal <$ horizontal_
  <|> Vertical   <$ vertical_
  <?> "layer_direction"

spacingTable :: Parser Table 
spacingTable = parallelrunlength_ >> Table
  <$> many1 double
  <*> many1 (width_ *> many1 double)
  <?> "spacing_table"

