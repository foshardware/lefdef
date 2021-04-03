
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
units = units_ >> Units <$> databaseList <* end_ <* units_
    <?> "units"

databaseList :: Parser DatabaseList
databaseList = database_ >> microns_ >> DatabaseList <$> integer
    <?> "database_list"

useMinSpacing :: Parser Option
useMinSpacing = useminspacing_ >> (obs_ <|> pin_) >> UseMinSpacing <$> boolean
    <?> "use_min_spacing"

clearanceMeasure :: Parser Option
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> identifier
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> decimal
    <?> "manufacturing_grid"

layer :: Parser Layer
layer = Layer
  <$> layerName
  <*> many layerOption
  <*> (end_ *> identifier)
  <?> "layer"

layerName :: Parser LayerName
layerName = layer_ *> identifier <?> "layer_name"

layerOption :: Parser LayerOption
layerOption
  =   Type         <$> (type_        *> identifier ) 
  <|> LayerSpacing <$> (spacing_     *> decimal) <*> many spacingOption
  <|> Direction    <$> (direction_   *> layerDirection)
  <|> Pitch        <$> (pitch_       *> decimal)
  <|> Offset       <$> (offset_      *> decimal <* optional decimal)
  <|> Thickness    <$> (thickness_   *> decimal)
  <|> Height       <$> (height_      *> decimal)
  <|> Width        <$> (width_       *> decimal)
  <|> Resistance   <$> (resistance_  *> optional identifier) <*> decimal
  <|> SpacingTable <$> (spacingtable_ *> spacingTable)
  <|> Capacitance  <$> (capacitance_ *> identifier) <*> decimal
  <|> EdgeCapacitance <$> (edgecapacitance_ *> decimal)
  <?> "layer_option"

spacingOption :: Parser SpacingOption
spacingOption
  =   Range <$> (range_ *> decimal) <*> decimal
  <?> "spacing_option"

via :: Parser Via
via = via_ >> Via
  <$> viaName
  <*> many viaLayer
  <*> (end_ *> identifier)
  <?> "via"

viaName :: Parser ViaName
viaName = ViaName <$> identifier <*> identifier <?> "via_name"

viaLayer :: Parser ViaLayer
viaLayer = ViaLayer
  <$> viaLayerName
  <*> many viaRect
  <?> "via_layer"

viaLayerName :: Parser ViaLayerName
viaLayerName = layer_ *> identifier <?> "via_layer_name"

viaRect :: Parser ViaRect
viaRect = rect_ >> ViaRect
  <$> decimal
  <*> decimal
  <*> decimal
  <*> decimal
  <?> "via_rect"

viaRule :: Parser ViaRule
viaRule = ViaRule
  <$> viaRuleName
  <*> many viaRuleLayer
  <*> (end_ *> identifier)
  <?> "via_rule"

viaRuleName :: Parser ViaRuleName
viaRuleName = viarule_ >> ViaRuleName
  <$> identifier
  <*> identifier
  <?> "via_rule_name"

viaRuleLayer :: Parser ViaRuleLayer
viaRuleLayer = ViaRuleLayer
  <$> viaRuleLayerName
  <*> many viaRuleLayerOption
  <?> "via_rule_layer"

viaRuleLayerName :: Parser ViaRuleLayerName
viaRuleLayerName = layer_ *> identifier <?> "via_rule_layer_name"

viaRuleLayerOption :: Parser ViaRuleLayerOption
viaRuleLayerOption
  =   ViaRuleLayerOptionDirection     <$> (direction_ *> layerDirection)
  <|> ViaRuleLayerOptionWidth         <$> (width_     *> decimal) <*> (to_ *> decimal)
  <|> ViaRuleLayerOptionSpacing       <$> (spacing_   *> decimal) <*> (by_ *> decimal)
  <|> ViaRuleLayerOptionEnclosure     <$> (enclosure_ *> decimal) <*> decimal
  <|> ViaRuleLayerOptionOverhang      <$> (overhang_  *> decimal)
  <|> ViaRuleLayerOptionMetalOverhang <$> (metaloverhang_ *> decimal)
  <|> ViaRuleLayerOptionRect          <$> (rect_ *> decimal) <*> decimal <*> decimal <*> decimal
  <?> "via_rule_layer_option"


spacing :: Parser Spacing
spacing = spacing_ >> Spacing
  <$> many samenet
  <*  (end_ *> spacing_)
  <?> "spacing"

samenet :: Parser Samenet
samenet = samenet_ >> Samenet
  <$> identifier
  <*> identifier
  <*> decimal
  <*  optional stack_
  <?> "samenet" 


site :: Parser Site
site = Site
  <$> siteName
  <*> many siteOption
  <*> (end_ *> identifier)
  <?> "site"

siteName :: Parser SiteName
siteName = site_ *> identifier <?> "site_name"

siteOption :: Parser SiteOption
siteOption
  =   SiteClass    <$> (class_    *> identifier )
  <|> SiteSymmetry <$> (symmetry_ *> identifier ) <*> optional identifier
  <|> SiteSize     <$> (size_     *> decimal) <*> (by_ *> decimal)
  <?> "site_option"

macro :: Parser Macro
macro = Macro
  <$> macroName
  <*> many macroOption
  <*> (end_ *> identifier)
  <?> "macro"

macroName :: Parser MacroName
macroName = macro_ *> identifier <?> "macro_name"

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
  =   MacroClass    <$> (class_    *> identifier ) <*> optional identifier
  <|> MacroForeign  <$> (foreign_  *> identifier ) <*> decimal <*> decimal
  <|> MacroOrigin   <$> (origin_   *> decimal) <*> decimal
  <|> MacroSize     <$> (size_     *> decimal) <*> (by_ *> decimal)
  <|> MacroSymmetry <$> (symmetry_ *> identifier ) <*> optional identifier <*> optional identifier
  <|> MacroSite     <$> (site_     *> identifier )
  <|> MacroPin      <$> (pin_ *> identifier) <*> many macroPinOption <*> (end_ *> identifier)
  <|> MacroObs      <$> (obs_ *> many macroObsInfo) <* end_
  <?> "macro_option"

macroObsInfo :: Parser MacroObsInfo
macroObsInfo
  =   MacroObsLayer <$> (layer_ *> identifier) <*> many (polygon_ *> many1 decimal)
  <|> MacroObsRect  <$> (rect_ *> decimal) <*> decimal <*> decimal <*> decimal
  <?> "macro_obs_info"

macroPinOption :: Parser MacroPinOption
macroPinOption
  =   MacroPinName      <$> (pin_ *> identifier)
  <|> MacroPinUse       <$> (use_ *> pinUsage)
  <|> MacroPinDirection <$> (direction_ *> portDirection) <*> optional identifier
  <|> MacroPinShape     <$> (shape_ *> identifier)
  <|> MacroPinPort      <$> (port_  *> many macroPinPortInfo) <* end_
  <|> MacroPinAntennaPartialMetalArea     <$> (antennapartialmetalarea_ *> decimal) <*> (layer_ *> identifier)
  <|> MacroPinAntennaPartialMetalSideArea <$> (antennapartialmetalsidearea_ *> decimal) <*> (layer_ *> identifier)
  <|> MacroPinAntennaGateArea             <$> (antennagatearea_ *> decimal)
  <|> MacroPinAntennaDiffArea             <$> (antennadiffarea_ *> decimal)
  <?> "macro_pin_option"

macroPinPortInfo :: Parser MacroPinPortInfo
macroPinPortInfo
  =   MacroPinPortLayer <$> (layer_ *> identifier ) <*> many (polygon_ *> many1 decimal)
  <|> MacroPinPortRect  <$> (rect_  *> decimal) <*> decimal <*> decimal <*> decimal
  <|> MacroPinPortClass <$> (class_ *> identifier)
  <|> MacroPinPortWidth <$> (width_ *> decimal)
  <|> MacroPinPortPath  <$> (path_  *> decimal) <*> decimal <*> decimal <*> decimal
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
  <$> many1 decimal
  <*> many1 (width_ *> many1 decimal)
  <?> "spacing_table"

