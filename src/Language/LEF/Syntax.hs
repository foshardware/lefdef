
module Language.LEF.Syntax where

import Data.Text
import Data.Fixed


type Name = Text

type Decimal = Fixed E12


data LEF = LEF [Option] [Layer] [Via] [ViaRule] Spacing [Site] [Macro]
  deriving (Eq, Show)

data Option
  = Version Decimal
  | Cases Bool
  | BitChars Name
  | DivideChar Name
  | Units DatabaseList
  | UseMinSpacing Bool
  | ClearanceMeasure Name
  | ManufacturingGrid Decimal
  deriving (Eq, Show)

newtype DatabaseList = DatabaseList Integer
  deriving (Eq, Show)

data Layer = Layer LayerName [LayerOption] Text
  deriving (Eq, Show)

type LayerName = Name

data LayerOption
  = Type Name
  | LayerSpacing Decimal [SpacingOption]
  | Direction LayerDirection
  | Pitch Decimal
  | Offset Decimal
  | Thickness Decimal
  | Height Decimal
  | Width Decimal
  | Resistance (Maybe Name) Decimal
  | Capacitance Name Decimal
  | EdgeCapacitance Decimal
  | SpacingTable Table
  deriving (Eq, Show)

data SpacingOption
  = Range Decimal Decimal
  deriving (Eq, Show)

data PortDirection = Input | Output | InputOutput
  deriving (Eq, Show)

data LayerDirection = Horizontal | Vertical
  deriving (Eq, Show)

data Via = Via ViaName [ViaLayer] Name
  deriving (Eq, Show)

data ViaName = ViaName Name Name
  deriving (Eq, Show)

data ViaLayer = ViaLayer ViaLayerName [ViaRect]
  deriving (Eq, Show)

type ViaLayerName = Name

data ViaRect = ViaRect Decimal Decimal Decimal Decimal
  deriving (Eq, Show)

data ViaRule = ViaRule ViaRuleName [ViaRuleLayer] Name
  deriving (Eq, Show)

data ViaRuleName = ViaRuleName Name Name
  deriving (Eq, Show)

data ViaRuleLayer = ViaRuleLayer ViaRuleLayerName [ViaRuleLayerOption]
  deriving (Eq, Show)

type ViaRuleLayerName = Name

data ViaRuleLayerOption
  = ViaRuleLayerOptionDirection LayerDirection
  | ViaRuleLayerOptionWidth Decimal Decimal
  | ViaRuleLayerOptionWidthDiscrete Decimal Integer
  | ViaRuleLayerOptionOverhang Decimal
  | ViaRuleLayerOptionEnclosure Decimal Decimal
  | ViaRuleLayerOptionOverhangDiscrete Integer
  | ViaRuleLayerOptionMetalOverhang Decimal
  | ViaRuleLayerOptionMetalOverhangDiscrete Integer
  | ViaRuleLayerOptionRect Decimal Decimal Decimal Decimal
  | ViaRuleLayerOptionSpacing Decimal Decimal
  deriving (Eq, Show)


data Spacing = Spacing [Samenet]
  deriving (Eq, Show)

data Samenet = Samenet Name Name Decimal
  deriving (Eq, Show)


data Site = Site SiteName [SiteOption] Name
  deriving (Eq, Show)

type SiteName = Name

data SiteOption
  = SiteClass Name
  | SiteSymmetry Name (Maybe Name)
  | SiteSize Decimal Decimal
  deriving (Eq, Show)

data Macro = Macro MacroName [MacroOption] Name
  deriving (Eq, Show)

data Table = Table [Decimal] [[Decimal]]
  deriving (Eq, Show)

type MacroName = Name

data PinUsage = Analog | Clock | Ground | Power | Signal
  deriving (Eq, Show)

data MacroOption
  = MacroClass Name (Maybe Name)
  | MacroForeign Name Decimal Decimal
  | MacroOrigin Decimal Decimal
  | MacroSize Decimal Decimal
  | MacroSymmetry Name (Maybe Name) (Maybe Name)
  | MacroSite Name
  | MacroPin Name [MacroPinOption] Name
  | MacroObs [MacroObsInfo]
  deriving (Eq, Show)

data MacroPinOption
  = MacroPinName Name
  | MacroPinUse PinUsage
  | MacroPinDirection PortDirection (Maybe Name)
  | MacroPinShape Name
  | MacroPinPort [MacroPinPortInfo]
  | MacroPinAntennaPartialMetalArea Decimal Name
  | MacroPinAntennaPartialMetalSideArea Decimal Name
  | MacroPinAntennaGateArea Decimal
  | MacroPinAntennaDiffArea Decimal
  deriving (Eq, Show)

data MacroPinPortInfo
  = MacroPinPortLayer Name [[Decimal]]
  | MacroPinPortRect Decimal Decimal Decimal Decimal
  | MacroPinPortClass Name
  | MacroPinPortWidth Decimal
  | MacroPinPortPath Decimal Decimal Decimal Decimal
  deriving (Eq, Show)

data MacroObsInfo
  = MacroObsLayer Name [[Decimal]]
  | MacroObsRect Decimal Decimal Decimal Decimal
  deriving (Eq, Show)


